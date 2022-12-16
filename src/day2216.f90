module day2216_mod
  use parse_mod, only : string_t, read_strings
  use djikstra_mod, only : djikstra_node_at, djikstra_node_ptr, djikstra_search
  implicit none

  integer, parameter :: MAXHASHVAL = 1000, MAX_PLAYERS=2
  type, extends(djikstra_node_at)::  state_t
    integer :: t = 0
    integer :: gain = 0
    logical :: opened(MAXHASHVAL) = .false.
    integer :: pos(MAX_PLAYERS)
    integer :: trg(MAX_PLAYERS) = 0
    integer :: eta(MAX_PLAYERS)
    integer :: flow = 0
    integer :: deadline = -1
    integer :: nc = -1
    integer, pointer :: aa(:,:) => null()
    integer, pointer :: dmap(:,:) => null()
    integer, pointer :: rate(:) => null()
    character(len=2), pointer :: label(:) => null()
  contains
    procedure :: istarget => state_istarget
    procedure :: isequal => state_isequal
    procedure :: nextngb => state_nextngb
    procedure :: moveto => state_moveto
    procedure :: onestep => state_onestep
    procedure :: print => state_print
    procedure :: wait => state_wait
  end type state_t

  integer, parameter :: DEADLINE_P1=30, DEADLINE_P2=26
contains

  subroutine day2216(file)
    character(len=*), intent(in) :: file


    integer, target :: aa(hash('ZZ'),hash('ZZ'))
    integer, target :: dmap(hash('ZZ'),hash('ZZ'))
    character(len=2), target :: label(hash('ZZ'))
    integer, target :: rate(hash('ZZ'))
    type(state_t) :: init, sout
    type(djikstra_node_ptr), allocatable :: dwrk(:)
    integer :: i, itmp, j, ans1, ans2
    integer, allocatable :: inext(:)

    call read_input(file, aa, rate, label)
    !call read_input('inp/16/test.txt', aa, rate, label)
    init%aa => aa
    init%rate => rate
    init%label => label

    ! Create distance map
    dmap = -1
    do i=1,size(label)
      if (label(i)=='  ') cycle
      init%pos = i
      if (allocated(dwrk)) deallocate(dwrk)
      call djikstra_search(dwrk, init, itmp)
      do j=1,size(dwrk)
        select type(dw=>dwrk(j)%ptr)
        class is (state_t)
          dmap(i,dw%pos) = dw%d
        end select
      end do
    end do
    init%dmap => dmap

    ! Print dmap
    do i=1, size(label)
      if (label(i)=='  ') cycle
      write(*,'(a,1x)',advance='no') label(i)
      do j=1, size(label)
        if (label(j)=='  ') cycle
        write(*,'(i2,1x)',advance='no') dmap(i,j)
      end do
      write(*,*)
    end do

    ! set initial position
    init%pos = findloc(label,'AA',1)

    ! Part 1
    init%deadline = DEADLINE_P1
    init%nc = 1
    call move(init, ans1)
    print '("Answer 16/1 ",i0,l2)', ans1, ans1==2359

    ! Part 2
    init%deadline = DEADLINE_P2
    init%nc = 2
    call move(init, ans2)
    print '("Answer 16/2 ",i0,l2)', ans2, ans2==2999
  end subroutine


  recursive subroutine move(s, cost)
    type(state_t), intent(in) :: s
    integer, intent(out) :: cost

    integer, allocatable :: imov1(:), imov2(:)
    integer :: i, ii, cost0, ichildren
    type(state_t) :: s0, s1
    logical :: isirq

    ichildren = 0
    cost = 0

    ! Simulate until decision must be made
    s0 = s
    do
      if (s0%t >= s0%deadline) exit
      call s0%onestep(isirq)
      if (isirq) exit
    end do
    if (s0%t==s0%deadline) goto 100

    ! Make several choices for each player who must decide
    allocate(imov1(0), imov2(0))
    if (s0%trg(1)==0) imov1 = next_to_open(s0,1)
    if (s0%nc>1) then
      if (s0%trg(2)==0) imov2 = next_to_open(s0,2)
    end if

    ! Three different loops
    ! - depending whether both players or just one player
    !   have choosen next move (TODO jak to zlepsit?)
    if (size(imov2)==0 .and. size(imov1)/=0) then
      do i=1, size(imov1)
        s1 = s0  
        call s1%moveto(imov1(i),1)
        call move(s1, cost0)
        ichildren = ichildren + 1
        if (cost0 > cost) cost = cost0
      end do

    else if (size(imov1)==0 .and. size(imov2)/=0) then
      do i=1, size(imov2)
        s1 = s0  
        call s1%moveto(imov2(i),2)
        call move(s1, cost0)
        ichildren = ichildren + 1
        if (cost0 > cost) cost = cost0
      end do
    
    else
      do i=1, size(imov1)
      do ii=1, size(imov2)
        if (imov2(ii)==imov1(i)) cycle
        s1 = s0  
        call s1%moveto(imov1(i),1)
        call s1%moveto(imov2(ii),2)
        call move(s1, cost0)
        ichildren = ichildren + 1
        if (cost0 > cost) cost = cost0
      end do
      end do
    end if

    ! One player has no more choices, but the second one
    ! is still moving
    if (ichildren==0 .and. any(s0%trg(1:s0%nc)/=0)) then
      call move(s0, cost0)
      if (cost0 > cost) cost = cost0
      ichildren = 1
    end if

    100 if (ichildren==0) then
      ! wait until deadline and report the actual state
      call s0%wait(s%deadline-s0%t)
      cost = s0%gain
    endif
  end subroutine move


  subroutine read_input(file, aa, rate, label)
    character(len=*), intent(in) :: file
    integer, intent(out) :: aa(:,:)
    integer, intent(out) :: rate(:)
    character(len=2), intent(out) :: label(:)

    type(string_t), allocatable :: lines(:)
    integer :: i, j0, j1
    integer :: ind_valve, ind_valve0
    character(len=2) :: valve, valve0

    lines = read_strings(file)
    aa = 0
    rate = 0
    label = '  '
    do i=1, size(lines)
      j0 = 1
      j1 = index(lines(i)%str(j0:), 'Valve ')
      j1 = j0 + j1 - 1 + 6
      read(lines(i)%str(j1:j1+1), *) valve
      ind_valve = hash(valve)
      label(ind_valve) = valve

      j0 = index(lines(i)%str, 'rate=') 
      j0 = j0 + 5
      j1 = scan(lines(i)%str, ';') 
      read(lines(i)%str(j0:j1-1),*) rate(ind_valve)
! print *, 'Valve: ', valve, ind_valve, rate(ind_valve)

      j0 = index(lines(i)%str, 'valves')
      j0 = j0 + 7
      NGB: do
        j1 = scan(lines(i)%str(j0:),',')
        if (j1==0) then
          j1 = len_trim(lines(i)%str)
        else
          j1 = j0 + j1 - 1 - 1
        end if
        read(lines(i)%str(j0:j1),*) valve0
        ind_valve0 = hash(valve0)
        aa(ind_valve,ind_valve0) = 1
        aa(ind_valve0,ind_valve) = 1
! print *, '-->',valve0, ind_valve0
        j0 = j1+3
        if (j0>len_trim(lines(i)%str)) exit NGB
      end do NGB
    end do
  end subroutine


  pure integer function hash(str)
    character(len=2), intent(in) :: str
    hash = (iachar(str(1:1))-iachar('A')+1)*30
    hash = iachar(str(2:2))-iachar('A')+1+hash
  end function hash


  subroutine state_nextngb(node, flag, node_ngb, distance)
    class(state_t), intent(in) :: node
    integer, intent(inout) :: flag !"0" on entry: first ngb, "0" on return: no more ngb
    class(djikstra_node_at), intent(out), allocatable :: node_ngb
    integer, intent(out) :: distance

    integer :: i

    if (.not. associated(node%aa)) error stop 'incidence matrix not associated'
    distance = 1
    i = flag
    allocate(node_ngb, source=node)
    select type (node_ngb)
    class is (state_t)
      do
        i = i + 1
        if (i > size(node%aa, 2)) then
          flag = 0
          exit 
        end if
        if (node%aa(node%pos(1), i) /= 1) cycle
        ! ngb found
        node_ngb%pos = i
        flag = i
        exit
      end do
    end select
  end subroutine

  logical function state_isequal(anode, bnode) result(isequal)
    class(state_t), intent(in) :: anode
    class(djikstra_node_at), intent(in) :: bnode
    select type(bnode)
    class is (state_t)
      isequal = anode%pos(1)==bnode%pos(1)
    end select
  end function

  logical function state_istarget(node)
    class(state_t), intent(in) :: node
    state_istarget = .false.
  end function


  subroutine state_onestep(this, isirq)
    class(state_t), intent(inout) :: this
    logical, intent(out) :: isirq

    integer :: k

    if (.not. associated(this%rate)) error stop 'open - rate pointer not associated'

    if (all(this%trg(1:this%nc)==0)) then
      ! when called from the root instance - no targets have been
      ! yet defined
      isirq = .true.
      return
    end if

    ! Update time
    isirq = .false.
    this%t = this%t + 1
    this%gain = this%gain + this%flow
!call this%print()

    do k=1,this%nc
      if (this%trg(k)==0) cycle
      if (this%eta(k)==0) then
        ! arrived - open the gate
        if (this%opened(this%trg(k)) .or. this%rate(this%trg(k))==0) &
          error stop 'open - valve already opened or stuck'
        if (count(this%trg==this%trg(k))/=1) error stop 'open - more than one plaer targeting same valve'
        this%opened(this%trg(k)) = .true.
        this%flow = this%flow + this%rate(this%trg(k))
        this%pos(k) = this%trg(k)
        this%trg(k) = 0
        isirq = .true.
      else
        ! still moving
        this%eta(k) = this%eta(k) - 1
      end if
    end do
  end subroutine state_onestep


  subroutine state_moveto(this, ito, ik)
    class(state_t), intent(inout) :: this
    integer, intent(in) :: ito, ik

    if (ik > this%nc) error stop 'moveto - invalid ik'
    if (.not. associated(this%dmap)) error stop 'moveto - dmap pointer not associated'
    if (ito==this%pos(ik)) error stop 'moveto - already here'
    if (this%opened(ito)) error stop 'moveto - target already opened'
    if (any(this%trg==ito)) error stop 'moveto - target already taken by other player'
    associate(d=>this%dmap(this%pos(ik), ito))
      if (d<1) error stop 'moveto - dmap value is not trusted'
      this%trg(ik) = ito
      this%eta(ik) = d
    end associate
  end subroutine state_moveto


  subroutine state_wait(this, i)
    class(state_t), intent(inout) :: this
    integer, intent(in) :: i

    if (i==0) return
    if (any(this%trg(1:this%nc)/=0)) then
      call this%print()
      error stop 'wait - there are still moving'
    end if
    this%t = this%t + i
    this%gain = this%gain + i*this%flow
  end subroutine


  subroutine state_print(th)
    class(state_t), intent(in) :: th

    write(*,'("t= ",i2," f=",i3," g=",i5," at ",a2,"/",a2, " trg ",a2,"/",a2," eta=",i2,"/",i2)') &
      th%t, th%flow, th%gain, safe_label(th%pos(1)), safe_label(th%pos(2)), &
      safe_label(th%trg(1)), safe_label(th%trg(2)), th%eta
  contains
    function safe_label(i)
      integer, intent(in) :: i
      character(len=2) :: safe_label
      if (i<1 .or. i>size(th%label)) then
        safe_label='??'
      else
        safe_label=th%label(i)
      end if
    end function
  end subroutine state_print


  function next_to_open(th, ik) result(pos)
    integer, allocatable :: pos(:)
    class(state_t), intent(in) :: th 
    integer, intent(in) :: ik

    ! Greedy algorithm - present three best choices for the
    ! next valve to open
    integer, parameter :: NSEL = 3

    integer :: i, n, pos0(NSEL)
    real :: val(size(th%rate))
    real :: mx

    if (ik>th%nc) error stop 'next_to_open - invalid ik'
    if (.not. associated(th%rate)) error stop 'next_to_open - rate pointer not associated'
    if (.not. associated(th%dmap)) error stop 'next_to_open - rate pointer not associated'

    val = 0.0
    do i=1, size(val)
      ! no point to consider opened or stuck valves
      if (th%label(i)=='  ') cycle
      if (th%opened(i) .or. th%rate(i)==0) cycle
      ! valve already taken
      if (any(th%trg(1:th%nc)==i)) cycle

      associate(d=>th%dmap(th%pos(ik), i))
        if (d<1) error stop 'next_to_open - dmap value is not trusted'
        val(i) = real(th%rate(i))/real(d+1) 
!  print *, th%label(i), val(i)
      end associate
    end do

    n = 0
    do i=1,NSEL
      if (maxval(val)==0.0) exit
      n=n+1
      pos0(n) = maxloc(val,1)
      val(pos0(n)) = 0.0
    end do
    allocate(pos(n))
    pos = pos0(1:n)
!print *, 'result =', pos

  end function
end module day2216_mod