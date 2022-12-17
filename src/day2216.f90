module day2216_mod
  use parse_mod, only : string_t, read_strings
  use djikstra_mod, only : djikstra_node_at, djikstra_node_ptr, djikstra_search
  implicit none
  private
  public day2216

  integer, parameter :: MAX_LETTERS=iachar('Z')-iachar('A')+1 
  integer, parameter :: MAX_PLAYERS=2
  integer, parameter :: DEADLINE_P1=30, DEADLINE_P2=26

  type, extends(djikstra_node_at) ::  state_t
    integer :: t = 0
    integer :: gain = 0
    logical :: opened(MAX_LETTERS**2) = .false.
    integer :: pos(MAX_PLAYERS)
    integer :: trg(MAX_PLAYERS) = 0
    integer :: eta(MAX_PLAYERS)
!   integer :: flow = 0
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
  end type state_t

  integer, save :: counter_for_stat = 0

contains

  subroutine day2216(file)
    character(len=*), intent(in) :: file

    integer, target :: aa(hash('ZZ'),hash('ZZ'))
    integer, target :: dmap(hash('ZZ'),hash('ZZ'))
    character(len=2), target :: label(hash('ZZ'))
    integer, target :: rate(hash('ZZ'))
    type(state_t) :: init
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
    counter_for_stat = 0
    call move(init, ans1)
    print '("Answer 16/1 ",i0,l2)', ans1, ans1==2359
  print *, 'paths evaluated ',counter_for_stat

    ! Part 2
    counter_for_stat = 0
    init%deadline = DEADLINE_P2
    init%nc = 2
    call move(init, ans2)
    print '("Answer 16/2 ",i0,l2)', ans2, ans2==2999
print *, 'paths evaluated ',counter_for_stat
  end subroutine day2216


  recursive subroutine move(s, cost)
    type(state_t), intent(in) :: s
    integer, intent(out) :: cost

    integer, allocatable :: imov1(:), imov2(:)
    integer :: i1, i2, n1, n2, cost0
    type(state_t) :: s0, s1
    logical :: isirq

    ! Target valve was selected before the recursive call.
    ! Now simulate until a next decision can be made.
    cost = -1
    s0 = s
    do
      if (s0%t >= s0%deadline) exit
      call s0%onestep(.false., isirq)
      if (isirq) exit
    end do

    ! Opening valve in the last minute makes no difference
    if (s0%t>=s0%deadline-1) goto 100

    ! Suggest best moves for each player who must decide
    allocate(imov1(0), imov2(0))
    if (s0%trg(1)==0) imov1 = next_to_open(s0,1)
    if (s0%nc>1) then
      if (s0%trg(2)==0) imov2 = next_to_open(s0,2)
    end if

    ! Three cases must be resolved: 
    ! Player 1, player 2 or both players are choosing. 
    ! Loops start with index "0" to allow for at least one pass
    ! in a situation when either "n1" or "n2" are zero.
    n1 = size(imov1)
    n2 = size(imov2)
    OUT: do i1 = 0, n1
      INN: do i2 = 0, n2
        if (n1/=0 .and. n2/=0) then
          ! negate "0" as a starting index if both players choosing
          if (i1==0 .or. i2==0) cycle
          ! avoid both players choosing the same valve
          if (imov2(i2)==imov1(i1)) cycle
        else
          ! at least one player must choose
          if (i1==0 .and. i2==0) cycle
        end if

        ! Make a choice and recursively call itself
        s1 = s0
        if (i1/=0) call s1%moveto(imov1(i1),1)
        if (i2/=0) call s1%moveto(imov2(i2),2)
        call move(s1, cost0)

        ! Keep only the best output to be returned
        if (cost0 > cost) cost = cost0
      end do INN 
    end do OUT

    ! If no more choices were possible just simulate till the end
    ! and report the result
    100 if (cost==-1) then
      do
        if (s0%t >= s0%deadline) exit
        call s0%onestep(.true., isirq)
        if (isirq .and. s0%t<s0%deadline) error stop 'move - no more choices expected'
      end do
      cost = s0%gain
      counter_for_stat = counter_for_stat + 1
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


  pure integer function hash(str) result(h)
    character(len=2), intent(in) :: str
    h = 1
    h = h + (iachar(str(1:1))-iachar('A')) * MAX_LETTERS
    h = h + (iachar(str(2:2))-iachar('A'))
  end function hash


  subroutine state_nextngb(node, flag, node_ngb, distance)
    class(state_t), intent(in) :: node
    integer, intent(inout) :: flag !"0" on entry: first ngb, "0" on return: no more ngb
    class(djikstra_node_at), intent(out), allocatable :: node_ngb
    integer, intent(out) :: distance

    if (.not. associated(node%aa)) error stop 'incidence matrix not associated'
    distance = 1
    allocate(node_ngb, source=node)
    select type (node_ngb)
    class is (state_t)
      do
        flag = flag + 1
        if (flag > size(node%aa, 2)) then
          flag = 0
          exit 
        end if
        if (node%aa(node%pos(1), flag) /= 1) cycle
        ! ngb found
        node_ngb%pos = flag
        exit
      end do
    end select
  end subroutine


  pure logical function state_isequal(anode, bnode) result(isequal)
    class(state_t), intent(in) :: anode
    class(djikstra_node_at), intent(in) :: bnode
    select type(bnode)
    class is (state_t)
      isequal = all(anode%pos==bnode%pos)
    end select
  end function


  pure logical function state_istarget(node)
    class(state_t), intent(in) :: node
    state_istarget = .false.
  end function


  pure subroutine state_onestep(this, force, isirq)
    class(state_t), intent(inout) :: this
    logical, intent(in) :: force
    logical, intent(out) :: isirq

    integer :: k

    if (.not. associated(this%rate)) error stop 'open - rate pointer not associated'

    if (all(this%trg(1:this%nc)==0) .and. .not. force) then
      ! Do nothing when called from the root instance:
      ! no targets have been yet defined
      isirq = .true.
      return
    end if

    ! Update time
    isirq = .false.
    this%t = this%t + 1
!   this%gain = this%gain + this%flow

    do k=1,this%nc
      if (this%trg(k)==0) cycle
      if (this%eta(k)==0) then
        ! arrived - open the gate
        !if (this%opened(this%trg(k)) .or. this%rate(this%trg(k))==0) &
        if (this%rate(this%trg(k))==0) &
          error stop 'open - valve already opened or stuck'
        if (count(this%trg==this%trg(k))/=1) error stop 'open - more than one plaer targeting same valve'
!       this%opened(this%trg(k)) = .true.
!       this%flow = this%flow + this%rate(this%trg(k))
!     this%gain = this%gain + (this%deadline-this%t)*this%rate(this%trg(k))
        this%pos(k) = this%trg(k)
        this%trg(k) = 0
        isirq = .true.
      else
        ! still moving
        this%eta(k) = this%eta(k) - 1
      end if
    end do
  end subroutine state_onestep


  pure subroutine state_moveto(this, ito, ik)
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
      this%gain = this%gain + max(0,(this%deadline-this%t-1-d)*this%rate(ito))
      this%opened(ito) = .true.
    end associate
  end subroutine state_moveto


  subroutine state_print(th)
    class(state_t), intent(in) :: th

    write(*,'("t= ",i2," f=",i3," g=",i5," at ",a2,"/",a2, " trg ",a2,"/",a2," eta=",i2,"/",i2)') &
      th%t, -1, th%gain, safe_label(th%pos(1)), safe_label(th%pos(2)), &
      !th%t, th%flow, th%gain, safe_label(th%pos(1)), safe_label(th%pos(2)), &
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
!
! Greedy algorithm:
! present three best choices for the next valve to open
!
    integer, parameter :: NSEL = 3
    integer :: i, n, pos0(NSEL)
    real :: val(size(th%rate))

    if (ik>th%nc) error stop 'next_to_open - invalid ik'
    if (.not. associated(th%rate)) error stop 'next_to_open - rate pointer not associated'
    if (.not. associated(th%dmap)) error stop 'next_to_open - rate pointer not associated'

    val = 0.0
    do i=1, size(val)
      ! no point to consider opened or stuck valves
      if (th%label(i)=='  ') cycle
      if (th%opened(i) .or. th%rate(i)==0) cycle
      ! valve already taken by another player
      if (any(th%trg(1:th%nc)==i)) cycle

      associate(d=>th%dmap(th%pos(ik), i))
        if (d<1) error stop 'next_to_open - dmap value is not trusted'
        val(i) = real(th%rate(i))/real(d+1) 
        !val(i) = real((th%deadline-th%t-d-1)*th%rate(i))
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
  end function next_to_open

end module day2216_mod