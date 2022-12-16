module day2216_mod
  use parse_mod, only : string_t, read_strings
  use djikstra_mod, only : djikstra_node_at, djikstra_node_ptr, djikstra_search
  implicit none

  integer, parameter :: MAXHASHVAL = 1000
  type, extends(djikstra_node_at)::  state_t
    integer :: t = 0
    integer :: gain = 0
    logical :: opened(MAXHASHVAL) = .false.
    integer :: pos
    integer :: flow = 0
    integer, pointer :: aa(:,:) => null()
    integer, pointer :: dmap(:,:) => null()
    integer, pointer :: rate(:) => null()
    character(len=2), pointer :: label(:) => null()
  contains
    procedure :: istarget => state_istarget
    procedure :: isequal => state_isequal
    procedure :: nextngb => state_nextngb
    procedure :: moveto => state_moveto
    procedure :: open => state_open
    procedure :: print => state_print
    procedure :: wait => state_wait
  end type state_t
contains

  subroutine day2216(file)
    character(len=*), intent(in) :: file


    integer, target :: aa(hash('ZZ'),hash('ZZ'))
    integer, target :: dmap(hash('ZZ'),hash('ZZ'))
    character(len=2), target :: label(hash('ZZ'))
    integer, target :: rate(hash('ZZ'))
    type(state_t) :: init, sout
    type(djikstra_node_ptr), allocatable :: dwrk(:)
    integer :: i, itmp, j, ans
    integer, allocatable :: inext(:)

    call read_input(file, aa, rate, label)
    !call read_input('inp/16/test.txt', aa, rate, label)

    ! Create distance map
    dmap = -1
    init%aa => aa
    init%rate => rate
    init%label => label
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
    init%dmap => dmap

    ! set initial position
    init%pos = findloc(label,'AA',1)
call init%print()

goto 100
inext = next_to_open(init)
print *, label(inext(1)), label(inext(2))
    call init%moveto(findloc(label,'DD',1))
    call init%open()
inext = next_to_open(init)
print *, label(inext(1)), label(inext(2))
    call init%moveto(findloc(label,'BB',1))
    call init%open()
    call init%moveto(findloc(label,'JJ',1))
    call init%open()
    call init%moveto(findloc(label,'HH',1))
    call init%open()
    call init%moveto(findloc(label,'EE',1))
    call init%open()
    call init%moveto(findloc(label,'CC',1))
    call init%open()
call init%print()
    call init%wait(6)
call init%print()

stop
100 continue
    call move(init, ans)
  print *, ans, ans==2359
  end subroutine


  recursive subroutine move(s, cost)
    type(state_t), intent(in) :: s
    integer, intent(out) :: cost

    integer, allocatable :: imov(:)
    integer :: i, cost0, ichildren
    type(state_t) :: s0

    imov = next_to_open(s)
    ichildren = 0
    cost = 0
    do i=1, size(imov)
      s0 = s  
      call s0%moveto(imov(i))
      call s0%open()
      if (s0%t <= 30) then
        call move(s0, cost0)
        ichildren = ichildren + 1
        if (cost0 > cost) cost = cost0
      end if
    end do

    if (ichildren==0) then
      ! wait until deadline and report the actual state
      s0 = s
      call s0%wait(30-s%t)
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
  print *, 'Valve: ', valve, ind_valve, rate(ind_valve)

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
  print *, '-->',valve0, ind_valve0
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
        if (node%aa(node%pos, i) /= 1) cycle
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
      isequal = anode%pos==bnode%pos
    end select
  end function

  logical function state_istarget(node)
    class(state_t), intent(in) :: node
    state_istarget = .false.
  end function


  subroutine state_open(this)
    class(state_t), intent(inout) :: this

    if (.not. associated(this%rate)) error stop 'open - rate pointer not associated'
    if (this%opened(this%pos) .or. this%rate(this%pos)==0) &
      error stop 'open - valve already opened or stuck'
    this%t = this%t + 1
    this%gain = this%gain + this%flow
    this%opened(this%pos) = .true.
    this%flow = this%flow + this%rate(this%pos)
  end subroutine state_open


  subroutine state_moveto(this, ito)
    class(state_t), intent(inout) :: this
    integer, intent(in) :: ito

    if (.not. associated(this%dmap)) error stop 'moveto - dmap pointer not associated'
    if (ito==this%pos) error stop 'moveto - already here'
    associate(d=>this%dmap(this%pos, ito))
      if (d<1) error stop 'moveto - dmap value is not trusted'
      this%pos = ito
      this%t = this%t + d
      this%gain = this%gain + d*this%flow
    end associate
  end subroutine state_moveto


  subroutine state_wait(this, i)
    class(state_t), intent(inout) :: this
    integer, intent(in) :: i
    this%t = this%t + i
    this%gain = this%gain + i*this%flow
  end subroutine


  subroutine state_print(th)
    class(state_t), intent(in) :: th

    write(*,'("t= ",i2," f=",i3," g=",i5," at ",a2)') &
      th%t, th%flow, th%gain, th%label(th%pos)
  end subroutine state_print


  function next_to_open(th) result(pos)
    integer, allocatable :: pos(:)
    class(state_t), intent(in) :: th 
integer, parameter :: NSEL = 3

    integer :: i, n, pos0(NSEL)
    real :: val(size(th%rate))
    real :: mx

    if (.not. associated(th%rate)) error stop 'next_to_open - rate pointer not associated'
    if (.not. associated(th%dmap)) error stop 'next_to_open - rate pointer not associated'
    val = 0.0
    do i=1, size(val)
      ! no point to consider opened or stuck valves
      if (th%label(i)=='  ') cycle
      if (th%opened(i) .or. th%rate(i)==0) cycle
      associate(d=>th%dmap(th%pos, i))
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