module day2224_mod
  use parse_mod, only : string_t, read_strings
  use djikstra_mod, only: djikstra_node_at, djikstra_node_ptr, djikstra_search
  implicit none

  integer :: global_maxpos(2)
  integer :: global_goal = 1


  character(len=1), parameter :: BLIBRARY(4) = ['v','>','^','<']
  integer, parameter :: MOVDIR(2,5)=reshape( [ 0,1, 1,0, 0,-1,-1,0, 0,0],[2,5])

  type blizzard_t
    integer :: v(2), x0(2)
  contains
    procedure :: char => blizzard_char
    procedure :: pos => blizzard_pos
  end type blizzard_t
  interface blizzard_t
    module procedure blizzard_new
  end interface

  type, extends(djikstra_node_at) :: state_t
    integer :: e(2), t
    type(blizzard_t), pointer :: b(:) 
  contains
    procedure :: istarget, isequal, nextngb
  end type

  type(state_t), allocatable :: global_s(:)
contains

  subroutine day2224(file)
    character(len=*), intent(in) :: file
    type(blizzard_t), allocatable, target :: bzards(:)
    type(djikstra_node_ptr), allocatable :: wdik(:), wdik2(:)
    integer :: ex0(2), ex1(2), i, j, ans1, ans2
    type(state_t) :: s

    call read_input(file, bzards)
    !call read_input('inp/24/test2.txt', bzards)
    ex0 = [1,0]
    ex1 = [global_maxpos(1),global_maxpos(2)+1]

    call print_situation(bzards, ex0, 0)

    ! First trip
    s%e = ex0
    s%t = 0
    s%b => bzards

    call djikstra_search(wdik, s, ans1)
    print *, 'djikstra - there', ans1, 'nodes = ',size(wdik)

    s%e = ex1
    s%t = ans1
    global_goal = 2
    call djikstra_search(wdik2, s, i)
    print *, 'djikstra - back', i, 'nodes = ',size(wdik2)

    s%e = ex0
    s%t = ans1 + i
    global_goal = 1
    deallocate(wdik)
    call djikstra_search(wdik, s, j)
    print *, 'djikstra - there again', j, 'node = ',size(wdik)
    ans2 = ans1 + i + j
    print *, ans2, ans2==739

    stop
    allocate(global_s(0))
    global_s = [global_s, s]
    i = huge(i)
    j = i
    !call search(s, i)
    !call bsearch(i, j)
    print *, i, j
    call print_situation(bzards, s%e, s%t)


  end subroutine day2224


  subroutine bsearch(mpath, best)
    integer, intent(inout) :: mpath, best

    type(state_t) :: s, s0
    logical :: opt(5)
    integer :: i, mpath0, counter

    best = huge(best)
    counter = 0
    do
      if (mod(counter,100000)==0) then
        print *, size(global_s), best, global_s(1)%t
      end if
      counter = counter + 1

      if (size(global_s)==0) exit
      s = global_s(1)
      global_s = global_s(2:)

      ! Exit found
      if (s%e(1)==global_maxpos(1) .and. s%e(2)==global_maxpos(2)+1) then
        mpath = s%t
        print *, 'Exit hit in ', mpath
        if (mpath < best) best = mpath
      end if

      ! Advance time and branch new options
      s%t = s%t+1

      ! Do not continue with suboptimal path
      if (s%t > best) cycle
      opt = can_move_to(s%e, s%t, s%b)

      do i=1,5
        s0 = s
        s0%e = s0%e + MOVDIR(:,i)
        !global_s = [global_s, s0]
        global_s = [s0, global_s]
      end do

    end do
  end subroutine bsearch


  recursive subroutine search(s, minpath)
    type(state_t), intent(inout) :: s
    integer, intent(out) :: minpath

    logical :: opt(5)
    integer :: i, minpath0 
    type(state_t) :: s0, sbest

    minpath = huge(minpath)
 !print *, s%t, s%e
    if (s%e(1)==global_maxpos(1) .and. s%e(2)==global_maxpos(2)+1) then
      minpath = s%t
      return
    end if
    if (s%t > 30) then
      return
    end if

    s%t = s%t + 1
   !call blizzard_move(s%b)
    opt = can_move_to(s%e, s%t, s%b)
   !if (.not. any(opt)) return
    do i=1,5
      if (.not. opt(i)) cycle
      s0 = s
      s0%e = s0%e + MOVDIR(:,i)
      call search(s0, minpath0)
      if (minpath0 < minpath) then
        minpath = minpath0
        sbest = s0
      end if
    end do
    if (minpath < huge(minpath)) then
      s = sbest
    end if
  end subroutine search


  subroutine print_situation(bzards, exy, t)
    class(blizzard_t), intent(in) :: bzards(:)
    integer, intent(in) :: exy(2), t

    character(len=1) :: map(global_maxpos(1)+2,global_maxpos(2)+2)
    integer :: ib, i, j

    ! Plot border
    map = '.'
    map(1,:) = '#'
    map(global_maxpos(1)+2,:) = '#'
    map(:,1) = '#'
    map(:,global_maxpos(2)+2) = '#'
    map(2,1) = '.'
    map(global_maxpos(1)+1, global_maxpos(2)+2) = '.'

    do ib = 1, size(bzards)
      associate(x=>bzards(ib)%pos(t)+1)
        if (map(x(1),x(2))=='.') then
          map(x(1),x(2)) = bzards(ib)%char()
        else if (map(x(1),x(2))=='>' .or. map(x(1),x(2))=='<' .or. &
                 map(x(1),x(2))=='^' .or. map(x(1),x(2))=='v') then
          map(x(1),x(2)) = achar(50)
        else
          map(x(1),x(2)) = achar(iachar(map(x(1),x(2)))+1)
        end if
      end associate
    end do
    associate(m=>map(exy(1)+1,exy(2)+1))
      if (m=='.') then
        m = 'E'
      else
        m = '@'
      end if
    end associate

    where (map/='.' .and. map/='E' .and. map/='#') map=' '
    do j=1,global_maxpos(2)+2
      write(*,'(*(a))') (map(i,j),i=1,global_maxpos(1)+2)
    end do
  end subroutine print_situation



  subroutine read_input(file, bzards)
    character(len=*), intent(in) :: file
    type(blizzard_t), allocatable, intent(out) :: bzards(:)

    type(string_t), allocatable :: lines(:)
    type(blizzard_t), allocatable :: btemp(:)
    integer :: nb, i, j

    ! add blizzards
    lines = read_strings(file)
    allocate(btemp(0))
    do j=1,size(lines)
      if (j==1) cycle ! first line contains entry
      if (j==size(lines)) cycle ! last line contains exit
      do i=2,len(lines(j)%str)-1
        associate(b=>lines(j)%str(i:i))
          if (b=='.') cycle
          btemp = [btemp, blizzard_t(b, i-1, j-1)]
        end associate
      end do
    end do
    call move_alloc(btemp, bzards)
    global_maxpos(1) = len(lines(1)%str)-2
    global_maxpos(2) = size(lines)-2
print *, 'board ', global_maxpos, 'blizzards ',size(bzards)
  end subroutine read_input



  type(blizzard_t) function blizzard_new(ch,i,j) result(new)
    character(len=1), intent(in) :: ch
    integer, intent(in) :: i, j

    integer :: ind
    ind = findloc(BLIBRARY, ch, dim=1)
    if (ind==0) error stop 'invalid character'
    new%v = MOVDIR(:,ind)
    new%x0 = [i,j]
  end function blizzard_new



  function blizzard_pos(this, t) result(x)
    class(blizzard_t), intent(in) :: this
    integer, intent(in) :: t
    integer :: x(2)

    x = this%x0 + t*this%v
    if (this%v(1)/=0) x(1)=modulo(x(1)-1,global_maxpos(1))+1
    if (this%v(2)/=0) x(2)=modulo(x(2)-1,global_maxpos(2))+1
  end function blizzard_pos


  function blizzard_char(this) result(ch)
    class(blizzard_t), intent(in) :: this
    character(len=1) :: ch
    select case(this%v(1))
    case(1)
      ch = '>'
    case(-1)
      ch = '<'
    case(0)
      select case(this%v(2))
      case(-1)
        ch = '^'
      case(1)
        ch = 'v'
      case default
        error stop 'invalid velocity'
      end select
    case default
      error stop 'invalid velocity'
    end select
  end function blizzard_char


  integer function count_blizzards(p, t, bzards) result(n)
    class(blizzard_t), intent(in) :: bzards(:)
    integer, intent(in) :: p(2), t

    integer :: i

    n = 0
    do i=1,size(bzards)
      if (all(bzards(i)%pos(t)==p)) n = n+1
    end do
  end function count_blizzards


  function can_move_to(p, t, bzards) result(options)
    integer, intent(in) :: p(2), t
    type(blizzard_t), intent(in) :: bzards(:)
    logical :: options(5)

    integer :: i, q(2)

    do i=1,5
      q = p + MOVDIR(:,i)
      if (q(1)<1 .or. q(1)>global_maxpos(1)  .or. &
      & (q(2)>global_maxpos(2) .and. q(1)/=global_maxpos(1)) .or. &
      & (q(2)==0 .and. q(1)/=1) .or. q(2)<0) then
        options(i) = .false.
      else if (count_blizzards(q, t, bzards)>0) then
        options(i) = .false.
      else
        options(i) = .true.
      end if
    end do
  end function



  subroutine nextngb(node, flag, node_ngb, distance)
    class(state_t), intent(in) :: node
    integer, intent(inout) :: flag !"0" on entry: first ngb, "0" on return: no more ngb
    class(djikstra_node_at), intent(out), allocatable :: node_ngb
    integer, intent(out) :: distance

    integer :: q(2)
    logical :: can

    distance = 1
    allocate(node_ngb, source=node)
    select type(node_ngb)
    class is(state_t)
      node_ngb%t = node_ngb%t+1
      flag = flag + 1
      do
        if (flag > 5) then
          flag = 0
          exit 
        end if
        q = node_ngb%e + MOVDIR(:,flag)
        if (q(1)<1 .or. q(1)>global_maxpos(1)  .or. &
        & (q(2)>global_maxpos(2) .and. q(1)/=global_maxpos(1)) .or. &
        & (q(2)==0 .and. q(1)/=1) .or. q(2)<0) then
          can = .false.
        else if (count_blizzards(q, node_ngb%t, node_ngb%b)>0) then
          can = .false.
        else
          can = .true.
        end if
        if (.not. can) then
          flag = flag + 1
          cycle
        end if
        node_ngb%e = q
        exit
      end do
    end select
  end subroutine nextngb

  logical function isequal(anode, bnode)
    class(state_t), intent(in) :: anode
    class(djikstra_node_at), intent(in) :: bnode
    select type(bnode)
    class is(state_t)
      isequal = anode%t==bnode%t .and. all(anode%e==bnode%e)
    end select
  end function

  logical function istarget(node)
    class(state_t), intent(in) :: node
    select case (global_goal)
    case(1)
      istarget = (node%e(1) == global_maxpos(1) .and.  node%e(2) == global_maxpos(2)+1)
    case(2)
      istarget = (node%e(1)== 1 .and. node%e(2)==0)
    case default
      error stop 'is target - invalid goal'
    end select
  end function

end module day2224_mod