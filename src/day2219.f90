module day2219_mod
  use parse_mod, only : string_t, read_strings
  implicit none
  private
  public day2219

  type relation_t
    type(string_t), allocatable :: src(:)
    integer, allocatable :: num(:)
    type(string_t) :: product
  end type
  interface relation_t
    module procedure relation_new
  end interface

  integer, parameter :: ID_R=1, ID_C=2, ID_B=3, ID_G=4, ID_ROB=+4
  integer, parameter :: INV_MAX=8, TIME_MAX=24
  character(len=*), parameter :: CH_R='ore', CH_C='clay', CH_B='obsidian', CH_G='geode'

  type blueprint_t
    integer :: id
    integer :: cost(ID_ROB, ID_ROB) = 0
  end type

  type state_t
    integer :: inv(INV_MAX, TIME_MAX) = 0
    integer :: factory(TIME_MAX) = 0
    type(blueprint_t), pointer :: bp => null()
  end type

contains

  subroutine day2219(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    type(blueprint_t), allocatable, target :: bb(:)
    type(state_t), allocatable :: init(:)
    integer :: i, j, t, ans

    lines = read_strings(file)
    !lines = read_strings('inp/19/test.txt')
    allocate(bb(size(lines)))
    do i=1,size(lines)
      bb(i) = blueprint_new(lines(i)%str)
    end do

    allocate(init(size(bb)))
    ans = 0 
    do i=1,size(bb)
      init(i) = state_new(bb(i))
      t=3
      call deep_search(init(i), t)
      call state_print(init(i))
      ans = ans + init(i)%inv(ID_G,TIME_MAX) * init(i)%bp%id
    end do
    print *, ans, ans==1766
  end subroutine day2219


  recursive subroutine deep_search(s, time)
    type(state_t), intent(inout) :: s
    integer, intent(inout) :: time

    type(state_t) :: s0, sbest
    integer :: t0, k, tbest
    logical :: select_factory(4)

    if (time==TIME_MAX) return

    do
      select_factory = build_choice(s, time)

      if (select_factory(ID_G)) then
        call state_build(s, ID_G, time)
        time = time + 1
      else if (select_factory(ID_B)) then
        call state_build(s, ID_B, time)
        time = time + 1
      else if (count(select_factory)==0) then
        time = time + 1
      else 
        exit
      end if

      ! no choice, just advance time and try again
      if (time==TIME_MAX) return
    end do

    sbest = s
    tbest = time

    ! try also doing nothing
    s0 = s
    t0 = time + 1
    call deep_search(s0, t0)
    if (s0%inv(ID_G,TIME_MAX) > sbest%inv(ID_G,TIME_MAX)) then
      sbest = s0
      tbest = t0
       !print *, ' improved  ',s0%inv(ID_G,TIME_MAX), t0
    end if

    do k=4,1,-1
      if (.not. select_factory(k)) cycle
      s0 = s
      t0 = time
      call state_build(s0, k, t0)
      t0 = t0 + 1
      call deep_search(s0, t0)
      if (s0%inv(ID_G,TIME_MAX) > sbest%inv(ID_G,TIME_MAX)) then
        sbest = s0
        tbest = t0
       !print *, ' improved  ',s0%inv(ID_G,TIME_MAX), t0
      else
      end if
    end do

    s = sbest
    time = tbest
  end subroutine deep_search


  type(state_t) function state_new(bp) result(new)
    integer :: t
    type(blueprint_t), intent(in), target :: bp

    new%bp => bp
    new%inv = 0
    new%inv(ID_ROB+ID_R,:) = 1
    new%factory = 0

    new%inv(ID_R,1) = new%inv(ID_R+ID_ROB,1)
    do t=2, TIME_MAX
      new%inv(ID_R,t) = new%inv(ID_R,t-1) + new%inv(ID_R+ID_ROB,t)
    end do
  end function state_new


  function build_choice(th,time)  result(can_build)
    logical :: can_build(4)
    class(state_t), intent(in) :: th
    integer, intent(in) :: time

    integer :: k, id_robot

    can_build = .true.
    do id_robot=1,4
      do k=1,size(th%bp%cost,1)
        if (th%bp%cost(k, id_robot)==0) cycle
        if (th%inv(k,time-1) < th%bp%cost(k, id_robot)) then
          can_build(id_robot) = .false.
          exit
        end if
      end do
    end do
  end function


  subroutine state_build(th,id_robot,time)
    class(state_t), intent(inout) :: th
    integer, intent(in) :: id_robot, time

    integer :: k, t

    if (time<3) error stop 'nothing can be made in 1 or 2 minute'
    ! make sure there are the resources
    do k=1, size(th%bp%cost,1)
      if (th%bp%cost(k, id_robot)==0) cycle
      if (th%inv(k,time-1) < th%bp%cost(k, id_robot)) error stop 'not enough resources'
      do t = time, TIME_MAX
        ! add robot to stock in the future
        if (t>time .and. k==1) th%inv(id_robot+ID_ROB,t) = th%inv(id_robot+ID_ROB,t) + 1

        ! update resources / pay for building a robot
        th%inv(k,t) = th%inv(k,t) - th%bp%cost(k, id_robot)
        if (th%inv(k,t)<0) error stop 'not enough resources in the future'
      end do
    end do

    ! Because of the new robot recalculate its products 
    do t= time+1, TIME_MAX
      th%inv(id_robot,t) = th%inv(id_robot,t-1) + th%inv(id_robot+ID_ROB,t)
    end do

    ! Mark factory
    th%factory(time) = id_robot

  end subroutine state_build


  subroutine state_print(th)
    class(state_t), intent(in) :: th
    print '(24(i3,1x))', transpose(th%inv)
    print *, 'blueprint id: ',th%bp%id 
    print '(24(i3,1x))', th%factory
    print *
  end subroutine state_print


  type(blueprint_t) function blueprint_new(str) result(new)
    character(len=*), intent(in) :: str

    type(relation_t) :: rel
    integer :: i0, i1, is, k, id_robot, id_resource

    i0 = index(str,'Blueprint')
    i1 = index(str, ':')
    read(str(i0+10:i1-1),*) new%id
    new%cost = 0

    is = 1
    do
      i0 = index(str(is:),'Each')
      i1 = scan(str(is:),'.')
      i0 = is + i0 - 1
      i1 = is + i1 - 1
      rel = relation_t(str(i0:i1))
      id_robot = get_resource_id(rel%product%str) 
      do k=1,size(rel%src)
        id_resource = get_resource_id(rel%src(k)%str)
        new%cost(id_resource,id_robot) = rel%num(k)
      end do
      if (i1==len_trim(str)) exit
      is = i1 + 1
    end do
  print *, 'Bluprint id :', new%id
  print '(4(i2,1x))', new%cost
  end function blueprint_new


  type(relation_t) function relation_new(str) result(new)
    character(len=*), intent(in) :: str

    integer :: n, i0, i1, is
    character(len=20) :: ch_rest
    logical :: is_last_item
    type(string_t) :: src
    integer :: num

    n = len_trim(str)
    if (str(1:4)/='Each ' .or. str(n:n)/='.') error stop 'new relation - wrong input string'
    allocate(new%num(0))
    allocate(new%src(0))

    i0 = index(str,' costs')
    new%product = string_t(str(6:i0-1))
!print *, '*'//new%product%str//'* <--'

    is = i0 + 7
    do
      i1 = index(str(is:), 'and')
      if (i1==0) then
        is_last_item = .true.
        i1 = n-1
      else 
        is_last_item = .false.
        i1 = is + i1 - 1
      end if
      read(str(is:i1),*) num, ch_rest
      src = string_t(ch_rest(1:len_trim(ch_rest)))
      new%src = [new%src, src]
      new%num = [new%num, num]
!print *, '-',new%num(size(new%num)), '*'//new%src(size(new%src))%str//'*'
      if (is_last_item) exit
      is = i1+4
    end do
!print *,'  there is ',size(new%num),' ingredients'
  end function relation_new


  integer function get_resource_id(str) result(id)
    character(len=*), intent(in) :: str
    integer :: i
    i = scan(str,' ')
    id = 0
    if (i==0) then
      i=len_trim(str)
      !id = ID_B
    else 
      i = i-1
    end if
    select case(str(:i))
    case(CH_R)
      id = id + ID_R
    case(CH_C)
      id = id + ID_C
    case(CH_B)
      id = id + ID_B
    case(CH_G)
      id = id + ID_G
    case default
      error stop 'uknown mineral'
    end select
  end function get_resource_id

end module day2219_mod