module day2219_mod
  use parse_mod, only : string_t, read_strings
  implicit none
  private
  public day2219

  integer, parameter :: ID_R=1, ID_C=2, ID_B=3, ID_G=4, ID_ROB=+4
  integer, parameter :: INV_MAX=8, TIME_MAX=32
  character(len=*), parameter :: CH_R='ore', CH_C='clay', CH_B='obsidian', CH_G='geode'

  type relation_t
    type(string_t), allocatable :: src(:)
    integer, allocatable :: num(:)
    type(string_t) :: product
  end type
  interface relation_t
    module procedure relation_new
  end interface

  type blueprint_t
    integer :: id
    integer :: cost(ID_ROB, ID_ROB) = 0
    integer :: maxcost(ID_ROB)
  end type

  type state_t
    integer :: deadline=24
    integer :: t
    integer :: next=0
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
    integer :: i, j, t, ans, score
    integer, parameter :: time_part_1=24, time_part_2=32

    lines = read_strings(file)
!   lines = read_strings('inp/19/test.txt')
    allocate(bb(size(lines)))
    do i=1,size(lines)
      bb(i) = blueprint_new(lines(i)%str)
    end do

    allocate(init(size(bb)))
    ans = 0 
    do i=1,size(bb)
      init(i) = state_new(bb(i), time_part_1)
      call deep_search(init(i),score)
      call state_print(init(i))
      print '("Score: ",i0,/,a)',score
      ans = ans + init(i)%inv(ID_G,init(i)%deadline) * init(i)%bp%id
    end do
    print '("Answer 19/1 ",i0,l2)', ans, ans==1766 .or. ans==33

    ! Part 2
    ans = 1
    do i=1,min(size(bb),3)
      init(i) = state_new(bb(i), time_part_2)
      call deep_search(init(i), score)
      call state_print(init(i))
      print '("Score: ",i0,/,a)',score
      ans = ans * init(i)%inv(ID_G,init(i)%deadline)
    end do
    print '("Answer 19/2 ",i0,l2)', ans, ans==30780 .or. ans==56*62
    print *
    print *
  end subroutine day2219


  recursive subroutine deep_search(s, score)
    type(state_t), intent(inout) :: s
    integer, intent(out) :: score

    type(state_t) :: s0, sbest
    integer :: k, score0
    logical :: worth_build(4)

    score = s%inv(ID_G,s%deadline)
    if (s%t==s%deadline) return
    worth_build = build_choice(s)

    sbest = s
    do k=1,size(worth_build)
      if (.not. worth_build(k)) cycle
      s0=s
      s0%next = k
      call state_advance(s0)
 !if (state_maxscore(s0)<score) cycle
      call deep_search(s0, score0)
      if (s0%inv(ID_G,s%deadline) > sbest%inv(ID_G,s%deadline)) &
      & sbest = s0
      score = sbest%inv(ID_G,sbest%deadline)
    end do
    s = sbest
    score = s%inv(ID_G,s%deadline)
  end subroutine deep_search


  type(state_t) function state_new(bp, deadline) result(new)
    integer, intent(in) :: deadline
    type(blueprint_t), intent(in), target :: bp
    integer :: t

    new%deadline = deadline
    new%t = 3
    new%bp => bp
    new%inv = 0
    new%inv(ID_ROB+ID_R,:) = 1
    new%factory = 0

    new%inv(ID_R,1) = new%inv(ID_R+ID_ROB,1)
    do t=2, new%deadline
      new%inv(ID_R,t) = new%inv(ID_R,t-1) + new%inv(ID_R+ID_ROB,t)
    end do
  end function state_new


  subroutine state_advance(th)
    class(state_t), intent(inout) :: th

    integer :: k
    logical :: can_build

    if (th%next<1) error stop 'advance - no plan ahead'
    do
      can_build = .true.
      do k=1,size(th%bp%cost,1)
        associate(cost=>th%bp%cost(k, th%next))
          if (cost==0) cycle
          if (th%inv(k,th%t-1) < cost) can_build = .false.
        end associate
      end do

      if (can_build) then
        call state_build(th, th%next)
        th%next=0
        th%t=th%t+1
        exit
      else
        th%t = th%t+1
      end if
      if (th%t==th%deadline) exit
    end do
  end subroutine state_advance


  function build_choice(th)  result(can_build)
    logical :: can_build(4)
    class(state_t), intent(in) :: th

    integer :: k, id_robot

    ! no point building anything at the deadline
    if (th%t==th%deadline) then
      can_build = .false.
      return
    end if

    can_build = .true.

    ! remove choices that are no longer needed
    do id_robot=1,4
      ! there is never too many geodes
      if (id_robot==ID_G) cycle
      if (.not. can_build(id_robot)) cycle
     !if (th%inv(id_robot+ID_ROB,th%t) >= th%bp%maxcost(id_robot)) &
     !& can_build(id_robot) = .false.
      associate(x=>th%inv(id_robot+ID_ROB,th%t), &
        &       z=>th%bp%maxcost(id_robot), &
        &       y=>th%inv(id_robot,th%t), &
        &       t=>th%deadline-th%t-0)
        if (x*t + y >= t*z) can_build(id_robot)=.false.
      end associate
      ! https://www.reddit.com/r/adventofcode/comments/zpy5rm/2022_day_19_what_are_your_insights_and/
    end do

  end function


  subroutine state_build(th,id_robot)
    class(state_t), intent(inout) :: th
    integer, intent(in) :: id_robot

    integer :: k, t

    if (th%t<3) error stop 'nothing can be made in 1 or 2 minute'
    ! make sure there are the resources
    do k=1, size(th%bp%cost,1)
      if (th%bp%cost(k, id_robot)==0) cycle
      if (th%inv(k,th%t-1) < th%bp%cost(k, id_robot)) error stop 'not enough resources'
      do t = th%t, th%deadline
        ! add robot to stock in the future
        if (t>th%t .and. k==1) th%inv(id_robot+ID_ROB,t) = th%inv(id_robot+ID_ROB,t) + 1

        ! update resources / pay for building a robot
        th%inv(k,t) = th%inv(k,t) - th%bp%cost(k, id_robot)
        if (th%inv(k,t)<0) then
          call state_print(th)
          error stop 'not enough resources in the future'
        end if
      end do
    end do

    ! Because of the new robot recalculate its products 
    do t= th%t+1, th%deadline
      th%inv(id_robot,t) = th%inv(id_robot,t-1) + th%inv(id_robot+ID_ROB,t)
    end do

    ! Mark factory
    th%factory(th%t) = id_robot
  end subroutine state_build


  integer function state_maxscore(this) result(score)
    class(state_t), intent(in) :: this

    type(state_t) :: th
    integer :: t

    score=huge(score)
    return

    th = this
    do t=th%t+1,th%deadline
      associate (ng=>th%inv(ID_G+ID_ROB,t:th%deadline))
        ng = ng + 1
      end associate
    end do
    do t=th%t+1,th%deadline
      th%inv(ID_G,t) = th%inv(ID_G,t-1) + th%inv(ID_G+ID_ROB,t)
    end do
    score = th%inv(ID_G,th%deadline)
  end function


  subroutine state_print(th)
    class(state_t), intent(in) :: th
    if (th%deadline==24) then
      print '(24(i2,1x))', transpose(th%inv(:,1:th%deadline))
      print *, 'blueprint id: ',th%bp%id 
      print '(24(i2,1x))', th%factory(1:th%deadline)
      print *
    else
      print '(32(i2,1x))', transpose(th%inv(:,1:th%deadline))
      print *, 'blueprint id: ',th%bp%id 
      print '(32(i2,1x))', th%factory(1:th%deadline)
      print *
    end if
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
    do k=1,size(new%maxcost)
      new%maxcost(k) = maxval(new%cost(k,:))
    end do
print *, 'Bluprint id :', new%id
print '(4(i2,1x))', new%cost
print '("Max cost: ",4(i2,1x))', new%maxcost
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
