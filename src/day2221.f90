module day2221_mod
  use parse_mod, only : string_t, read_strings, split
  use iso_fortran_env, only : I8=>int64
  implicit none
  private
  public day2221

  integer, parameter :: NAME_LEN=4
  integer, parameter :: OP_VAL=0, OP_ADD=1, OP_SUB=2, OP_MUL=3, OP_DIV=4
  integer, parameter :: NULL_VAL = -1
  character(len=NAME_LEN), parameter :: ROOT='root', HUMN='humn'

  type monkey_t
    integer :: op
    integer(I8) :: val = NULL_VAL
    character(len=NAME_LEN) :: name
    character(len=NAME_LEN) :: opnames(2) = ['none','none']
  end type
  interface monkey_t
    module procedure monkey_new
  end interface 

contains

  subroutine day2221(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    type(monkey_t), allocatable :: monkeys(:)
    integer :: i, nm, iroot, ihumn
    integer(I8) :: ans1, ans2

    lines = read_strings(file)
    !lines = read_strings('inp/21/test.txt')
    nm = size(lines)
    allocate(monkeys(nm))
    do i=1,nm
      monkeys(i) = monkey_t(lines(i)%str)
    end do
    iroot = monkey_find(ROOT, monkeys)
    ihumn = monkey_find(HUMN, monkeys)
    print *, 'nm =',nm, 'root =', iroot, 'humn =', ihumn

    ! Part 1
    ans1 =  monkey_eval(monkeys(iroot), monkeys)
    print '("Answer 21/1 ",i0,l2)', ans1, ans1==276156919469632_I8 .or. ans1==152

    ! Dependency of root on humn
    !call monkey_print(monkeys(iroot), monkeys)

    ! Part 2
    ans2 = monkey_expected(monkeys(iroot), 0_I8, monkeys)
    print '("Answer 21/2 ",i0,l2)', ans2, ans2==3441198826073_I8 .or. ans2==301

  end subroutine day2221


  type(monkey_t) function monkey_new(str) result(new)
    character(len=*), intent(in) :: str

    type(string_t), allocatable :: toks(:)

    call split(str, ' ', toks)

    associate(s=>toks(1)%str)
      if (s(len(s):len(s)) /= ':') error stop 'name without :'
      new%name = s(1:len(s)-1)
    end associate

    select case(size(toks))
    case(2)
      new%op = OP_VAL
      read(toks(2)%str,*) new%val
    case(4)
      select case(toks(3)%str)
      case('+')
        new%op = OP_ADD
      case('-')
        new%op = OP_SUB
      case('*')
        new%op = OP_MUL
      case('/')
        new%op = OP_DIV
      case default
        error stop 'monkey_new - invalid operator'
      end select
      new%opnames(1) = toks(2)%str
      new%opnames(2) = toks(4)%str
    case default
      error stop 'monkey_new - unexpected format'
    end select
  end function monkey_new


  pure integer function monkey_find(name, monkeys) result(ind)
    character(len=NAME_LEN), intent(in) :: name
    type(monkey_t), intent(in) :: monkeys(:)

    integer :: i

    ind = 1
    do i=1, size(monkeys)
      if (monkeys(i)%name/=name) cycle
      ind = i
      exit
    end do
  end function monkey_find


  pure recursive function monkey_eval(this, monkeys) result(val)
    type(monkey_t), intent(in) :: this
    type(monkey_t), intent(in) :: monkeys(:)
    integer(I8) :: val

    integer :: i1, i2
    integer(I8) :: s(2)

    if (this%op==OP_VAL) then
      val = this%val
    else
      i1 = monkey_find(this%opnames(1),monkeys)
      i2 = monkey_find(this%opnames(2),monkeys)
      if (i1<1 .or. i2<1) error stop 'monkey not found'
      s(1) = monkey_eval(monkeys(i1),monkeys)
      s(2) = monkey_eval(monkeys(i2),monkeys)
      select case (this%op)
      case(OP_ADD)
        val = s(1) + s(2)
      case(OP_SUB)
        val = s(1) - s(2)
      case(OP_MUL)
        val = s(1) * s(2)
      case(OP_DIV)
        val = s(1) / s(2)
      case default
        error stop 'eval - invalid op'
      end select
    end if
  end function monkey_eval


  pure recursive function monkey_ancestorof(this, name, monkeys) result(isancestor)
    logical :: isancestor
    class(monkey_t), intent(in) :: this
    character(len=NAME_LEN), intent(in) :: name
    type(monkey_t), intent(in) :: monkeys(:)

    logical :: is(2)
    integer :: i1, i2

    if (this%op==OP_VAL) then
      isancestor = this%name==name 
    else
      i1 = monkey_find(this%opnames(1),monkeys)
      i2 = monkey_find(this%opnames(2),monkeys)
      if (i1<1 .or. i2<1) error stop 'monkey not found'
      is(1) = monkey_ancestorof(monkeys(i1),name,monkeys)
      is(2) = monkey_ancestorof(monkeys(i2),name,monkeys)
      isancestor = is(1) .or. is(2)
    end if
  end function monkey_ancestorof


  recursive subroutine monkey_print(this,monkeys,lev)
    class(monkey_t), intent(in) :: this
    type(monkey_t), intent(in) :: monkeys(:)
    integer, intent(in), optional :: lev
!
! Only print monkeys that depend on the HUMN monkey
!
    integer :: i, j, lev0

    if (.not. monkey_ancestorof(this,HUMN,monkeys)) return
    lev0 = 0
    if (present(lev)) lev0 = lev

    write(*,'(a)',advance='no') repeat('--',lev0)
    write(*,'(a,1x,a,1x,a," V=",i0)') &
    & this%name, this%opnames, this%val
    if (this%op==OP_VAL) return

    do i=1,2
      j = monkey_find(this%opnames(i), monkeys)
      call monkey_print(monkeys(j),monkeys,lev0+1)
    end do
  end subroutine monkey_print


  recursive function monkey_expected(this, req, monkeys) result(val)
    class(monkey_t), intent(in) :: this
    integer(I8), intent(in) :: req
    type(monkey_t), intent(in) :: monkeys(:)
    integer(I8) :: val

    integer :: i, m(2)
    logical :: ishumn(2)
    integer(I8) :: a, b, req0

    ! If humn return directly
    if (this%name==HUMN) then
      val = req
      return
    end if

    ! Assert this is an operator with only one 
    ! humn dependend operand
    if (this%op == OP_VAL) error stop 'expected - value not expected'
    do i=1,2
      m(i) = monkey_find(this%opnames(i), monkeys)
      if (m(i)<1) error stop 'monkey not found'
      ishumn(i) = monkey_ancestorof(monkeys(m(i)),HUMN,monkeys)
    end do
    if (.not. xor(ishumn(1), ishumn(2))) error stop 'expected - not linear dependency'

    ! Inverse the operators
    if (.not. ishumn(1)) a = monkey_eval(monkeys(m(1)),monkeys)
    if (.not. ishumn(2)) b = monkey_eval(monkeys(m(2)),monkeys)

    if (this%name==ROOT) then
      ! Root is an "==" operator
      if (ishumn(1)) req0 = b
      if (ishumn(2)) req0 = a

    else
      select case(this%op)
      case(OP_ADD)
        if (ishumn(1)) req0 = req - b
        if (ishumn(2)) req0 = req - a

      case(OP_MUL)
        if (ishumn(1)) req0 = req / b
        if (ishumn(2)) req0 = req / a

      case(OP_SUB)
        if (ishumn(1)) req0 = req + b
        if (ishumn(2)) req0 = a - req

      case(OP_DIV)
        if (ishumn(1)) req0 = req * b
        if (ishumn(2)) req0 = a / req

      case default
        error stop 'invalid operator'
      end select
    end if

    if (ishumn(1)) val = monkey_expected(monkeys(m(1)), req0, monkeys)
    if (ishumn(2)) val = monkey_expected(monkeys(m(2)), req0, monkeys)
    
  end function monkey_expected

end module day2221_mod