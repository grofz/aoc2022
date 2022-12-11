module day2211_mod
  use parse_mod, only : string_t, read_strings, split
  use iso_fortran_env, only : int64
  implicit none
  private
  public day2211

  integer, parameter :: MAXITEMS = 100
  integer, parameter :: OP_ADD = 1, OP_MUL = 2, OP_POW = 3 

  type :: monkey_t
    integer :: id
    integer(int64) :: items(MAXITEMS)
    integer :: ni = 0
    integer :: opcode, oparg, test
    integer :: ifail, ipass
    integer :: counter=0
    integer(int64) :: maxx = 1
  contains
    procedure, pass(from) :: pass_item
    procedure, pass(this) :: inspect_item
    procedure, pass(this) :: where_throw_item
  end type

contains
  subroutine day2211(file)
    character(len=*), intent(in) :: file

    type(monkey_t), allocatable :: monkeys(:)
    integer :: i, im, nm, iround, i1, i2, cnt(100) 
    integer(int64) :: ans2, ans1

    ! Part 1
    monkeys = read_monkeys(file, 1)
    print '("There are ",i0," monkeys in the wood.")', size(monkeys)
    do iround=1, 20
      call one_round(monkeys)
    end do
    ans1 = get_answer(monkeys)
    print '("Answer 11/2 ",i0,l2)', ans1, ans1==57348_int64

    ! Part 2
    monkeys = read_monkeys(file, 2)
    do iround=1, 10000
      call one_round(monkeys)
    end do
    ans2 = get_answer(monkeys)
    print '("Answer 11/2 ",i0,l2)', ans2, ans2==14106266886_int64

  contains
      pure function get_answer(marr) result(ans)
        integer(int64) :: ans
        type(monkey_t), intent(in) :: marr(:)
        integer :: ii
        integer, allocatable :: a(:)
        a = marr(:)%counter
        ii = maxloc(a,1)
        ans = a(ii)
        a(ii) = 0
        ii = maxloc(a,1)
        ans = ans*a(ii)
      end function
  end subroutine day2211


  function read_monkeys(file, ipart) result(monkeys)
    character(len=*), intent(in) :: file
    integer, intent(in) :: ipart
    type(monkey_t), allocatable :: monkeys(:)

    type(string_t), allocatable :: lines(:)
    integer :: i, im, nm 
    integer(int64) :: maxx

    lines = read_strings(file)
    nm = (size(lines)+1)/7
    allocate(monkeys(0:nm-1))
    im = -1
    do i=1, size(lines), 7
      im = im + 1
      monkeys(im) = monkey_new(lines(i:i+5))
      if (monkeys(im)%id /= im) error stop 'read_monkeys - order of monkey''s id mismatch'
    end do
    if (im/=nm-1) error stop 'read_monkeys - mismatch in number of lines'

    ! If part 1, set "maxx" to "1"
    ! If part 2, set "maxx" to least common multiplier
    maxx = 1
    if (ipart==2) maxx = product(monkeys(:)%test)
    monkeys(:)%maxx = maxx
  end function read_monkeys


  type(monkey_t) function monkey_new(lin) result(new)
    type(string_t), intent(in) :: lin(:)

    type(string_t), allocatable :: tokens(:)
    integer :: ind, ind1, i

    ! 1. line - monkey id
    ind = index(lin(1)%str,"Monkey ")
    ind = ind + 7
    ind1 = scan(lin(1)%str,':')
    read(lin(1)%str(ind:ind1-1),*) new%id

    ! 2. line - starting items
    ind = index(lin(2)%str,"Starting items: ")
    ind = ind + 16
    call split(lin(2)%str(ind:),',',tokens)
    do i=1, size(tokens)
      read(tokens(i)%str,*) new%items(i)
    end do
    new%ni = size(tokens)

    ! 3. line - operation
    ind = index(lin(3)%str,"Operation: new = old ")
    ind = ind + 21
    if (lin(3)%str(ind:)=='* old') then
      new%opcode = OP_POW
      new%oparg = 2
    else if (lin(3)%str(ind:ind)=='*') then
      new%opcode = OP_MUL
    else if (lin(3)%str(ind:ind)=='+') then
      new%opcode = OP_ADD
    else
      error stop 'impossible branch'
    end if
    if (new%opcode /= OP_POW) then
      read(lin(3)%str(ind+1:),*) new%oparg
    end if

    ! 4. line - test
    ind = index(lin(4)%str,"Test: divisible by ")
    ind = ind + 19
    read(lin(4)%str(ind:),*) new%test

    ! 5-6. line - true/false
    ind = index(lin(5)%str,"If true: throw to monkey ")
    ind = ind + 25
    read(lin(5)%str(ind:),*) new%ipass
    ind = index(lin(6)%str,"If false: throw to monkey ")
    ind = ind + 26
    read(lin(6)%str(ind:),*) new%ifail

    return
print '("Monkey: ",i0)', new%id
print '("  has items: ", *(i0,1x))', new%items(1:new%ni)
print '("  operation: ",i0,1x,i0)', new%opcode, new%oparg
print '("  test and T/F outcomes ",i0,1x,i0,1x,i0)', new%test ,new%ipass, new%ifail
  end function


  subroutine one_round(monkeys)
    type(monkey_t), intent(inout) :: monkeys(0:)

    integer :: im, id

    do im = 0, size(monkeys)-1
      ! inspect and pass all items
      do id = 1, monkeys(im)%ni
        call monkeys(im)%inspect_item(id)
        call monkeys(im)%pass_item(id, monkeys( monkeys(im)%where_throw_item(id) ))
      end do

      ! clear list of items
      monkeys(im)%counter = monkeys(im)%counter+monkeys(im)%ni
      monkeys(im)%ni = 0
    end do
  end subroutine one_round


  subroutine pass_item(from, id, to)
    class(monkey_t), intent(in) :: from
    integer, intent(in) :: id
    type(monkey_t), intent(inout) :: to

    to%ni = to%ni + 1
    if (to%ni > MAXITEMS) error stop 'Too many items for a little monkey. Increase MAXITEMS and recompile'
    to%items(to%ni) = from%items(id)
  end subroutine pass_item


  subroutine inspect_item(this, id)
    class(monkey_t), intent(inout) :: this
    integer, intent(in) :: id

    if (id > this%ni) error stop 'inspect_item - not so many items'
    associate(i=>this%items(id))
      select case(this%opcode)
      case(OP_POW)
        i = i * i
      case(OP_MUL)
        i = i * this%oparg
      case(OP_ADD)
        i = i + this%oparg
      end select

      ! decrease the worrying level
      if (this%maxx==1) then
        i = i/3
      else
        if (i > this%maxx) i = mod(i, this%maxx)
      end if
    end associate
  end subroutine inspect_item


  pure integer function where_throw_item(this, id) result(to)
    class(monkey_t), intent(in) :: this
    integer, intent(in) :: id
    if (id > this%ni) error stop 'where_throw_item - not so many items'

    if (mod(this%items(id), this%test)==0) then
      to = this%ipass
    else
      to = this%ifail
    end if
  end function where_throw_item


  subroutine print_items(this)
    type(monkey_t), intent(in) :: this
    print *, 'Monkey ',this%id, ' has items '
    print '(*(i0,1x))', this%items(1:this%ni)
  end subroutine

end module day2211_mod