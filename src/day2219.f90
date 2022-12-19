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

  type blueprint_t
    integer :: id
    type(relation_t), allocatable :: rels(:)
  end type

contains

  subroutine day2219(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    type(blueprint_t), allocatable :: bb(:)
    integer :: i, j

    !lines = read_strings(file)
    lines = read_strings('inp\19\test.txt')
    allocate(bb(size(lines)))
    do i=1,size(lines)
      bb(i) = blueprint_new(lines(i)%str)
    end do
  end subroutine day2219


  type(blueprint_t) function blueprint_new(str) result(new)
    character(len=*), intent(in) :: str

    type(relation_t) :: rel
    integer :: i0, i1, is

    i0 = index(str,'Blueprint')
    i1 = index(str, ':')
    read(str(i0+10:i1-1),*) new%id

    allocate(new%rels(0))
    is = 1
    do
      i0 = index(str(is:),'Each')
      i1 = scan(str(is:),'.')
      i0 = is + i0 - 1
      i1 = is + i1 - 1
      rel = relation_t(str(i0:i1))
      new%rels = [new%rels, rel]
      if (i1==len_trim(str)) exit
      is = i1 + 1
    end do
  print *, new%id,': We have ',size(new%rels),' relations'
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
print *, '*'//new%product%str//'* <--'

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
print *, '-',new%num(size(new%num)), '*'//new%src(size(new%src))%str//'*'
      if (is_last_item) exit
      is = i1+4
    end do
print *,'  there is ',size(new%num),' ingredients'
  end function relation_new

end module day2219_mod