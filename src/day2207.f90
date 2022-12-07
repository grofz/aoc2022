module day2207_mod
  use parse_mod, only : string_t, read_strings, split
  implicit none

  type item_t
    logical :: isdir = .false.
    type(item_ptr), allocatable :: ch(:)
    type(item_t), pointer :: p => null()
    type(string_t) :: name
    integer :: size
  end type item_t

  type item_ptr
    type(item_t), pointer :: ptr
  end type

  integer :: smallsize, smallestneeded, tobedeleted

contains
  subroutine day2207(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:), tokens(:)
    integer :: i, cursize, totsize
    type(item_t), pointer :: root, curdir, new

    integer, parameter :: totspace = 70000000
    integer, parameter :: reqspace = 30000000

    lines = read_strings(file)
    root => item_new('/', 0, .true., null())
    curdir => root

    do i=2, size(lines)
      call split(lines(i)%str,' ',tokens)
      if (lines(i)%str(1:4)=='$ cd') then
        curdir => item_cd(curdir, tokens(3)%str)
        cycle
      else if (lines(i)%str(1:4)=='$ ls') then
        print *, 'reading the content of curdir ',curdir%name%str
        cycle
      end if

      ! read content of a current directory
      if (tokens(1)%str=='dir') then
        new => item_new(tokens(2)%str, 0, .true., curdir)
      else
        read(tokens(1)%str,*) cursize
        new => item_new(tokens(2)%str, cursize, .false.,curdir)
      end if
      call item_add(curdir, new)
      print *, 'added ',new%name%str, new%size
    end do

    print *, 'directory read'
    smallsize = 0
    call calc_size(root, totsize)
    print *, 'ans1', smallsize

    tobedeleted = reqspace - (totspace - totsize)
    smallestneeded = totsize
    print *, 'to be deleted ',tobedeleted
    call calc_size(root, totsize)
    print *, 'ans2', smallestneeded
  end subroutine day2207


  function item_new(name, size, isdir, p) result(new)
    character(len=*), intent(in) :: name
    integer, intent(in) :: size
    logical, intent(in) :: isdir
    type(item_t), pointer, intent(in) :: p
    type(item_t), pointer :: new

    allocate(new)
    new % name = string_t(name)
    new % isdir = isdir
    if (isdir) then
      new % size = 0
    else
      new % size = size
    end if
    new % p => p
  end function item_new


  subroutine item_add(this, item)
    class(item_t), intent(inout), target :: this
    type(item_t), intent(inout), pointer :: item

    type(item_ptr), allocatable :: wrk(:)
    integer :: nch

    if (.not. this%isdir) error stop 'item_add - file can not contain anything'
    if (.not. allocated(this%ch)) allocate(this%ch(0))

    nch = size(this%ch)
    allocate(wrk(nch+1))
    wrk(1:nch) = this%ch
    wrk(nch+1)%ptr => item
    item % p => this
    this % size = this % size + item % size
    call move_alloc(wrk, this%ch)
  end subroutine


  function item_cd(this, name) result(dir)
    class(item_t), intent(in) :: this
    character(len=*), intent(in) :: name
    type(item_t), pointer :: dir

    integer :: i, nch
    if (.not. this%isdir) error stop 'item_cd - not a directory'

    if (name == '/') then
      print *, 'cahnging to root'
      error stop
    end if

    if (name == '..') then
      dir => this % p
    else
      nch = size(this%ch)
      do i=1,nch
        if (this%ch(i)%ptr%name%str==name) then
          dir => this%ch(i)%ptr
          exit
        end if
      end do
      if (i==nch+1) error stop 'item_cd - not found'
    end if

  end function


  recursive subroutine calc_size(dir, totsize)
    type(item_t), pointer :: dir
    integer :: totsize

    integer :: i, size0

    if (.not. dir%isdir) then
      totsize = dir%size
      return
    end if
    totsize = 0
    do i=1, size(dir%ch)
      call calc_size(dir%ch(i)%ptr, size0)
      totsize = totsize + size0
    end do
    dir%size = totsize
  print *, 'size ',dir%size, dir%name%str
  if (dir%size <= 100000) smallsize = smallsize+dir%size
  if (dir%size >= tobedeleted) then
    if (dir%size<smallestneeded) smallestneeded = dir%size
  end if
  end subroutine
  

  recursive function totsize_small(dir) result(sizesmall)
    type(item_t), pointer :: dir
    integer :: sizesmall, i, size0

    sizesmall = 0
    if (.not. dir%isdir) return
    !if (dir%size > 100000) return
    do i=1,size(dir%ch)
      size0 = totsize_small(dir%ch(i)%ptr)

    end do



  end function
end module day2207_mod