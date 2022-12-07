module day2207_mod
  use parse_mod, only : string_t, read_strings, split
  implicit none
  private
  public day2207

  type item_t
    logical :: isdir = .false.
    type(item_ptr), allocatable :: ch(:)
    type(item_t), pointer :: p => null()
    type(string_t) :: name
    integer :: size = 0
  end type item_t

  type item_ptr
    type(item_t), pointer :: ptr
  end type

  integer, parameter :: SMALLDIR_THRESHOLD = 100000, &
  &                     TOTSPACE = 70000000, REQSPACE = 30000000

contains
  subroutine day2207(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:), tokens(:)
    integer :: i, cursize
    type(item_t), pointer :: root, curdir, new

    integer :: ans1, ans2


    ! Create empty root directory and set it as a current
    root => item_new('/', 0, .true., null())
    curdir => root

    lines = read_strings(file)
    do i=1, size(lines)
      call split(lines(i)%str,' ',tokens)
      if (lines(i)%str(1:4)=='$ cd') then
        ! Change current directory command
        curdir => item_cd(curdir, tokens(3)%str)
        cycle
      else if (lines(i)%str(1:4)=='$ ls') then
        ! List command - can be ignored
        cycle
      end if

      ! Add new file/directory to the current directory
      if (tokens(1)%str=='dir') then
        new => item_new(tokens(2)%str, 0, .true., curdir)
      else
        read(tokens(1)%str,*) cursize
        new => item_new(tokens(2)%str, cursize, .false.,curdir)
      end if
      call item_add(curdir, new)
    end do
    print '(a)', 'The file system reading complete.'

    ans1 = sum_small_dirs(root)
    print '("Answer 7/1 ",i0,l2)', ans1, ans1==1232307

    ans2 = find_smallest_above(root, REQSPACE-(TOTSPACE-root%size))
    print '("Answer 7/2 ",i0,l2)', ans2, ans2==7268994

    ! Free memory
    call item_free(root)
  end subroutine day2207


  function item_new(name, size, isdir, p) result(new)
    type(item_t), pointer :: new
    character(len=*), intent(in) :: name
    integer, intent(in) :: size
    logical, intent(in) :: isdir
    type(item_t), pointer, intent(in) :: p

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
    class(item_t), pointer :: curdir
    integer :: nch

    if (.not. this%isdir) error stop 'item_add - file can not contain anything'
    if (.not. allocated(this%ch)) allocate(this%ch(0))

    ! increase array of pointers to items by one
    nch = size(this%ch)
    allocate(wrk(nch+1))
    wrk(1:nch) = this%ch
    wrk(nch+1)%ptr => item
    item % p => this
    call move_alloc(wrk, this%ch)

    ! up-date size of current directory and all dirs above
    curdir => this
    do
      curdir%size = curdir%size + item%size
      curdir => curdir%p
      if (.not. associated(curdir)) exit
    end do
  end subroutine item_add


  function item_cd(this, name) result(dir)
    class(item_t), intent(in), pointer :: this
    character(len=*), intent(in) :: name
    type(item_t), pointer :: dir
!
! Change directory command: return pointer to "name"
!
    integer :: i, nch
    if (.not. this%isdir) error stop 'item_cd - not a directory'

    if (name == '/') then
      ! "cd /" - goto root
      dir => this
      do
        if (.not. associated(dir%p)) exit
        dir => dir%p
      end do

    else if (name == '..') then
      ! "cd .."
      dir => this % p

    else
      ! "cd <name>"
      nch = size(this%ch)
      do i=1,nch
        if (this%ch(i)%ptr%name%str==name) then
          dir => this%ch(i)%ptr
          exit
        end if
      end do
      if (i==nch+1) error stop 'item_cd - directory not found'
    end if
  end function item_cd


  integer function sum_small_dirs(root) result(ans)
    type(item_t), pointer, intent(in) :: root

    ans = 0
    call crawl(root)
  contains
    recursive subroutine crawl(dir)
      type(item_t), pointer, intent(in) :: dir
      integer :: i

      if (.not. dir%isdir) return
      do i=1,size(dir%ch)
        call crawl(dir%ch(i)%ptr)
      end do
      if (dir%size <= SMALLDIR_THRESHOLD) ans = ans + dir%size
    end subroutine
  end function sum_small_dirs


  integer function find_smallest_above(root, minsize) result(ans)
    type(item_t), pointer, intent(in) :: root
    integer, intent(in) :: minsize

    ans = huge(ans)
    call crawl(root)
  contains
    recursive subroutine crawl(dir)
      type(item_t), pointer, intent(in) :: dir
      integer :: i

      if (.not. dir%isdir) return
      do i=1,size(dir%ch)
        call crawl(dir%ch(i)%ptr)
      end do
      if (dir%size >= minsize) then
        if (dir%size < ans) ans = dir%size
      end if
    end subroutine
  end function find_smallest_above


  recursive subroutine item_free(dir)
    type(item_t), pointer, intent(inout) :: dir

    integer :: i

    ! free all contained files and directories
    if (dir%isdir .and. allocated(dir%ch)) then
      do i=1,size(dir%ch)
        call item_free(dir%ch(i)%ptr)
      end do
    end if

    ! deallocate the actual item
    deallocate(dir)
  end subroutine item_free
  
end module day2207_mod