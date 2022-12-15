  module tiles_mod
    implicit none
    private
    public operator(-)

    type interval_t
      integer :: x(2) = [0, -1] ! empty interval
      logical :: flag(2) = .false.
    contains
      procedure :: isnull => interval_isnull
    end type

    type, public :: tile_t
      type(interval_t) :: lim(2)
    contains
      procedure :: area => tile_area
    end type
    interface tile_t
      module procedure tile_from_lims, tile_new
    end interface

    interface operator(-)
      module procedure tile_subtract
    end interface

  contains

    type(tile_t) function tile_from_lims(lim) result(new)
      integer, intent(in) :: lim(4)

      type(interval_t) :: ints(2)

      ints(1) = interval_new([lim(1), lim(2)], [.false., .false.])
      ints(2) = interval_new([lim(3), lim(4)], [.false., .false.])
      new = tile_new(ints)
    end function


    type(tile_t) function tile_new(ints) result(new)
      type(interval_t), intent(in) :: ints(2)

      integer :: i
      
      do i=1,2
        new%lim(i) = ints(i)
        new%lim(i)%flag = .false.
      end do
    end function


    type(interval_t) function interval_new(x,flag) result(new)
      integer :: x(2)
      logical :: flag(2)

      if (x(2)>=x(1)) then
        new%x = x
        new%flag = flag
      else
        ! leave default initialization
      end if
    end function


    function interval_combine(inta, intb) result(ints)
      type(interval_t), intent(in) :: inta, intb
      type(interval_t), allocatable :: ints(:)

      type(interval_t) :: a, b, c(3)
      integer :: ni, ii, x1, x2
      logical :: isreverse

      if (inta%x(1) <= intb%x(1)) then
        a = inta
        b = intb
        isreverse = .false.
      else
        a = intb
        b = inta
        call swap_flag(a)
        call swap_flag(b)
        isreverse = .true.
      end if

      ! If any interval is empty, the result is the non-empty one
      ! If both are empty the result is null array
      if (a%isnull() .and. b%isnull()) then
        ni = 0
        goto 100
      else if (a%isnull() .or. b%isnull()) then
        ni = 1
        c(1) = a
        if (a%isnull()) c(1) = b
        goto 100
      end if

      ! There are four points (two intervals) that partition space into three
      ! non-overlaping intervals. Some intervals may be null.
! TODO There must be a way for more compact way

      if (b%x(1) == a%x(1)) then
        c(1) = interval_new([0,-1],[.false.,.false.])
        if (b%x(2) < a%x(2)) then
          c(2) = interval_new([b%x(1),b%x(2)],[.true.,.true.])
          c(3) = interval_new([b%x(2)+1, a%x(2)], a%flag)
        else if (b%x(2) == a%x(2)) then
          c(2) = interval_new([b%x(1),b%x(2)],[.true.,.true.])
          c(3) = interval_new([0,-1],[.false.,.false.])
        else if (b%x(2) > a%x(2)) then
          c(2) = interval_new([a%x(1),a%x(2)],[.true.,.true.])
          c(3) = interval_new([a%x(2)+1, b%x(2)], b%flag)
        else
          error stop 'impossible branch 1'
        end if

      else if (b%x(1) > a%x(1) .and. b%x(1) < a%x(2)) then
        c(1) = interval_new([a%x(1),b%x(1)-1], a%flag)
        if (b%x(2) < a%x(2)) then
          c(2) = interval_new([b%x(1),b%x(2)],[.true.,.true.])
          c(3) = interval_new([b%x(2)+1, a%x(2)], a%flag)
        else if (b%x(2) == a%x(2)) then
          c(2) = interval_new([b%x(1),b%x(2)],[.true.,.true.])
          c(3) = interval_new([0,-1],[.false.,.false.])
        else if (b%x(2) > a%x(2)) then
          c(2) = interval_new([b%x(1),a%x(2)],[.true.,.true.])
          c(3) = interval_new([a%x(2)+1, b%x(2)], b%flag)
        else
          error stop 'impossible branch 2'
        end if

      else if (b%x(1) == a%x(2)) then
        c(1) = interval_new([a%x(1), a%x(2)-1], a%flag)
        c(2) = interval_new([a%x(2), b%x(1)],[.true., .true.])
        c(3) = interval_new([b%x(1)+1, b%x(2)], b%flag)
      else if (b%x(1) > a%x(2)) then 
        c(1) = a
        c(2) = interval_new([a%x(2)+1, b%x(1)-1], [.false.,.false.])
        c(3) = b
      else
        error stop 'impossible branch'
      end if

      ! Remove empty intervals
      ni = 0
      do ii=1,3
        if (c(ii)%isnull()) then
        else
          ni = ni + 1
          c(ni) = c(ii)
        end if
      end do

      ! Pack-up the result
      100 allocate(ints(ni))
      do ii=1,ni
        ints(ii) = c(ii)
        if (isreverse) call swap_flag(ints(ii))
      end do

    contains

      subroutine swap_flag(this)
        type(interval_t), intent(inout) :: this
        logical :: tmp
        tmp = this%flag(1)
        this%flag(1) = this%flag(2)
        this%flag(2) = tmp
      end subroutine

    end function interval_combine


    logical function interval_isnull(this) result(isnull)
      class(interval_t), intent(in) :: this
      isnull = .false.
      if (this%x(1)>this%x(2)) isnull = .true.
    end function


    function tile_subtract(tilea, tileb) result(a_sub_b)
      type(tile_t), intent(in) :: tilea, tileb
      type(tile_t), allocatable :: a_sub_b(:)

      type(tile_t) :: a, b
      integer :: i, j, k, z
      type(interval_t), allocatable :: limx(:), limy(:)
      type(tile_t) :: res(9)

      a = tilea
      b = tileb

      do i=1,2
        a%lim(i)%flag = .false.
        b%lim(i)%flag = .false.
        a%lim(i)%flag(1) = .true.
        b%lim(i)%flag(2) = .true.
      end do
      limx = interval_combine(a%lim(1), b%lim(1))
      limy = interval_combine(a%lim(2), b%lim(2))

      z = 0
      do j=1,size(limx)
      do k=1,size(limy)
        if (limx(j)%flag(1) .and. limy(k)%flag(1)) then
          ! tile is A
          if (.not. (limx(j)%flag(2) .and. limy(k)%flag(2)) ) then
            ! and tile is not an intersection A with B
            z = z + 1
            res(z) = tile_new([limx(j), limy(k)])
          end if
        end if
      end do
      end do
      allocate(a_sub_b(z))
      a_sub_b(1:z) = res(1:z)
    end function tile_subtract


    subroutine tile_print(this, board, char)
      class(tile_t), intent(in) :: this
      character(len=1), intent(inout), allocatable :: board(:,:)
      character(len=1), intent(in) :: char

      integer :: i, j
      integer :: i0, i1, j0, j1

      i0 = lbound(board,1)
      j0 = lbound(board,2)
      i1 = ubound(board,1)
      j1 = ubound(board,2)
      do i=max(i0,this%lim(1)%x(1)), min(i1,this%lim(1)%x(2))
      do j=max(j0,this%lim(2)%x(1)), min(j1,this%lim(2)%x(2))
        board(i,j) = char
      end do
      end do
    end subroutine


    subroutine show_board(board)
      character(len=1), intent(in), allocatable :: board(:,:)
      integer :: i, j
      integer :: i0, i1, j0, j1

      i0 = lbound(board,1)
      j0 = lbound(board,2)
      i1 = ubound(board,1)
      j1 = ubound(board,2)
      do j=j1,j0,-1
        write(*,'(*(a))') (board(i,j), i=i0,i1)
      end do
    end subroutine


    integer function tile_area(this) result(area)
      class(tile_t), intent(in) :: this

      area = (this%lim(1)%x(2)-this%lim(1)%x(1)+1)
      area = (this%lim(2)%x(2)-this%lim(2)%x(1)+1) * area
    end function

  end module tiles_mod
