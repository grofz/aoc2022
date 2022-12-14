module day2214_mod
  use parse_mod, only : read_strings, string_t
  implicit none
  private
  public day2214

  integer, parameter :: XSIZE=664, YSIZE=163
  integer, parameter :: XSOURCE=500, YSOURCE=0
  type map_t
    character(len=1) :: b(0:XSIZE-1,0:YSIZE-1)
  end type
  character(len=1), parameter :: CH_AIR='.', CH_ROCK='#', CH_SAND='o'

contains
  subroutine day2214(file)
    character(len=*), intent(in) :: file

    type(map_t) :: map
    type(string_t), allocatable :: lines(:)
    integer :: i, ans, lims(4)
    logical :: isrest

    map%b = CH_AIR
    lines = read_strings(file)
    !lines = read_strings('inp/14/test.txt')
    do i=1,size(lines)
      call draw_line(lines(i)%str, map)
    end do

    ! bottom rock-bed for part 2
    lims = limits(map)
    map%b(:,lims(4)+2) = CH_ROCK

    ! fill by sand
    ans = 0
    do
      call place_sand(map, isrest)
      if (.not. isrest) exit
      ans = ans + 1
    end do
    call map_display(map)
    print '("Answer 14/2 ",i0,l2)', ans, ans==23925
  end subroutine day2214


  subroutine draw_line(str, map)
    character(len=*), intent(in) :: str
    type(map_t), intent(inout) :: map

    integer :: ib, ie, x, y, xn, yn

    ib = 1
    do
      ie = index(str(ib:), '->')
      ie = ib + ie - 1
      if (ie < ib) ie = len_trim(str)+1
      read(str(ib:ie-1), *) xn, yn
      if (ib /= 1) call draw(x,y,xn,yn)
      x = xn
      y = yn
      ib = ie+2
      if (ib > len_trim(str)) exit
    end do
  contains
    subroutine draw(x0,y0,x1,y1)
      integer, intent(in) :: x0, y0, x1, y1
      integer :: i, j
      if (x0<1 .or. x1<1 .or. x0>size(map%b,1) .or. x1>size(map%b,1)) error stop 'draw - out of map xposition'
      if (y0<1 .or. y1<1 .or. y0>size(map%b,2) .or. y1>size(map%b,2)) error stop 'draw - out of map yposition'
      do i=min(x0,x1), max(x0,x1)
      do j=min(y0,y1), max(y0,y1)
        map%b(i,j) = CH_ROCK
      end do
      end do
    end subroutine
  end subroutine draw_line


  subroutine map_display(this)
    class(map_t), intent(in) :: this

    integer :: i, j, nx, ny
    nx = size(this%b,1)
    ny = size(this%b,2)
    do j=0,ny-1
      write(*,'(*(a1))') (this%b(i,j), i=0,nx-1)
    end do
  end subroutine


  subroutine place_sand(map, isrest)
    class(map_t), intent(inout) :: map
    logical, intent(out) :: isrest

    integer :: x, y

    isrest = .false.
    if (map%b(XSOURCE, YSOURCE)/=CH_AIR) then
      print *, 'place_sand - full'
      return
    end if

    x = XSOURCE
    y = YSOURCE
    do
      if (map%b(x,y)/=CH_AIR) error stop 'place_sand - went wrong'
      if (x<0 .or. x>size(map%b,1)-1) error stop 'place_sand - went xposition'
      if (y+1>size(map%b,2)-1) exit
      if (map%b(x,y+1)==CH_AIR) then
        y = y + 1
      else if (map%b(x-1,y+1)==CH_AIR) then
        y = y + 1
        x = x - 1
      else if (map%b(x+1,y+1)==CH_AIR) then
        y = y + 1
        x = x + 1
      else 
        isrest = .true.
        exit
      end if
    end do
    if (isrest) map%b(x,y) = CH_SAND
  end subroutine


  function limits(this) result(lims)
    class(map_t), intent(in) :: this
    integer :: lims(4)

    integer :: i, j, nx, ny
    nx = size(this%b,1)
    ny = size(this%b,2)
    lims(1) = huge(lims)
    lims(3) = huge(lims)
    lims(2) = -huge(lims)
    lims(4) = -huge(lims)
    do i=0, nx-1
    do j=0, ny-1
      if (this%b(i,j)==CH_AIR) cycle
      if (i > lims(2)) lims(2) = i
      if (i < lims(1)) lims(1) = i
      if (j > lims(4)) lims(4) = j
      if (j < lims(3)) lims(3) = j
    end do
    end do
  end function
end module day2214_mod