module day2218_mod
  use parse_mod, only : string_t, read_strings
  implicit none
  private
  public day2218

  integer, parameter :: PORE=0, SOLID=1, WATER=2
  integer, parameter :: DIR(3,6) = reshape(&
  & [1,0,0, -1,0,0, 0,1,0, 0,-1,0, 0,0,1, 0,0,-1], [3,6])

contains

  subroutine day2218(file)
    character(len=*), intent(in) :: file

    integer, allocatable :: aa(:,:,:)
    integer :: ans1, ans2

    aa = read_matrix(file)
    call fill_water(aa)
    associate (w=>count(aa==WATER), p=>count(aa==PORE), s=>count(aa==SOLID))
      print '("Porosity: external ",f4.1,",  internal ",f4.1,",  rock ",f4.1)', &
      & real(100*w)/real(w+p+s), &
      & real(100*p)/real(w+p+s), &
        real(100*s)/real(w+p+s)
    end associate
    ans1 = count_area(aa, .false.)
    ans2 = count_area(aa, .true.)
    print '("Answer 18/1 ", i0, l2)', ans1, ans1==4340
    print '("Answer 18/2 ", i0, l2)', ans2, ans2==2468
  end subroutine day2218


  function read_matrix(file) result(aa)
    character(len=*), intent(in) :: file
    integer, allocatable :: aa(:,:,:)

    type(string_t), allocatable :: lines(:)
    integer :: i, x(3), x0(3), x1(3)

    ! find required dimensions, leave gap at boundaries
    lines = read_strings(file)
    x0 = +huge(x0)
    x1 = -huge(x1)
    do i = 1, size(lines)
      x = read_line(lines(i)%str)
      where (x < x0) x0=x
      where (x > x1) x1=x
    end do
    x0 = x0 - 1
    x1 = x1 + 1

    ! fill matrix by a rock
    allocate(aa(x0(1):x1(1), x0(2):x1(2), x0(3):x1(3)))
    aa = PORE
    do i = 1, size(lines)
      x = read_line(lines(i)%str)
      aa(x(1),x(2),x(3)) = SOLID
    end do
    if (count(aa==SOLID)/=size(lines)) &
    &   error stop 'read_matrix - rock volume and number of lines inconsistency'
  end function read_matrix


  pure function read_line(str) result(x)
    character(len=*), intent(in) :: str
    integer :: x(3)

    integer :: p0, p1

    p0 = scan(str,',')
    p1 = scan(str,',', BACK=.true.)
    read(str(:p0-1),*) x(1)
    read(str(p0+1:p1-1),*) x(2)
    read(str(p1+1:),*) x(3)
  end function read_line


  pure integer function count_area(aa, mode) result (area)
    integer, intent(in) :: aa(:,:,:)
    logical, intent(in) :: mode 
!
! Calculate external area of rocks. Mode:
! - F: all pores are counted
! - T: only closed pores are counted
!
    integer :: i, j, k, d, n(3)

    area = 0
    do i=1,size(aa,1)
    do j=1,size(aa,2)
    do k=1,size(aa,3)
      if (aa(i,j,k)/=SOLID) cycle
      do d=1,6
        n = [i,j,k]+DIR(:,d)
        select case(aa(n(1),n(2),n(3)))
        case(WATER)
          area = area + 1
        case(PORE)
          if (.not. mode) area = area + 1
        end select
      end do
    end do
    end do
    end do
  end function count_area


  pure subroutine fill_water(aa)
    integer, intent(inout) :: aa(:,:,:)
!
! Fill external pores by water
!
    logical :: was_flood, isfinished(size(aa,1),size(aa,2),size(aa,3))
    integer :: i, j, k, d, n(3), n_pore, n_water, n_solid

    ! Make sure there is only pore/water at the boundary
    ! Fill boundary by water to speed-up flooding
    n_pore = count(aa==PORE)
    n_water = count(aa==WATER)
    n_solid = count(aa==SOLID)
    aa(1,:,:) = WATER
    aa(:,1,:) = WATER
    aa(:,:,1) = WATER
    aa(size(aa,1),:,:) = WATER
    aa(:,size(aa,2),:) = WATER
    aa(:,:,size(aa,3)) = WATER
    if (count(aa==SOLID)/=n_solid) error stop 'solid at boundary?'
    if (count(aa==PORE .or. aa==WATER) /= n_water+n_pore) error stop 'some thing in border gap'

    ! Logical array marking unfinished water voxels
    where (aa==WATER)
      isfinished = .false.
    else where
      isfinished = .true.
    end where

    MAIN: do
      was_flood = .false.
      ! Loop over unfinished WATER voxels...
      do i = 1, size(aa,1)
      do j = 1, size(aa,2)
      do k = 1, size(aa,3)
        if (isfinished(i,j,k)) cycle

        !... and flood all its PORE neighbors
        isfinished(i,j,k) = .true.
        do d=1,6
          n = [i,j,k] + DIR(:,d)
          if (any(n<1) .or. n(1)>size(aa,1) .or. n(2)>size(aa,2) .or. n(3)>size(aa,3)) cycle
          if (aa(n(1),n(2),n(3)) /= PORE) cycle

          was_flood = .true.
          aa(n(1),n(2),n(3)) = WATER
          isfinished(n(1),n(2),n(3)) = .false.
        end do
      end do
      end do
      end do
      if (.not. was_flood) exit
    end do MAIN
  end subroutine fill_water

end module day2218_mod
