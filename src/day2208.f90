module day2208_mod
  use parse_mod, only : read_strings, string_t
  implicit none
  private
  public day2208

  integer, parameter :: DIR(2,4)=reshape([-1,0, 1,0, 0,-1, 0,1],[2,4])

contains
  subroutine day2208(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    integer, allocatable :: map(:,:), viewdist(:,:,:)
    logical, allocatable :: visible(:,:,:)
    integer :: i, j, ni, nj, ans1, ans2

    lines = read_strings(file)
    ! assuming all lines have the same length?
    ni = size(lines)
    nj = len_trim(lines(1)%str)
    allocate(map(ni,nj))
    do i=1,ni
      if (len_trim(lines(i)%str)/=nj) error stop 'day2208 - line has different size'
      do j=1,nj
        read(lines(i)%str(j:j),'(i1)') map(i,j)
      end do
    end do
    print '("Tree map size ",i0," x ",i0)', ni, nj
    visible = check_visibility(map)
    viewdist = check_viewdist(map)

    ans1 = count(any(visible,dim=1))
    ans2 = maxval(product(viewdist,dim=1))
    print '("Answer 8/1 ",i0,l2)', ans1, ans1==1803
    print '("Answer 8/2 ",i0,l2)', ans2, ans2==268912
  end subroutine day2208


  function check_visibility(map) result(visible)
    integer, intent(in) :: map(:,:)
    logical :: visible(4, size(map,1), size(map,2))

    integer :: i, j, i0, j0, k

    visible = .true.
    do i=1,size(map,1)
    do j=1,size(map,2)
      do k=1,4
        i0 = i
        j0 = j
        do
          i0 = i0 + DIR(1,k)
          j0 = j0 + DIR(2,k)
          if (i0<1 .or. j0<1 .or. i0>size(map,1) .or. j0>size(map,2)) exit
          if (map(i0,j0)>=map(i,j)) then
            visible(k,i,j) = .false.
            exit
          end if
        end do
      end do
    end do
    end do
  end function check_visibility


  function check_viewdist(map) result(viewdist)
    integer, intent(in) :: map(:,:)
    integer :: viewdist(4, size(map,1), size(map,2))

    integer :: i, j, i0, j0, k

    viewdist= 0
    do i=1,size(map,1)
    do j=1,size(map,2)
      do k=1,4
        i0 = i
        j0 = j
        do
          i0 = i0 + DIR(1,k)
          j0 = j0 + DIR(2,k)
          if (i0<1 .or. j0<1 .or. i0>size(map,1) .or. j0>size(map,2)) exit
          viewdist(k,i,j) = viewdist(k,i,j)+1
          if (map(i0,j0)>=map(i,j)) then
            exit
          end if
        end do
      end do
    end do
    end do
  end function check_viewdist

end module day2208_mod