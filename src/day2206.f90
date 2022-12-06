module day2206_mod
  use parse_mod, only : string_t, read_strings
  implicit none
  private
  public day2206

contains

  subroutine day2206(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    integer :: ans1, ans2
    ! required number of different characters for parts 1 and 2
    integer, parameter :: kdif(2) = [4, 14]

    lines = read_strings(file)
    if (size(lines)/=1) error stop 'day2206 - just one line of input expected'
    ans1 = first_different(lines(1)%str, kdif(1))
    print '("Answer 6/1 ",i0,l2)', ans1, ans1==1892
    ans2 = first_different(lines(1)%str, kdif(2))
    print '("Answer 6/1 ",i0,l2)', ans2, ans2==2313
  end subroutine day2206


  function first_different(str, kdif) result(ans)
    character(len=*), intent(in) :: str
    integer, intent(in) :: kdif
    integer :: ans
!
! Return the lowest position "ans" in the string for which
! "kdif" previous characters (including the character at the
! position "ans") are different
!
    integer :: i, n

    n = len(str)
    do i=kdif, n
      if (alldifferent(str(i-kdif+1:i))) exit
    end do
    ans = i
    if (i==n+1) print *, 'first_different - position not found'
  end function


  function alldifferent(str) 
    character(len=*), intent(in) :: str
    logical alldifferent
!
! Are all characters in the string different?
!
    integer :: n, i, j

    n = len(str)
    alldifferent = .true.
    do i=1,n-1
      do j=i+1,n
        if (str(i:i)==str(j:j)) then
          alldifferent=.false.
          return
        end if
      end do
    end do
  end function
end module day2206_mod