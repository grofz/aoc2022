module day2203_mod
  use parse_mod, only : string_t, read_strings
  implicit none
  private
  public day2203

contains
  subroutine day2203(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: L(:)
    integer :: i, ans1, ans2

    L = read_strings(file)
    ans1 = 0
    do i=1,size(L)
      ans1 = ans1 + priority(dupl(L(i)%str))
    end do
    print '("Answer 3/1 ",i0,l2)', ans1, ans1==8105

    ans2 = 0
    do i=1,size(L)-2, 3
      ans2 = ans2 + priority(dupl2(L(i)%str,L(i+1)%str,L(i+2)%str))
    end do
    print '("Answer 3/2 ",i0,l2)', ans2, ans2==2363
  end subroutine day2203


  character(len=1) function dupl(str) result(ch)
    character(len=*), intent(in) :: str
!
! Find a letter that appears both in the first and the second
! half of the string.
!
    integer :: n, i, j

    if (mod(len(str),2)/=0) error stop 'dupl - odd number of items'
    n = len(str)/2
    do i=1,n
      j = scan(str(n+1:), str(i:i))
      if (j/=0) then
        ch = str(i:i)
        exit
      end if
    end do
    if (i==n+1) error stop 'dupl - letter not found'
  end function dupl


  character(len=1) function dupl2(s1, s2, s3) result(ch)
    character(len=*), intent(in) :: s1, s2, s3
!
! Find a letter that appears in all three strings.
!
    integer :: i, j2, j3

    do i=1,len(s1)
      j2 = scan(s2, s1(i:i))
      j3 = scan(s3, s1(i:i))
      if (j2/=0 .and. j3/=0) then
        ch = s1(i:i)
        exit
      end if
    end do
    if (i==len(s1)+1) error stop 'dupl2 - letter not found'
  end function dupl2


  integer function priority(ch)
    character(len=1), intent(in) :: ch
!
! Priority of lower and upper case letters is 1-26 and 26-52 respectively
!
    if (iachar(ch)>=iachar('a')) then
      priority = iachar(ch) - iachar('a')+1
    else
      priority = iachar(ch) - iachar('A')+27
    end if
  end function priority

end module day2203_mod