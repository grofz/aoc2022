module day2225_mod
  use iso_fortran_env, only : I8K=>int64
  use parse_mod, only : string_t, read_strings
  implicit none
  private
  public day2225

  ! Maximum number of pentary number digits
  integer, parameter :: MAXLEN=26
contains

  subroutine day2225(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    integer :: i
    integer(I8K) :: v, ans
    character(len=MAXLEN) :: ans2

    lines = read_strings(file)
    !lines = read_strings('inp/25/test.txt')


    ans = 0
    do i = 1, size(lines)
      v = fromsnafu(lines(i)%str)
      print '(i15,2x,a)', v, lines(i)%str
      ans = ans + v
    end do
    print '("Sum is ",i0)', ans
    ans2 = tosnafu(ans)
    print '("Answer 25 ",a,l2)', ans2, ans2=='2-0-0=1-0=2====20=-2' 
    print *, fromsnafu(ans2), fromsnafu(ans2)==ans

  end subroutine day2225


  pure integer function dig2val(dig) result(val)
    character(len=1), intent(in) :: dig
    select case(dig)
    case('2')
      val = 2
    case('1')
      val = 1
    case('0')
      val = 0
    case('-')
      val = -1
    case('=')
      val = -2
    case default
      error stop 'invalid digit'
    end select
  end function dig2val


  pure integer(I8K) function fromsnafu(str) result(val)
    character(len=*), intent(in) :: str

    integer :: i, k, n
    val = 0
    n = len_trim(str)
    do i = n, 1, -1
      k = n-i
      val = val + dig2val(str(i:i))*5_I8K**int(k,I8K)
    end do
  end function fromsnafu


  pure function tosnafu(v) result(str)
    character(len=MAXLEN) :: str
    integer(I8K), intent(in) :: v
!
! Convert a decimal number to pentary 
!
    integer :: i, f(MAXLEN), c
    integer(I8K) :: v0, r, k
    logical :: is_trailing

    ! Convert decimal number to pentary number
    v0 = v
    if (abs(v0) > 5_I8K**int(MAXLEN-1)) error stop 'tosnafu - number too big'
    do k = MAXLEN-1, 0, -1
      f(MAXLEN-k) = v0 / 5_I8K**int(k,I8K)
      r = mod(v0, 5**int(k,I8K))
      v0 = v0 - f(MAXLEN-k)*5_I8K**int(k,I8K) 
    end do

    ! If a digit is 3 or 4: 
    ! change it to -2 or -1 and add 1 to the higher order digit
    do k = MAXLEN, 2, -1
      if (f(k) > 2) then
        f(k) = f(k) - 5
        f(k-1) = f(k-1) + 1
      else if (f(k) < -2) then
        ! Dealing with negative numbers is a bonus
        f(k) = f(k) + 5
        f(k-1) = f(k-1) - 1
      end if
    end do

    ! Convert integers to strings
    str = ''
    is_trailing = .true.
    do i = 1, MAXLEN
      if (abs(f(i)) /= 0) is_trailing = .false.
      select case (f(i))
      case(-2)
        str(i:i) = '='
      case(-1)
        str(i:i) = '-'
      case(0)
        if (is_trailing) then
          str(i:i) = ' '
        else
          str(i:i) = '0'
        end if
      case(1)
        str(i:i) = '1'
      case(2)
        str(i:i) = '2'
      case default
        error stop 'tosnafu - invalid digit'
      end select
    end do
    str = adjustl(str)
  end function tosnafu

end module day2225_mod