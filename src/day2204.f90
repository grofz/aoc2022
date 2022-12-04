module day2204_mod
  use parse_mod, only : string_t, read_strings, split
  implicit none
  private
  public day2204

  type intpair_t
    integer :: a(2), b(2)
  end type
  interface intpair_t
    module procedure intpair_new
  end interface

contains

  subroutine day2204(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    type(intpair_t), allocatable :: ints(:)
    integer :: i, n, ans1, ans2

    lines = read_strings(file)
    n = size(lines)
    allocate(ints(n))
    ans1 = 0
    ans2 = 0
    do i=1,n
      ints(i) = intpair_t(lines(i)%str)
      if (fully_overlap(ints(i))) ans1 = ans1+1
      if (overlap(ints(i))) ans2 = ans2+1
    end do
    print '("Answer 4/1 ",i0,l2)', ans1, ans1==475
    print '("Answer 4/2 ",i0,l2)', ans2, ans2==825
  end subroutine day2204


  type(intpair_t) function intpair_new(str) result (new)
    character(len=*), intent(in) :: str

    type(string_t), allocatable :: elfs(:), int1(:), int2(:)

    call split(str,',',elfs)
    call split(elfs(1)%str,'-',int1)
    call split(elfs(2)%str,'-',int2)
    read(int1(1)%str,*) new%a(1)
    read(int1(2)%str,*) new%b(1)
    read(int2(1)%str,*) new%a(2)
    read(int2(2)%str,*) new%b(2)
  end function intpair_new


  logical function fully_overlap(ip)
    type(intpair_t), intent(in) :: ip

    integer :: i

    fully_overlap = .false.
    do i=1,2
      if (ip%a(i) <= ip%a(3-i) .and. ip%b(i) >= ip%b(3-i)) then
        fully_overlap = .true.
        exit
      end if
    end do
  end function fully_overlap


  logical function overlap(ip)
    type(intpair_t), intent(in) :: ip

    overlap = .false.
    if (ip%a(1) <= ip%a(2)) then
      if (ip%b(1)>=ip%a(2)) overlap = .true.
    else
      if (ip%b(2)>=ip%a(1)) overlap = .true.
    end if
  end function overlap

end module day2204_mod