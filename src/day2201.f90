module day2201_mod
  use parse_mod, only : string_t, read_strings
  implicit none
  private
  public day2201

contains

  subroutine day2201(file)
    character(len=*), intent(in)  :: file

    type(string_t), allocatable :: lines(:)
    integer, allocatable :: cals(:)

    integer :: nelves, i, cal_item, ans1, ans2, j

    lines = read_strings(file)
    ! each line of cals corresponds to a single elf
    ! there wont be more elves than lines in the input
    allocate(cals(size(lines)))
    cals = 0

    nelves = 1
    do i=1,size(lines)
      if (len_trim(lines(i)%str)==0) then
        nelves = nelves + 1
      else
        read(lines(i)%str,*) cal_item
        cals(nelves) = cals(nelves) + cal_item
      end if
    end do
    ans1 = maxval(cals(1:nelves))
    print '("Answer 1/1 ",i0,l2)', ans1, ans1==67450

    ! Part 2
    ans2 = 0
    do i=1,3
      j = maxloc(cals(1:nelves), dim=1)
      ans2 = ans2 + cals(j)
      ! to find the next largest value in the next cycle
      cals(j) = 0
    end do
    print '("Answer 1/2 ",i0,l2)', ans2, ans2==199357
  end subroutine day2201

end module day2201_mod
