module day2202_mod
  use parse_mod, only : string_t, read_strings
  implicit none

contains

  subroutine day2202(file)
    character(len=*), intent(in) :: file

    print *, 'Ready for Day 2'
  end subroutine day2202
end module day2202_mod