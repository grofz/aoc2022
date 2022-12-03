module day2204_mod
  use parse_mod, only : string_t, read_strings
  implicit none

contains

  subroutine day2204(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)

    !lines = read_strings(file)
    print *,'ready for day 4!'
  end subroutine day2204

end module day2204_mod