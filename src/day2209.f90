module day2209_mod
  use parse_mod, only : string_t, read_strings
  implicit none

contains
  subroutine day2209(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)

    !lines = read_strings(file)
    print *, 'day 9'
  end subroutine day2209
end module day2209_mod