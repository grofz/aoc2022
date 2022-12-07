module day2208_mod
  use parse_mod, only : read_strings, string_t
  implicit none

contains
  subroutine day2208(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)

    !lines = read_strings(file)
    print *, 'day 8 ready skeleton'
  end subroutine day2208
end module day2208_mod