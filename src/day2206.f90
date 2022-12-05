module day2206_mod
  use parse_mod, only : string_t, read_strings
  implicit none
contains
  subroutine day2206(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)

    !lines = read_strings(file)
    print *, 'day 6 ready!'
  end subroutine day2206
end module day2206_mod