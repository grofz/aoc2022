module day2205_mod
  use parse_mod, only : string_t, read_strings
  implicit none

contains

  subroutine day2205(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    print *, 'day 5 ready!'
  end subroutine
end module day2205_mod