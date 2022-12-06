module day2207_mod
  use parse_mod, only : string_t, read_strings
  implicit none
contains
  subroutine day2207(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)

    !lines = read_strings(file)
    print *, 'day 7 ready '//file
  end subroutine day2207
end module day2207_mod