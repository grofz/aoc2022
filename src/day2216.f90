module day2216_mod
  use parse_mod, only : string_t, read_strings
  implicit none
contains

  subroutine day2216(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)

    !lines = read_strings(file)
    print *, 'Day 16'
  end subroutine
end module day2216_mod