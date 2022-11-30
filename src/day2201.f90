module day2201_mod
  use parse_mod, only : string_t, read_strings
  implicit none

contains
  subroutine day2201(file)
    character(len=*), intent(in)  :: file
    print *, "Hello, aoc2022! ", file
  end subroutine day2201
end module day2201_mod
