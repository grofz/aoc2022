module day2213_mod
  use parse_mod, only : string_t, read_strings
  implicit none

contains
  subroutine day2213(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)

    !lines = read_strings(file)
    print *, 'happy day 13'
  end subroutine day2213
end module day2213_mod