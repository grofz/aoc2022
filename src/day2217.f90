module day2217_mod
  use parse_mod, only : read_strings, string_t
  implicit none
  private
  public day2217

contains
  subroutine day2217(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)

    !lines = read_strings(file)

    print *, 'Day 17 saturday '
  end subroutine day2217
end module day2217_mod