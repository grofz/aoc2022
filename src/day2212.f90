module day2212_mod
  use parse_mod, only : string_t, read_strings
  implicit none
  private
  public day2212
contains
  subroutine day2212(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)

    !lines = read_strings(file)
    print *, 'DDDAAAYY 12'
  end subroutine day2212

end module day2212_mod