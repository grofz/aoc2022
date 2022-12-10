module day2211_mod
  use parse_mod, only : string_t, read_strings
  implicit none
  private
  public day2211

contains
  subroutine day2211(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)

    !lines = read_strings(file)
    print *, 'day 11 is waiting'
  end subroutine day2211

end module day2211_mod