module day2210_mod
  use parse_mod, only : string_t, read_strings
  implicit none
  private
  public day2210

contains
  subroutine day2210(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)

    !lines = read_strings(file)
    print *, 'hello from day10'
  end subroutine day2210

end module day2210_mod