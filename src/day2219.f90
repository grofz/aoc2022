module day2219_mod
  use parse_mod, only : string_t, read_strings
  implicit none
  private
  public day2219

contains

  subroutine day2219(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)

    print *, 'Day 19 for Monday morning'
  end subroutine day2219

end module day2219_mod