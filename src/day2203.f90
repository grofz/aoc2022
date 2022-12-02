module day2203_mod
  use parse_mod, only : string_t, read_strings
  implicit none

contains
  subroutine day2203(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)

    !lines = read_strings(file)
    print *, 'Day 3 skeleton ready'
  end subroutine
end module day2203_mod