module day2215_mod
  use parse_mod, only : read_strings, string_t
  implicit none
contains

  subroutine day2215(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)

    print *, 'Day 15'
  end subroutine day2215
end module day2215_mod