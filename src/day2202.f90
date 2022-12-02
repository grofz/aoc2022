module day2202_mod
  use parse_mod, only : string_t, read_strings
  implicit none
  private
  public day2202

  integer, parameter :: PWIN=6, PLOST=0, PDRAW=3

contains

  subroutine day2202(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    integer :: i, ans1, ans2

    lines = read_strings(file)
    ans1=0
    ans2=0
    do i=1,size(lines)
      associate(s=>lines(i)%str)
        ans1 = ans1 + points(s)
        ans2 = ans2 + points(s(1:1)//'$'//my_choice(s))
      end associate
    end do
    print '("Answer 2/1 ",i0,l2)', ans1, ans1==11603
    print '("Answer 2/2 ",i0,l2)', ans2, ans2==12725
  end subroutine day2202


  integer function value(ch)
    character(len=1), intent(in) :: ch
    select case(ch)
    case('X','A') ! rock
      value = 1
    case('Y','B') ! paper
      value = 2
    case('Z','C') ! scissors
      value = 3
    case default
      error stop 'value - invalid'
    end select
  end function value


  integer function outcome(chelf, chme)
    character(len=1), intent(in) :: chme, chelf
    integer :: me, elf, mewin

    ! my and elf's move
    me = value(chme)
    elf = value(chelf)
    ! value: 1-ROCK, 2-PAPER, 3-SCISSORS
    ! if elf selects previous move, I win (mewin = me-1)
    mewin = mod(1+me,3)+1
    if (elf==mewin) then
      outcome = PWIN
    else if (elf==me) then
      outcome = PDRAW
    else
      outcome = PLOST
    end if
  end function outcome


  integer function points(str)
    character(len=*) str
    points = value(str(3:3)) + outcome(str(1:1),str(3:3))
  end function points


  character(len=1) function my_choice(str) result(mech)
    character(len=*) str
    integer :: out(3), i
    character(len=1) :: CHOICE(3)=['X', 'Y', 'Z']

    do i=1,3
      out(i) = outcome(str(1:1), CHOICE(i))
    end do
    select case(str(3:3))
    case('X')
      i = findloc(out,PLOST,dim=1)
    case('Y')
      i = findloc(out,PDRAW,dim=1)
    case('Z')
      i = findloc(out,PWIN,dim=1)
    case default
      error stop 'my_choice - invalid value'
    end select
    mech = CHOICE(i)
  end function my_choice

end module day2202_mod