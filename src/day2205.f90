module day2205_mod
  use parse_mod, only : string_t, read_strings, split
  use stack_mod, only : stack_t
  implicit none
  private
  public day2205

  type move_t
    integer :: irep, ifrom, ito
  end type
  interface move_t
    module procedure move_new
  end interface

  integer, parameter :: MAX_STACKS = 9

contains

  subroutine day2205(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    type(stack_t) :: S1(MAX_STACKS), S2(MAX_STACKS)
    integer :: nempt, i
    character(len=:), allocatable :: ans1, ans2

    lines = read_strings(file)
    ! finds an empty line
    do i=1,size(lines)
      if (len_trim(lines(i)%str)==0) exit
    end do
    nempt = i

    ! fill-up stacks, follow moving instructions, get answer
    call fill_stacks(lines(1:nempt-2), S1)
    call fill_stacks(lines(1:nempt-2), S2)
    do i=nempt+1,size(lines)
      call make_move(move_t(lines(i)%str), S1, 1)
      call make_move(move_t(lines(i)%str), S2, 2)
    end do
    ans1 = build_answer(S1)
    ans2 = build_answer(S2)
    print '("Answer 5/1 ",a,l2)', ans1, ans1=='ZSQVCCJLL'
    print '("Answer 5/2 ",a,l2)', ans2, ans2=='QZFJRWHGS'
  end subroutine day2205


  type(move_t) function move_new(str) result(new)
    character(len=*), intent(in) :: str
!
! Parse the input instruction line
! eg. "move 6 from 5 to 7"
!
    type(string_t), allocatable :: words(:)

    call split(str, ' ', words)
    read(words(2)%str,*) new%irep
    read(words(4)%str,*) new%ifrom
    read(words(6)%str,*) new%ito
  end function


  subroutine make_move(this, stacks, version)
    class(move_t), intent(in) :: this
    type(stack_t), intent(inout) :: stacks(:)
    integer, intent(in) :: version

    type(stack_t) :: stmp
    integer :: j, val

    if (version/=1 .and. version/=2) error stop 'make_move - invalid version'

    do j=1,this%irep
      call stacks(this%ifrom)%pop(val)
      select case(version)
      case(1)
        call stacks(this%ito)%put(val)
      case(2)
        call stmp%put(val)
      end select
    end do

    if (version==2) then
      do j=1,this%irep
        call stmp%pop(val)
        call stacks(this%ito)%put(val)
      end do
    end if
  end subroutine make_move


  subroutine fill_stacks(lines, stacks)
    type(string_t), intent(in) :: lines(:)
    type(stack_t), intent(out) :: stacks(:)
!
! Parse the initial stacks state
!
    integer :: i, j, pos, val

    do i=size(lines),1,-1
      do j=1,size(stacks)
        pos = (j-1)*4+1
        if (pos > len_trim(lines(i)%str)) cycle
        if (lines(i)%str(pos:pos)/='[') cycle
        val = iachar(lines(i)%str(pos+1:pos+1))
        call stacks(j)%put(val)
      end do
    end do
  end subroutine fill_stacks


  function build_answer(stacks) result(str)
    type(stack_t), intent(in) :: stacks(:)
    character(len=size(stacks)) :: str
!
! What crates are on the top of each stack?
!
    integer :: i
    do i=1,size(stacks)
      str(i:i) = achar(stacks(i)%peek())
    end do
  end function build_answer

end module day2205_mod