module day2210_mod
  use parse_mod, only : string_t, read_strings, split
  implicit none
  private
  public day2210

  character(len=1), parameter :: LIT='#', NOLIT='.'
  integer, parameter :: DSP_XSIZE=40, DSP_YSIZE=6
  type gpu_t
    integer :: x = 1 ! sprite position
    integer :: t = 1 ! actual clock time
    logical :: isinmiddle = .false. 
    integer :: tmp
    character(len=1) :: dsp(0:DSP_XSIZE-1,0:DSP_YSIZE-1)=NOLIT
  contains 
    procedure :: onecycle => gpu_onecycle
    procedure :: display => gpu_display
  end type

contains
  subroutine day2210(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    integer :: ip, ans1
    type(gpu_t) :: gpu

    lines = read_strings(file)
    ip = 1
    ans1 = 0
    do
      call gpu_onecycle(gpu, lines(ip)%str, ip)
      if ((gpu%t==20 .or. mod(gpu%t-20,40)==0) .and. gpu%t<=220) then
        ans1 = ans1 + gpu%x * gpu%t
      end if
      if (ip > size(lines)) exit
    end do
    print '("Answer 10/1 ", i0, l2)', ans1, ans1==15220
    call gpu%display()
  end subroutine day2210


  subroutine gpu_onecycle(this, str, ip)
    class(gpu_t), intent(inout) :: this
    character(len=*), intent(in) :: str
    integer, intent(inout) :: ip

    type(string_t), allocatable :: words(:)
    integer :: x, y

    ! First draw pixel if sprite is in a correct position
    x = cycle2x(this)
    y = cycle2y(this)
    if (x >= this%x-1 .and. x <= this%x+1) then
      this%dsp(x,y) = LIT
    else
      this%dsp(x,y) = NOLIT
    end if

    ! Increase the clock
    this%t = this%t + 1

    if (this%isinmiddle) then
      ! Finish the two-cycle instruction...
      ip = ip + 1
      this%x = this%x + this%tmp
      this%isinmiddle = .false.
    else
      ! ...or read a new instruction
      call split(str, ' ', words)
      select case(words(1)%str)
      case('noop')
        ! one-cycle instruction does nothing
        ip = ip + 1
      case('addx')
        ! begin a two-cycle instruction
        this%isinmiddle = .true.
        read(words(2)%str,*) this%tmp
      case default
        error stop 'gpu_onestep - command not recognized'
      end select
    end if
  end subroutine gpu_onecycle


  pure integer function cycle2y(this) result(y)
    class(gpu_t), intent(in) :: this
    y = mod( (this%t-1)/DSP_XSIZE, DSP_YSIZE)   
  end function


  pure integer function cycle2x(this) result(x)
    class(gpu_t), intent(in) :: this
    x = mod(this%t-1, DSP_XSIZE)
  end function


  subroutine gpu_display(this)
    class(gpu_t), intent(in) :: this

    integer :: x, y
    print *
    do y = 0, DSP_YSIZE-1
      print '(*(a1))', (this%dsp(x,y), x=0, DSP_XSIZE-1)
    end do
    print *
  end subroutine gpu_display

end module day2210_mod