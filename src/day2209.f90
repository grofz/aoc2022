module day2209_mod
  use parse_mod, only : string_t, read_strings
  implicit none
  private
  public day2209

  type rope_t(rope_len)
    integer, len :: rope_len
    !integer :: r(2, rope_len)=0
    integer :: r(2, rope_len)
  end type

  integer, parameter :: DIR(2,4)=reshape([-1,0, 0,1, 1,0, 0,-1],[2,4])
  character(len=1) :: CHDIR(4) = ['L', 'U', 'R', 'D']
  integer, parameter :: ROPE_LEN_PART1=2, ROPE_LEN_PART2=10

contains
  subroutine day2209(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    type(rope_t(ROPE_LEN_PART1)) :: rope1
    type(rope_t(ROPE_LEN_PART2)) :: rope2
    integer :: i, ans1, ans2
    integer :: lim(4)
    integer, allocatable :: map1(:,:), map2(:,:)

    lines = read_strings(file)

    call simulate_tail(rope1, lines, limits=lim)
    allocate(map1(lim(1):lim(2),lim(3):lim(4)))
    call simulate_tail(rope1, lines, map=map1)
    ans1 = sum(map1)

    call simulate_tail(rope2, lines, limits=lim)
    allocate(map2(lim(1):lim(2),lim(3):lim(4)))
    print *, lim
    call simulate_tail(rope2, lines, map=map2)
    ans2 = sum(map2)
    
    print '("Answer 9/1 ",i0,l2)', ans1, ans1==5513
    print '("Answer 9/2 ",i0,l2)', ans2, ans2==2427
  end subroutine day2209


  subroutine simulate_tail(this, list, map, limits)
    type(rope_t(rope_len=*)), intent(out) :: this ! OUT!
    type(string_t), intent(in) :: list(:)
    integer, intent(inout), allocatable, optional :: map(:,:)
    integer, intent(inout), optional :: limits(4)

    integer :: i

    ! "this" should be initialized to default values
    ! due to "intent(out)" attribute (but it does not work?)
    this%r = 0

    ! initialize "map" or "limits"
    if (present(map)) then
      map = 0
      map(this%r(1,this%rope_len),this%r(2,this%rope_len)) = 1
    end if
    if (present(limits)) then
      limits(1) = huge(limits)
      limits(2) = -huge(limits)
      limits(3:4) = limits(1:2)
    end if

    ! move rope according to the list of instructions
    do i=1,size(list)
      call move_head(this, list(i)%str, map=map, limits=limits)
    end do
  end subroutine simulate_tail


  subroutine move_head(this, str, map, limits)
    type(rope_t(rope_len=*)), intent(inout) :: this
    character(len=*), intent(in) :: str
    integer, intent(inout), optional, allocatable :: map(:,:)
    integer, intent(inout), optional :: limits(4)

    integer :: idir, nsteps, j, k
    idir = findloc(CHDIR, str(1:1), dim=1)
    if (idir==0) error stop 'move_head - direction unknown'
    read(str(3:),*) nsteps

    do j=1,nsteps
      this%r(:,1) = this%r(:,1) + DIR(:,idir)
      do k=2, this%rope_len
        call adjust_tail(this%r(:,k-1), this%r(:,k))
      end do
      associate(tail=>this%r(:,this%rope_len))
        if (present(map)) then
          map(tail(1),tail(2)) = 1
        else if (present(limits)) then
          call update_limits(tail, limits)
        end if
      end associate
    end do
  contains
    subroutine update_limits(x, lim)
      integer, intent(in) :: x(2)
      integer, intent(inout) :: lim(4)
      if (x(1)<lim(1)) lim(1) = x(1)
      if (x(1)>lim(2)) lim(2) = x(1)
      if (x(2)<lim(3)) lim(3) = x(2)
      if (x(2)>lim(4)) lim(4) = x(2)
    end subroutine
  end subroutine move_head


  subroutine adjust_tail(head, tail)
    integer, intent(in) :: head(2)
    integer, intent(inout) :: tail(2)

    integer :: d(2), d0(2)

    d = head-tail
    ! no adjustment needed if head and tail are close enough
    if (all(abs(d) <= 1)) return

    ! assuming that head did not move by more than one pixel
    ! from the last time tail was adjusted
    if (any(abs(d)>2)) error stop 'adjust_tail - head moved too far'

    ! abs(d0) will be 0 or 1
    where (d/=0)
      d0 = d/abs(d)
    elsewhere
      d0 = 0
    end where
    tail = tail + d0
  end subroutine adjust_tail

end module day2209_mod