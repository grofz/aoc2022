module day2223_mod
  use parse_mod, only : read_pattern
  implicit none
  private
  public day2223

  ! "read_pattern" reads lines as x and columns as y
  integer, parameter :: &
  & P_N=1, P_NE=2, P_NW=3, P_S=4, P_SE=5, P_SW=6, &
  & P_W=7, P_E=8,  P_STUCK=-1, P_FREE=-2

  integer, parameter :: MOVDIR(2,8) = reshape(&
  & [-1,0, -1,1, -1,-1, 1,0, 1,1, 1,-1, 0,-1, 0,1], [2,8])

  character(len=1), parameter :: CH_ELF='#', CH_NON='.'

contains

  subroutine day2223(file)
    character(len=*), intent(in) :: file

    character(len=1), allocatable :: bb(:,:), btemp(:,:)
    integer, allocatable :: pp(:,:), qq(:,:)
    integer :: k, nx, ny, ans1, nelves, nstuck, nfree, ans2
    integer, parameter :: NROUND=10

    ans1 = 0
    ans2 = 0
    bb = read_pattern(file)
    !bb = read_pattern('inp/23/test2.txt')
    nx = size(bb,1)
    ny = size(bb,2)
    allocate(btemp(1-nx:nx+nx, 1-ny:ny+ny))
    btemp = CH_NON
    btemp(1:nx,1:ny) = bb
    call move_alloc(btemp, bb)
    print *, size(bb,1), size(bb,2)
    allocate(pp(size(bb,1), size(bb,2)))
    allocate(qq(size(bb,1), size(bb,2)))

    do k=1,3000*NROUND
      call first_half(bb, pp, qq, k)
      call second_half(bb, pp, qq)

      nelves = count(bb==CH_ELF)
      nstuck = count(qq==P_STUCK .and. bb==CH_ELF)
      nfree = count(qq==P_FREE .and. bb==CH_ELF)
!print *, k, nelves, nstuck, nfree
      if (nelves==nfree) then
        ans2 = k
        exit
      end if
      if (k==NROUND) ans1 = empty_squares(bb)
    end do
    if (nelves/=nfree) error stop 'day2223 - simulation did not finish'
!call print_board(bb)
    print '("Answer 23/1 ",i0,l2)', ans1, ans1==3780
    print '("Answer 23/2 ",i0,l1)', ans2, ans2==930
  end subroutine day2223


  subroutine second_half(bb, pp, qq)
    character(len=1), intent(inout) :: bb(:,:)
    integer, intent(in) :: pp(size(bb,1),size(bb,2))
    integer, intent(in) :: qq(size(bb,1),size(bb,2))
!
! Update elves positions in "bb" using:
! "pp" - number of elves that want to move to that position
! "qq" - intended move of the elf
!
    integer :: i, j, nx, ny, p(2)

    nx = size(bb,1)
    ny = size(bb,2)

    do i=1,nx
    do j=1,ny
      if (qq(i,j)==0) cycle
      if (qq(i,j)==P_FREE .or. qq(i,j)==P_STUCK) cycle
      if (bb(i,j)/=CH_ELF) error stop 'elf not where he is expected'
      p = [i,j] + MOVDIR(:,qq(i,j))
      if (pp(p(1),p(2))<1) error stop 'moving to unclaimed position'
      if (pp(p(1),p(2))==1) then
        ! elf can move
        bb(i,j) = CH_NON
        bb(p(1),p(2)) = CH_ELF
      end if
    end do
    end do
  end subroutine second_half


  subroutine first_half(bb, pp, qq, iround)
    character(len=1), intent(in) :: bb(:,:)
    integer, intent(out) :: pp(size(bb,1),size(bb,2))
    integer, intent(out) :: qq(size(bb,1),size(bb,2))
    integer, intent(in) :: iround
  !
  ! Using "bb" as current elves positions, return:
  ! "pp" number of elves aiming for the position
  ! "qq" where the elf wants to move
  !
    integer :: i, j, k, nx, ny, p(2), q(2,4), iq(4)
    logical :: ngbelf(8)

    nx = size(bb,1)
    ny = size(bb,2)
    pp = 0
    qq = 0
    do i=1,nx
    do j=1,ny
      if (bb(i,j)/=CH_ELF) cycle

      ! Do not move if no elves around
      do k=1,8
        ngbelf(k) = is_elf([i,j],k)
      end do
      if (.not. any(ngbelf)) then
        qq(i,j) = -2
        cycle
      end if

      q = -1
      select case(mod(iround, 4)) 
      case(1)
        if (.not.(is_elf([i,j],P_N) .or. is_elf([i,j],P_NE) .or. is_elf([i,j],P_NW))) then
          q(:,1) = [i,j] + MOVDIR(:,P_N)
          iq(1) = P_N
        else if (.not. (is_elf([i,j],P_S) .or. is_elf([i,j],P_SE) .or. is_elf([i,j],P_SW))) then
          q(:,2) = [i,j] + MOVDIR(:,P_S)
          iq(2) = P_S
        else if (.not. (is_elf([i,j],P_W) .or. is_elf([i,j],P_SW) .or. is_elf([i,j],P_NW))) then
          q(:,3) = [i,j] + MOVDIR(:,P_W)
          iq(3) = P_W
        else if (.not. (is_elf([i,j],P_E) .or. is_elf([i,j],P_SE) .or. is_elf([i,j],P_NE))) then
          q(:,4) = [i,j] + MOVDIR(:,P_E)
          iq(4) = P_E
        end if
      case(2)
        if (.not. (is_elf([i,j],P_S) .or. is_elf([i,j],P_SE) .or. is_elf([i,j],P_SW))) then
          q(:,2) = [i,j] + MOVDIR(:,P_S)
          iq(2) = P_S
        else if (.not. (is_elf([i,j],P_W) .or. is_elf([i,j],P_SW) .or. is_elf([i,j],P_NW))) then
          q(:,3) = [i,j] + MOVDIR(:,P_W)
          iq(3) = P_W
        else if (.not. (is_elf([i,j],P_E) .or. is_elf([i,j],P_SE) .or. is_elf([i,j],P_NE))) then
          q(:,4) = [i,j] + MOVDIR(:,P_E)
          iq(4) = P_E
        else if (.not.(is_elf([i,j],P_N) .or. is_elf([i,j],P_NE) .or. is_elf([i,j],P_NW))) then
          q(:,1) = [i,j] + MOVDIR(:,P_N)
          iq(1) = P_N
        end if
      case(3)
        if (.not. (is_elf([i,j],P_W) .or. is_elf([i,j],P_SW) .or. is_elf([i,j],P_NW))) then
          q(:,3) = [i,j] + MOVDIR(:,P_W)
          iq(3) = P_W
        else if (.not. (is_elf([i,j],P_E) .or. is_elf([i,j],P_SE) .or. is_elf([i,j],P_NE))) then
          q(:,4) = [i,j] + MOVDIR(:,P_E)
          iq(4) = P_E
        else if (.not.(is_elf([i,j],P_N) .or. is_elf([i,j],P_NE) .or. is_elf([i,j],P_NW))) then
          q(:,1) = [i,j] + MOVDIR(:,P_N)
          iq(1) = P_N
        else if (.not. (is_elf([i,j],P_S) .or. is_elf([i,j],P_SE) .or. is_elf([i,j],P_SW))) then
          q(:,2) = [i,j] + MOVDIR(:,P_S)
          iq(2) = P_S
        end if
      case(0)
        if (.not. (is_elf([i,j],P_E) .or. is_elf([i,j],P_SE) .or. is_elf([i,j],P_NE))) then
          q(:,4) = [i,j] + MOVDIR(:,P_E)
          iq(4) = P_E
        else if (.not.(is_elf([i,j],P_N) .or. is_elf([i,j],P_NE) .or. is_elf([i,j],P_NW))) then
          q(:,1) = [i,j] + MOVDIR(:,P_N)
          iq(1) = P_N
        else if (.not. (is_elf([i,j],P_S) .or. is_elf([i,j],P_SE) .or. is_elf([i,j],P_SW))) then
          q(:,2) = [i,j] + MOVDIR(:,P_S)
          iq(2) = P_S
        else if (.not. (is_elf([i,j],P_W) .or. is_elf([i,j],P_SW) .or. is_elf([i,j],P_NW))) then
          q(:,3) = [i,j] + MOVDIR(:,P_W)
          iq(3) = P_W
        end if
      end select

      ! accept first valid move
      do k=1,4
        ! skip null values, but throw error if the elf is
        ! at the border
        if (all(q(:,k)==-1)) cycle
        if (any(q(:,k)<1) .or. q(1,k)>nx .or. q(2,k)>ny) &
        & error stop 'elf at the border - allocate larger map and try again'
        qq(i,j) = iq(k)
        pp(q(1,k),q(2,k)) = pp(q(1,k),q(2,k)) + 1
        exit
      end do
      ! elf wants to move, but he can not
      if (qq(i,j)==0) qq(i,j)=P_STUCK

    end do
    end do

  contains

    logical function is_elf(p0,dir0)
      integer, intent(in) :: p0(2), dir0
      integer :: p1(2)
      p1 = p0 + MOVDIR(:,dir0)
      is_elf = .false.
      if (any(p1<1) .or. p1(1)>size(bb,1) .or. p1(2)>size(bb,2)) return
      is_elf = bb(p1(1),p1(2))==CH_ELF
    end function is_elf

  end subroutine first_half


  integer function empty_squares(bb) result(ans)
    character(len=1), intent(in) :: bb(:,:)

    integer :: i, j, lo(2), hi(2)

    lo = size(bb,1)
    hi = 0
    do i=1,size(bb,1)
    do j=1,size(bb,2)
      if (bb(i,j)/=CH_ELF) cycle
      if (i<lo(1)) lo(1) = i
      if (i>hi(1)) hi(1) = i
      if (j<lo(2)) lo(2) = j
      if (j>hi(2)) hi(2) = j
    end do
    end do
    ans = count(bb(lo(1):hi(1),lo(2):hi(2))==CH_NON)

  end function empty_squares


! ===============================
! For debugging and visualization
! ===============================


  subroutine print_board(bb)
    character(len=1), intent(in) :: bb(:,:)

    integer :: i,j,nx,ny

    nx = size(bb,1)
    ny = size(bb,2)

    do i=1,nx
      print '(*(a))', (bb(i,j), j=1,ny)
    end do
  end subroutine print_board


  subroutine print_board_int(bb)
    integer, intent(in) :: bb(:,:)

    integer :: i,j,nx,ny

    nx = size(bb,1)
    ny = size(bb,2)

    do i=1,nx
      print '(*(i1,1x))', (bb(i,j), j=1,ny)
    end do
  end subroutine print_board_int

end module day2223_mod