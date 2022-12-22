module day2222_mod
  use parse_mod, only : read_strings, string_t
  implicit none

  integer, parameter :: DIR(2,4) = reshape([1,0, 0,1, -1,0, 0,-1], [2,4])
  character(len=1), parameter :: CHDIR(4)=['R','D','L','U']
  character(len=1), parameter :: CHWALL='#', CHOPEN='.'

  type robot_t
    integer :: p(2)
    integer :: f
    character(len=1), pointer :: bb(:,:) => null()
    integer, pointer :: movins(:,:) => null()
  contains
    procedure :: move => robot_move
    procedure :: turn => robot_turn
  end type
  interface robot_t
    module procedure robot_new
  end interface

contains

  subroutine day2222(file)
    character(len=*), intent(in) :: file

    character(len=1), allocatable :: bb(:,:) 
    integer, allocatable :: movins(:,:)
    type(robot_t) :: r
    integer :: i, ans1

    call read_input(file, bb, movins)
    !call read_input('inp/22/test.txt', bb, movins)
    r = robot_t(bb,movins)
  print *, 'position', r%p

    do i=1,size(movins,2)
      call r%turn(movins(2,i))
      call r%move(movins(1,i))
    end do
  print *, 'position+f', r%p, r%f
  ans1 = 1000*r%p(2) + 4*r%p(1) + (r%f-1)
  print *, ans1



  end subroutine day2222


  type(robot_t) function robot_new(bb, movins) result(new)
    character(len=1), intent(in), target :: bb(:,:) 
    integer, intent(in), target :: movins(:,:)

    integer :: i

    new%bb => bb
    new%movins => movins
    new%f = 1 ! Facing right
    new%p(2) = 1
    do i=1, size(bb,1)
      if (bb(i,1)/=' ') exit
    end do
    if (i==size(bb,1)+1) error stop 'not first position'
    new%p(1) = i
  end function robot_new


  subroutine robot_move(this, n)
    class(robot_t), intent(inout) :: this
    integer, intent(in) :: n
!
! Move safely "n" moves in current direction
!
    character(len=1) :: next
    integer :: i, pn(2)

    do i=1,n
      pn = position_ngb(this%p, DIR(:,this%f), this%bb) 
      if (this%bb(pn(1),pn(2))==CHWALL) exit
      this%p = pn
    end do
print *, 'new position =',this%p
  end subroutine


  subroutine robot_turn(this, turn)
    class(robot_t), intent(inout) :: this
    integer, intent(in) :: turn

    if (abs(turn)>1) error stop 'too many turns'
    this%f = this%f + turn
    if (this%f==5) this%f = 1
    if (this%f==0) this%f = 4
print *, 'new orientation =',this%f
  end subroutine robot_turn


  subroutine read_input(file, bb, movins)
    character(len=*), intent(in) :: file
    character(len=1), allocatable, intent(out) :: bb(:,:) 
    integer, allocatable, intent(out) :: movins(:,:)

    type(string_t), allocatable :: lines(:)
    integer :: ny, nx, i, j, p0, p1, nturn, iturn
    integer :: lastturn

    lines = read_strings(file)
    ny = size(lines)-2 ! last empty line and instructions
    nx = 0
    do i=1, ny
      if (len_trim(lines(i)%str)>nx) nx=len_trim(lines(i)%str)
    enddo
    allocate(bb(nx,ny))
    do j=1, ny
      do i=1,nx
        if (i>len_trim(lines(j)%str)) then
          bb(i,j) = ' '
        else
          bb(i,j) = lines(j)%str(i:i)
        end if
      end do
    end do
    if (len_trim(lines(ny+1)%str)/=0) error stop 'not empty line'

    ! Decode moving instructions
    associate(s=>lines(ny+2)%str)
      p0 = 1
      nturn = 0
      do
        p1 = scan(s(p0:), 'RDLU')
        if (p1==0) exit
        nturn = nturn + 1
        p1 = p0 + p1 - 1
        p0 = p1+1
      end do
      allocate(movins(2,nturn+1))

      p0 = 1
      lastturn = 0
      do iturn=1, nturn
        p1 = scan(s(p0:), 'RDLU')
        p1 = p0 + p1 - 1
        read(s(p0:p1-1),*) movins(1,iturn)
        movins(2,iturn) = lastturn
        !lastturn = findloc(CHDIR, s(p1:p1), dim=1)
        select case(s(p1:p1))
        case('L')
          lastturn = -1
        case('R')
          lastturn = +1
        case default
          error stop 'instruction invalid'
        end select
        p0 = p1+1
      end do
      read(s(p0:),*) movins(1,nturn+1)
      movins(2,nturn+1) = lastturn
    end associate

    print *, nx, ny, size(movins,2)
  end subroutine read_input


  pure function position_ngb(p, d, bb) result(pn) 
    integer, intent(in) :: d(2), p(2)
    character(len=1), intent(in) :: bb(:,:)

    integer :: pn(2), pmax(2)

    pmax(1) = size(bb,1)
    pmax(2) = size(bb,2)
    pn = p

    do
      pn = pn+d
      where(pn<1)
        pn = pmax
      else where (pn>pmax)
        pn = 1
      end where

      ! if in the void, continue in same direction until no void
      if (bb(pn(1),pn(2))/=' ') exit
    end do
  end function position_ngb


  subroutine cube_position(p0, p1, nt, cp, f)
    integer, intent(out) :: cp(3)
    integer, intent(inout) :: f
    integer, intent(in) :: p0(2), p1(2), nt

    integer :: leto(2,6), ribo(2,6), i, cp0(3)

    leto(:,1) = [2*nt+1, 1]
    leto(:,2) = [1, nt+1]
    leto(:,3) = [nt+1, nt+1]
    leto(:,4) = [2*nt+1, nt+1]
    leto(:,5) = [2*nt+1, 2*nt+1]
    leto(:,6) = [3*nt+1, 2*nt+1]
    ribo(1,:) = leto(1,:) + nt - 1
    ribo(2,:) = leto(2,:) + nt - 1

    do i=1,6
      cp0(3) = i
      cp0(1) = p0(1) - leto(1,i) + 1
      cp0(2) = p0(2) - leto(2,i) + 1
      if (cp0(1)<1 .or. cp0(1)>nt .or. cp0(2)<1 .or. cp0(2)>nt) cycle
      exit
    end do
    if (i==6+1) error stop 'position cp0 not valid'

    do i=1,6
      cp(3) = i
      cp(1) = p1(1) - leto(1,i) + 1
      cp(2) = p1(2) - leto(2,i) + 1
      if (cp(1)<1 .or. cp(1)>nt .or. cp(2)<1 .or. cp(2)>nt) cycle
      exit
    end do

    ! p1 is outside "map"
    if (i==6+1) then
      select case(cp0(3))
      case(1)
        if (f==4) then ! up
          cp(1) = nt - cp0(1) + 1
          cp(2) = 1
          cp(3) = 2
          f = 2 ! now down
        else if (f==3) then ! left
          cp(1) = cp0(2)
          cp(2) = 1
          cp(3) = 3
          f = 2 ! now down
        else if (f==1) then ! right
          cp(1) = nt
          cp(2) = nt - cp0(2) + 1
          cp(3) = 6
          f = 3 ! now left
        else
          error stop 'face 1 out of range'
        end if
      case(2)
        if (f==4) then ! up
          cp(1) = nt - cp0(1) + 1
          cp(2) = 1
          cp(3) = 1
          f = 2
        else if (f==2) then ! down
          cp(1) = nt - cp0(1) + 1
          cp(2) = nt
          cp(3) = 5
          f = 4
        else if (f==3) then ! left
          cp(1) = nt - cp0(2) + 1
          cp(2) = nt
          cp(3) = 6
          f = 4
        else
          error stop 'face 2 out of range'
        end if
      case(3)
        if (f==4) then
          ! "3" => "1"
          cp(1) = 1
          cp(2) = cp0(1)
          cp(3) = 1
          f = 1
        else if (f==2) then
          ! "3" => "5"
          cp(1) = 1
          cp(2) = nt - cp0(1) + 1
          cp(3) = 5
          f = 1
        else
          error stop 'face 3 out of range'
        end if
      case(4)
        ! "4" => "6"
        if (f/=1) error stop 'face 4 out of range'
        cp(1) = nt - cp0(2) + 1
        cp(2) = 1
        cp(3) = 6
        f = 2
      case(5)
        if (f==3) then
        ! "5" => "3"
          cp(1) = nt - cp0(2) + 1
          cp(2) = nt
          cp(3) = 3
          f = 4
        else if (f==2) then
        ! "5" => "2"
          cp(1) = nt - cp0(1) + 1
          cp(2) = nt
          cp(3) = 2
          f = 4
        else
          error stop 'face 5 out of range'
        end if
      case(6)
        if (f==4) then ! up
          cp(1) = nt
          cp(2) = nt - cp0(1) + 1
          cp(3) = 4
          f = 3 ! now left
        else if (f==1) then ! right
          cp(1) = nt
          cp(2) = nt - cp0(2) + 1
          cp(3) = 1
          f = 3 ! now left
        else if (f==2) then ! down
          cp(1) = 1
          cp(2) = nt - cp0(1) + 1
          cp(3) = 2
          f = 1 ! now right
        else
          error stop 'face 6 out of range'
        end if
      end select
    end if


  end subroutine cube_position


end module day2222_mod