module day2217_mod
  use iso_fortran_env, only : I8 => int64
  use parse_mod, only : read_strings, string_t
  implicit none
  private
  public day2217

  integer, parameter :: SPRITE_X=4, SPRITE_Y=4, NSPRITE=5
  integer, parameter :: WIDTH=7, START_YR=4, START_X=3
  character(len=1), parameter :: CH_ROCK='#'
  ! STARTING POSITION
  ! left edge two units away from left wall
  ! bottom edge three units above highest rock
  ! floor is at Y=0

  character(len=1) :: sprite_lib(SPRITE_X,SPRITE_Y,NSPRITE)
  integer :: sprite_maxy(NSPRITE)
  character(len=1), allocatable :: jet_pattern(:)
  integer :: global_maxy

  type rock_t
    integer :: p(2)   ! left-bottom corner
    integer :: is     ! sprite library index
    integer :: ijet=1 !
    integer :: id     ! id-value
  end type
  interface rock_t
    module procedure rock_new
  end interface


contains
  subroutine day2217(file)
    character(len=*), intent(in) :: file

    type(rock_t), allocatable :: rocks(:)
    integer :: h0, h1, rper(2), hper(2), ans1, ans
    integer(I8) :: ans2, z0, z1, ztot, ncycle, nremain
    integer, parameter :: NROCKS_P1 = 2022
    integer(I8), parameter :: NROCKS_P2=1000000000000_I8

    ! Part 1
    call fill_sprite_library()
!   call fill_jet_pattern('inp/17/test.txt')
    call fill_jet_pattern(file)
    call throw_rocks(NROCKS_P1, rocks, ans1)
    print '("Answer 17/1 ", i0, l2)', ans1, ans1==3106 .or. ans1==3068

    ! Part 2
    ! minimum rocks required to detect period for the test case
    call throw_rocks(max(size(jet_pattern),200), rocks, ans, rper, hper)
    print '("Initial period ",i0,"  with tower height  ",i0)', &
    &   rper(1), hper(1)
    print '("Next periods   ",i0,"  increase height by ",i0)', &
    &   rper(2), hper(2)

    associate(a=>NROCKS_P2-rper(1), b=>int(rper(2),I8))
      if (b==0) error stop 'period not detected properly'
      ncycle = a/b
      nremain = mod(a,b) 
    end associate
    print '("Repeating the period ",i0," times with remainder ",i0)', &
    &   ncycle, nremain

    ! Calculate the height of the remainder rocks
    z0 = rper(1)+rper(2)
    call throw_rocks(int(z0), rocks, h0)
    z1 = rper(1)+rper(2)+nremain
    call throw_rocks(int(z1), rocks, h1)
    
    ztot = rper(1) + rper(2)*ncycle + nremain
    ans2 = hper(1) + hper(2)*ncycle + (h1-h0)
    if (ztot /= NROCKS_P2) error stop 'day2217 - counting error'
    print '("Answer 17/2 ", i0, l2)', ans2, ans2==1537175792495_I8 .or. ans2==1514285714288_I8

  end subroutine day2217


  subroutine throw_rocks(n, rocks, height, rper, hper)
    integer, intent(in) :: n
    type(rock_t), allocatable, intent(out) :: rocks(:)
    integer, intent(out) :: height
    integer, intent(out), optional :: rper(2), hper(2)
!
! Throw "n" rocks (from the beginning of time). 
! On the output:
! - an array with rocks position "rocks"
! - height of the rock tower "height"
! - number of rocks in the initial period "rper(1)"
! - height of tower at the end of the initial period "hper(1)"
! - number of rocks in additional periods "rper(2)"
! - tower height increase during additional periods "hper(2)"
!
    integer :: i, jet_last, h_last, r_last, i_last
    integer :: rper0(2), hper0(2)
    logical :: isrest

    rper0 = 0
    hper0 = 0

    allocate(rocks(n))
    global_maxy = 0
    jet_last = size(jet_pattern)
    h_last = global_maxy
    r_last = 1
    i_last = -2 ! skip a first candidate for a period
    do i=1,size(rocks)
      rocks(i) = rock_t(i)
      if (i>1) rocks(i)%ijet = rocks(i-1)%ijet

      ! This block is for period detection
      PERIOD: if (rocks(i)%is==1 .and. rocks(i)%ijet<jet_last .and. &
      &   (rocks(i)%ijet==i_last .or. i_last<0)) then
        ! Here, a period may start
        if (i==1) then
          continue ! ignore the first rock
        else if (i_last < 0) then
          ! ignore first few candidates (until i_last is -1)
          if (i_last==-1) then
            i_last = rocks(i)%ijet
          else
            i_last = i_last+1
          end if
        else if (rper0(1)==0) then
          ! this seems as the end of initial period
          rper0(1) = i - r_last
          hper0(1) = global_maxy - h_last
        else if (rper0(2)==0) then
          ! this seems as the end of a regular period
          rper0(2) = i - r_last
          hper0(2) = global_maxy - h_last
        else ! check the regular period has not changed
          if (rper0(2) /= i-r_last .or. hper0(2) /= global_maxy-h_last) then
            error stop 'throw_rock - Period has changed'
          else
            if (present(rper)) &
            & print '(3x,a,i0,a)', 'Period detection confirmed (rocks =',i,')'
          end if
        end if
!print *, rocks(i)%ijet, global_maxy-h_last, i-r_last, i
!call visualize(rocks(1:i),8)

        ! start updating after the initial period was established
        if (rper0(1)/=0) then
          h_last = global_maxy
          r_last = i
        end if
        jet_last = rocks(i)%ijet 
      else if (rocks(i)%is==1) then
        jet_last = rocks(i)%ijet 
      end if PERIOD

      ! Now settle down the rock
      do
        call rock_step(rocks(i), rocks(1:i-1), isrest)
        if (isrest) exit
      end do
    end do

    !call visualize(rocks)
    height = global_maxy
    if (present(rper) .and. present(hper)) then
      rper = rper0
      hper = hper0
    end if
  end subroutine throw_rocks


  type(rock_t) function rock_new(id) result(new)
    integer, intent(in) :: id
    new%p = [START_X, START_YR+global_maxy]
    new%is = mod(id-1,NSPRITE)+1  
    new%id = id     
  end function rock_new


  subroutine rock_step(th, rocks, isrest)
    class(rock_t), intent(inout) :: th
    type(rock_t), intent(in) :: rocks(:)
    logical, intent(out) :: isrest

    character(len=1) :: jetch

    ! 1. move horizontaly via jet
    jetch = jet_pattern(th%ijet)
    th%ijet = th%ijet + 1
    if (th%ijet>size(jet_pattern)) th%ijet=1
    select case(jetch)
    case('<')
      if (.not. iscollision(th,rocks,[-1,0])) th%p=th%p+[-1,0]
    case('>')
      if (.not. iscollision(th,rocks,[+1,0])) th%p=th%p+[+1,0]
    case default
      error stop 'rock_step - invalid jet pattern'
    end select

    ! 2. move one down
    if (iscollision(th,rocks,[0,-1])) then
      isrest = .true.
      global_maxy = max(global_maxy, th%p(2)+sprite_maxy(th%is)-1)
    else
      th%p = th%p + [0,-1]
      isrest = .false.
    end if
  end subroutine


  logical function iscollision(aobj, objs, dir) result(iscol)
    type(rock_t), intent(in) :: aobj
    type(rock_t), intent(in) :: objs(:)
    integer, intent(in) :: dir(2)

    type(rock_t) :: obj
    integer :: rx, ry, x, y, io
    integer :: rxx, ryy, xx, yy

    iscol = .false.
    obj = aobj
    obj%p = obj%p + dir

    ! Check collision with the surroundings
    if (obj%p(2)<=0) then
      ! check bottom row(s)
      OUT1: do ry=1, min(SPRITE_Y, -obj%p(2)+1)
        do rx=1, SPRITE_X
          if (sprite_lib(rx,ry,obj%is)/=CH_ROCK) cycle
          iscol = .true.
          return
        end do
      end do OUT1
    end if

    if (obj%p(1)<=0) then
      ! check left column(s)
      OUT2: do rx=1, min(SPRITE_X, -obj%p(1)+1)
        do ry=1, SPRITE_Y
          if (sprite_lib(rx,ry,obj%is)/=CH_ROCK) cycle
          iscol = .true.
          return
        end do
      end do OUT2
    end if

    if (obj%p(1)+SPRITE_X-1 >= WIDTH+1) then
      ! check right column(s)
      OUT3: do rx=SPRITE_X,max(1, WIDTH+2-obj%p(1)),-1
        do ry=1, SPRITE_Y
          if (sprite_lib(rx,ry,obj%is)/=CH_ROCK) cycle
          iscol = .true.
          return
        end do
      end do OUT3
    end if

    ! Check collision with other rocks
    ! To optimize, only few last rocks are checked (15 is safe)

    MAIN: do io=size(objs),max(size(objs)-20,1),-1
    !MAIN: do io=size(objs),1,-1
      ! ignore itself
      if (objs(io)%id==obj%id) cycle

      if (abs(obj%p(1)-objs(io)%p(1)) < SPRITE_X .and. &
          abs(obj%p(2)-objs(io)%p(2)) < SPRITE_Y) then
        ! check collision
        XLOOP: do rx=1,SPRITE_X
          x = obj%p(1) + rx -1
          YLOOP: do ry=1,SPRITE_Y
            if (sprite_lib(rx,ry,obj%is)/=CH_ROCK) cycle
            y = obj%p(2) + ry -1

            ! check what is in the other object
            rxx = x - objs(io)%p(1) + 1
            ryy = y - objs(io)%p(2) + 1
            if (rxx>0 .and. rxx<=SPRITE_X .and. ryy>0 .and. ryy<=SPRITE_Y) then
              if (sprite_lib(rxx,ryy,objs(io)%is)/=CH_ROCK) cycle
              iscol = .true.
!if (size(objs)-io>10) print *, 'collision with i-10 ',size(objs)-io
!print *, 'collision with object ',io
              exit MAIN
            end if
          end do YLOOP
        end do XLOOP
      end if
    end do MAIN
  end function iscollision


  subroutine fill_jet_pattern(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    integer :: i

    lines = read_strings(file)
    if (size(lines)/=1) error stop 'only one line of input expected'
    associate (s=>lines(1)%str)
      allocate(jet_pattern(len_trim(s)))
      do i=1,len_trim(s)
        jet_pattern(i)=s(i:i)
      end do
    end associate
  end subroutine fill_jet_pattern


  subroutine fill_sprite_library
    character(len=1) :: sprite(SPRITE_X, SPRITE_Y)
    character(len=1), parameter :: R=CH_ROCK, S=' '

    ! "_"
    sprite = reshape(&
    & [R,R,R,R, S,S,S,S, S,S,S,S, S,S,S,S], [SPRITE_X,SPRITE_Y])
    sprite_lib(:,:,1) = sprite
    sprite_maxy(1)=1

    ! "+"
    sprite = reshape(&
    & [S,R,S,S, R,R,R,S, S,R,S,S, S,S,S,S], [SPRITE_X,SPRITE_Y])
    sprite_lib(:,:,2) = sprite
    sprite_maxy(2)=3

    ! "_|"
    sprite = reshape(&
    & [R,R,R,S, S,S,R,S, S,S,R,S, S,S,S,S], [SPRITE_X,SPRITE_Y])
    sprite_lib(:,:,3) = sprite
    sprite_maxy(3)=3

    ! "|"
    sprite = reshape(&
    & [R,S,S,S, R,S,S,S, R,S,S,S, R,S,S,S], [SPRITE_X,SPRITE_Y])
    sprite_lib(:,:,4) = sprite
    sprite_maxy(4)=4

    ! "#"
    sprite = reshape(&
    & [R,R,S,S, R,R,S,S, S,S,S,S, S,S,S,S], [SPRITE_X,SPRITE_Y])
    sprite_lib(:,:,5) = sprite
    sprite_maxy(5)=2
  end subroutine fill_sprite_library


  subroutine visualize(objs, ntop)
    type(rock_t), intent(in) :: objs(:)
    integer, optional, intent(in) :: ntop

    character(len=1), allocatable :: a(:,:)
    integer :: x, y, io, rx, ry, y1, y0

    allocate(a(0:WIDTH+1, 0:global_maxy+START_YR+SPRITE_Y))

    a = '.'
    a(:,0) = '-'
    a(0,:) = '|'
    a(WIDTH+1,:) = '|'
    do io=1,size(objs)
      do rx=1,SPRITE_X
      do ry=1,SPRITE_Y
        x = objs(io)%p(1)+rx-1
        y = objs(io)%p(2)+ry-1
        if (sprite_lib(rx,ry,objs(io)%is)==CH_ROCK) &
          a(x, y) = '#'
      end do
      end do
    end do

    print *
    y1 = ubound(a,2)
    y0 = lbound(a,2)
    if (present(ntop)) then
      y1 = global_maxy
      y0 = max(y0, global_maxy - ntop)
    end if
    do y=y1,y0,-1
      write(*,'(*(a))') (a(x,y), x=lbound(a,1),ubound(a,1))
    end do

  end subroutine

end module day2217_mod
