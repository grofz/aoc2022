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
    integer :: ans1, ans, n2, ans0, i, i2, pos0, pos1, dpos
    integer(I8) :: ans2, nrep, nleft, k0, k1
    integer(I8) :: z0, z1, z2, ztot
    integer :: h0, h1, h2
    integer, allocatable :: aa(:)

    integer, parameter :: NROCKS_P1 = 2022
    integer(I8), parameter :: NROCKS_P2=1000000000000_I8

    ! Part 1
    call fill_sprite_library()
    !call fill_jet_pattern('inp/17/test.txt')
    call fill_jet_pattern(file)
    call throw_rocks(NROCKS_P1, rocks, ans1)
    print '("Answer 17/1 ", i0, l2)', ans1, ans1==3106 .or. ans1==3068

    ! Part 2
    print '("Jet pattern length =",i0)', size(jet_pattern) 
    ! for real case, pattern is not divisible by number of sprites
    n2 = size(jet_pattern)*NSPRITE
    print '("Searching for pattern in chunks of ",i0)', n2

    call throw_rocks(1000*n2, rocks, ans, n2, aa)
    pos0 = 0
    pos1 = 0
    i=1
    do
      ! I know value "77580" occurs only once within a pattern
      i2 = findloc(aa(i:), 77580, dim=1)
      if (i2==0) exit
      i = i-1+i2
      print *,'pattern marker at:',i, 'vals=', aa(i-3:i+3)
      if (pos0==0) then
        pos0 = i
      else if (pos1==0) then
        pos1 = i
      end if
      i = i + 1
    end do

    ! Find the beginning of a pattern
    do
      if (aa(pos0-1)==aa(pos1-1)) then
        pos0 = pos0 - 1
        pos1 = pos1 - 1
      else
        exit
      end if
    end do
    print *, 'Pattern begins at ', pos0
    dpos = pos1-pos0
    print *, 'Pattern length ', dpos
    print *, all(aa(pos0:pos1-1)==aa(pos1:pos1+dpos-1))

    ! How many chunks are needed / what is the remainder
    nrep = NROCKS_P2/int(n2,I8)
    nleft = mod(NROCKS_P2, int(n2,I8))
    print *, 'nrep / left ', nrep, nleft

    ! We know that the first chunk is out of pattern
    ! h0 = a non repeating chunk
    z0 = (pos0-1)*n2
    call throw_rocks(int(z0), rocks, h0)
    ! h1 = one chunk + (347 chunks)
    z1 = n2*(pos0-1+dpos)
    call throw_rocks(int(z1), rocks, h1)
    k0 = (nrep-(pos0-1))/dpos
    k1 = mod((nrep-(pos0-1)), dpos)
    print *, 'k0 / k1 ', k0, k1
    ! h2 = one_chunk + (347 chunks) + k1 chunks + remainder
    z2 = n2*((pos0-1)+dpos+k1)+nleft
    call throw_rocks(int(z2), rocks, h2)
    print *, 'h0 ',z0, h0
    print *, 'h1 ',z1, h1
    print *, 'h2 ',z2, h2

    ztot = z0 + k0*(z1-z0) + (z2-z1)
    print *, ztot, NROCKS_P2, NROCKS_P2==ztot

    ans2 = h0 + k0*(h1-h0) + (h2-h1)
    print *, 'ans 2',ans2
    print '("Answer 17/2 ", i0, l2)', ans2, ans2==1537175792495_I8 .or. ans2==1514285714288_I8

  end subroutine day2217


  subroutine throw_rocks(n, rocks, ans, istep, aa)
    integer, intent(in) :: n
    type(rock_t), allocatable, intent(out) :: rocks(:)
    integer, intent(out) :: ans
    integer, intent(in), optional :: istep
    integer, allocatable, intent(out), optional :: aa(:)

    integer :: i, ans0
    logical :: isrest

    if (present(aa) .and. present(istep)) then
      allocate(aa(n/istep))
      if (mod(n,istep)/=0) error stop 'not a multiple'
      aa = 0
    end if

    ans0 = 0
    allocate(rocks(n))
    global_maxy = 0
    do i=1,size(rocks)
      rocks(i) = rock_t(i)
      if (i>1) rocks(i)%ijet = rocks(i-1)%ijet
      do
        call rock_step(rocks(i), rocks(1:i-1), isrest)
        if (isrest) exit
      end do

      if (present(istep) .and. present(aa)) then
        if (mod(i,istep)==0) then
          ans = global_maxy
          !print *, ans-ans0
          aa(i/istep) = ans-ans0
          ans0 = ans
        end if
      end if
    end do
    !call visualize(rocks)
    ans = global_maxy
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
!print *, 'left'
    case('>')
      if (.not. iscollision(th,rocks,[+1,0])) th%p=th%p+[+1,0]
!print *, 'right'
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
!print *, 'collision with the bottom'
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
!print *, 'collision with the left'
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
!print *, 'collision with the right'
          return
        end do
      end do OUT3
    end if

    ! Check collision with other rocks

    ! No colision possible, if it is above the maximum
   !if (obj%p(2)>global_maxy) return
    ! To optimize, only few last rocks are checked
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