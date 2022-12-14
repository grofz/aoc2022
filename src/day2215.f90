module day2215_mod
  use parse_mod, only : read_strings, string_t
  use iso_fortran_env, only : int64
  use tiles_mod , only : tile_t, operator(-)
  implicit none
  private
  public day2215

  type sensor_t
    integer :: p(2)
    integer :: pc(2,4) ! corners in trasnformed co-ordinates
    integer :: beacon(2) ! position of an associated beacon
    integer :: mdis ! Manhattan coverage distance
  end type

contains

  subroutine day2215(file)
    character(len=*), intent(in) :: file

    type(sensor_t), allocatable :: ss(:)
    type(tile_t), allocatable :: scover(:), s1cover(:)
    type(tile_t) :: possible(1000)
    integer :: ns, lims(4), ans1, pbeacon(2)
    integer(int64) :: ans2, area
    integer :: ipos, pc(2,4), i, j
    integer, parameter :: MAXV = 4000000, YLINE=2000000

    real :: t0, t1

call cpu_time(t0)
    call read_input(file, ss, lims, scover)
    print *, 'limits: ',lims

    !ans1 = count_impossible(ss, lims(1:2), 2000000)

    ! Alternative for Part 1 (using tiles_t made for Part 2)
    ipos = 1
    possible(ipos) = tile_t([lims(1),lims(2),YLINE, YLINE])
    allocate(s1cover(size(ss)))
    do i=1,size(ss)
      associate (x1=>ss(i)%p(1)-(ss(i)%mdis-abs(YLINE-ss(i)%p(2))),&
                 x2=>ss(i)%p(1)+(ss(i)%mdis-abs(YLINE-ss(i)%p(2))))
        s1cover(i) = tile_t([x1, x2, YLINE, YLINE])
      end associate
    end do
    call remove_covered(possible, ipos, s1cover)
    area = lims(2)-lims(1)
    do i=1,ipos
      area = area - possible(i)%area()
    end do
    ans1 = area
    print '("Answer 15/1 ",i0,l2)', ans1, ans1==5564017
call cpu_time(t1)
print '("Part 1 took (seconds) ", f7.4)',t1-t0
print *

    ! Part 2
call cpu_time(t0)
    pc(:,1) = pos2new([0,0])
    pc(:,2) = pos2new([0,MAXV])
    pc(:,3) = pos2new([MAXV,0])
    pc(:,4) = pos2new([MAXV,MAXV])
    ipos = 1
    associate (x=>pc(1,:), y=>pc(2,:))
      possible(ipos) = tile_t([minval(x),maxval(x),minval(y), maxval(y)])
    end associate
print *, 'Starting with ', possible(1)%lim(1)%x, possible(1)%lim(2)%x
    call remove_covered(possible, ipos, scover)


    print *, 'Analyzing what has left. Tiles =',ipos
    do i=1, ipos
      associate(x1=>possible(i)%lim(1)%x(1), &
                x2=>possible(i)%lim(1)%x(2), &
                y1=>possible(i)%lim(2)%x(1), &
                y2=>possible(i)%lim(2)%x(2))
        if (possible(i)%area()/=1) cycle
        if (.not. all(new2pos([x1,y1])<MAXV)) then
          !print *, '...not in range'
        else
          print *, '...beacon is in range'
          print *, possible(i)%area(), new2pos([x1,y1]), new2pos([x2,y2])
          pbeacon = new2pos([x1,y1])
        end if
      end associate
    end do
    print '("Beacon position: ",i0,1x,i0)', pbeacon
    ans2 = pbeacon(1)*4000000_int64+pbeacon(2)
    print '("Answer 15/2 ",i0,l2)', ans2, ans2==11558423398893_int64
call cpu_time(t1)
print '("Part 2 took (seconds) ", f7.4)',t1-t0
print *

    ! Another aproach for Part 2
    ! The beacon will be in a gap between two sensor area.
    ! Find a pair of becons with one pixel gap betweem them.
call cpu_time(t0)
    do i=1,size(ss)-1
    do j=i+1,size(ss)
      if (manhattan(ss(i)%p, ss(j)%p)==ss(i)%mdis+ss(j)%mdis+2) then
        print *, i, j, 'beacon can be between this pair'
        call find_gap(ss(i),ss(j),ss)
      end if
    end do
    end do
call cpu_time(t1)
print '("Part 2b took (seconds) ", f7.4)',t1-t0
  end subroutine day2215


  subroutine remove_covered(possible, ipos, scover)
    type(tile_t), intent(inout) :: possible(:)
    integer, intent(inout) :: ipos
    type(tile_t), intent(in) :: scover(:)

    integer :: i, j, k
    type(tile_t), allocatable :: tmp(:)
    integer(int64) :: area

    do i=1,size(scover)
      j = 1
      do 
        tmp = possible(j) - scover(i)
        if (size(tmp)==0) then
          possible(j) = possible(ipos)
          ipos = ipos - 1
        else if (size(tmp)==1) then
          possible(j) = tmp(1)
          j = j + 1
        else
          possible(j) = tmp(1)
          j = j + 1
          possible(ipos+1:ipos+size(tmp)-1) = tmp(2:size(tmp))
          ipos = ipos + size(tmp) - 1
        end if
        if (j>ipos) exit
      end do

      area = 0
      do k=1,ipos
        area = area + possible(k)%area()
      end do
!print *, 'area = ',possible(1)%area()
    end do
  end subroutine


  subroutine read_input(file, ss, lims, scover)
    character(len=*), intent(in) :: file
    type(sensor_t), intent(out), allocatable :: ss(:)
    integer, intent(out) :: lims(4)
    type(tile_t), allocatable :: scover(:)

    type(string_t), allocatable :: lines(:)
    integer :: i, ns

    lines = read_strings(file)
    ns = size(lines)

    lims(1) = huge(lims)
    lims(2) = -huge(lims)
    lims(3:4) = lims(1:2)

    allocate(ss(ns))
    do i=1,ns
      call read_oneline(lines(i)%str, ss(i))

      ! Identify limits
      associate(p1=>ss(i)%p(1), p2=>ss(i)%p(2), mdis=>ss(i)%mdis)
      if (p1-mdis < lims(1)) lims(1) = p1-mdis
      if (p1+mdis > lims(2)) lims(2) = p1+mdis
      if (p2-mdis < lims(3)) lims(3) = p2-mdis
      if (p2+mdis > lims(4)) lims(4) = p2+mdis
      end associate
    end do

    allocate(scover(ns))
    do i=1,ns
      associate (x=>ss(i)%pc(1,:), y=>ss(i)%pc(2,:))
        scover(i) = tile_t([minval(x),maxval(x),minval(y), maxval(y)])
      end associate
    end do

    do i=1,ns
      if (.not. all(new2pos(pos2new(ss(i)%p))==ss(i)%p)) error stop 'read_input - transformation check fails'
!print *, 'scovered ',scover(i)%lim(1)%x, scover(i)%lim(2)%x
    end do
  end subroutine read_input


  subroutine read_oneline(str, news)
    character(len=*), intent(in) :: str
    type(sensor_t), intent(out) :: news

    integer :: i, i1, i2, j

    i = 1
    i1 = index(str(i:), 'x=')
    i2 = index(str(i:), ',')
    i1 = i1 + i-1
    i2 = i2 + i-1
    read(str(i1+2:i2-1),*) news%p(1)
    i = i2+1

    i1 = index(str(i:), 'y=')
    i2 = index(str(i:), ':')
    i1 = i1 + i-1
    i2 = i2 + i-1
    read(str(i1+2:i2-1),*) news%p(2)
    i = i2+1

    i1 = index(str(i:), 'x=')
    i2 = index(str(i:), ',')
    i1 = i1 + i-1
    i2 = i2 + i-1
    read(str(i1+2:i2-1),*) news%beacon(1)
    i = i2+1

    i1 = index(str(i:), 'y=')
    i1 = i1 + i-1
    read(str(i1+2:),*) news%beacon(2)

    news%mdis = manhattan(news%p, news%beacon)

    news%pc(:,1) = news%p + [1,0] *news%mdis
    news%pc(:,2) = news%p + [-1,0]*news%mdis
    news%pc(:,3) = news%p + [0,1] *news%mdis
    news%pc(:,4) = news%p + [0,-1]*news%mdis
    do j=1,4
      news%pc(:,j) = pos2new(news%pc(:,j))
    end do
  end subroutine read_oneline


  pure integer function manhattan(p1, p2) result(md)
    integer, intent(in) :: p1(2), p2(2)
    md = sum(abs(p1-p2))
  end function


  integer function count_impossible(ss, lims, y) result(ans)
    type(sensor_t), intent(in) :: ss(:)
    integer, intent(in) :: lims(2), y

    integer :: j, is, ib
    logical :: iscovered, isknownbeacon

    ans = 0
    do j=lims(1),lims(2)
      iscovered = .false.
      do is=1,size(ss)
        if (manhattan([j,y], ss(is)%p) <= ss(is)%mdis) then
          iscovered = .true.
          exit
        end if
      end do
      if (iscovered) then
        isknownbeacon = .false.
        do ib=1,size(ss)
          if (all(ss(ib)%beacon==[j,y])) then
            isknownbeacon = .true.
            exit
          end if
        end do
        if (.not. isknownbeacon) ans = ans+1
      end if
    end do
  end function

  ! Transform coordinates by rotating 45 degrees
  ! Area covered by a sensor becomes a tile

  function pos2new(p) result(pn)
    integer, intent(in) :: p(2)
    integer :: pn(2)
    pn(1) = p(1) - p(2)
    pn(2) = p(1) + p(2)
    !if (mod(pn(1),2)/=0 .or. mod(pn(2),2)/=0) then
!   !  error stop 'transformation 1 error'
    !end if
  end function

  function new2pos(p) result(pn)
    integer, intent(in) :: p(2)
    integer :: pn(2)
    pn(1) = p(1) + p(2)
    pn(2) = -p(1) + p(2)
    if (mod(pn(1),2)/=0 .or. mod(pn(2),2)/=0) then
        error stop 'can not transform position back'
    end if
    pn = pn/2
  end function


  subroutine find_gap(a, b, ss)
    type(sensor_t), intent(in) :: a, b
    type(sensor_t), intent(in) :: ss(:)

    integer :: x(2), y, k
    do y=a%p(2)-a%mdis-1, a%p(2)+a%mdis+1
      x(1) = a%p(1)-(a%mdis+1-abs(y-a%p(2)))
      x(2) = a%p(1)+(a%mdis+1-abs(y-a%p(2)))
      do k=1,2
        if (manhattan([x(k),y],b%p)==b%mdis+1) then
          if (.not. iscovered(x(k), y, ss)) print *, 'Beacon found ',x(k), y
        end if
      end do
    end do
  end subroutine

  pure function iscovered(x0, y0, ss)
    logical :: iscovered
    integer, intent(in) :: x0, y0
    type(sensor_t), intent(in) :: ss(:)

    integer :: is
    iscovered = .false.
    do is=1,size(ss)
      if (manhattan([x0,y0], ss(is)%p) <= ss(is)%mdis) then
        iscovered = .true.
        exit
      end if
    end do
  end function
end module day2215_mod
