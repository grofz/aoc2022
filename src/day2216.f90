module day2216_mod
  use parse_mod, only : string_t, read_strings
  implicit none

  integer, parameter :: MAXHASHVAL = 1000
  type state_t
    integer :: t = 0
    integer :: gain = 0
    logical :: opened(MAXHASHVAL) = .false.
    integer :: pos
  end type state_t
contains

  subroutine day2216(file)
    character(len=*), intent(in) :: file


    integer :: aa(hash('ZZ'),hash('ZZ'))
    character(len=2) :: label(hash('ZZ'))
    integer :: rate(hash('ZZ'))
    type(state_t) :: init, sout

    call read_input(file, aa, rate, label)
    !call read_input('inp/16/test.txt', aa, rate, label)
    init%pos = findloc(label,'AA',1)
  print *, 'init position', init%pos
    call move(init, -1, rate, aa, label, sout)
  print *, 'Reporting sbest: ', sout%gain, label(sout%pos), sout%t
  end subroutine


  recursive subroutine move(s, ifrom, rate, aa, label, sout)
    type(state_t), intent(in) :: s
    integer, intent(in) :: rate(:), ifrom
    integer, intent(in) :: aa(:,:)
    character(len=2), intent(in) :: label(:)
    type(state_t), intent(out) :: sout

    type(state_t) :: s1, s0, s2, s3
    integer :: ngb, flow, i, flow0

    ! Update current time and gain
    s0 = s
    s0%t = s%t + 1
    flow = 0
    do i=1, size(rate)
      if (.not. s%opened(i)) cycle
      flow = flow + rate(i)
    end do
    s0%gain = s%gain + flow
    sout = s0
!print *, s0%t, s%gain, flow, 'at pos ',label(s0%pos)

    ! If time is up, nothing more can be done
    if (s0%t >= 30) then
!  print *, '... time up, return'
      return
    end if


    ! We can open a valve (if possible)
    if (.not. s%opened(s%pos) .and. rate(s%pos)/=0) then
      s1 = s0
      s1%opened(s%pos) = .true.
      call move(s1, s%pos, rate, aa, label, s2)
    else
      s2%gain = -1
    end if

    ! We can also move
    ngb = 0
    do 
      ngb = ngb + 1
      if (ngb > size(aa,2)) exit
      if (aa(s%pos, ngb)/=1) then
        cycle
      end if
      !ngb = findloc(aa(s%pos,ngb+1:),1,dim=1)
      !if (ngb == 0) exit

      if (ngb==ifrom) cycle

      s1 = s0
      s1%pos = ngb
      call move(s1, s%pos, rate, aa, label, s3)
      if (s3%gain > sout%gain) sout = s3
    end do

    if (s2%gain > sout%gain) sout = s2

  !print *, 'Reporting sbest: ', sout%gain, label(sout%pos), sout%t
  end subroutine move


  subroutine read_input(file, aa, rate, label)
    character(len=*), intent(in) :: file
    integer, intent(out) :: aa(:,:)
    integer, intent(out) :: rate(:)
    character(len=2), intent(out) :: label(:)

    type(string_t), allocatable :: lines(:)
    integer :: i, j0, j1
    integer :: ind_valve, ind_valve0
    character(len=2) :: valve, valve0

    lines = read_strings(file)
    aa = 0
    rate = 0
    label = '  '
    do i=1, size(lines)
      j0 = 1
      j1 = index(lines(i)%str(j0:), 'Valve ')
      j1 = j0 + j1 - 1 + 6
      read(lines(i)%str(j1:j1+1), *) valve
      ind_valve = hash(valve)
      label(ind_valve) = valve

      j0 = index(lines(i)%str, 'rate=') 
      j0 = j0 + 5
      j1 = scan(lines(i)%str, ';') 
      read(lines(i)%str(j0:j1-1),*) rate(ind_valve)
  print *, 'Valve: ', valve, ind_valve, rate(ind_valve)

      j0 = index(lines(i)%str, 'valves')
      j0 = j0 + 7
      NGB: do
        j1 = scan(lines(i)%str(j0:),',')
        if (j1==0) then
          j1 = len_trim(lines(i)%str)
        else
          j1 = j0 + j1 - 1 - 1
        end if
        read(lines(i)%str(j0:j1),*) valve0
        ind_valve0 = hash(valve0)
        aa(ind_valve,ind_valve0) = 1
        aa(ind_valve0,ind_valve) = 1
  print *, '-->',valve0, ind_valve0
        j0 = j1+3
        if (j0>len_trim(lines(i)%str)) exit NGB
      end do NGB
    end do
  end subroutine


  pure integer function hash(str)
    character(len=2), intent(in) :: str
    hash = (iachar(str(1:1))-iachar('A')+1)*30
    hash = iachar(str(2:2))-iachar('A')+1+hash
  end function hash
end module day2216_mod