module day2215_mod
  use parse_mod, only : read_strings, string_t
  use iso_fortran_env, only : int64
  implicit none

  type sensor_t
    integer :: p(2)
    integer :: pc(2,4) ! corners in trasnformed co-ordinates
    integer :: mdis ! Manhattan coverage distance
  end type

  type beacon_t
    integer :: p(2)
  end type

contains

  subroutine day2215(file)
    character(len=*), intent(in) :: file
    integer, parameter :: MAXSS=30, MAXBB=50
    type(sensor_t) :: ss(MAXSS)
    type(beacon_t) :: bb(MAXBB)
    integer :: ns, nb, lims(4), ans1, pbeacon(2)
    integer(int64) :: ans2

    call read_input(file, ss, ns, bb, nb, lims)
    !call read_input('inp/15/test.txt', ss, ns, bb, nb, lims)
    print *, 'limits: ',lims

    !ans1 = count_impossible(ss(1:ns), bb(1:nb), lims(1:2), 10)
    ans1 = count_impossible(ss(1:ns), bb(1:nb), lims(1:2), 2000000)
    print *, ans1, ans1==5564017

    !pbeacon = find_uknown(ss(1:ns), [0,20,0,20])
    pbeacon = find_uknown(ss(1:ns), [0,4000000,0,4000000])
    ans2 = pbeacon(1)*4000000+pbeacon(2)
    print *, pbeacon, ans2


  end subroutine day2215


  subroutine read_input(file, ss, ns, bb, nb, lims)
    character(len=*), intent(in) :: file
    type(sensor_t), intent(out) :: ss(:)
    type(beacon_t), intent(out) :: bb(:)
    integer, intent(out) :: ns, nb, lims(4)

    type(string_t), allocatable :: lines(:)
    integer :: i

    lines = read_strings(file)

    lims(1) = huge(lims)
    lims(2) = -huge(lims)
    lims(3:4) = lims(1:2)

    ns = 0
    nb = 0
    do i=1,size(lines)
      ns = ns + 1
      nb = nb + 1
      call read_oneline(lines(i)%str, ss(ns), bb(nb))

      if (ss(ns)%p(1)-ss(ns)%mdis < lims(1)) lims(1) = ss(ns)%p(1)-ss(ns)%mdis
      if (ss(ns)%p(1)+ss(ns)%mdis > lims(2)) lims(2) = ss(ns)%p(1)+ss(ns)%mdis
      if (ss(ns)%p(2)-ss(ns)%mdis < lims(3)) lims(3) = ss(ns)%p(2)-ss(ns)%mdis
      if (ss(ns)%p(2)+ss(ns)%mdis > lims(4)) lims(4) = ss(ns)%p(2)+ss(ns)%mdis
print *, pos2new(ss(ns)%p), new2pos(pos2new(ss(ns)%p))
print *, all(new2pos(pos2new(ss(ns)%p))==ss(ns)%p)
    end do
  end subroutine read_input


  subroutine read_oneline(str, news, newb)
    character(len=*), intent(in) :: str
    type(sensor_t), intent(out) :: news
    type(beacon_t), intent(out) :: newb

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
    read(str(i1+2:i2-1),*) newb%p(1)
    i = i2+1

    i1 = index(str(i:), 'y=')
    i1 = i1 + i-1
    read(str(i1+2:),*) newb%p(2)

    news%mdis = manhattan(news%p, newb%p)
!print *, news%p, newb%p, news%mdis

    news%pc(:,1) = news%p + [1,0] *news%mdis
    news%pc(:,2) = news%p + [-1,0]*news%mdis
    news%pc(:,3) = news%p + [0,1] *news%mdis
    news%pc(:,4) = news%p + [0,-1]*news%mdis
    do j=1,4
      news%pc(:,j) = pos2new(news%pc(:,j))
print *, news%pc(:,j)
    end do
print *
  end subroutine read_oneline


  integer function manhattan(p1, p2) result(md)
    integer, intent(in) :: p1(2), p2(2)
    md = sum(abs(p1-p2))
  end function

  integer function count_impossible(ss, bb, lims, y) result(ans)
    type(sensor_t), intent(in) :: ss(:)
    type(beacon_t), intent(in) :: bb(:)
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
        do ib=1,size(bb)
          if (all(bb(ib)%p==[j,y])) then
            isknownbeacon = .true.
            exit
          end if
        end do
        if (.not. isknownbeacon) ans = ans+1
      end if
    end do
  end function


  function find_uknown(ss,lims) result(p)
    integer :: p(2)
    type(sensor_t), intent(in) :: ss(:)
    integer, intent(in) :: lims(4)

    integer :: i, j, is

    do i=lims(1),lims(2)
    XROW: do j=lims(3),lims(4)
      do is=1,size(ss)
        if (manhattan([i,j], ss(is)%p) <= ss(is)%mdis) cycle XROW
      end do
      if (is==size(ss)+1) then
        p = [i, j]
        print *, 'found'
        return
      end if
    end do XROW
    end do
  end function


  function pos2new(p) result(pn)
    integer, intent(in) :: p(2)
    integer :: pn(2)
    pn(1) = p(1) - p(2)
    pn(2) = p(1) + p(2)
  end function
  function new2pos(p) result(pn)
    integer, intent(in) :: p(2)
    integer :: pn(2)
    pn(1) = p(1) + p(2)
    pn(2) = -p(1) + p(2)
    if (mod(pn(1),2)/=0 .or. mod(pn(2),2)/=0) error stop 'transformation error'
    pn = pn/2
  end function
end module day2215_mod