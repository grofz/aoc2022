module day2220_mod
  use iso_fortran_env, only : I8=>int64
  use parse_mod, only : read_numbers
  implicit none

  type cll_t
    integer(I8) :: val
    type(cll_t), pointer :: next => null()
    type(cll_t), pointer :: prev => null()
    logical :: mixed=.false.
  end type cll_t

  type cll_ptr
    type(cll_t), pointer :: ptr => null()
  end type 

contains

  subroutine day2220(file)
    character(len=*), intent(in) :: file

    integer, allocatable :: alist(:)
    integer :: i, izero, ans, k
    integer(I8) :: ans2
    type(cll_t), pointer :: head, last, dest, head2
    type(cll_ptr), allocatable :: aindex(:), aindex2(:)

    integer(I8), parameter :: KEY = 811589153_I8
    integer :: NMIXED = 10

    alist = read_numbers(file)
    !alist = read_numbers('inp/20/test.txt')
   !print '(10(i5,1x))', alist
    print *, 'numbers = ',size(alist)

    allocate(aindex(size(alist)))
    do i=1,size(alist)
      aindex(i)%ptr => cll_new(int(alist(i),I8))
      if (i/=1) call cll_addbehind(last, aindex(i)%ptr)
      last => aindex(i)%ptr
      if (i==1) head => last
    end do
    izero = findloc(alist, 0, dim=1)
    print *, 'n =',cll_count(head), cll_count_reverse(head)
    !print '(10(i5,1x))', cll_export(head)

    ! mix
    do i=1,size(alist)
      if (aindex(i)%ptr%mixed) error stop 'already mixed'
      call cll_mix(aindex(i)%ptr)
      aindex(i)%ptr%mixed = .true.
    end do

    ans = 0
    print *, 'zero =',aindex(izero)%ptr%val
    dest => cll_jump(aindex(izero)%ptr, 1000_I8)
    print *, '1000 =', dest%val
    ans = ans + dest%val
    dest => cll_jump(dest, 1000_I8)
    print *, '2000 =', dest%val
    ans = ans + dest%val
    dest => cll_jump(dest, 1000_I8)
    print *, '3000 =', dest%val
    ans = ans + dest%val
    print *, 'ans =', ans

    ! Part 2
    allocate(aindex2(size(alist)))
    do i=1,size(alist)
      aindex2(i)%ptr => cll_new(int(alist(i),I8)*KEY)
      if (i/=1) call cll_addbehind(last, aindex2(i)%ptr)
      last => aindex2(i)%ptr
      if (i==1) head2 => last
    end do

    ! mix
    do k=1,NMIXED
    do i=1,size(alist)
      !if (aindex2(i)%ptr%mixed) error stop 'already mixed'
      call cll_mix(aindex2(i)%ptr)
      !aindex2(i)%ptr%mixed = .true.
    end do
    end do

    ans2 = 0
    print *, 'zero =',aindex2(izero)%ptr%val
    dest => cll_jump(aindex2(izero)%ptr, 1000_I8)
    print *, '1000 =', dest%val
    ans2 = ans2 + dest%val
    dest => cll_jump(dest, 1000_I8)
    print *, '2000 =', dest%val
    ans2 = ans2 + dest%val
    dest => cll_jump(dest, 1000_I8)
    print *, '3000 =', dest%val
    ans2 = ans2 + dest%val
    print *, 'ans2 =', ans2

  end subroutine day2220


  function cll_new(val) result(new)
    integer(I8), intent(in) :: val
    type(cll_t), pointer :: new

    allocate(new)
    new%prev => new
    new%next => new
    new%val = val
  end function


  subroutine cll_addbehind(node, added)
    type(cll_t), pointer, intent(in) :: node
    type(cll_t), pointer, intent(in) :: added

    if (.not. associated(added)) error stop 'null node to add'
    if (.not. associated(node)) then
      error stop 'cll_addbehind - is not inteded to add a first node'
    end if
    added%next => node%next
    node%next%prev => added
    node%next => added
    added%prev => node
  end subroutine cll_addbehind


  subroutine cll_movebehind(moved, dest)
    type(cll_t), pointer, intent(in) :: moved 
    type(cll_t), pointer, intent(in) :: dest

    ! todo may be error if less than three elements
    if (cll_count(moved) < 3) error stop 'movebehind not intended to work with small circles yeyt'

    ! Do nothing if moved is same as destination
    if (associated(moved,dest)) return

    ! Unlink "moved" from the circle
    moved%next%prev => moved%prev
    moved%prev%next => moved%next

    ! Place "moved" behind "dest"
    call cll_addbehind(dest, moved)
  end subroutine cll_movebehind


  subroutine cll_mix(moved)
    type(cll_t), pointer, intent(in) :: moved 

    type(cll_t), pointer :: dest
    integer :: n
    ! todo may be error if less than three elements
    if (cll_count(moved) < 3) error stop 'movebehind not intended to work with small circles yeyt'

    if (moved%val==0) return
    n = cll_count(moved)

    ! Unlink "moved" from the circle
    moved%next%prev => moved%prev
    moved%prev%next => moved%next

    ! find new position
    dest => cll_jump(moved, moved%val, n-1)

    ! Relink the node back
    call cll_addbehind(dest, moved)
  end subroutine


  function cll_jump(node, distance, n) result(moved)
    type(cll_t), pointer, intent(in) :: node
    integer(I8), intent(in) :: distance
    integer, intent(in), optional :: n
    type(cll_t), pointer :: moved

    integer :: i, distance0

    distance0 = distance
    if (present(n)) then
      distance0 = mod(abs(distance)-1_I8, n) + 1
      if (distance < 0) distance0 = -distance0
    end if
    
    moved => node
    if (.not. associated(node)) error stop 'cll_jump - node not associated'

    do i=1,abs(distance0)
      if (distance0>0) then
        moved => moved%next
      else
        moved => moved%prev
      end if
    end do
    if (distance0<0) moved => moved%prev
  end function cll_jump


  integer function cll_count(node) result(n)
    type(cll_t), pointer, intent(in) :: node

    type(cll_t), pointer :: current

    n = 0
    current => node
    do
      if (associated(current,node) .and. n/=0) exit
      n = n + 1
      current => current%next
    end do
  end function cll_count


  integer function cll_count_reverse(node) result(n)
    type(cll_t), pointer, intent(in) :: node

    type(cll_t), pointer :: current

    n = 0
    current => node
    do
      if (associated(current,node) .and. n/=0) exit
      n = n + 1
      current => current%prev
    end do
  end function cll_count_reverse


  function cll_export(node) result(arr)
    type(cll_t), pointer, intent(in) :: node
    integer, allocatable :: arr(:)

    type(cll_t), pointer :: current
    integer :: n, i

    n = cll_count(node)
    allocate(arr(n))
    current => node
    do i=1,n
      arr(i) = current%val
      current => current%next
    end do
    if (.not. associated(current, node)) error stop 'not returned at the same position'
  end function cll_export


  function cll_export_reverse(node) result(arr)
    type(cll_t), pointer, intent(in) :: node
    integer, allocatable :: arr(:)

    type(cll_t), pointer :: current
    integer :: n, i

    n = cll_count(node)
    allocate(arr(n))
    current => node
    do i=1,n
      arr(i) = current%val
      current => current%prev
    end do
    if (.not. associated(current, node)) error stop 'not returned at the same position'
  end function cll_export_reverse

end module day2220_mod