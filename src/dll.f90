! =================================================
! COURSE
! Advanced programming and programming technologies
!
! Linked-list implementation example in Fortran
! Version: 27.10.2022
! grofz@vscht.cz
! =================================================

module dll_mod
  implicit none

  type :: node_t
    integer :: val
    type(node_t), pointer :: next => null()
    type(node_t), pointer :: prev => null()
  end type

contains

  function dll_newnode(val) result(new)
    type(node_t), pointer :: new
    integer, intent(in) :: val

    allocate(new)
    new % val = val
  end function dll_newnode


  subroutine dll_addnodebehind(current, added)
    type(node_t), pointer, intent(inout) :: current
    type(node_t), pointer, intent(in) :: added

    if (associated(added%next) .or. associated(added%prev)) &
        error stop 'dll_addnodebehind: Added node is not isolated'      

    if (.not. associated(current)) then
      ! List is empty: adding the first node
      current => added
    else
      ! List is not empty
      added % prev => current
      added % next => current % next
      if (associated(current%next)) current % next % prev => added
      current % next => added
    end if
  end subroutine dll_addnodebehind


  function dll_export(head) result(arr)
    integer, allocatable :: arr(:)
    type(node_t), pointer, intent(in) :: head
    integer :: n, i
    type(node_t), pointer :: current

    n = dll_count(head)
    allocate(arr(n))
    current => head
    do i=1,n
      if (.not. associated(current)) &
        error stop "dll_export FAILED"
      arr(i) = current % val
      current => current % next
    end do 
  end function dll_export


  function dll_count(head) result(n)
    integer :: n
    type(node_t), pointer, intent(in) :: head
    type(node_t), pointer :: current

    n = 0
    current => head
    do
      if (.not. associated(current)) exit
      n = n + 1
      current => current % next
    end do
  end function dll_count


  function dll_search(head, val) result(found)
    type(node_t), pointer :: found
    type(node_t), pointer, intent(in) :: head
    integer, intent(in) :: val
    type(node_t), pointer :: current

    found => null()
    current => head
    do 
      if (.not. associated(current)) exit
      if (current % val == val) then
        found => current
        exit
      else
        current => current % next
      end if
    end do
  end function


  subroutine dll_removenode(current, head)
    type(node_t), pointer, intent(in) :: current
    type(node_t), pointer, intent(inout) :: head
    type(node_t), pointer :: removed

    if (.not. associated(current)) return
    ! Re-point head if removing first node
    if (associated(head, current)) then
      head => current % next
    end if
    ! Remove current node
    if (associated(current%next)) &
      current%next%prev => current%prev
    if (associated(current%prev)) &
      current%prev%next => current%next
    removed => current
    deallocate(removed)
  end subroutine dll_removenode


  ! Additional procedures (left as an exercise)


  subroutine dll_addnodefirst(head, added)
    type(node_t), pointer, intent(inout) :: head
    type(node_t), pointer, intent(in)    :: added

    if (associated(added%next) .or. associated(added%prev)) &
        error stop 'dll_addnodefirst: Added node is not isolated'      

    added % next => head
    if (associated(head)) head % prev => added
    head => added
  end subroutine dll_addnodefirst


  function dll_verify(head) result(isvalid)
    logical :: isvalid
    type(node_t), pointer, intent(in) :: head
    type(node_t), pointer :: current, expected
    integer :: n

    isvalid = .true.
    if (associated(head)) then
      if (associated(head%prev)) isvalid = .false.
      expected => head
      current => head%next
      n = 1
      do
        if (.not. associated(current)) exit
        if (.not. associated(current%prev, expected)) isvalid = .false.
        n = n + 1
        expected => current
        current  => current % next
      end do
    else
      ! empty list
      n = 0
    end if
    if (dll_count(head) /= n) isvalid = .false.
    !if (.not. isvalid) error stop 'dll_isvalid: validation FAILS'
  end function dll_verify


  function dll_import(arr) result(head)
    type(node_t), pointer :: head
    integer, intent(in) :: arr(:)
    integer :: i
    head => null()
    do i=1,size(arr)
      call dll_addnodefirst(head, dll_newnode(arr(i)))
    end do
  end function dll_import


  function dll_copy(oldhead) result(newhead)
    type(node_t), pointer :: newhead
    type(node_t), pointer, intent(in) :: oldhead
    integer, allocatable :: arr(:)
    arr = dll_export(oldhead)
    newhead => dll_import(arr(size(arr):1:-1))
  end function dll_copy


  subroutine dll_removeall(head)
    type(node_t), intent(inout), pointer :: head
    type(node_t), pointer :: current

    do
      current => head
      if (.not. associated(current)) exit
      call dll_removenode(current, head)
    end do
  end subroutine dll_removeall

end module dll_mod


! To be implemented later if needed
! dll_sort
! dll_join



! ==============
! Example driver
! ==============
! removed 
