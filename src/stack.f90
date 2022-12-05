  !
  ! 2022, grofz@vscht.cz
  ! Working stack implementation
  !

  module stack_mod
    use dll_mod
    implicit none
    private

    type, public :: stack_t
      type(node_t), pointer :: head => null()
    contains
      procedure :: put => stack_put
      procedure :: pop => stack_pop
      procedure :: peek => stack_peek
      procedure :: isempty => stack_isempty
      procedure, private, pass(this) :: copy => stack_copy
      generic :: assignment(=) => copy
      final :: stack_removeall
    end type

  contains

    subroutine stack_put(this, val, ierr)
      class(stack_t), intent(inout) :: this
      integer, intent(in) :: val
      integer, intent(out), optional :: ierr
      call dll_addnodefirst(this%head, dll_newnode(val))
      if (present(ierr)) ierr = 0
    end subroutine


    subroutine stack_pop(this, val, ierr)
      class(stack_t), intent(inout) :: this
      integer, intent(out) :: val
      integer, intent(out), optional :: ierr

      type(node_t), pointer :: current
      if (this%isempty()) then
        if (present(ierr)) then
          ierr = -1
        else
          error stop 'stack_pop: stack is empty'
        end if
      else
        val = this%head%val
        current => this%head
        call dll_removenode(current, this%head)
        if (present(ierr)) ierr = 0
      end if
    end subroutine


    function stack_peek(this, ierr) result(val)
      integer :: val
      class(stack_t), intent(in) :: this
      integer, intent(out), optional :: ierr

      if (this%isempty()) then
        if (present(ierr)) then
          ierr = -1
        else
          error stop 'stack_peek: stack is empty'
        end if
      else
        val = this%head%val
        if (present(ierr)) ierr = 0
      end if
    end function


    function stack_isempty(this) result(isempty)
      logical :: isempty
      class(stack_t), intent(in) :: this
      !isempty = dll_count(this%head)==0
      isempty = .not. associated(this%head)
    end function


    function stack_export(this) result(arr)
      integer, allocatable :: arr(:)
      class(stack_t), intent(in) :: this
    end function


    subroutine stack_copy(newstack, this)
      type(stack_t), intent(inout) :: newstack
      class(stack_t), intent(in) :: this
      newstack%head => dll_copy(this%head)
    end subroutine


    subroutine stack_removeall(stack)
      type(stack_t), intent(inout) :: stack
      call dll_removeall(stack%head)
    end subroutine

  end module stack_mod
