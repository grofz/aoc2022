module day2212_mod
  use parse_mod, only : read_pattern
  use djikstra_mod, only : djikstra_node_at, djikstra_node_ptr, djikstra_search
  implicit none
  private
  public day2212

  type, extends(djikstra_node_at) :: state_t
    character(len=1), pointer :: h(:,:) => null()
    integer :: xy(2), tar(2)
  contains
    procedure :: nextngb, isequal, istarget
  end type

contains

  subroutine day2212(file)
    character(len=*), intent(in) :: file

    character(len=1), allocatable, target :: h(:,:)
    type(djikstra_node_ptr), allocatable :: wrk(:)
    type(state_t) :: start, start0
    integer :: ans1, i, j, ans0, ans2

    !h = read_pattern('inp/12/test.txt')
    h = read_pattern(file)
    start%h => h
    start%xy = findloc(h,'S')
    h(start%xy(1),start%xy(2)) = 'a'
    start%tar = findloc(h,'E')
    h(start%tar(1),start%tar(2)) = 'z'

    call djikstra_search(wrk, start, ans1)
    print '("Answer 12/1 ",i0,l2)', ans1, ans1==383
    stop

    ans2 = ans1
    do i=1,size(h,1)
    do j=1,size(h,2)
      if (h(i,j)/='a') cycle
      start0 = start
      start0%xy = [i,j]
      call djikstra_search(wrk, start0, ans0)
      print *, i, j, ans0
      if (ans0<ans2) ans2 = ans0
    end do
    end do
    print *, 'ans2 = ',ans2

  end subroutine day2212

  subroutine nextngb(node, flag, node_ngb, distance)
    class(state_t), intent(in) :: node
    integer, intent(inout) :: flag !"0" on entry: first ngb, "0" on return: no more ngb
    class(djikstra_node_at), intent(out), allocatable :: node_ngb
    integer, intent(out) :: distance

    integer, parameter :: DIR(2,4) = reshape([-1,0, 0,1, 1,0, 0,-1], [2,4])
    integer :: i

    distance = 1
    allocate(node_ngb, source=node)
    select type(node_ngb)
    class is (state_t)
      do i=flag+1,4
        node_ngb%xy = node%xy + DIR(:,i)
        ! ignore out of map positions
        if (any(node_ngb%xy<1) .or. node_ngb%xy(1)>size(node%h,1) .or. node_ngb%xy(2)>size(node%h,2)) cycle
        ! ignore higher than one places
        if (iachar(node%h(node_ngb%xy(1),node_ngb%xy(2))) > iachar(node%h(node%xy(1),node%xy(2)))+1 ) cycle
        ! position is possible
        exit
      end do
      if (i==4+1) then
        flag = 0
      else
        flag = i
      end if
    end select
  end subroutine

  logical function isequal(anode, bnode)
    class(state_t), intent(in) :: anode
    class(djikstra_node_at), intent(in) :: bnode

    select type(bnode)
    class is (state_t)
      isequal = all(anode%xy == bnode%xy)
    class default
      error stop 'invalid class'
    end select
  end function

  logical function istarget(node)
    class(state_t), intent(in) :: node
    istarget = all(node%xy==node%tar)
  end function
end module day2212_mod