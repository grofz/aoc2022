! TO-DO storing states in an array is ineffective
! better structure should be added (e.g. a priority queue)

module djikstra_mod
    implicit none
    private
    public djikstra_search

    integer :: dummy_int

    type, abstract, public :: djikstra_node_at
        logical :: visited = .false.
        integer :: d = huge(dummy_int)
    contains
        procedure(nextngb_ai), deferred :: nextngb
        procedure(isequal_ai), deferred :: isequal
        procedure(istarget_ai), deferred :: istarget
    end type

    abstract interface
        subroutine nextngb_ai(node, flag, node_ngb, distance)
            import djikstra_node_at
            implicit none
            class(djikstra_node_at), intent(in) :: node
            integer, intent(inout) :: flag !"0" on entry: first ngb, "0" on return: no more ngb
            class(djikstra_node_at), intent(out), allocatable :: node_ngb
            integer, intent(out) :: distance
        end subroutine

        logical function isequal_ai(anode, bnode)
            import djikstra_node_at
            implicit none
            class(djikstra_node_at), intent(in) :: anode, bnode
        end function

        logical function istarget_ai(node)
            import djikstra_node_at
            implicit none
            class(djikstra_node_at), intent(in) :: node
        end function
    end interface

    type, public :: djikstra_node_ptr
        class(djikstra_node_at), allocatable :: ptr
    end type

contains

    subroutine djikstra_search(nodes, startnode, shortest_d)
        type(djikstra_node_ptr), allocatable, intent(inout) :: nodes(:)
        class(djikstra_node_at), intent(in) :: startnode
        integer, intent(out) :: shortest_d

        integer :: n,  i, curr_i, ngb_i, d, flag
        class(djikstra_node_at), allocatable :: ngb, ngb2
        type(djikstra_node_ptr), allocatable :: nodes1(:)

        ! Set all nodes unvisited and of infinite distance
        ! Add (if needed) the starting node to the list
        ! Set "d" of starting node to "0"
        shortest_d = huge(shortest_d)
        if (.not. allocated(nodes)) allocate(nodes(0))
        n = size(nodes)
        do i=1, n
            if (.not. allocated(nodes(i)%ptr)) error stop 'djikstra_search - unallocated node in the list'
            nodes(i)%ptr%visited = .false.
            nodes(i)%ptr%d = huge(nodes(i)%ptr%d)
        end do
        call addnode_unique(startnode, nodes, n)
        i = findnode(startnode, nodes, n)
        if (i==0) error stop 'djikstra_search - error 1'
        nodes(i)%ptr%d = 0
        nodes(i)%ptr%visited = .false.

        ! Main loop
        MAIN: do 
            curr_i = findsmallest(nodes, n)
            if (curr_i == 0) then
                print *, 'djikstra - all nodes searched, target not found'
                exit MAIN
            end if

            ! Find and process all neighbors of the current node
            flag = 0
            do
                call nodes(curr_i)%ptr%nextngb(flag, ngb, d)
                if (flag==0) exit
                call addnode_unique(ngb, nodes, n)
                ngb_i = findnode(ngb, nodes, n)
                d = d + nodes(curr_i)%ptr%d
                if (nodes(ngb_i)%ptr%d > d) nodes(ngb_i)%ptr%d = d
            end do

            ! Mark current node as visited
            nodes(curr_i)%ptr%visited = .true.

            ! Exit from loop if target has been reached
            if (nodes(curr_i)%ptr%istarget()) then
                shortest_d = nodes(curr_i)%ptr%d
                exit MAIN
            end if
            
        end do MAIN

        ! return "nodes" at a correct size
        allocate(nodes1(n))
        do i=1,n
            allocate(nodes1(i)%ptr, source=nodes(i)%ptr)
        end do
        call move_alloc(nodes1, nodes)
    end subroutine djikstra_search


    integer function findnode(node, nodes, n) result(i_found)
        class(djikstra_node_at), intent(in) :: node
        type(djikstra_node_ptr), intent(in) :: nodes(:)
        integer, intent(in) :: n

        integer :: i
        i_found = 0
        do i = n, 1, -1
            if (nodes(i)%ptr%isequal(node)) then
                i_found = i
                exit
            end if
        end do
    end function findnode


    integer function findsmallest(nodes, n) result(i_smallest)
        type(djikstra_node_ptr), intent(in) :: nodes(:)
        integer, intent(in) :: n

        integer :: min_d, i
        min_d = huge(min_d)
        i_smallest = 0
        do i = 1, n
            ! only unvisited nodes are considered
            if (nodes(i)%ptr%visited) cycle
            if (nodes(i)%ptr%d < min_d) then
                i_smallest = i
                min_d = nodes(i)%ptr%d
            end if
        end do
    end function findsmallest


    subroutine addnode_unique(newnode, nodes, n)
        class(djikstra_node_at), intent(in) :: newnode
        type(djikstra_node_ptr), allocatable, intent(inout) :: nodes(:)
        integer, intent(inout) :: n

        type(djikstra_node_ptr), allocatable :: wrk(:)
        integer :: nmax, i

        ! expand if needed
        nmax = size(nodes)
        if (nmax<=n) then
            allocate(wrk(max(2*nmax,100)))
            do i=1, n
                allocate(wrk(i)%ptr, source=nodes(i)%ptr)
            end do
            call move_alloc(wrk, nodes)
        end if

        i = findnode(newnode, nodes, n)
        if (i==0) then
            ! node is not in list, add it at the end
            n = n + 1
            allocate(nodes(n)%ptr, source=newnode)
            ! initialize workings of a new node
            nodes(n)%ptr%d = huge(nodes(n)%ptr%d)
            nodes(n)%ptr%visited = .false.

        end if
    end subroutine addnode_unique

end module djikstra_mod