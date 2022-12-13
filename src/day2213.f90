module day2213_mod
  use parse_mod, only : string_t, read_strings
  implicit none

  type token_t
    logical :: is_list = .false.
    integer :: val
    type(token_ptr), allocatable :: list(:)
  contains
    procedure :: add => token_additem
  end type
  interface token_t
    module procedure token_newlist
    module procedure token_newnumber
  end interface

  type token_ptr
    type(token_t), pointer :: ptr
  end type

  integer, parameter :: ORD_OK=1, ORD_REVERSE=2, ORD_UNKNOWN=3

contains

  subroutine day2213(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    type(token_ptr), allocatable :: tokens(:)
    integer, allocatable :: pos(:)
    integer :: ires, i, j, ipair, ans1, ans2, ntok

    lines = read_strings(file)
    !lines = read_strings('inp/13/test.txt')

    ntok = 0
    do i=1,size(lines)
      if (len_trim(lines(i)%str) /= 0 ) ntok = ntok + 1
    end do
    allocate(tokens(ntok+2))
    allocate(pos(ntok+2))
    do i=1,size(pos)
      pos(i) = i
    end do

    ipair = 0
    ans1 = 0
    j = 0
    do i=1,size(lines),3
      ipair = ipair + 1
      tokens(j+1) = str2token(lines(i)%str)
      tokens(j+2) = str2token(lines(i+1)%str)
      call compare(tokens(j+1)%ptr, tokens(j+2)%ptr, ires)
      if (ires==ORD_OK) ans1 = ans1 + ipair
      j = j + 2
    end do
    print '("Answer 13/1 ",i0,l2)', ans1, ans1==5806

    ! Part2;  Add two divisors and sort
    tokens(ntok+1) = str2token('[[2]]')
    tokens(ntok+2) = str2token('[[6]]')
    call sort(tokens, pos)
    ans2 = findloc(pos, ntok+1, 1)*findloc(pos, ntok+2, 1)
    print *, 'div 2', findloc(pos, ntok+1)
    print *, 'div 6', findloc(pos, ntok+2)
    print '("Answer 13/2 ",i0,l2)', ans2, ans2==23600

    ! Free tokens
    do i=1, size(tokens)
      call token_free(tokens(i)%ptr)
    end do
  end subroutine day2213


  type(token_ptr) function str2token(str) result(new)
    character(len=*), intent(in) :: str

    integer :: ind
    ind = 1
    if (len(str)<2) error stop 'str2token - empty string'
    if (str(1:1)/='[' .or. str(len(str):len(str))/=']') &
      error stop 'str2token - string seems not complete'
    call parse_tokens(str, ind, new%ptr)
  end function str2token


  recursive subroutine parse_tokens(str, ind, new)
    character(len=*), intent(in) :: str
    integer, intent(inout) :: ind
    type(token_t), intent(out), pointer :: new

    integer :: iend, ind0, tmp
    type(token_t), pointer :: new0
    type(token_ptr) :: new_ptr

    if (str(ind:ind)=='[') then
      ! is a list
      new => token_t()
      ind0 = ind+1
      do 
        call parse_tokens(str, ind0, new0)
        call new%add(new0)
        if (str(ind0-1:ind0-1)==']') exit

        if (ind0 > len_trim(str)) error stop 'parse_tokens - unfinicshed...'
      end do
      ind = ind0 + 1

    else
      ! is a number (or empty)
      iend = scan(str(ind:),',]')
      iend = ind + iend - 1
      if (iend<ind) error stop 'parse_tokens - null number'
      if (iend==ind) then
        ! empty string
        new => null()
      else
        read(str(ind:iend-1),*) tmp
        new => token_t(tmp)
      end if
      ind = iend + 1
    end if
  end subroutine parse_tokens


  function token_newnumber(val) result(new)
    integer, intent(in) :: val
    type(token_t), pointer :: new
    allocate(new)
    new%is_list = .false.
    new%val = val
  end function


  function token_newlist() result(new)
    type(token_t), pointer :: new
    allocate(new)
    new%is_list = .true.
    allocate(new%list(0))
  end function


  subroutine token_additem(this, add)
    class(token_t), intent(inout) :: this
    type(token_t), pointer, intent(in) :: add

    type(token_ptr) :: add_ptr

    if (.not. associated(add)) return
    add_ptr%ptr => add
    this%list = [this%list, add_ptr]
  end subroutine


  recursive subroutine compare(toka, tokb, ires)
    type(token_t), intent(in) :: toka, tokb
    integer, intent(out) :: ires 

    logical :: isa_list, isb_list
    integer :: ind
    type(token_t), pointer :: tmp_list

    isa_list = toka%is_list
    isb_list = tokb%is_list
    tmp_list => null()

    if (isa_list .and. isb_list) then
      ! both are lists
      ind = 1
      do
        if (size(toka%list)<ind .and. size(tokb%list)>=ind) then
          ires = ORD_OK
          exit
        else if (size(tokb%list)<ind .and. size(toka%list)>=ind) then
          ires = ORD_REVERSE
          exit
        else if (size(tokb%list)<ind .and. size(tokb%list)<ind) then
          ires = ORD_UNKNOWN
          exit
        end if
        call compare(toka%list(ind)%ptr, tokb%list(ind)%ptr, ires)
        if (ires /= ORD_UNKNOWN) exit
        ind = ind + 1
      end do

    else if (.not. isa_list .and. .not. isb_list) then
      ! both are numbers
      if (toka%val < tokb%val) then
        ires = ORD_OK
      else if (toka%val > tokb%val) then
        ires = ORD_REVERSE
      else
        ires = ORD_UNKNOWN
      end if

    else if (isa_list .and. .not. isb_list) then
      ! list and number
      tmp_list => token_t()
      call tmp_list%add(token_t(tokb%val))
      call compare(toka, tmp_list, ires)

    else if (isb_list .and. .not. isa_list) then
      ! number and list
      tmp_list => token_t()
      call tmp_list%add(token_t(toka%val))
      call compare(tmp_list, tokb, ires)

    else
      error stop 'compare - impossible branch'
    end if

    if (associated(tmp_list)) call token_free(tmp_list)
  end subroutine compare


  subroutine sort(tok, arr)
    type(token_ptr), intent(in) :: tok(:)
    integer, intent(inout) :: arr(:)

    integer :: i, ires, itmp
    logical :: swapped

    ! Bubble sort
    do
      swapped = .false.
      do i=1, size(arr)-1
        call compare(tok(arr(i))%ptr, tok(arr(i+1))%ptr, ires)
        if (ires == ORD_OK) then
          ! correct order
        else if (ires == ORD_REVERSE) then
          ! swap
          swapped = .true.
          itmp = arr(i)
          arr(i) = arr(i+1)
          arr(i+1) = itmp
        else
          error stop 'sort - can not decide'
        end if
      end do
      if (.not. swapped) exit
    end do
  end subroutine sort


  recursive subroutine token_free(this)
    type(token_t), pointer, intent(inout) :: this
    integer :: i
    if (.not. associated(this)) return
    if (this%is_list) then
      do i=1,size(this%list)
        call token_free(this%list(i)%ptr)
      end do
    end if
    deallocate(this)
  end subroutine

end module day2213_mod
