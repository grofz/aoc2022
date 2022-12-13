module day2213_mod
  use parse_mod, only : string_t, read_strings
  implicit none

  type token_t
    logical :: is_list = .false.
    integer :: val
    type(token_ptr), allocatable :: list(:)
  end type

  type token_ptr
    type(token_t), pointer :: ptr
  end type

contains
  subroutine day2213(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    type(token_t), pointer :: tokenA, tokenB
    integer :: ind, i, ipair, ans

    lines = read_strings(file)
    !lines = read_strings('inp/13/test.txt')

   !  ind = 1
   !  call parse_tokens(lines(13)%str, ind, tokenA )
   !  ind = 1
   !  call parse_tokens(lines(14)%str, ind, tokenB )
   !  call compare(tokenA, tokenB, ind)
   !  print *, ind

   !  stop

    ipair = 0
    ans = 0
    do i=1,size(lines),3
      ind = 1
      call parse_tokens(lines(i)%str, ind, tokenA )
      ind = 1
      call parse_tokens(lines(i+1)%str, ind, tokenB )
      call compare(tokenA, tokenB, ind)

      ipair = ipair + 1
      print *, lines(i)%str
      print *, lines(i+1)%str
      print *, ipair, ind
      print *
      if (ind==1) ans = ans + ipair
    end do
    print *, 'answer ', ans

  end subroutine day2213


  recursive subroutine parse_tokens(str, ind, new)
    character(len=*), intent(in) :: str
    integer, intent(inout) :: ind
    type(token_t), intent(out), pointer :: new

    integer :: iend, ind0
    type(token_t), pointer :: new0
    type(token_ptr) :: new_ptr

    if (str(ind:ind)=='[') then
      ! is a list
      new => new_token()
      new%is_list = .true.
      allocate(new%list(0))

      ind0 = ind+1
      do 
        call parse_tokens(str, ind0, new0)
        if (associated(new0)) then
          new_ptr%ptr => new0
          new%list = [new%list, new_ptr]
        else
          ! null list
!         print *, 'null list', ind0
        end if
        if (str(ind0-1:ind0-1)==']') exit
        if (ind0 > len_trim(str)) then
          error stop 'unfinicshed...'
        end if
      end do
      ind = ind0 + 1
!print *, 'new list of items = ', size(new%list), 'next postion: ', ind
!print *, '{'//str(ind:)//'}'

    else if (str(ind:ind)==']') then ! empty list
      nullify(new)
      ind = ind + 1
    
    else
      ! is a number
      iend = scan(str(ind:),',]')
      iend = ind + iend - 1
      new => new_token()
      if (iend>ind) then
        new%is_list = .false.
        read(str(ind:iend-1),*) new%val
      else
        error stop 'parse_tokens - null number'
      end if

      ind = iend + 1
!print *, 'new number = ', new%val, 'next postion: ', ind
!print *, '{'//str(ind:)//'}'
    end if

  end subroutine


  function new_token() result(new)
    type(token_t), pointer :: new
    allocate(new)
  end function


  function new_token_list(val) result(new)
    type(token_t), pointer :: new
    integer, intent(in) :: val

    allocate(new)
    new%is_list = .true.
    allocate(new%list(1))
    new%list(1)%ptr => new_token()
    new%list(1)%ptr%is_list = .false.
    new%list(1)%ptr%val = val
  end function


  recursive subroutine compare(toka, tokb, ires)
    type(token_t), intent(in) :: toka, tokb
    integer, intent(out) :: ires 
    ! 1=right order, 2=not right order, 3=not determined

    logical :: isa_list, isb_list
    integer :: ind, ires0
    type(token_t), pointer :: tmp_list

    isa_list = toka%is_list
    isb_list = tokb%is_list
    tmp_list => null()

    if (isa_list .and. isb_list) then
      ! both are lists
      ind = 1
      do
        if (size(toka%list)<ind .and. size(tokb%list)>=ind) then
          ires = 1
          exit
        else if (size(tokb%list)<ind .and. size(toka%list)>=ind) then
          ires = 2
          exit
        else if (size(tokb%list)<ind .and. size(tokb%list)<ind) then
          ires = 3
          exit
        end if

        call compare(toka%list(ind)%ptr, tokb%list(ind)%ptr, ires0)
        ires = ires0
        if (ires /= 3) exit
        ind = ind + 1
      end do

    else if (.not. isa_list .and. .not. isb_list) then
      ! both are numbers
      if (toka%val < tokb%val) then
        ires = 1
      else if (toka%val > tokb%val) then
        ires = 2
      else
        ires = 3
      end if

    else if (isa_list .and. .not. isb_list) then
      ! list and number
      tmp_list => new_token_list(tokb%val)
      call compare(toka, tmp_list, ires)
    else if (isb_list .and. .not. isa_list) then
      ! number and list
      tmp_list => new_token_list(toka%val)
      call compare(tmp_list, tokb, ires)
    else
      error stop 'impossible branch'
    end if
  end subroutine

end module day2213_mod