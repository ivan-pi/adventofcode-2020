module day2

  implicit none

contains

  integer function count_valid_passwords(input,validator,echo) result(n)
    character(len=*), intent(in) :: input
    interface
      logical function validator(pw,l,r)
        character(len=*), intent(in) :: pw
        character(len=1), intent(in) :: l
        integer, intent(in) :: r(2)
      end function
    end interface
    logical, intent(in) :: echo

    integer :: unit
    character(len=100) :: fmt, password, c3
    character(len=5) :: c1
    character(len=2) :: c2
    character(len=1) :: letter
    integer :: i1, i2, err, pos

    open(newunit=unit,file=trim(input),status="old",action="read")

    fmt = "(I0,'-',I0,' ',A1,': ',A)"
    n = 0
    do
      ! read input
      read(unit,*,iostat=err) c1, c2, c3
      
      ! parse input
      letter = c2(1:1)
      pos = index(c1,'-')
      read(c1(1:pos-1),*) i1
      read(c1(pos+1:),*) i2
      password = trim(c3)

      ! echo input 
      if (echo) then
        write(*,fmt) i1, i2, letter, trim(password)
      end if

      if (err /= 0) then
        if (err > 0) then
          error stop "Problem encountered in "//input
        else
          ! end of file
          exit
        end if
      end if
      if (validator(trim(password),letter,[i1,i2])) n = n + 1
    end do
    close(unit)

  end function

  logical function is_valid(password,letter,freq_range)
    character(len=*), intent(in) :: password
    character(len=1), intent(in) :: letter
    integer, intent(in) :: freq_range(2)

    integer :: n, i

    is_valid = .false.

    n = 0
    do i = 1, len_trim(password)    
      if (password(i:i) == letter) then
        n = n + 1
      end if
    end do

    if (freq_range(1) <= n .and. n <= freq_range(2)) then
      is_valid = .true.
    end if 
  end function

  logical function is_valid2(password,letter,freq_range)
    character(len=*), intent(in) :: password
    character(len=1), intent(in) :: letter
    integer, intent(in) :: freq_range(2)

    character(len=1) :: l1, l2

    is_valid2 = .false.
    associate(i1 => freq_range(1), i2 => freq_range(2))
    l1 = password(i1:i1)
    l2 = password(i2:i2)
    if ((l1 == letter .and. l2 /= letter) .or. &
        (l1 /= letter .and. l2 == letter)) then
      is_valid2 = .true.
    end if 
    end associate
  end function

end module

program main

  use day2
  implicit none

  print *, is_valid('abcde','a',[1,3])
  print *, is_valid('cdefg','b',[1,3])
  print *, is_valid('ccccccccc','c',[2,9])

  print *, count_valid_passwords("input",is_valid,echo=.true.)
  
  print *, is_valid2('abcde','a',[1,3])
  print *, is_valid2('cdefg','b',[1,3])
  print *, is_valid2('ccccccccc','c',[2,9])

  print *, count_valid_passwords("input",is_valid2,echo=.false.)

end program