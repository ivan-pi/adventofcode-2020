module day1_mod

  implicit none

contains

  integer function count_lines(unit,iostat) result(n)
    integer, intent(in) :: unit
    integer, intent(out), optional :: iostat

    integer :: err

    n = 0
    do
      read(unit,*,iostat=err)
      if (err /= 0) exit
      n = n + 1
    end do

    rewind(unit)

    if (present(iostat)) iostat = err

  end function


  subroutine get_expenses(filename,expenses)
    character(len=*), intent(in) :: filename
    integer, intent(out), allocatable :: expenses(:)

    integer :: n, i
    integer :: unit

    open(newunit=unit,file=filename,status="old",action="read")

    n = count_lines(unit)

    allocate(expenses(n))

    do i = 1, n
      read(unit,*) expenses(i)
    end do

    close(unit)

  end subroutine

end module

program main
  
  use day1_mod
  implicit none

  integer, allocatable :: expenses(:)
  integer :: n, i, j, k, s, p

  call get_expenses("input",expenses)
  n = size(expenses)

  !
  ! Part 1
  !
  do i = 1, n
    do j = i + 1, n
      s = expenses(i) + expenses(j)
      if (s == 2020) then
        p = expenses(i)*expenses(j)
        exit
      end if
    end do
  end do

  print *, p

  !
  ! Part 2
  !
  do i = 1, n
    do j = i + 1, n
      do k = j + 1, n
      s = sum(expenses([i,j,k]))
        if (s == 2020) then
          p = product(expenses([i,j,k]))
          exit
        end if
      end do
    end do
  end do

  print *, p

end program