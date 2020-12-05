module day5

  implicit none

contains

  subroutine decode_boarding_pass(bp,row,col,seat_id)
    character(len=10), intent(in) :: bp
    integer, intent(out) :: row, col
    integer, intent(out) :: seat_id

    integer :: i
    row = 0
    do i = 1, 7
      if (bp(i:i) == 'B') row = ibset(row,7 - i)
    end do

    col = 0
    do i = 1, 3
      if (bp(i+7:i+7) == 'R') col = ibset(col,3 - i)
    end do

    seat_id = row*8 + col
  end subroutine

  subroutine read_boarding_pass_seat_ids(file,seats)
    character(len=*), intent(in) :: file
    integer, intent(out), allocatable :: seats(:)

    integer :: unit, n, i, err
    character(len=100) buffer
    integer :: row, col

    open(newunit=unit,file=file,status="old",action="read")

    ! Find number of lines
    n = 0
    do
      read(unit,*,iostat=err) buffer
      if (err < 0) exit
      n = n + 1
    end do
    rewind(unit)

    ! Allocate space
    allocate(seats(n))

    ! Iterature through the file, decoding the boarding passes
    do i = 1, n
      read(unit,*) buffer
      call decode_boarding_pass(trim(buffer),row,col,seats(i))
    end do

    close(unit)

  end subroutine

end module

program main

  use m, only: my_qsort
  use day5
  implicit none

  integer :: i, loc(1)

  integer, allocatable :: seats(:)
  logical, allocatable :: seat_taken(:)

  call read_boarding_pass_seat_ids("input",seats)
  write(*,'(A,I0)') "Highest seat ID: ", maxval(seats)


  allocate(seat_taken(minval(seats):maxval(seats)))
  seat_taken = .false.

  call my_qsort(seats,size(seats))

  do i = 1, size(seats)
    seat_taken(seats(i)) = .true.
  end do

  loc = findloc(seat_taken,.false.)
  print *, loc + lbound(seat_taken) - 1, seats(loc(1) - 1) + 1

end program main