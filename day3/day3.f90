module day3

  use iso_fortran_env, only: ik => int64
  implicit none

  type :: tree_map
    integer(ik) :: h, w
    character(len=:), allocatable :: s(:)
  contains
    procedure :: traverse => traverse_map
  end type

contains

  subroutine traverse_map(self,slope,trees_encountered)
    class(tree_map), intent(in) :: self
    integer(ik), intent(in) :: slope(2) ! right, down
    integer(ik), intent(out) :: trees_encountered

    integer(ik) :: i, im, jm
    character(len=100) :: buffer

    trees_encountered = 0_ik

    ! Starting position
    im = 1
    jm = 1
    ! Move down the rows of the map
    do
      im = im + slope(2)
      jm = jm + slope(1)

      if (jm > self%w) then
        jm = mod(jm,self%w)
      end if

      if (im > self%h) exit

      buffer = self%s(im)
      if (buffer(jm:jm) == '.') then
        buffer(jm:jm) = 'O'
      else
        buffer(jm:jm) = 'X'
      end if

      ! write(*,*) trim(buffer)//"   "//self%s(im)

      if (self%s(im)(jm:jm) == '#') then
        trees_encountered = trees_encountered + 1
      end if
    end do
  end subroutine

  subroutine load_input(file,map)
    character(len=*), intent(in) :: file
    type(tree_map), intent(out) :: map

    integer(ik) :: unit, rows, cols, i
    character(len=100) :: buffer

    open(newunit=unit,file=file,status='old',action='read')
    rows = count_rows(unit)

    read(unit,*) buffer
    cols = len_trim(buffer)
    rewind(unit)

    map%h = rows
    map%w = cols
    allocate(character(len=cols) :: map%s(rows))

    do i = 1, rows
      read(unit,*) map%s(i)
    end do

    close(unit)
  end subroutine

  integer(ik) function count_rows(unit) result(n)
    integer(ik), intent(in) :: unit

    integer(ik) :: err
    character(len=100) :: str, msg

    n = 0
    do
      read(unit,*,iostat=err) str
      ! print *, err, str
      if (err /= 0) then
        if (err > 0) then
          write(msg,'(A,I0,A,I0)') "Error encountered in unit ", unit, ". Error code: ", err
          error stop msg
        else
          ! end of file
          exit
        end if
      end if
      n = n + 1
    end do
    rewind(unit)
  end function

end module

program main

  use day3
  implicit none

  type(tree_map) :: map
  integer(ik) :: n, i, p
  integer(ik) :: si(5), sj(5)

  call load_input("input",map)

  call map%traverse(slope=[integer(ik) :: 3, 1],trees_encountered=n)
  print *, "trees encountered: ", n

  sj = [1,3,5,7,1] ! slope right
  si = [1,1,1,1,2] ! slope down
  p = 1
  do i = 1, 5
    call map%traverse(slope=[sj(i),si(i)],trees_encountered=n)
    p = p * n
  end do
  print *, "result: ", p

end program