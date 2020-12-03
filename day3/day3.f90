module day3

  use iso_fortran_env, only: ik => int64
  implicit none
  private

  public :: ik
  public :: tree_map

  type :: tree_map
    integer(ik) :: h, w
    character(len=:), allocatable :: s(:)
  contains
    procedure :: traverse => traverse_map
    procedure :: load => load_input
  end type

contains

  function traverse_map(self,slope) result(trees_encountered)
    class(tree_map), intent(in) :: self
    integer(ik), intent(in) :: slope(2) ! right, down
    integer(ik) :: trees_encountered

    integer(ik) :: i, im, jm
    character(len=100) :: buffer

    trees_encountered = 0

    ! Starting position
    im = 1
    jm = 1
    ! Move down the rows of the map
    do
      im = im + slope(2)
      jm = jm + slope(1)

      ! The map is repetive
      if (jm > self%w) then
        jm = mod(jm,self%w)
      end if

      ! Check we have reached the end
      if (im > self%h) exit

      ! Just for checking
      ! buffer = self%s(im)
      ! if (buffer(jm:jm) == '.') then
      !   buffer(jm:jm) = 'O'
      ! else
      !   buffer(jm:jm) = 'X'
      ! end if
      ! write(*,*) trim(buffer)//"   "//self%s(im)

      ! Count trees we encounter
      if (self%s(im)(jm:jm) == '#') then
        trees_encountered = trees_encountered + 1
      end if
    end do

  end function

  subroutine load_input(map,file)
    class(tree_map), intent(out) :: map
    character(len=*), intent(in) :: file

    integer(ik) :: unit, rows, cols, i
    character(len=100) :: buffer

    open(newunit=unit,file=file,status='old',action='read')
    
    rows = count_rows(unit)

    ! Count columns
    read(unit,*) buffer
    cols = len_trim(buffer)
    rewind(unit)

    ! Initizalize tree map
    map%h = rows
    map%w = cols
    allocate(character(len=cols) :: map%s(rows))

    ! Read trees from file unit
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
  integer(ik) :: i, s(2), sr(5), sd(5)

  call map%load("input")

  s = [3, 1]
  print *, "trees encountered: ", map%traverse(s)

  sr = [1,3,5,7,1]
  sd = [1,1,1,1,2]
  print *, "result: ", product([(map%traverse([sr(i),sd(i)]),i=1,5)])

end program