module day6

  use, intrinsic :: iso_fortran_env, only: int32
  implicit none

  character(len=*), parameter :: lowercase = 'abcdefghijklmnopqrstuvwxyz'

contains

  subroutine process_forms(file,nyes_any,nyes_all)
    character(len=*), intent(in) :: file
    integer, intent(out) :: nyes_any, nyes_all

    integer :: unit, err, ngroup, i, ii, npeople
    character(len=100) :: buffer
    logical :: eof
    integer(int32) :: mask_or, mask_and, mask

    open(newunit=unit,file=file,action="read",status="old")

    nyes_any = 0
    nyes_all = 0

    ngroup = 0
    npeople = 0
    
    mask_or = 0
    mask_and = 0

    do
      read(unit,'(A)',iostat=err) buffer

      if (err < 0) eof = .true.

      if (len_trim(buffer) > 0) then
        npeople = npeople + 1
        mask = 0
        do i = 1, len_trim(buffer)
          ii = index(lowercase,buffer(i:i))
          if (ii > 0) mask = ibset(mask,ii-1)
        end do
        mask_or = ior(mask_or,mask)
        if (npeople == 1) then
          mask_and = mask
        else
          mask_and = iand(mask_and,mask)
        end if
      end if

      if (len_trim(buffer) == 0 .or. eof) then
        nyes_any = nyes_any + popcnt(mask_or)
        nyes_all = nyes_all + popcnt(mask_and)
        
        ngroup = ngroup + 1
        npeople = 0

        mask_or = 0
        mask_and = 0
      end if

      if (eof) exit

    end do

    close(unit)

  end subroutine

end module

program main

  use day6
  implicit none
  
  integer :: nyes, nyes2

  call process_forms("input",nyes,nyes2)
  print *, "nyes_any = ", nyes
  print *, "nyes_all = ", nyes2
end program