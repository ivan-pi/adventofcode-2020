module day4

  use fortran202x_split, only: split
    !! The `fortran202x_split` module can be downloaded from:
    !!   https://github.com/milancurcic/fortran202x_split
  implicit none
  private

  public :: count_passports

  integer, parameter :: valid_mask = int(b'01111111')

  character(len=*), parameter :: digits = "1234567890"
  character(len=*), parameter :: lowercase = "abcdefghijklmnopqrstuvwxyz"

  type :: passport
    integer :: key_mask = 0, value_mask = 0
    integer :: byr, iyr, eyr
    integer :: hgt
    character(len=2) :: hgt_unit
    character(len=7) :: hcl
    character(len=3) :: ecl
    character(len=9) :: pid
    integer :: cid
  contains
    procedure :: add_key
    procedure :: add_key_value
    procedure :: clear
    procedure :: keys_present
    procedure :: keys_present_and_valid
    procedure :: print => print_passport
  end type

contains

  subroutine add_key(self,key)
    class(passport), intent(inout) :: self
    character(len=3), intent(in) :: key
    select case(key)
    case('byr') ! Birth year
      self%key_mask = ibset(self%key_mask,0)
    case('iyr') ! Issue year
      self%key_mask = ibset(self%key_mask,1)
    case('eyr') ! Expiration year
      self%key_mask = ibset(self%key_mask,2)
    case('hgt') ! Height
      self%key_mask = ibset(self%key_mask,3)
    case('hcl') ! Hair color
      self%key_mask = ibset(self%key_mask,4)
    case('ecl') ! Eye color
      self%key_mask = ibset(self%key_mask,5)
    case('pid') ! Passport ID
      self%key_mask = ibset(self%key_mask,6)
    case('cid') ! Country ID
      self%key_mask = ibset(self%key_mask,7)
    case default
      write(*,*) "[add_key] Invalid key."
      error stop 1
    end select
  end subroutine

  subroutine add_key_value(self,key,value,stat)
    class(passport), intent(inout) :: self
      !! Passport object.
    character(len=3), intent(in) :: key
      !! Key string (3-letters).
    character(len=*), intent(in) :: value
      !! Trimmed value string.
    integer, intent(out), optional :: stat
      !! Error flag.

    integer :: stat_, lt

    stat_ = 0

    associate(mask => self%value_mask)

    select case(key)
    case('byr') 
      ! Birth year
      read(value,*,iostat=stat_) self%byr
      if (self%byr >= 1920 .and. self%byr <= 2002) mask = ibset(mask,0)
    case('iyr')
      ! Issue year
      read(value,*,iostat=stat_) self%iyr
      if (self%iyr >= 2010 .and. self%iyr <= 2020) mask = ibset(mask,1)
    case('eyr')
      ! Expiration year
      read(value,*,iostat=stat_) self%eyr
      if (self%eyr >= 2020 .and. self%eyr <= 2030) mask = ibset(mask,2)
    case('hgt')
      ! Height
      lt = len(value)
      if (lt >= 4) then
        self%hgt_unit = value(lt-1:lt)
        read(value(1:lt-2),*,iostat=stat_) self%hgt
        select case(self%hgt_unit)
        case('cm')
          if (self%hgt >= 150 .and. self%hgt <= 193) mask = ibset(mask,3)
        case('in')
          if (self%hgt >= 59 .and. self%hgt <= 76) mask = ibset(mask,3)
        end select
      end if
    case('hcl')
      ! Hair color
      if (len(value) ==  7) then
        self%hcl = value(1:7)
        if (self%hcl(1:1) == '#') then 
          if (verify(self%hcl(2:),digits//lowercase) == 0) mask = ibset(mask,4)
        end if
      end if
    case('ecl') 
      ! Eye color
      if (len(value) == 3) then
        self%ecl = value(1:3)
        select case(self%ecl)
        case('amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth')
          mask = ibset(mask,5)
        end select
      end if
    case('pid')
      ! Passport ID
      if (len(value) == 9) then
        self%pid = value(1:9)
        if (len_trim(self%pid) == 9 .and. &
            verify(self%pid,digits) == 0) mask = ibset(mask,6)
      end if
    case('cid') 
      ! Country ID
      read(value,*,iostat=stat_) self%cid
    case default
      write(*,*) "[add_key_value] Invalid key."
      error stop 1
    end select

    end associate

    if (present(stat)) stat = stat_
  end subroutine

  subroutine clear(self)
    class(passport), intent(out) :: self
    ! reset self%key_mask by specifying intent(out)
  end subroutine

  logical function keys_present(self)
    class(passport), intent(in) :: self
    keys_present = iand(self%key_mask,valid_mask) >= valid_mask
  end function

  logical function keys_present_and_valid(self) result(valid)
    class(passport), intent(in) :: self
    if (keys_present(self)) then
      valid = iand(self%value_mask,valid_mask) >= valid_mask
    else
      valid = .false.
    end if
  end function

  integer function count_passports(file,validate) result(nvalid)
    character(len=*), intent(in) :: file
    logical, intent(in), optional :: validate

    integer :: unit, stat, i, npass
    character(len=1000) :: buffer
    character(len=3) :: key
    character(len=30) :: value
    character(:), allocatable :: tokens(:)
    logical :: validate_, eof

    type(passport) :: pp

    validate_ = .false.
    if (present(validate)) validate_ = validate

    open(newunit=unit,file=file,action="read",status="old")

    npass = 0
    nvalid = 0
    eof = .false.

    do 
      ! Load buffer
      read(unit,'(A)',iostat=stat) buffer
      if (stat < 0) eof = .true.
      if (stat > 0) then
        ! Error while reading
        write(*,'(A,I0,A)') "Error encountered in unit ", unit, " associated with file "//trim(file)
        write(*,'(A,I0)') "Error code: ", stat
        error stop 1
      end if

      if (len_trim(buffer) > 0) then

        call split(trim(buffer),' ',tokens)

        do i = 1, size(tokens)
          key = tokens(i)(1:3)
          value = trim(tokens(i)(5:))

          call pp%add_key(key)
          if (validate_) then
            call pp%add_key_value(key,trim(value),stat)
            if (stat /= 0) then
              error stop "[count_passports] Error inserting value."
            end if
          end if

        end do
      end if

      if (len_trim(buffer) == 0 .or. eof) then

        npass = npass + 1
        
        if (validate_) then
          if (pp%keys_present_and_valid()) nvalid = nvalid + 1
        else
          if (pp%keys_present()) nvalid = nvalid + 1
        end if

        call pp%clear()
      end if

      if (eof) exit
    end do

    close(unit)

  end function

  subroutine print_passport(self,unit)
    class(passport), intent(in) :: self
    integer, intent(in) :: unit

    if (btest(self%key_mask,0)) write(unit,'(A,I0)') 'byr:',self%byr
    if (btest(self%key_mask,1)) write(unit,'(A,I0)') 'iyr:',self%iyr 
    if (btest(self%key_mask,2)) write(unit,'(A,I0)') 'eyr:',self%eyr
    if (btest(self%key_mask,3)) write(unit,'(A,I0,A)') 'hgt:',self%hgt,self%hgt_unit
    if (btest(self%key_mask,4)) write(unit,'(A)') 'hcl:'//self%hcl
    if (btest(self%key_mask,5)) write(unit,'(A)') 'ecl:'//self%ecl
    if (btest(self%key_mask,6)) write(unit,'(A)') 'pid:'//self%pid
    if (btest(self%key_mask,7)) write(unit,'(A,I3)') 'cid:',self%cid

  end subroutine

end module

program main

    use day4
    implicit none

    integer :: n 

    ! Part 1
    n = count_passports("input",validate=.false.)
    write(*,'(A,I0)') "# of valid passports: ", n

    ! Part 2
    n = count_passports("input",validate=.true.)
    write(*,'(A,I0)') "# of valid passports: ", n

end program