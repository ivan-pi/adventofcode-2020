module m
  use, intrinsic :: iso_c_binding
  implicit none
  private

  abstract interface
    function compar_iface(a, b) bind(c)
      import c_int, c_ptr
      implicit none
      integer(c_int) compar_iface
      type(c_ptr), value :: a, b
    end function
  end interface

  interface
    subroutine qsort(base, nel, width, compar) bind(c, name='qsort')
      import c_size_t, c_int, compar_iface
      type(*), intent(inout) :: base(*)
      integer(c_size_t), value :: nel
      integer(c_size_t), value :: width
      procedure(compar_iface) :: compar
    end subroutine
  end interface

  interface my_qsort
    module procedure my_qsort_int4
    module procedure my_qsort_int8
    module procedure my_qsort_real4
    module procedure my_qsort_real8
  end interface
  public my_qsort
contains

  subroutine my_qsort_int4(a, nel)
    integer(c_int), intent(inout) :: a(*)
    integer(4), value :: nel

    call qsort(a, int(nel, c_size_t), c_sizeof(a(1)), less_int4)
  end subroutine

  subroutine my_qsort_int8(a, nel)
    integer(c_long_long), intent(inout) :: a(*)
    integer(4), value :: nel
    call qsort(a, int(nel, c_size_t), c_sizeof(a(1)), less_int8)
  end subroutine

  subroutine my_qsort_real4(a, nel)
    real(c_float), intent(inout) :: a(*)
    integer(4), value :: nel
    call qsort(a, int(nel, c_size_t), c_sizeof(a(1)), less_real4)
  end subroutine

  subroutine my_qsort_real8(a, nel)
    real(c_double), intent(inout) :: a(*)
    integer(4), value :: nel
    call qsort(a, int(nel, c_size_t), c_sizeof(a(1)), less_real8)
  end subroutine

  function less_int4(a, b) result(result)
    integer(c_int) result
    type(c_ptr), value :: a, b
    integer(c_int), pointer :: ap, bp
    call c_f_pointer(a, ap)
    call c_f_pointer(b, bp)
    result = int(ap - bp, c_int)
  end function

  function less_int8(a, b) result(result)
    integer(c_int) result
    type(c_ptr), value :: a, b
    integer(c_long_long), pointer :: ap, bp
    call c_f_pointer(a, ap)
    call c_f_pointer(b, bp)
    result = int(ap - bp, c_int)
  end function

  function less_real4(a, b) result(result)
    integer(c_int) result
    type(c_ptr), value :: a, b
    real(c_float), pointer :: ap, bp
    call c_f_pointer(a, ap)
    call c_f_pointer(b, bp)
    result = int(ap - bp, c_int)
  end function

  function less_real8(a, b) result(result)
    integer(c_int) result
    type(c_ptr), value :: a, b
    real(c_double), pointer :: ap, bp
    call c_f_pointer(a, ap)
    call c_f_pointer(b, bp)
    result = int(ap - bp, c_int)
  end function
end module