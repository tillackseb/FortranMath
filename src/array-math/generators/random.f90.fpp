#: include 'fypp-macros.fypp'
$: FYPP_HEADER()
!> Generators for arbitrary rank random real matrices.
module m_random_templates
  use, intrinsic :: iso_fortran_env, only : ${', '.join(sorted(set([t['kind'] for t in ALL_NUMERICAL_TYPES])))}$
  implicit none
  private

  #: for t in ALL_NUMERICAL_TYPES
  public :: random_${t['name']}$
  #: endfor
  
contains

  #: for t in ALL_NUMERICAL_TYPES
  !> [[m_random(module):fill_random(interface)]] for arrays of type `${t['type']}$(kind=${t['kind']}$)`.
  ! NOTE: This cannot be pure, because intrinsic `random_number` is not pure.
  subroutine random_${t['name']}$( A, shapeA, interval, sub_shape, sub_first )
    use math_constants, only : zero => ZERO_${t['name'].upper()}$, one => ONE_${t['name'].upper()}$
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    ${t['type']}$(kind=${t['kind']}$), intent(inout) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> interval to generate random numbers from  
    !> default: [0, 1]
    ${t['type']}$(kind=${t['kind']}$), optional, intent(in) :: interval(2)
    !> shape of subarray to fill with random  
    !> default: `shapeA`
    integer, optional, intent(in) :: sub_shape(size(shapeA))
    !> indices of element in array at which first element of random matrix should be placed   
    !> default: first element of array
    integer, optional, intent(in) :: sub_first(size(shapeA))

    ${t['type']}$(kind=${t['kind']}$) :: intv(2)
    integer, allocatable :: shapeR(:), subidx(:)

    allocate( shapeR, source=shapeA )
    if (present(sub_shape)) shapeR = sub_shape

    intv = [zero, one]
    #{if t['type'] == 'complex'}#intv(2) = cmplx( aimag(intv(2)), real(intv(2)), kind=${t['kind']}$ )#{endif}#
    if (present(interval)) intv = interval

    subidx = subarray_in_flattened( shapeA, shapeR, offset=sub_first )
    if (size( subidx ) > 0) then
      call fill_random_${t['name']}$( A, subidx, intv )
    end if

    deallocate( shapeR, subidx )
  end subroutine random_${t['name']}$

  #: endfor

  subroutine fill_random_integer32( A, subidx, interval )
    integer(kind=int32), intent(inout) :: A(*)
    integer, intent(in) :: subidx(:)
    integer(kind=kind(A)), intent(in) :: interval(2)

    integer :: n
    real(kind=real32), allocatable :: rand(:)

    n = size( subidx )
    allocate( rand(n) )
    call random_number( rand )
    A(subidx) = interval(1) + floor( (interval(2)-interval(1)+1) * rand, kind=kind(A) )
    deallocate( rand )
  end subroutine fill_random_integer32

  subroutine fill_random_integer64( A, subidx, interval )
    integer(kind=int64), intent(inout) :: A(*)
    integer, intent(in) :: subidx(:)
    integer(kind=kind(A)), intent(in) :: interval(2)

    integer :: n
    real(kind=real64), allocatable :: rand(:)

    n = size( subidx )
    allocate( rand(n) )
    call random_number( rand )
    A(subidx) = interval(1) + floor( (interval(2)-interval(1)+1) * rand, kind=kind(A) )
    deallocate( rand )
  end subroutine fill_random_integer64

  subroutine fill_random_real32( A, subidx, interval )
    real(kind=int32), intent(inout) :: A(*)
    integer, intent(in) :: subidx(:)
    real(kind=kind(A)), intent(in) :: interval(2)

    call random_number( A(subidx) )
    A(subidx) = interval(1) + (interval(2)-interval(1)) * A(subidx)
  end subroutine fill_random_real32

  subroutine fill_random_real64( A, subidx, interval )
    real(kind=int64), intent(inout) :: A(*)
    integer, intent(in) :: subidx(:)
    real(kind=kind(A)), intent(in) :: interval(2)

    call random_number( A(subidx) )
    A(subidx) = interval(1) + (interval(2)-interval(1)) * A(subidx)
  end subroutine fill_random_real64

  subroutine fill_random_complex32( A, subidx, interval )
    complex(kind=int32), intent(inout) :: A(*)
    integer, intent(in) :: subidx(:)
    complex(kind=kind(A)), intent(in) :: interval(2)

    integer :: n
    real(kind=kind(A)), allocatable :: rand(:)

    n = size( subidx )
    allocate( rand(2*n) )
    call random_number( rand )
    A(subidx) = interval(1) + cmplx( &
      ( real(interval(2)) -  real(interval(1))) * rand(:n), &
      (aimag(interval(2)) - aimag(interval(1))) * rand(n+1:), kind=kind(A) )
    deallocate( rand )
  end subroutine fill_random_complex32

  subroutine fill_random_complex64( A, subidx, interval )
    complex(kind=int64), intent(inout) :: A(*)
    integer, intent(in) :: subidx(:)
    complex(kind=kind(A)), intent(in) :: interval(2)

    integer :: n
    real(kind=kind(A)), allocatable :: rand(:)

    n = size( subidx )
    allocate( rand(2*n) )
    call random_number( rand )
    A(subidx) = interval(1) + cmplx( &
      ( real(interval(2)) -  real(interval(1))) * rand(:n), &
      (aimag(interval(2)) - aimag(interval(1))) * rand(n+1:), kind=kind(A) )
    deallocate( rand )
  end subroutine fill_random_complex64

end module m_random_templates
