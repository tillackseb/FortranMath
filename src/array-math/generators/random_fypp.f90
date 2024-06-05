!FYPP HEADER====================================================================
!
! INFORMATION
! This file has been created by the fypp preprocessor. DO NOT MODIFY THIS FILE!
! Implement changes in the original preprocessed file.
! original file ::       src/array-math/generators/random.f90.fpp
! preprocessing time ::  2024-06-04 23:20:10 UTC+0200
!
!END FYPP HEADER================================================================
!> Generators for arbitrary rank random real matrices.
module m_random_templates
  use, intrinsic :: iso_fortran_env, only : int32, int64, real32, real64
  implicit none
  private

  public :: random_integer32
  public :: random_integer64
  public :: random_real32
  public :: random_real64
  public :: random_complex32
  public :: random_complex64
  
contains

  !> [[m_random(module):fill_random(interface)]] for arrays of type `integer(kind=int32)`.
  ! NOTE: This cannot be pure, because intrinsic `random_number` is not pure.
  subroutine random_integer32( A, shapeA, interval, sub_shape, sub_first )
    use math_constants, only : zero => ZERO_INTEGER32, one => ONE_INTEGER32
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    integer(kind=int32), intent(inout) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> interval to generate random numbers from  
    !> default: [0, 1]
    integer(kind=int32), optional, intent(in) :: interval(2)
    !> shape of subarray to fill with random  
    !> default: `shapeA`
    integer, optional, intent(in) :: sub_shape(size(shapeA))
    !> indices of element in array at which first element of random matrix should be placed   
    !> default: first element of array
    integer, optional, intent(in) :: sub_first(size(shapeA))

    integer(kind=int32) :: intv(2)
    integer, allocatable :: shapeR(:), subidx(:)

    allocate( shapeR, source=shapeA )
    if (present(sub_shape)) shapeR = sub_shape

    intv = [zero, one]
    
    if (present(interval)) intv = interval

    subidx = subarray_in_flattened( shapeA, shapeR, offset=sub_first )
    if (size( subidx ) > 0) then
      call fill_random_integer32( A, subidx, intv )
    end if

    deallocate( shapeR, subidx )
  end subroutine random_integer32

  !> [[m_random(module):fill_random(interface)]] for arrays of type `integer(kind=int64)`.
  ! NOTE: This cannot be pure, because intrinsic `random_number` is not pure.
  subroutine random_integer64( A, shapeA, interval, sub_shape, sub_first )
    use math_constants, only : zero => ZERO_INTEGER64, one => ONE_INTEGER64
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    integer(kind=int64), intent(inout) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> interval to generate random numbers from  
    !> default: [0, 1]
    integer(kind=int64), optional, intent(in) :: interval(2)
    !> shape of subarray to fill with random  
    !> default: `shapeA`
    integer, optional, intent(in) :: sub_shape(size(shapeA))
    !> indices of element in array at which first element of random matrix should be placed   
    !> default: first element of array
    integer, optional, intent(in) :: sub_first(size(shapeA))

    integer(kind=int64) :: intv(2)
    integer, allocatable :: shapeR(:), subidx(:)

    allocate( shapeR, source=shapeA )
    if (present(sub_shape)) shapeR = sub_shape

    intv = [zero, one]
    
    if (present(interval)) intv = interval

    subidx = subarray_in_flattened( shapeA, shapeR, offset=sub_first )
    if (size( subidx ) > 0) then
      call fill_random_integer64( A, subidx, intv )
    end if

    deallocate( shapeR, subidx )
  end subroutine random_integer64

  !> [[m_random(module):fill_random(interface)]] for arrays of type `real(kind=real32)`.
  ! NOTE: This cannot be pure, because intrinsic `random_number` is not pure.
  subroutine random_real32( A, shapeA, interval, sub_shape, sub_first )
    use math_constants, only : zero => ZERO_REAL32, one => ONE_REAL32
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    real(kind=real32), intent(inout) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> interval to generate random numbers from  
    !> default: [0, 1]
    real(kind=real32), optional, intent(in) :: interval(2)
    !> shape of subarray to fill with random  
    !> default: `shapeA`
    integer, optional, intent(in) :: sub_shape(size(shapeA))
    !> indices of element in array at which first element of random matrix should be placed   
    !> default: first element of array
    integer, optional, intent(in) :: sub_first(size(shapeA))

    real(kind=real32) :: intv(2)
    integer, allocatable :: shapeR(:), subidx(:)

    allocate( shapeR, source=shapeA )
    if (present(sub_shape)) shapeR = sub_shape

    intv = [zero, one]
    
    if (present(interval)) intv = interval

    subidx = subarray_in_flattened( shapeA, shapeR, offset=sub_first )
    if (size( subidx ) > 0) then
      call fill_random_real32( A, subidx, intv )
    end if

    deallocate( shapeR, subidx )
  end subroutine random_real32

  !> [[m_random(module):fill_random(interface)]] for arrays of type `real(kind=real64)`.
  ! NOTE: This cannot be pure, because intrinsic `random_number` is not pure.
  subroutine random_real64( A, shapeA, interval, sub_shape, sub_first )
    use math_constants, only : zero => ZERO_REAL64, one => ONE_REAL64
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    real(kind=real64), intent(inout) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> interval to generate random numbers from  
    !> default: [0, 1]
    real(kind=real64), optional, intent(in) :: interval(2)
    !> shape of subarray to fill with random  
    !> default: `shapeA`
    integer, optional, intent(in) :: sub_shape(size(shapeA))
    !> indices of element in array at which first element of random matrix should be placed   
    !> default: first element of array
    integer, optional, intent(in) :: sub_first(size(shapeA))

    real(kind=real64) :: intv(2)
    integer, allocatable :: shapeR(:), subidx(:)

    allocate( shapeR, source=shapeA )
    if (present(sub_shape)) shapeR = sub_shape

    intv = [zero, one]
    
    if (present(interval)) intv = interval

    subidx = subarray_in_flattened( shapeA, shapeR, offset=sub_first )
    if (size( subidx ) > 0) then
      call fill_random_real64( A, subidx, intv )
    end if

    deallocate( shapeR, subidx )
  end subroutine random_real64

  !> [[m_random(module):fill_random(interface)]] for arrays of type `complex(kind=real32)`.
  ! NOTE: This cannot be pure, because intrinsic `random_number` is not pure.
  subroutine random_complex32( A, shapeA, interval, sub_shape, sub_first )
    use math_constants, only : zero => ZERO_COMPLEX32, one => ONE_COMPLEX32
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    complex(kind=real32), intent(inout) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> interval to generate random numbers from  
    !> default: [0, 1]
    complex(kind=real32), optional, intent(in) :: interval(2)
    !> shape of subarray to fill with random  
    !> default: `shapeA`
    integer, optional, intent(in) :: sub_shape(size(shapeA))
    !> indices of element in array at which first element of random matrix should be placed   
    !> default: first element of array
    integer, optional, intent(in) :: sub_first(size(shapeA))

    complex(kind=real32) :: intv(2)
    integer, allocatable :: shapeR(:), subidx(:)

    allocate( shapeR, source=shapeA )
    if (present(sub_shape)) shapeR = sub_shape

    intv = [zero, one]
    intv(2) = cmplx( aimag(intv(2)), real(intv(2)), kind=real32 )
    if (present(interval)) intv = interval

    subidx = subarray_in_flattened( shapeA, shapeR, offset=sub_first )
    if (size( subidx ) > 0) then
      call fill_random_complex32( A, subidx, intv )
    end if

    deallocate( shapeR, subidx )
  end subroutine random_complex32

  !> [[m_random(module):fill_random(interface)]] for arrays of type `complex(kind=real64)`.
  ! NOTE: This cannot be pure, because intrinsic `random_number` is not pure.
  subroutine random_complex64( A, shapeA, interval, sub_shape, sub_first )
    use math_constants, only : zero => ZERO_COMPLEX64, one => ONE_COMPLEX64
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    complex(kind=real64), intent(inout) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> interval to generate random numbers from  
    !> default: [0, 1]
    complex(kind=real64), optional, intent(in) :: interval(2)
    !> shape of subarray to fill with random  
    !> default: `shapeA`
    integer, optional, intent(in) :: sub_shape(size(shapeA))
    !> indices of element in array at which first element of random matrix should be placed   
    !> default: first element of array
    integer, optional, intent(in) :: sub_first(size(shapeA))

    complex(kind=real64) :: intv(2)
    integer, allocatable :: shapeR(:), subidx(:)

    allocate( shapeR, source=shapeA )
    if (present(sub_shape)) shapeR = sub_shape

    intv = [zero, one]
    intv(2) = cmplx( aimag(intv(2)), real(intv(2)), kind=real64 )
    if (present(interval)) intv = interval

    subidx = subarray_in_flattened( shapeA, shapeR, offset=sub_first )
    if (size( subidx ) > 0) then
      call fill_random_complex64( A, subidx, intv )
    end if

    deallocate( shapeR, subidx )
  end subroutine random_complex64


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
    real(kind=real32), intent(inout) :: A(*)
    integer, intent(in) :: subidx(:)
    real(kind=kind(A)), intent(in) :: interval(2)

    integer :: n
    real(kind=kind(A)), allocatable :: rand(:)

    n = size( subidx )
    allocate( rand(n) )
    call random_number( rand )
    A(subidx) = interval(1) + (interval(2)-interval(1)) * rand
    deallocate( rand )
  end subroutine fill_random_real32

  subroutine fill_random_real64( A, subidx, interval )
    real(kind=real64), intent(inout) :: A(*)
    integer, intent(in) :: subidx(:)
    real(kind=kind(A)), intent(in) :: interval(2)

    integer :: n
    real(kind=kind(A)), allocatable :: rand(:)

    n = size( subidx )
    allocate( rand(n) )
    call random_number( rand )
    A(subidx) = interval(1) + (interval(2)-interval(1)) * rand
    deallocate( rand )
  end subroutine fill_random_real64

  subroutine fill_random_complex32( A, subidx, interval )
    complex(kind=real32), intent(inout) :: A(*)
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
    complex(kind=real64), intent(inout) :: A(*)
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
