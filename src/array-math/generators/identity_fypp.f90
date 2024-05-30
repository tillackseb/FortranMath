!FYPP HEADER====================================================================
!
! INFORMATION
! This file has been created by the fypp preprocessor. DO NOT MODIFY THIS FILE!
! Implement changes in the original preprocessed file.
! original file ::       src/array-math/generators/identity.f90.fpp
! preprocessing time ::  2024-05-30 14:51:19 UTC+0200
!
!END FYPP HEADER================================================================
!> Generators for identity matrix.
module m_identity_templates
  use, intrinsic :: iso_fortran_env, only : int32, int64, real32, real64, real32, real64
  use math_constants, only : &
    ZERO_INTEGER32, ZERO_INTEGER64, ZERO_REAL32, ZERO_REAL64, ZERO_COMPLEX32, ZERO_COMPLEX64, &
    ONE_INTEGER32, ONE_INTEGER64, ONE_REAL32, ONE_REAL64, ONE_COMPLEX32, ONE_COMPLEX64
  implicit none
  private

  public :: identity_integer32
  public :: identity_integer64
  public :: identity_real32
  public :: identity_real64
  public :: identity_complex32
  public :: identity_complex64
  
contains

  !> [[m_identity(module):to_identity(interface)]] for arrays of type `integer(kind=int32)`.
  pure subroutine identity_integer32( A, shapeA, fill_shape, fill_at )
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    integer(kind=int32), intent(inout) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> shape of subarray to fill with identity (default: `shapeA`)
    integer, optional, intent(in) :: fill_shape(size(shapeA))
    !> indices of element in array at which first element of identity should be placed (default: first element of array)
    integer, optional, intent(in) :: fill_at(size(shapeA))

    integer, allocatable :: shapeI(:), subidx(:)

    allocate( shapeI, source=shapeA )
    if (present(fill_shape)) shapeI = fill_shape

    subidx = subarray_in_flattened( shapeA, shapeI, offset=fill_at )
    if (allocated(subidx)) then
      A(subidx) = ZERO_INTEGER32
      subidx = diagonal_in_flattened( shapeA, shapeI, offset=fill_at )
      A(subidx) = ONE_INTEGER32
    end if

    if (allocated(shapeI)) deallocate( shapeI )
    if (allocated(subidx)) deallocate( subidx )
  end subroutine identity_integer32

  !> [[m_identity(module):to_identity(interface)]] for arrays of type `integer(kind=int64)`.
  pure subroutine identity_integer64( A, shapeA, fill_shape, fill_at )
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    integer(kind=int64), intent(inout) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> shape of subarray to fill with identity (default: `shapeA`)
    integer, optional, intent(in) :: fill_shape(size(shapeA))
    !> indices of element in array at which first element of identity should be placed (default: first element of array)
    integer, optional, intent(in) :: fill_at(size(shapeA))

    integer, allocatable :: shapeI(:), subidx(:)

    allocate( shapeI, source=shapeA )
    if (present(fill_shape)) shapeI = fill_shape

    subidx = subarray_in_flattened( shapeA, shapeI, offset=fill_at )
    if (allocated(subidx)) then
      A(subidx) = ZERO_INTEGER64
      subidx = diagonal_in_flattened( shapeA, shapeI, offset=fill_at )
      A(subidx) = ONE_INTEGER64
    end if

    if (allocated(shapeI)) deallocate( shapeI )
    if (allocated(subidx)) deallocate( subidx )
  end subroutine identity_integer64

  !> [[m_identity(module):to_identity(interface)]] for arrays of type `real(kind=real32)`.
  pure subroutine identity_real32( A, shapeA, fill_shape, fill_at )
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    real(kind=real32), intent(inout) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> shape of subarray to fill with identity (default: `shapeA`)
    integer, optional, intent(in) :: fill_shape(size(shapeA))
    !> indices of element in array at which first element of identity should be placed (default: first element of array)
    integer, optional, intent(in) :: fill_at(size(shapeA))

    integer, allocatable :: shapeI(:), subidx(:)

    allocate( shapeI, source=shapeA )
    if (present(fill_shape)) shapeI = fill_shape

    subidx = subarray_in_flattened( shapeA, shapeI, offset=fill_at )
    if (allocated(subidx)) then
      A(subidx) = ZERO_REAL32
      subidx = diagonal_in_flattened( shapeA, shapeI, offset=fill_at )
      A(subidx) = ONE_REAL32
    end if

    if (allocated(shapeI)) deallocate( shapeI )
    if (allocated(subidx)) deallocate( subidx )
  end subroutine identity_real32

  !> [[m_identity(module):to_identity(interface)]] for arrays of type `real(kind=real64)`.
  pure subroutine identity_real64( A, shapeA, fill_shape, fill_at )
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    real(kind=real64), intent(inout) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> shape of subarray to fill with identity (default: `shapeA`)
    integer, optional, intent(in) :: fill_shape(size(shapeA))
    !> indices of element in array at which first element of identity should be placed (default: first element of array)
    integer, optional, intent(in) :: fill_at(size(shapeA))

    integer, allocatable :: shapeI(:), subidx(:)

    allocate( shapeI, source=shapeA )
    if (present(fill_shape)) shapeI = fill_shape

    subidx = subarray_in_flattened( shapeA, shapeI, offset=fill_at )
    if (allocated(subidx)) then
      A(subidx) = ZERO_REAL64
      subidx = diagonal_in_flattened( shapeA, shapeI, offset=fill_at )
      A(subidx) = ONE_REAL64
    end if

    if (allocated(shapeI)) deallocate( shapeI )
    if (allocated(subidx)) deallocate( subidx )
  end subroutine identity_real64

  !> [[m_identity(module):to_identity(interface)]] for arrays of type `complex(kind=real32)`.
  pure subroutine identity_complex32( A, shapeA, fill_shape, fill_at )
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    complex(kind=real32), intent(inout) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> shape of subarray to fill with identity (default: `shapeA`)
    integer, optional, intent(in) :: fill_shape(size(shapeA))
    !> indices of element in array at which first element of identity should be placed (default: first element of array)
    integer, optional, intent(in) :: fill_at(size(shapeA))

    integer, allocatable :: shapeI(:), subidx(:)

    allocate( shapeI, source=shapeA )
    if (present(fill_shape)) shapeI = fill_shape

    subidx = subarray_in_flattened( shapeA, shapeI, offset=fill_at )
    if (allocated(subidx)) then
      A(subidx) = ZERO_COMPLEX32
      subidx = diagonal_in_flattened( shapeA, shapeI, offset=fill_at )
      A(subidx) = ONE_COMPLEX32
    end if

    if (allocated(shapeI)) deallocate( shapeI )
    if (allocated(subidx)) deallocate( subidx )
  end subroutine identity_complex32

  !> [[m_identity(module):to_identity(interface)]] for arrays of type `complex(kind=real64)`.
  pure subroutine identity_complex64( A, shapeA, fill_shape, fill_at )
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    complex(kind=real64), intent(inout) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> shape of subarray to fill with identity (default: `shapeA`)
    integer, optional, intent(in) :: fill_shape(size(shapeA))
    !> indices of element in array at which first element of identity should be placed (default: first element of array)
    integer, optional, intent(in) :: fill_at(size(shapeA))

    integer, allocatable :: shapeI(:), subidx(:)

    allocate( shapeI, source=shapeA )
    if (present(fill_shape)) shapeI = fill_shape

    subidx = subarray_in_flattened( shapeA, shapeI, offset=fill_at )
    if (allocated(subidx)) then
      A(subidx) = ZERO_COMPLEX64
      subidx = diagonal_in_flattened( shapeA, shapeI, offset=fill_at )
      A(subidx) = ONE_COMPLEX64
    end if

    if (allocated(shapeI)) deallocate( shapeI )
    if (allocated(subidx)) deallocate( subidx )
  end subroutine identity_complex64


end module m_identity_templates
