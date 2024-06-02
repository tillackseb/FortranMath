!FYPP HEADER====================================================================
!
! INFORMATION
! This file has been created by the fypp preprocessor. DO NOT MODIFY THIS FILE!
! Implement changes in the original preprocessed file.
! original file ::       src/array-math/generators/identity.f90.fpp
! preprocessing time ::  2024-05-31 10:35:02 UTC+0200
!
!END FYPP HEADER================================================================
!> Generators for identity matrix.
module m_identity_templates
  use, intrinsic :: iso_fortran_env, only : int32, int64, real32, real64
  implicit none
  private

  public :: identity_integer32
  public :: identity_integer64
  public :: identity_real32
  public :: identity_real64
  public :: identity_complex32
  public :: identity_complex64
  
contains

  !> [[m_identity(module):fill_identity(interface)]] for arrays of type `integer(kind=int32)`.
  pure subroutine identity_integer32( A, shapeA, sub_shape, sub_first )
    use math_constants, only : zero => ZERO_INTEGER32, one => ONE_INTEGER32
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    integer(kind=int32), intent(inout) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> shape of subarray to fill with identity  
    !> default: `shapeA`
    integer, optional, intent(in) :: sub_shape(size(shapeA))
    !> indices of element in array at which first element of identity should be placed   
    !> default: first element of array
    integer, optional, intent(in) :: sub_first(size(shapeA))

    integer, allocatable :: shapeI(:), subidx(:)

    allocate( shapeI, source=shapeA )
    if (present(sub_shape)) shapeI = sub_shape

    subidx = subarray_in_flattened( shapeA, shapeI, offset=sub_first )
    if (size( subidx ) > 0) then
      A(subidx) = zero
      subidx = diagonal_in_flattened( shapeA, shapeI, offset=sub_first )
      A(subidx) = one
    end if

    deallocate( shapeI, subidx )
  end subroutine identity_integer32

  !> [[m_identity(module):fill_identity(interface)]] for arrays of type `integer(kind=int64)`.
  pure subroutine identity_integer64( A, shapeA, sub_shape, sub_first )
    use math_constants, only : zero => ZERO_INTEGER64, one => ONE_INTEGER64
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    integer(kind=int64), intent(inout) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> shape of subarray to fill with identity  
    !> default: `shapeA`
    integer, optional, intent(in) :: sub_shape(size(shapeA))
    !> indices of element in array at which first element of identity should be placed   
    !> default: first element of array
    integer, optional, intent(in) :: sub_first(size(shapeA))

    integer, allocatable :: shapeI(:), subidx(:)

    allocate( shapeI, source=shapeA )
    if (present(sub_shape)) shapeI = sub_shape

    subidx = subarray_in_flattened( shapeA, shapeI, offset=sub_first )
    if (size( subidx ) > 0) then
      A(subidx) = zero
      subidx = diagonal_in_flattened( shapeA, shapeI, offset=sub_first )
      A(subidx) = one
    end if

    deallocate( shapeI, subidx )
  end subroutine identity_integer64

  !> [[m_identity(module):fill_identity(interface)]] for arrays of type `real(kind=real32)`.
  pure subroutine identity_real32( A, shapeA, sub_shape, sub_first )
    use math_constants, only : zero => ZERO_REAL32, one => ONE_REAL32
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    real(kind=real32), intent(inout) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> shape of subarray to fill with identity  
    !> default: `shapeA`
    integer, optional, intent(in) :: sub_shape(size(shapeA))
    !> indices of element in array at which first element of identity should be placed   
    !> default: first element of array
    integer, optional, intent(in) :: sub_first(size(shapeA))

    integer, allocatable :: shapeI(:), subidx(:)

    allocate( shapeI, source=shapeA )
    if (present(sub_shape)) shapeI = sub_shape

    subidx = subarray_in_flattened( shapeA, shapeI, offset=sub_first )
    if (size( subidx ) > 0) then
      A(subidx) = zero
      subidx = diagonal_in_flattened( shapeA, shapeI, offset=sub_first )
      A(subidx) = one
    end if

    deallocate( shapeI, subidx )
  end subroutine identity_real32

  !> [[m_identity(module):fill_identity(interface)]] for arrays of type `real(kind=real64)`.
  pure subroutine identity_real64( A, shapeA, sub_shape, sub_first )
    use math_constants, only : zero => ZERO_REAL64, one => ONE_REAL64
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    real(kind=real64), intent(inout) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> shape of subarray to fill with identity  
    !> default: `shapeA`
    integer, optional, intent(in) :: sub_shape(size(shapeA))
    !> indices of element in array at which first element of identity should be placed   
    !> default: first element of array
    integer, optional, intent(in) :: sub_first(size(shapeA))

    integer, allocatable :: shapeI(:), subidx(:)

    allocate( shapeI, source=shapeA )
    if (present(sub_shape)) shapeI = sub_shape

    subidx = subarray_in_flattened( shapeA, shapeI, offset=sub_first )
    if (size( subidx ) > 0) then
      A(subidx) = zero
      subidx = diagonal_in_flattened( shapeA, shapeI, offset=sub_first )
      A(subidx) = one
    end if

    deallocate( shapeI, subidx )
  end subroutine identity_real64

  !> [[m_identity(module):fill_identity(interface)]] for arrays of type `complex(kind=real32)`.
  pure subroutine identity_complex32( A, shapeA, sub_shape, sub_first )
    use math_constants, only : zero => ZERO_COMPLEX32, one => ONE_COMPLEX32
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    complex(kind=real32), intent(inout) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> shape of subarray to fill with identity  
    !> default: `shapeA`
    integer, optional, intent(in) :: sub_shape(size(shapeA))
    !> indices of element in array at which first element of identity should be placed   
    !> default: first element of array
    integer, optional, intent(in) :: sub_first(size(shapeA))

    integer, allocatable :: shapeI(:), subidx(:)

    allocate( shapeI, source=shapeA )
    if (present(sub_shape)) shapeI = sub_shape

    subidx = subarray_in_flattened( shapeA, shapeI, offset=sub_first )
    if (size( subidx ) > 0) then
      A(subidx) = zero
      subidx = diagonal_in_flattened( shapeA, shapeI, offset=sub_first )
      A(subidx) = one
    end if

    deallocate( shapeI, subidx )
  end subroutine identity_complex32

  !> [[m_identity(module):fill_identity(interface)]] for arrays of type `complex(kind=real64)`.
  pure subroutine identity_complex64( A, shapeA, sub_shape, sub_first )
    use math_constants, only : zero => ZERO_COMPLEX64, one => ONE_COMPLEX64
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    complex(kind=real64), intent(inout) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> shape of subarray to fill with identity  
    !> default: `shapeA`
    integer, optional, intent(in) :: sub_shape(size(shapeA))
    !> indices of element in array at which first element of identity should be placed   
    !> default: first element of array
    integer, optional, intent(in) :: sub_first(size(shapeA))

    integer, allocatable :: shapeI(:), subidx(:)

    allocate( shapeI, source=shapeA )
    if (present(sub_shape)) shapeI = sub_shape

    subidx = subarray_in_flattened( shapeA, shapeI, offset=sub_first )
    if (size( subidx ) > 0) then
      A(subidx) = zero
      subidx = diagonal_in_flattened( shapeA, shapeI, offset=sub_first )
      A(subidx) = one
    end if

    deallocate( shapeI, subidx )
  end subroutine identity_complex64


end module m_identity_templates
