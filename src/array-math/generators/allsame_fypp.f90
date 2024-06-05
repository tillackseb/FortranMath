!FYPP HEADER====================================================================
!
! INFORMATION
! This file has been created by the fypp preprocessor. DO NOT MODIFY THIS FILE!
! Implement changes in the original preprocessed file.
! original file ::       src/array-math/generators/allsame.f90.fpp
! preprocessing time ::  2024-06-02 20:50:57 UTC+0200
!
!END FYPP HEADER================================================================
!> Generators for allsame matrix.
module m_allsame_templates
  use, intrinsic :: iso_fortran_env, only : int32, int64, real32, real64
  implicit none
  private

  public :: allsame_integer32
  public :: allsame_integer64
  public :: allsame_real32
  public :: allsame_real64
  public :: allsame_complex32
  public :: allsame_complex64
  
contains

  !> [[m_allsame(module):fill_allsame(interface)]] for arrays of type `integer(kind=int32)`.
  pure subroutine allsame_integer32( A, shapeA, val, sub_shape, sub_first )
    use math_constants, only : zero => ZERO_INTEGER32, one => ONE_INTEGER32
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    integer(kind=int32), intent(inout) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> value to fill array with
    integer(kind=int32), intent(in) :: val
    !> shape of subarray to fill with same value  
    !> default: `shapeA`
    integer, optional, intent(in) :: sub_shape(size(shapeA))
    !> indices of element in array at which first element of allsame should be placed   
    !> default: first element of array
    integer, optional, intent(in) :: sub_first(size(shapeA))

    integer, allocatable :: shapeI(:), subidx(:)

    allocate( shapeI, source=shapeA )
    if (present(sub_shape)) shapeI = sub_shape

    subidx = subarray_in_flattened( shapeA, shapeI, offset=sub_first )
    if (size( subidx ) > 0) then
      A(subidx) = val
    end if

    deallocate( shapeI, subidx )
  end subroutine allsame_integer32

  !> [[m_allsame(module):fill_allsame(interface)]] for arrays of type `integer(kind=int64)`.
  pure subroutine allsame_integer64( A, shapeA, val, sub_shape, sub_first )
    use math_constants, only : zero => ZERO_INTEGER64, one => ONE_INTEGER64
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    integer(kind=int64), intent(inout) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> value to fill array with
    integer(kind=int64), intent(in) :: val
    !> shape of subarray to fill with same value  
    !> default: `shapeA`
    integer, optional, intent(in) :: sub_shape(size(shapeA))
    !> indices of element in array at which first element of allsame should be placed   
    !> default: first element of array
    integer, optional, intent(in) :: sub_first(size(shapeA))

    integer, allocatable :: shapeI(:), subidx(:)

    allocate( shapeI, source=shapeA )
    if (present(sub_shape)) shapeI = sub_shape

    subidx = subarray_in_flattened( shapeA, shapeI, offset=sub_first )
    if (size( subidx ) > 0) then
      A(subidx) = val
    end if

    deallocate( shapeI, subidx )
  end subroutine allsame_integer64

  !> [[m_allsame(module):fill_allsame(interface)]] for arrays of type `real(kind=real32)`.
  pure subroutine allsame_real32( A, shapeA, val, sub_shape, sub_first )
    use math_constants, only : zero => ZERO_REAL32, one => ONE_REAL32
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    real(kind=real32), intent(inout) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> value to fill array with
    real(kind=real32), intent(in) :: val
    !> shape of subarray to fill with same value  
    !> default: `shapeA`
    integer, optional, intent(in) :: sub_shape(size(shapeA))
    !> indices of element in array at which first element of allsame should be placed   
    !> default: first element of array
    integer, optional, intent(in) :: sub_first(size(shapeA))

    integer, allocatable :: shapeI(:), subidx(:)

    allocate( shapeI, source=shapeA )
    if (present(sub_shape)) shapeI = sub_shape

    subidx = subarray_in_flattened( shapeA, shapeI, offset=sub_first )
    if (size( subidx ) > 0) then
      A(subidx) = val
    end if

    deallocate( shapeI, subidx )
  end subroutine allsame_real32

  !> [[m_allsame(module):fill_allsame(interface)]] for arrays of type `real(kind=real64)`.
  pure subroutine allsame_real64( A, shapeA, val, sub_shape, sub_first )
    use math_constants, only : zero => ZERO_REAL64, one => ONE_REAL64
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    real(kind=real64), intent(inout) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> value to fill array with
    real(kind=real64), intent(in) :: val
    !> shape of subarray to fill with same value  
    !> default: `shapeA`
    integer, optional, intent(in) :: sub_shape(size(shapeA))
    !> indices of element in array at which first element of allsame should be placed   
    !> default: first element of array
    integer, optional, intent(in) :: sub_first(size(shapeA))

    integer, allocatable :: shapeI(:), subidx(:)

    allocate( shapeI, source=shapeA )
    if (present(sub_shape)) shapeI = sub_shape

    subidx = subarray_in_flattened( shapeA, shapeI, offset=sub_first )
    if (size( subidx ) > 0) then
      A(subidx) = val
    end if

    deallocate( shapeI, subidx )
  end subroutine allsame_real64

  !> [[m_allsame(module):fill_allsame(interface)]] for arrays of type `complex(kind=real32)`.
  pure subroutine allsame_complex32( A, shapeA, val, sub_shape, sub_first )
    use math_constants, only : zero => ZERO_COMPLEX32, one => ONE_COMPLEX32
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    complex(kind=real32), intent(inout) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> value to fill array with
    complex(kind=real32), intent(in) :: val
    !> shape of subarray to fill with same value  
    !> default: `shapeA`
    integer, optional, intent(in) :: sub_shape(size(shapeA))
    !> indices of element in array at which first element of allsame should be placed   
    !> default: first element of array
    integer, optional, intent(in) :: sub_first(size(shapeA))

    integer, allocatable :: shapeI(:), subidx(:)

    allocate( shapeI, source=shapeA )
    if (present(sub_shape)) shapeI = sub_shape

    subidx = subarray_in_flattened( shapeA, shapeI, offset=sub_first )
    if (size( subidx ) > 0) then
      A(subidx) = val
    end if

    deallocate( shapeI, subidx )
  end subroutine allsame_complex32

  !> [[m_allsame(module):fill_allsame(interface)]] for arrays of type `complex(kind=real64)`.
  pure subroutine allsame_complex64( A, shapeA, val, sub_shape, sub_first )
    use math_constants, only : zero => ZERO_COMPLEX64, one => ONE_COMPLEX64
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    complex(kind=real64), intent(inout) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> value to fill array with
    complex(kind=real64), intent(in) :: val
    !> shape of subarray to fill with same value  
    !> default: `shapeA`
    integer, optional, intent(in) :: sub_shape(size(shapeA))
    !> indices of element in array at which first element of allsame should be placed   
    !> default: first element of array
    integer, optional, intent(in) :: sub_first(size(shapeA))

    integer, allocatable :: shapeI(:), subidx(:)

    allocate( shapeI, source=shapeA )
    if (present(sub_shape)) shapeI = sub_shape

    subidx = subarray_in_flattened( shapeA, shapeI, offset=sub_first )
    if (size( subidx ) > 0) then
      A(subidx) = val
    end if

    deallocate( shapeI, subidx )
  end subroutine allsame_complex64

end module m_allsame_templates
