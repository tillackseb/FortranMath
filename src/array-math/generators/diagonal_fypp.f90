!FYPP HEADER====================================================================
!
! INFORMATION
! This file has been created by the fypp preprocessor. DO NOT MODIFY THIS FILE!
! Implement changes in the original preprocessed file.
! original file ::       src/array-math/generators/diagonal.f90.fpp
! preprocessing time ::  2024-06-04 23:03:47 UTC+0200
!
!END FYPP HEADER================================================================
!> Generators for diagonal matrix.
module m_diagonal_templates
  use, intrinsic :: iso_fortran_env, only : int32, int64, real32, real64
  implicit none
  private

  public :: diagonal_integer32, get_diagonal_integer32
  public :: diagonal_integer64, get_diagonal_integer64
  public :: diagonal_real32, get_diagonal_real32
  public :: diagonal_real64, get_diagonal_real64
  public :: diagonal_complex32, get_diagonal_complex32
  public :: diagonal_complex64, get_diagonal_complex64
  
contains

  !> [[m_diagonal(module):fill_diagonal(interface)]] for arrays of type `integer(kind=int32)`.
  pure subroutine diagonal_integer32( A, shapeA, diag, sub_shape, sub_first )
    use math_constants, only : zero => ZERO_INTEGER32
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    integer(kind=int32), intent(inout) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> values to fill diagonal with
    integer(kind=int32), intent(in) :: diag(*)
    !> shape of subarray to fill with diagonal  
    !> default: `shapeA`
    integer, optional, intent(in) :: sub_shape(size(shapeA))
    !> indices of element in array at which first element of diagonal should be placed   
    !> default: first element of array
    integer, optional, intent(in) :: sub_first(size(shapeA))

    integer :: n
    integer, allocatable :: shapeI(:), subidx(:)

    allocate( shapeI, source=shapeA )
    if (present(sub_shape)) shapeI = sub_shape

    subidx = subarray_in_flattened( shapeA, shapeI, offset=sub_first )
    if (size( subidx ) > 0) then
      A(subidx) = zero
      subidx = diagonal_in_flattened( shapeA, shapeI, offset=sub_first )
      n = size( subidx )
      A(subidx) = diag(:n)
    end if

    deallocate( shapeI, subidx )
  end subroutine diagonal_integer32

  !> [[m_diagonal(module):get_diagonal(interface)]] for arrays of type `integer(kind=int32)`.
  pure subroutine get_diagonal_integer32( A, shapeA, diag, sub_shape, sub_first )
    use math_constants, only : zero => ZERO_INTEGER32
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    integer(kind=int32), intent(in) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> array to write diagonal values to
    integer(kind=int32), intent(out) :: diag(*)
    !> shape of subarray to fill with diagonal  
    !> default: `shapeA`
    integer, optional, intent(in) :: sub_shape(size(shapeA))
    !> indices of element in array at which first element of diagonal should be placed   
    !> default: first element of array
    integer, optional, intent(in) :: sub_first(size(shapeA))

    integer :: n
    integer, allocatable :: shapeI(:), subidx(:)

    allocate( shapeI, source=shapeA )
    if (present(sub_shape)) shapeI = sub_shape

    subidx = diagonal_in_flattened( shapeA, shapeI, offset=sub_first )
    n = size( subidx )
    if (n > 0) then
      diag(:n) = A(subidx)
    end if

    deallocate( shapeI, subidx )
  end subroutine get_diagonal_integer32
  
  !> [[m_diagonal(module):fill_diagonal(interface)]] for arrays of type `integer(kind=int64)`.
  pure subroutine diagonal_integer64( A, shapeA, diag, sub_shape, sub_first )
    use math_constants, only : zero => ZERO_INTEGER64
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    integer(kind=int64), intent(inout) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> values to fill diagonal with
    integer(kind=int64), intent(in) :: diag(*)
    !> shape of subarray to fill with diagonal  
    !> default: `shapeA`
    integer, optional, intent(in) :: sub_shape(size(shapeA))
    !> indices of element in array at which first element of diagonal should be placed   
    !> default: first element of array
    integer, optional, intent(in) :: sub_first(size(shapeA))

    integer :: n
    integer, allocatable :: shapeI(:), subidx(:)

    allocate( shapeI, source=shapeA )
    if (present(sub_shape)) shapeI = sub_shape

    subidx = subarray_in_flattened( shapeA, shapeI, offset=sub_first )
    if (size( subidx ) > 0) then
      A(subidx) = zero
      subidx = diagonal_in_flattened( shapeA, shapeI, offset=sub_first )
      n = size( subidx )
      A(subidx) = diag(:n)
    end if

    deallocate( shapeI, subidx )
  end subroutine diagonal_integer64

  !> [[m_diagonal(module):get_diagonal(interface)]] for arrays of type `integer(kind=int64)`.
  pure subroutine get_diagonal_integer64( A, shapeA, diag, sub_shape, sub_first )
    use math_constants, only : zero => ZERO_INTEGER64
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    integer(kind=int64), intent(in) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> array to write diagonal values to
    integer(kind=int64), intent(out) :: diag(*)
    !> shape of subarray to fill with diagonal  
    !> default: `shapeA`
    integer, optional, intent(in) :: sub_shape(size(shapeA))
    !> indices of element in array at which first element of diagonal should be placed   
    !> default: first element of array
    integer, optional, intent(in) :: sub_first(size(shapeA))

    integer :: n
    integer, allocatable :: shapeI(:), subidx(:)

    allocate( shapeI, source=shapeA )
    if (present(sub_shape)) shapeI = sub_shape

    subidx = diagonal_in_flattened( shapeA, shapeI, offset=sub_first )
    n = size( subidx )
    if (n > 0) then
      diag(:n) = A(subidx)
    end if

    deallocate( shapeI, subidx )
  end subroutine get_diagonal_integer64
  
  !> [[m_diagonal(module):fill_diagonal(interface)]] for arrays of type `real(kind=real32)`.
  pure subroutine diagonal_real32( A, shapeA, diag, sub_shape, sub_first )
    use math_constants, only : zero => ZERO_REAL32
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    real(kind=real32), intent(inout) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> values to fill diagonal with
    real(kind=real32), intent(in) :: diag(*)
    !> shape of subarray to fill with diagonal  
    !> default: `shapeA`
    integer, optional, intent(in) :: sub_shape(size(shapeA))
    !> indices of element in array at which first element of diagonal should be placed   
    !> default: first element of array
    integer, optional, intent(in) :: sub_first(size(shapeA))

    integer :: n
    integer, allocatable :: shapeI(:), subidx(:)

    allocate( shapeI, source=shapeA )
    if (present(sub_shape)) shapeI = sub_shape

    subidx = subarray_in_flattened( shapeA, shapeI, offset=sub_first )
    if (size( subidx ) > 0) then
      A(subidx) = zero
      subidx = diagonal_in_flattened( shapeA, shapeI, offset=sub_first )
      n = size( subidx )
      A(subidx) = diag(:n)
    end if

    deallocate( shapeI, subidx )
  end subroutine diagonal_real32

  !> [[m_diagonal(module):get_diagonal(interface)]] for arrays of type `real(kind=real32)`.
  pure subroutine get_diagonal_real32( A, shapeA, diag, sub_shape, sub_first )
    use math_constants, only : zero => ZERO_REAL32
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    real(kind=real32), intent(in) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> array to write diagonal values to
    real(kind=real32), intent(out) :: diag(*)
    !> shape of subarray to fill with diagonal  
    !> default: `shapeA`
    integer, optional, intent(in) :: sub_shape(size(shapeA))
    !> indices of element in array at which first element of diagonal should be placed   
    !> default: first element of array
    integer, optional, intent(in) :: sub_first(size(shapeA))

    integer :: n
    integer, allocatable :: shapeI(:), subidx(:)

    allocate( shapeI, source=shapeA )
    if (present(sub_shape)) shapeI = sub_shape

    subidx = diagonal_in_flattened( shapeA, shapeI, offset=sub_first )
    n = size( subidx )
    if (n > 0) then
      diag(:n) = A(subidx)
    end if

    deallocate( shapeI, subidx )
  end subroutine get_diagonal_real32
  
  !> [[m_diagonal(module):fill_diagonal(interface)]] for arrays of type `real(kind=real64)`.
  pure subroutine diagonal_real64( A, shapeA, diag, sub_shape, sub_first )
    use math_constants, only : zero => ZERO_REAL64
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    real(kind=real64), intent(inout) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> values to fill diagonal with
    real(kind=real64), intent(in) :: diag(*)
    !> shape of subarray to fill with diagonal  
    !> default: `shapeA`
    integer, optional, intent(in) :: sub_shape(size(shapeA))
    !> indices of element in array at which first element of diagonal should be placed   
    !> default: first element of array
    integer, optional, intent(in) :: sub_first(size(shapeA))

    integer :: n
    integer, allocatable :: shapeI(:), subidx(:)

    allocate( shapeI, source=shapeA )
    if (present(sub_shape)) shapeI = sub_shape

    subidx = subarray_in_flattened( shapeA, shapeI, offset=sub_first )
    if (size( subidx ) > 0) then
      A(subidx) = zero
      subidx = diagonal_in_flattened( shapeA, shapeI, offset=sub_first )
      n = size( subidx )
      A(subidx) = diag(:n)
    end if

    deallocate( shapeI, subidx )
  end subroutine diagonal_real64

  !> [[m_diagonal(module):get_diagonal(interface)]] for arrays of type `real(kind=real64)`.
  pure subroutine get_diagonal_real64( A, shapeA, diag, sub_shape, sub_first )
    use math_constants, only : zero => ZERO_REAL64
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    real(kind=real64), intent(in) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> array to write diagonal values to
    real(kind=real64), intent(out) :: diag(*)
    !> shape of subarray to fill with diagonal  
    !> default: `shapeA`
    integer, optional, intent(in) :: sub_shape(size(shapeA))
    !> indices of element in array at which first element of diagonal should be placed   
    !> default: first element of array
    integer, optional, intent(in) :: sub_first(size(shapeA))

    integer :: n
    integer, allocatable :: shapeI(:), subidx(:)

    allocate( shapeI, source=shapeA )
    if (present(sub_shape)) shapeI = sub_shape

    subidx = diagonal_in_flattened( shapeA, shapeI, offset=sub_first )
    n = size( subidx )
    if (n > 0) then
      diag(:n) = A(subidx)
    end if

    deallocate( shapeI, subidx )
  end subroutine get_diagonal_real64
  
  !> [[m_diagonal(module):fill_diagonal(interface)]] for arrays of type `complex(kind=real32)`.
  pure subroutine diagonal_complex32( A, shapeA, diag, sub_shape, sub_first )
    use math_constants, only : zero => ZERO_COMPLEX32
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    complex(kind=real32), intent(inout) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> values to fill diagonal with
    complex(kind=real32), intent(in) :: diag(*)
    !> shape of subarray to fill with diagonal  
    !> default: `shapeA`
    integer, optional, intent(in) :: sub_shape(size(shapeA))
    !> indices of element in array at which first element of diagonal should be placed   
    !> default: first element of array
    integer, optional, intent(in) :: sub_first(size(shapeA))

    integer :: n
    integer, allocatable :: shapeI(:), subidx(:)

    allocate( shapeI, source=shapeA )
    if (present(sub_shape)) shapeI = sub_shape

    subidx = subarray_in_flattened( shapeA, shapeI, offset=sub_first )
    if (size( subidx ) > 0) then
      A(subidx) = zero
      subidx = diagonal_in_flattened( shapeA, shapeI, offset=sub_first )
      n = size( subidx )
      A(subidx) = diag(:n)
    end if

    deallocate( shapeI, subidx )
  end subroutine diagonal_complex32

  !> [[m_diagonal(module):get_diagonal(interface)]] for arrays of type `complex(kind=real32)`.
  pure subroutine get_diagonal_complex32( A, shapeA, diag, sub_shape, sub_first )
    use math_constants, only : zero => ZERO_COMPLEX32
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    complex(kind=real32), intent(in) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> array to write diagonal values to
    complex(kind=real32), intent(out) :: diag(*)
    !> shape of subarray to fill with diagonal  
    !> default: `shapeA`
    integer, optional, intent(in) :: sub_shape(size(shapeA))
    !> indices of element in array at which first element of diagonal should be placed   
    !> default: first element of array
    integer, optional, intent(in) :: sub_first(size(shapeA))

    integer :: n
    integer, allocatable :: shapeI(:), subidx(:)

    allocate( shapeI, source=shapeA )
    if (present(sub_shape)) shapeI = sub_shape

    subidx = diagonal_in_flattened( shapeA, shapeI, offset=sub_first )
    n = size( subidx )
    if (n > 0) then
      diag(:n) = A(subidx)
    end if

    deallocate( shapeI, subidx )
  end subroutine get_diagonal_complex32
  
  !> [[m_diagonal(module):fill_diagonal(interface)]] for arrays of type `complex(kind=real64)`.
  pure subroutine diagonal_complex64( A, shapeA, diag, sub_shape, sub_first )
    use math_constants, only : zero => ZERO_COMPLEX64
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    complex(kind=real64), intent(inout) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> values to fill diagonal with
    complex(kind=real64), intent(in) :: diag(*)
    !> shape of subarray to fill with diagonal  
    !> default: `shapeA`
    integer, optional, intent(in) :: sub_shape(size(shapeA))
    !> indices of element in array at which first element of diagonal should be placed   
    !> default: first element of array
    integer, optional, intent(in) :: sub_first(size(shapeA))

    integer :: n
    integer, allocatable :: shapeI(:), subidx(:)

    allocate( shapeI, source=shapeA )
    if (present(sub_shape)) shapeI = sub_shape

    subidx = subarray_in_flattened( shapeA, shapeI, offset=sub_first )
    if (size( subidx ) > 0) then
      A(subidx) = zero
      subidx = diagonal_in_flattened( shapeA, shapeI, offset=sub_first )
      n = size( subidx )
      A(subidx) = diag(:n)
    end if

    deallocate( shapeI, subidx )
  end subroutine diagonal_complex64

  !> [[m_diagonal(module):get_diagonal(interface)]] for arrays of type `complex(kind=real64)`.
  pure subroutine get_diagonal_complex64( A, shapeA, diag, sub_shape, sub_first )
    use math_constants, only : zero => ZERO_COMPLEX64
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    complex(kind=real64), intent(in) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> array to write diagonal values to
    complex(kind=real64), intent(out) :: diag(*)
    !> shape of subarray to fill with diagonal  
    !> default: `shapeA`
    integer, optional, intent(in) :: sub_shape(size(shapeA))
    !> indices of element in array at which first element of diagonal should be placed   
    !> default: first element of array
    integer, optional, intent(in) :: sub_first(size(shapeA))

    integer :: n
    integer, allocatable :: shapeI(:), subidx(:)

    allocate( shapeI, source=shapeA )
    if (present(sub_shape)) shapeI = sub_shape

    subidx = diagonal_in_flattened( shapeA, shapeI, offset=sub_first )
    n = size( subidx )
    if (n > 0) then
      diag(:n) = A(subidx)
    end if

    deallocate( shapeI, subidx )
  end subroutine get_diagonal_complex64
  
end module m_diagonal_templates
