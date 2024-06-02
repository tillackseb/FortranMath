!> Collection of helpers to get indices of subarrays within larger arrays.
module m_subarrays
  implicit none
  private

  public :: index_to_flattened, index_from_flattened
  public :: subarray_in_flattened, diagonal_in_flattened
  
contains

  !> Get index of element in flattened array by indices in unflattened array.
  pure function index_to_flattened( shapeA, idx_unflat ) result( idx_flat )
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> indices of element in unflattened array
    integer, intent(in) :: idx_unflat(size(shapeA))
    !> index of element in flattened array
    integer :: idx_flat

    integer :: i

    idx_flat = idx_unflat(1)
    do i = 2, size( shapeA )
      idx_flat = idx_flat + (idx_unflat(i) - 1) * product( shapeA(:i-1) )
    end do
  end function index_to_flattened

  !> Get indices of element in unflattened array by index in flattened array.
  pure function index_from_flattened( shapeA, idx_flat ) result( idx_unflat )
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> index of element in flattened array
    integer, intent(in) :: idx_flat
    !> indices of element in unflattened array
    integer :: idx_unflat(size(shapeA))

    integer :: i, j, n

    j = idx_flat - 1
    do i = size( shapeA ), 2, -1
      n = product( shapeA(:i-1) )
      idx_unflat(i) = j / n
      j = j - idx_unflat(i) * n
    end do
    idx_unflat(1) = j
    idx_unflat = idx_unflat + 1 
  end function index_from_flattened

  !> Compute indices of elements in flattened `A` that form a subarray `B` within `A`.
  !>
  !> Let `A` be an array of arbitray. Let `B` be an array of the same rank that is fully contained within 
  !> `A`, i.e., `all( shapeB <= shapeA ) = .true.`. Then this routines returns the indices of all elements 
  !> of `A` that are in `B`.   
  !> Indices correspond to flattened array `A`.  
  !> Optionally, an offset can be provided that specifies the position of the fist element of `B` within 
  !> `A`. In that case the containment condition reads `all( shapeB + offset - 1 <= shapeA )`.  
  !> If the containment condition is violated, the function will return an array of size 0.
  pure function subarray_in_flattened( shapeA, shapeB, offset ) result( sub )
    !> shape of outer array `A`
    integer, intent(in) :: shapeA(:)
    !> shape of inner array `B`
    integer, intent(in) :: shapeB(size(shapeA))
    !> indices of first element of `B` within `A`   
    !> default: first element of `A`
    integer, optional, intent(in) :: offset(size(shapeA))
    !> indices of elements in `A` that form `B`
    integer, allocatable :: sub(:)

    integer :: i
    integer, allocatable :: off(:), idxA(:), idxB(:)

    if (allocated(sub)) deallocate( sub )

    allocate( off(size(shapeA)), source=1 )
    if (present(offset)) off = offset

    if (all( shapeB + off - 1 <= shapeA )) then
      allocate( sub(product(shapeB)), source=0 )
      allocate( idxA(size(shapeA)), idxB(size(shapeA)) )
      do i = 1, size( sub )
        idxB = index_from_flattened( shapeB, i )
        idxA = off + idxB - 1
        sub(i) = index_to_flattened( shapeA, idxA )
      end do
      deallocate( idxA, idxB )
    else
      allocate( sub(0) )
    end if
    deallocate( off )
  end function subarray_in_flattened

  !> Compute indices of elements in flattened `A` that form the main diagonal of a subarray `B` within 
  !> `A`.
  !>
  !> Let `A` be an array of arbitray. Let `B` be an array of the same rank that is fully contained within 
  !> `A`, i.e., `all( shapeB <= shapeA ) = .true.`. Then this routines returns the indices of all elements 
  !> of `A` that are on the main diagonal of `B`.  
  !> Indices correspond to flattened array `A`.  
  !> Optionally, an offset can be provided that specifies the position of the fist element of `B` within 
  !> `A`. In that case the containment condition reads `all( shapeB + offset - 1 <= shapeA )`.  
  !> If the containment condition is violated, the function will return an array of size 0.
  pure function diagonal_in_flattened( shapeA, shapeB, offset ) result( sub )
    !> shape of outer array `A`
    integer, intent(in) :: shapeA(:)
    !> shape of inner array `B`
    integer, intent(in) :: shapeB(size(shapeA))
    !> indices of first element of `B` within `A`   
    !> default: first element of `A`
    integer, optional, intent(in) :: offset(size(shapeA))
    !> indices of elements in `A` that form `B`
    integer, allocatable :: sub(:)

    integer :: i, inc
    integer, allocatable :: off(:), idxA(:)

    if (allocated(sub)) deallocate( sub )

    allocate( off(size(shapeA)), source=1 )
    if (present(offset)) off = offset

    if (all( shapeB + off - 1 <= shapeA )) then
      allocate( sub(minval(shapeB)), source=0 )
      allocate( idxA(size(shapeA)) )
      do i = 1, size( sub )
        idxA = off + i - 1
        sub(i) = index_to_flattened( shapeA, idxA )
      end do
      deallocate( idxA )
    else
      allocate( sub(0) )
    end if
    deallocate( off )
  end function diagonal_in_flattened

end module m_subarrays

