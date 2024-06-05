#: include 'fypp-macros.fypp'
$: FYPP_HEADER()
!> Generators for diagonal matrix.
module m_diagonal_templates
  use, intrinsic :: iso_fortran_env, only : ${', '.join(sorted(set([t['kind'] for t in ALL_NUMERICAL_TYPES])))}$
  implicit none
  private

  #: for t in ALL_NUMERICAL_TYPES
  public :: diagonal_${t['name']}$, get_diagonal_${t['name']}$
  #: endfor
  
contains

  #: for t in ALL_NUMERICAL_TYPES
  !> [[m_diagonal(module):fill_diagonal(interface)]] for arrays of type `${t['type']}$(kind=${t['kind']}$)`.
  pure subroutine diagonal_${t['name']}$( A, shapeA, diag, sub_shape, sub_first )
    use math_constants, only : zero => ZERO_${t['name'].upper()}$
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    ${t['type']}$(kind=${t['kind']}$), intent(inout) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> values to fill diagonal with
    ${t['type']}$(kind=${t['kind']}$), intent(in) :: diag(*)
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
  end subroutine diagonal_${t['name']}$

  !> [[m_diagonal(module):get_diagonal(interface)]] for arrays of type `${t['type']}$(kind=${t['kind']}$)`.
  pure subroutine get_diagonal_${t['name']}$( A, shapeA, diag, sub_shape, sub_first )
    use math_constants, only : zero => ZERO_${t['name'].upper()}$
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    ${t['type']}$(kind=${t['kind']}$), intent(in) :: A(*)
    !> shape of array
    integer, intent(in) :: shapeA(:)
    !> array to write diagonal values to
    ${t['type']}$(kind=${t['kind']}$), intent(out) :: diag(*)
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
  end subroutine get_diagonal_${t['name']}$
  
  #: endfor
end module m_diagonal_templates
