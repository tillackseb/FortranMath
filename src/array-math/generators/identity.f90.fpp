#: include 'fypp-macros.fypp'
$: FYPP_HEADER()
!> Generators for identity matrix.
module m_identity_templates
  use, intrinsic :: iso_fortran_env, only : ${', '.join(sorted(set([t['kind'] for t in ALL_NUMERICAL_TYPES])))}$
  implicit none
  private

  #: for t in ALL_NUMERICAL_TYPES
  public :: identity_${t['name']}$
  #: endfor
  
contains

  #: for t in ALL_NUMERICAL_TYPES
  !> [[m_identity(module):fill_identity(interface)]] for arrays of type `${t['type']}$(kind=${t['kind']}$)`.
  pure subroutine identity_${t['name']}$( A, shapeA, sub_shape, sub_first )
    use math_constants, only : zero => ZERO_${t['name'].upper()}$, one => ONE_${t['name'].upper()}$
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    ${t['type']}$(kind=${t['kind']}$), intent(inout) :: A(*)
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
  end subroutine identity_${t['name']}$

  #: endfor

end module m_identity_templates
