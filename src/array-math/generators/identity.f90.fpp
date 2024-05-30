#: include 'fypp-macros.fypp'
$: FYPP_HEADER()
#: set TYPES = ['integer', 'integer', 'real', 'real', 'complex', 'complex']
#: set KINDS = ['int32', 'int64', 'real32', 'real64', 'real32', 'real64']
#: set NAMES = ['integer32', 'integer64', 'real32', 'real64', 'complex32', 'complex64']
!> Generators for identity matrix.
module m_identity_templates
  use, intrinsic :: iso_fortran_env, only : ${', '.join(KINDS)}$
  use math_constants, only : &
    ${', '.join( ('zero_'+NAME for NAME in NAMES) ).upper()}$, &
    ${', '.join( ('one_'+NAME for NAME in NAMES) ).upper()}$
  implicit none
  private

  #: for NAME in NAMES
  public :: identity_${NAME}$
  #: endfor
  
contains

  #: for TYPE, KIND, NAME in list( zip( TYPES, KINDS, NAMES ) )
  !> [[m_identity(module):to_identity(interface)]] for arrays of type `${TYPE}$(kind=${KIND}$)`.
  pure subroutine identity_${NAME}$( A, shapeA, fill_shape, fill_at )
    use m_subarrays, only : subarray_in_flattened, diagonal_in_flattened
    !> array
    ${TYPE}$(kind=${KIND}$), intent(inout) :: A(*)
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
      A(subidx) = ZERO_${NAME.upper()}$
      subidx = diagonal_in_flattened( shapeA, shapeI, offset=fill_at )
      A(subidx) = ONE_${NAME.upper()}$
    end if

    if (allocated(shapeI)) deallocate( shapeI )
    if (allocated(subidx)) deallocate( subidx )
  end subroutine identity_${NAME}$

  #: endfor

end module m_identity_templates
