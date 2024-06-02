#: include 'fypp-macros.fypp'
$: FYPP_HEADER()
!> Unit tests for [[m_identity(module)]].
module m_identity_test
  use, intrinsic :: iso_fortran_env, only : ${', '.join(sorted(set([t['kind'] for t in ALL_NUMERICAL_TYPES])))}$
  use testdrive, only : error_type, check
  use m_identity
  implicit none
  private

  #: for t in ALL_NUMERICAL_TYPES
  public :: identity_${t['name']}$_test
  #: endfor
  
contains

  #: for t in ALL_NUMERICAL_TYPES
  !> Unit tests for [[m_identity(module):identity(interface)]] and [[m_identity(module):fill_identity(interface)]]
  !> with arrays of `type ${t['type']}$(kind=${t['kind']}$)`.
  subroutine identity_${t['name']}$_test( error )
    use math_constants, only : zero => ZERO_${t['name'].upper()}$, one => ONE_${t['name'].upper()}$
    !> Error object
    type(error_type), allocatable, intent(out) :: error

    integer, parameter :: shapeA(2) = [5, 3]
    integer, parameter :: shapeM(3) = [8, 5, 9]
    integer, parameter :: sub_shape_M(3) = [5, 4, 3]
    integer, parameter :: sub_first_M_pass(3) = [2, 1, 3]
    integer, parameter :: sub_first_M_fail(3) = [2, 3, 3]
    integer, parameter :: fill_fill_M(3) = sub_first_M_pass + sub_shape_M - 1

    integer :: i
    ${t['type']}$(kind=${t['kind']}$) :: s, M(shapeM(1), shapeM(2), shapeM(3))
    ${t['type']}$(kind=${t['kind']}$), allocatable :: A(:,:), Msub(:,:,:)

    ! check for scalar
    s = identity( like=zero )
    call check( error, &
      s == one, &
      '`identity` failed for scalar of type `${t['type']}$(kind=${t['kind']}$)`.', &
      'Returns incorrect value.' )
    if (allocated(error)) return
    ! check for allocatable rank 2
    A = identity( shapeA(1), shapeA(2), like=zero )
    call check( error, &
      all( shape(A) == shapeA ), &
      '`identity` failed for allocatable rank 2 array of type `${t['type']}$(kind=${t['kind']}$)`.', &
      'Returns incorrect shape.' )
    if (allocated(error)) return
    call check( error, &
      all( [(A(i, i) == one, i=1, minval(shapeA))] ), &
      '`identity` failed for allocatable rank 2 array of type `${t['type']}$(kind=${t['kind']}$)`.', &
      'Returns non unit diagonal element.' )
    if (allocated(error)) return
    do i = 1, minval( shapeA )
      A(i, i) = zero
    end do
    call check( error, &
      all( A == zero ), &
      '`identity` failed for allocatable rank 2 array of type `${t['type']}$(kind=${t['kind']}$)`.', &
      'Returns non zero off-diagonal element.' )
    if (allocated(error)) return
    ! check for rank 3
    M = -one
    call fill_identity( M(:,1,1), shapeM, sub_shape=sub_shape_M, sub_first=sub_first_M_pass )
    Msub = M(sub_first_M_pass(1):fill_fill_M(1), sub_first_M_pass(2):fill_fill_M(2), sub_first_M_pass(3):fill_fill_M(3))
    call check( error, &
      all( [(Msub(i, i, i) == one, i=1, minval(sub_shape_M))] ), &
      '`identity` failed for rank 3 array of type `${t['type']}$(kind=${t['kind']}$)`.', &
      'Returns non unit diagonal element.' )
    if (allocated(error)) return
    do i = 1, minval( sub_shape_M )
      Msub(i, i, i) = zero
    end do
    call check( error, &
      all( Msub == zero ), &
      '`identity` failed for rank 3 array of type `${t['type']}$(kind=${t['kind']}$)`.', &
      'Returns non zero off-diagonal element.' )
    if (allocated(error)) return
    M(sub_first_M_pass(1):fill_fill_M(1), sub_first_M_pass(2):fill_fill_M(2), sub_first_M_pass(3):fill_fill_M(3)) = -one
    call check( error, &
      all( M == -one ), &
      '`identity` failed for rank 3 array of type `${t['type']}$(kind=${t['kind']}$)`.', &
      'Elements outside fill area were changed.' )
    if (allocated(error)) return
    ! check violation of boundary check
    call fill_identity( M(:,1,1), shapeM, sub_shape=sub_shape_M, sub_first=sub_first_M_fail )
    call check( error, &
      all( M == -one ), &
      '`identity` failed for rank 3 array of type `${t['type']}$(kind=${t['kind']}$)`.', &
      'Changed array even if boundaries do not conform. Array should be left unchanged.' )
    if (allocated(error)) return

    if (allocated(A)) deallocate( A )
    if (allocated(Msub)) deallocate( Msub )
  end subroutine identity_${t['name']}$_test
  
  #: endfor
end module m_identity_test
