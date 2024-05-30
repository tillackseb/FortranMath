#: include 'fypp-macros.fypp'
$: FYPP_HEADER()
!> Unit tests for [[m_identity(module)]].
module m_identity_test
  use, intrinsic :: iso_fortran_env, only : ${', '.join(KINDS)}$
  use testdrive, only : error_type, check
  use m_identity
  implicit none
  private

  #: for t in ALL_NUMERICAL_TYPES
  public :: identity_${t['name']}$_test
  #: endfor
  
contains

  #: for t in ALL_NUMERICAL_TYPES
  !> Unit tests for [[m_identity(module):identity(interface)]] and [[m_identity(module):to_identity(interface)]]
  !> with arrays of `type ${t['type']}$(kind=${t['kind']}$)`.
  subroutine identity_${t['name']}$_test( error )
    use math_constants, only : zero => ZERO_${t['name'].upper()}$, one => ONE_${t['name'].upper()}$
    !> Error object
    type(error_type), allocatable, intent(out) :: error

    integer, parameter :: shapeA(2) = [5, 3]
    integer, parameter :: shapeM(3) = [8, 5, 9]
    integer, parameter :: fill_shape_M(3) = [5, 4, 3]
    integer, parameter :: fill_at_M_pass(3) = [2, 1, 3]
    integer, parameter :: fill_at_M_fail(3) = [2, 3, 3]

    integer :: i, j
    ${t['type']}$(kind=${t['kind']}$) :: s, M(shapeM(1), shapeM(2), shapeM(3))
    ${t['type']}$(kind=${t['kind']}$), allocatable :: A(:,:)

    ! check for scalar
    s = identity( like=zero )
    call check( error, s == one, &
      '`identity` failed for scalar of type `${t['type']}$(kind=${t['kind']}$)`.', &
      'Returns incorrect value.' )
    if (allocated(error)) return
    ! check for allocatable rank 2
    A = identity( shapeA(1), shapeA(2), like=zero )
    call check( error, all( shape(A) == shapeA ), &
      '`identity` failed for allocatable rank 2 array of type `${t['type']}$(kind=${t['kind']}$)`.', &
      'Returns incorrect shape.' )
    if (allocated(error)) return
    call check( error, all( [(A(i, i) == one, i=1, minval(shapeA))] ), &
      '`identity` failed for allocatable rank 2 array of type `${t['type']}$(kind=${t['kind']}$)`.', &
      'Returns non unit diagonal element.' )
    if (allocated(error)) return
    do i = 1, minval( shapeA )
      A(i, i) = zero
    end do
    call check( error, all( A == zero ), &
      '`identity` failed for allocatable rank 2 array of type `${t['type']}$(kind=${t['kind']}$)`.', &
      'Returns non zero off-diagonal element.' )
    if (allocated(error)) return
    ! check for rank 3
    call to_identity( M(:,1,1), shapeM, fill_shape=fill_shape_M, fill_at=fill_at_M_pass )
    call check( error, all( [(M(fill_at_M_pass(1)+i, fill_at_M_pass(2)+i, fill_at_M_pass(3)+i) == one, i=0, minval(fill_shape_M)-1)] ), &
      '`identity` failed for rank 3 array of type `${t['type']}$(kind=${t['kind']}$)`.', &
      'Returns non unit diagonal element.' )
    if (allocated(error)) return
    do i = 0, minval( fill_shape_M ) - 1
      M(fill_at_M_pass(1)+i, fill_at_M_pass(2)+i, fill_at_M_pass(3)+i) = zero
    end do
    call check( error, all( M(fill_at_M_pass(1):fill_shape_M(1)+fill_at_M_pass(1)-1, fill_at_M_pass(2):fill_shape_M(2)+fill_at_M_pass(2)-1, fill_at_M_pass(3):fill_shape_M(3)+fill_at_M_pass(3)-1) == zero ), &
      '`identity` failed for rank 3 array of type `${t['type']}$(kind=${t['kind']}$)`.', &
      'Returns non zero off-diagonal element.' )
    if (allocated(error)) return
    ! check violation of boundary check
    M = 2 * one
    call to_identity( M(:,1,1), shapeM, fill_shape=fill_shape_M, fill_at=fill_at_M_fail )
    call check( error, all( M == 2*one ), &
      '`identity` failed for rank 3 array of type `${t['type']}$(kind=${t['kind']}$)`.', &
      'Changed array even if boundaries do not conform. Array should be left unchanged.' )
    if (allocated(error)) return

    if (allocated(A)) deallocate( A )
  end subroutine identity_${t['name']}$_test
  
  #: endfor
end module m_identity_test
