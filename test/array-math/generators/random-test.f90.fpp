#: include 'fypp-macros.fypp'
$: FYPP_HEADER()
!> Unit tests for [[m_random(module)]].
module m_random_test
  use, intrinsic :: iso_fortran_env, only : ${', '.join(sorted(set([t['kind'] for t in ALL_NUMERICAL_TYPES])))}$
  use testdrive, only : error_type, check
  use m_random
  implicit none
  private

  #: for t in ALL_NUMERICAL_TYPES
  public :: random_${t['name']}$_test
  #: endfor
  
contains

  #: for t in ALL_NUMERICAL_TYPES
  !> Unit tests for [[m_random(module):identity(interface)]] and [[m_identity(module):fill_identity(interface)]]
  !> with arrays of `type ${t['type']}$(kind=${t['kind']}$)`.
  subroutine random_${t['name']}$_test( error )
    use math_constants, only : zero => ZERO_${t['name'].upper()}$
    !> Error object
    type(error_type), allocatable, intent(out) :: error

    integer, parameter :: interval(4) = [2, 5, 4, 6]
    integer, parameter :: shapeA(2) = [5, 3]
    integer, parameter :: shapeM(3) = [8, 5, 9]
    integer, parameter :: sub_shape_M(3) = [5, 4, 3]
    integer, parameter :: sub_first_M_pass(3) = [2, 1, 3]
    integer, parameter :: sub_first_M_fail(3) = [2, 3, 3]
    integer, parameter :: fill_fill_M(3) = sub_first_M_pass + sub_shape_M - 1

    ${t['type']}$(kind=${t['kind']}$) :: intv(2), s, M(shapeM(1), shapeM(2), shapeM(3))
    ${t['type']}$(kind=${t['kind']}$), allocatable :: A(:,:), Msub(:,:,:)

    intv = interval(:2)
    #{if t['type'] == 'complex'}#intv = intv + cmplx( 0, interval(3:), kind=${t['kind']}$ )#{endif}#

    ! check for scalar
    s = random( like=zero, interval=intv )
    call check( error, &
      (real(s - intv(1)) >= 0) .and. (real(s - intv(2)) <= 0)#{if t['type'] == 'complex'}# .and. (aimag(s - intv(1)) >= 0) .and. (aimag(s - intv(2)) <= 0)#{endif}#, &
      '`random` failed for scalar of type `${t['type']}$(kind=${t['kind']}$)`.', &
      'Returns value outside interval.' )
    if (allocated(error)) return
    ! check for allocatable rank 2
    A = random( shapeA(1), shapeA(2), like=zero, interval=intv )
    call check( error, all( shape(A) == shapeA ), &
      '`random` failed for allocatable rank 2 array of type `${t['type']}$(kind=${t['kind']}$)`.', &
      'Returns incorrect shape.' )
    if (allocated(error)) return
    call check( error, &
      all(real(A - intv(1)) >= 0) .and. all(real(A - intv(2)) <= 0)#{if t['type'] == 'complex'}# .and. all(aimag(A - intv(1)) >= 0) .and. all(aimag(A - intv(2)) <= 0)#{endif}#, &
      '`random` failed for allocatable rank 2 array of type `${t['type']}$(kind=${t['kind']}$)`.', &
      'Returns values outside interval.' )
    if (allocated(error)) return
    ! check for rank 3
    M = -intv(1)
    call fill_random( M(:,1,1), shapeM, sub_shape=sub_shape_M, sub_first=sub_first_M_pass, interval=intv )
    Msub = M(sub_first_M_pass(1):fill_fill_M(1), sub_first_M_pass(2):fill_fill_M(2), sub_first_M_pass(3):fill_fill_M(3))
    call check( error, &
      all(real(Msub - intv(1)) >= 0) .and. all(real(Msub - intv(2)) <= 0)#{if t['type'] == 'complex'}# .and. all(aimag(Msub - intv(1)) >= 0) .and. all(aimag(Msub - intv(2)) <= 0)#{endif}#, &
      '`random` failed for rank 3 array of type `${t['type']}$(kind=${t['kind']}$)`.', &
      'Returns values outside interval.' )
    if (allocated(error)) return
    M(sub_first_M_pass(1):fill_fill_M(1), sub_first_M_pass(2):fill_fill_M(2), sub_first_M_pass(3):fill_fill_M(3)) = -intv(1)
    call check( error, &
      all( M == -intv(1) ), &
      '`random` failed for rank 3 array of type `${t['type']}$(kind=${t['kind']}$)`.', &
      'Elements outside fill area were changed.' )
    if (allocated(error)) return
    ! check violation of boundary check
    call fill_random( M(:,1,1), shapeM, sub_shape=sub_shape_M, sub_first=sub_first_M_fail )
    call check( error, &
      all( M == -intv(1) ), &
      '`random` failed for rank 3 array of type `${t['type']}$(kind=${t['kind']}$)`.', &
      'Changed array even if boundaries do not conform. Array should be left unchanged.' )
    if (allocated(error)) return

    if (allocated(A)) deallocate( A )
    if (allocated(Msub)) deallocate( Msub )
  end subroutine random_${t['name']}$_test
  
  #: endfor
end module m_random_test
