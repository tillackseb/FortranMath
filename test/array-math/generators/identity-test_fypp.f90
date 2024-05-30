!FYPP HEADER====================================================================
!
! INFORMATION
! This file has been created by the fypp preprocessor. DO NOT MODIFY THIS FILE!
! Implement changes in the original preprocessed file.
! original file ::       test/array-math/generators/identity-test.f90.fpp
! preprocessing time ::  2024-05-30 15:29:35 UTC+0200
!
!END FYPP HEADER================================================================
!> Unit tests for [[m_identity(module)]].
module m_identity_test
  use, intrinsic :: iso_fortran_env, only : int32, int64, real32, real64, real32, real64
  use testdrive, only : error_type, check
  use m_identity
  implicit none
  private

  public :: identity_integer32_test
  public :: identity_integer64_test
  public :: identity_real32_test
  public :: identity_real64_test
  public :: identity_complex32_test
  public :: identity_complex64_test
  
contains

  !> Unit tests for [[m_identity(module):identity(interface)]] and [[m_identity(module):to_identity(interface)]]
  !> with arrays of `type integer(kind=int32)`.
  subroutine identity_integer32_test( error )
    use math_constants, only : zero => ZERO_INTEGER32, one => ONE_INTEGER32
    !> Error object
    type(error_type), allocatable, intent(out) :: error

    integer, parameter :: shapeA(2) = [5, 3]
    integer, parameter :: shapeM(3) = [8, 5, 9]
    integer, parameter :: fill_shape_M(3) = [5, 4, 3]
    integer, parameter :: fill_at_M_pass(3) = [2, 1, 3]
    integer, parameter :: fill_at_M_fail(3) = [2, 3, 3]

    integer :: i, j
    integer(kind=int32) :: s, M(shapeM(1), shapeM(2), shapeM(3))
    integer(kind=int32), allocatable :: A(:,:)

    ! check for scalar
    s = identity( like=zero )
    call check( error, s == one, &
      '`identity` failed for scalar of type `integer(kind=int32)`.', &
      'Returns incorrect value.' )
    if (allocated(error)) return
    ! check for allocatable rank 2
    A = identity( shapeA(1), shapeA(2), like=zero )
    call check( error, all( shape(A) == shapeA ), &
      '`identity` failed for allocatable rank 2 array of type `integer(kind=int32)`.', &
      'Returns incorrect shape.' )
    if (allocated(error)) return
    call check( error, all( [(A(i, i) == one, i=1, minval(shapeA))] ), &
      '`identity` failed for allocatable rank 2 array of type `integer(kind=int32)`.', &
      'Returns non unit diagonal element.' )
    if (allocated(error)) return
    do i = 1, minval( shapeA )
      A(i, i) = zero
    end do
    call check( error, all( A == zero ), &
      '`identity` failed for allocatable rank 2 array of type `integer(kind=int32)`.', &
      'Returns non zero off-diagonal element.' )
    if (allocated(error)) return
    ! check for rank 3
    call to_identity( M(:,1,1), shapeM, fill_shape=fill_shape_M, fill_at=fill_at_M_pass )
    call check( error, all( [(M(fill_at_M_pass(1)+i, fill_at_M_pass(2)+i, fill_at_M_pass(3)+i) == one, i=0, minval(fill_shape_M)-1)] ), &
      '`identity` failed for rank 3 array of type `integer(kind=int32)`.', &
      'Returns non unit diagonal element.' )
    if (allocated(error)) return
    do i = 0, minval( fill_shape_M ) - 1
      M(fill_at_M_pass(1)+i, fill_at_M_pass(2)+i, fill_at_M_pass(3)+i) = zero
    end do
    call check( error, all( M(fill_at_M_pass(1):fill_shape_M(1)+fill_at_M_pass(1)-1, fill_at_M_pass(2):fill_shape_M(2)+fill_at_M_pass(2)-1, fill_at_M_pass(3):fill_shape_M(3)+fill_at_M_pass(3)-1) == zero ), &
      '`identity` failed for rank 3 array of type `integer(kind=int32)`.', &
      'Returns non zero off-diagonal element.' )
    if (allocated(error)) return
    ! check violation of boundary check
    M = 2 * one
    call to_identity( M(:,1,1), shapeM, fill_shape=fill_shape_M, fill_at=fill_at_M_fail )
    call check( error, all( M == 2*one ), &
      '`identity` failed for rank 3 array of type `integer(kind=int32)`.', &
      'Changed array even if boundaries do not conform. Array should be left unchanged.' )
    if (allocated(error)) return

    if (allocated(A)) deallocate( A )
  end subroutine identity_integer32_test
  
  !> Unit tests for [[m_identity(module):identity(interface)]] and [[m_identity(module):to_identity(interface)]]
  !> with arrays of `type integer(kind=int64)`.
  subroutine identity_integer64_test( error )
    use math_constants, only : zero => ZERO_INTEGER64, one => ONE_INTEGER64
    !> Error object
    type(error_type), allocatable, intent(out) :: error

    integer, parameter :: shapeA(2) = [5, 3]
    integer, parameter :: shapeM(3) = [8, 5, 9]
    integer, parameter :: fill_shape_M(3) = [5, 4, 3]
    integer, parameter :: fill_at_M_pass(3) = [2, 1, 3]
    integer, parameter :: fill_at_M_fail(3) = [2, 3, 3]

    integer :: i, j
    integer(kind=int64) :: s, M(shapeM(1), shapeM(2), shapeM(3))
    integer(kind=int64), allocatable :: A(:,:)

    ! check for scalar
    s = identity( like=zero )
    call check( error, s == one, &
      '`identity` failed for scalar of type `integer(kind=int64)`.', &
      'Returns incorrect value.' )
    if (allocated(error)) return
    ! check for allocatable rank 2
    A = identity( shapeA(1), shapeA(2), like=zero )
    call check( error, all( shape(A) == shapeA ), &
      '`identity` failed for allocatable rank 2 array of type `integer(kind=int64)`.', &
      'Returns incorrect shape.' )
    if (allocated(error)) return
    call check( error, all( [(A(i, i) == one, i=1, minval(shapeA))] ), &
      '`identity` failed for allocatable rank 2 array of type `integer(kind=int64)`.', &
      'Returns non unit diagonal element.' )
    if (allocated(error)) return
    do i = 1, minval( shapeA )
      A(i, i) = zero
    end do
    call check( error, all( A == zero ), &
      '`identity` failed for allocatable rank 2 array of type `integer(kind=int64)`.', &
      'Returns non zero off-diagonal element.' )
    if (allocated(error)) return
    ! check for rank 3
    call to_identity( M(:,1,1), shapeM, fill_shape=fill_shape_M, fill_at=fill_at_M_pass )
    call check( error, all( [(M(fill_at_M_pass(1)+i, fill_at_M_pass(2)+i, fill_at_M_pass(3)+i) == one, i=0, minval(fill_shape_M)-1)] ), &
      '`identity` failed for rank 3 array of type `integer(kind=int64)`.', &
      'Returns non unit diagonal element.' )
    if (allocated(error)) return
    do i = 0, minval( fill_shape_M ) - 1
      M(fill_at_M_pass(1)+i, fill_at_M_pass(2)+i, fill_at_M_pass(3)+i) = zero
    end do
    call check( error, all( M(fill_at_M_pass(1):fill_shape_M(1)+fill_at_M_pass(1)-1, fill_at_M_pass(2):fill_shape_M(2)+fill_at_M_pass(2)-1, fill_at_M_pass(3):fill_shape_M(3)+fill_at_M_pass(3)-1) == zero ), &
      '`identity` failed for rank 3 array of type `integer(kind=int64)`.', &
      'Returns non zero off-diagonal element.' )
    if (allocated(error)) return
    ! check violation of boundary check
    M = 2 * one
    call to_identity( M(:,1,1), shapeM, fill_shape=fill_shape_M, fill_at=fill_at_M_fail )
    call check( error, all( M == 2*one ), &
      '`identity` failed for rank 3 array of type `integer(kind=int64)`.', &
      'Changed array even if boundaries do not conform. Array should be left unchanged.' )
    if (allocated(error)) return

    if (allocated(A)) deallocate( A )
  end subroutine identity_integer64_test
  
  !> Unit tests for [[m_identity(module):identity(interface)]] and [[m_identity(module):to_identity(interface)]]
  !> with arrays of `type real(kind=real32)`.
  subroutine identity_real32_test( error )
    use math_constants, only : zero => ZERO_REAL32, one => ONE_REAL32
    !> Error object
    type(error_type), allocatable, intent(out) :: error

    integer, parameter :: shapeA(2) = [5, 3]
    integer, parameter :: shapeM(3) = [8, 5, 9]
    integer, parameter :: fill_shape_M(3) = [5, 4, 3]
    integer, parameter :: fill_at_M_pass(3) = [2, 1, 3]
    integer, parameter :: fill_at_M_fail(3) = [2, 3, 3]

    integer :: i, j
    real(kind=real32) :: s, M(shapeM(1), shapeM(2), shapeM(3))
    real(kind=real32), allocatable :: A(:,:)

    ! check for scalar
    s = identity( like=zero )
    call check( error, s == one, &
      '`identity` failed for scalar of type `real(kind=real32)`.', &
      'Returns incorrect value.' )
    if (allocated(error)) return
    ! check for allocatable rank 2
    A = identity( shapeA(1), shapeA(2), like=zero )
    call check( error, all( shape(A) == shapeA ), &
      '`identity` failed for allocatable rank 2 array of type `real(kind=real32)`.', &
      'Returns incorrect shape.' )
    if (allocated(error)) return
    call check( error, all( [(A(i, i) == one, i=1, minval(shapeA))] ), &
      '`identity` failed for allocatable rank 2 array of type `real(kind=real32)`.', &
      'Returns non unit diagonal element.' )
    if (allocated(error)) return
    do i = 1, minval( shapeA )
      A(i, i) = zero
    end do
    call check( error, all( A == zero ), &
      '`identity` failed for allocatable rank 2 array of type `real(kind=real32)`.', &
      'Returns non zero off-diagonal element.' )
    if (allocated(error)) return
    ! check for rank 3
    call to_identity( M(:,1,1), shapeM, fill_shape=fill_shape_M, fill_at=fill_at_M_pass )
    call check( error, all( [(M(fill_at_M_pass(1)+i, fill_at_M_pass(2)+i, fill_at_M_pass(3)+i) == one, i=0, minval(fill_shape_M)-1)] ), &
      '`identity` failed for rank 3 array of type `real(kind=real32)`.', &
      'Returns non unit diagonal element.' )
    if (allocated(error)) return
    do i = 0, minval( fill_shape_M ) - 1
      M(fill_at_M_pass(1)+i, fill_at_M_pass(2)+i, fill_at_M_pass(3)+i) = zero
    end do
    call check( error, all( M(fill_at_M_pass(1):fill_shape_M(1)+fill_at_M_pass(1)-1, fill_at_M_pass(2):fill_shape_M(2)+fill_at_M_pass(2)-1, fill_at_M_pass(3):fill_shape_M(3)+fill_at_M_pass(3)-1) == zero ), &
      '`identity` failed for rank 3 array of type `real(kind=real32)`.', &
      'Returns non zero off-diagonal element.' )
    if (allocated(error)) return
    ! check violation of boundary check
    M = 2 * one
    call to_identity( M(:,1,1), shapeM, fill_shape=fill_shape_M, fill_at=fill_at_M_fail )
    call check( error, all( M == 2*one ), &
      '`identity` failed for rank 3 array of type `real(kind=real32)`.', &
      'Changed array even if boundaries do not conform. Array should be left unchanged.' )
    if (allocated(error)) return

    if (allocated(A)) deallocate( A )
  end subroutine identity_real32_test
  
  !> Unit tests for [[m_identity(module):identity(interface)]] and [[m_identity(module):to_identity(interface)]]
  !> with arrays of `type real(kind=real64)`.
  subroutine identity_real64_test( error )
    use math_constants, only : zero => ZERO_REAL64, one => ONE_REAL64
    !> Error object
    type(error_type), allocatable, intent(out) :: error

    integer, parameter :: shapeA(2) = [5, 3]
    integer, parameter :: shapeM(3) = [8, 5, 9]
    integer, parameter :: fill_shape_M(3) = [5, 4, 3]
    integer, parameter :: fill_at_M_pass(3) = [2, 1, 3]
    integer, parameter :: fill_at_M_fail(3) = [2, 3, 3]

    integer :: i, j
    real(kind=real64) :: s, M(shapeM(1), shapeM(2), shapeM(3))
    real(kind=real64), allocatable :: A(:,:)

    ! check for scalar
    s = identity( like=zero )
    call check( error, s == one, &
      '`identity` failed for scalar of type `real(kind=real64)`.', &
      'Returns incorrect value.' )
    if (allocated(error)) return
    ! check for allocatable rank 2
    A = identity( shapeA(1), shapeA(2), like=zero )
    call check( error, all( shape(A) == shapeA ), &
      '`identity` failed for allocatable rank 2 array of type `real(kind=real64)`.', &
      'Returns incorrect shape.' )
    if (allocated(error)) return
    call check( error, all( [(A(i, i) == one, i=1, minval(shapeA))] ), &
      '`identity` failed for allocatable rank 2 array of type `real(kind=real64)`.', &
      'Returns non unit diagonal element.' )
    if (allocated(error)) return
    do i = 1, minval( shapeA )
      A(i, i) = zero
    end do
    call check( error, all( A == zero ), &
      '`identity` failed for allocatable rank 2 array of type `real(kind=real64)`.', &
      'Returns non zero off-diagonal element.' )
    if (allocated(error)) return
    ! check for rank 3
    call to_identity( M(:,1,1), shapeM, fill_shape=fill_shape_M, fill_at=fill_at_M_pass )
    call check( error, all( [(M(fill_at_M_pass(1)+i, fill_at_M_pass(2)+i, fill_at_M_pass(3)+i) == one, i=0, minval(fill_shape_M)-1)] ), &
      '`identity` failed for rank 3 array of type `real(kind=real64)`.', &
      'Returns non unit diagonal element.' )
    if (allocated(error)) return
    do i = 0, minval( fill_shape_M ) - 1
      M(fill_at_M_pass(1)+i, fill_at_M_pass(2)+i, fill_at_M_pass(3)+i) = zero
    end do
    call check( error, all( M(fill_at_M_pass(1):fill_shape_M(1)+fill_at_M_pass(1)-1, fill_at_M_pass(2):fill_shape_M(2)+fill_at_M_pass(2)-1, fill_at_M_pass(3):fill_shape_M(3)+fill_at_M_pass(3)-1) == zero ), &
      '`identity` failed for rank 3 array of type `real(kind=real64)`.', &
      'Returns non zero off-diagonal element.' )
    if (allocated(error)) return
    ! check violation of boundary check
    M = 2 * one
    call to_identity( M(:,1,1), shapeM, fill_shape=fill_shape_M, fill_at=fill_at_M_fail )
    call check( error, all( M == 2*one ), &
      '`identity` failed for rank 3 array of type `real(kind=real64)`.', &
      'Changed array even if boundaries do not conform. Array should be left unchanged.' )
    if (allocated(error)) return

    if (allocated(A)) deallocate( A )
  end subroutine identity_real64_test
  
  !> Unit tests for [[m_identity(module):identity(interface)]] and [[m_identity(module):to_identity(interface)]]
  !> with arrays of `type complex(kind=real32)`.
  subroutine identity_complex32_test( error )
    use math_constants, only : zero => ZERO_COMPLEX32, one => ONE_COMPLEX32
    !> Error object
    type(error_type), allocatable, intent(out) :: error

    integer, parameter :: shapeA(2) = [5, 3]
    integer, parameter :: shapeM(3) = [8, 5, 9]
    integer, parameter :: fill_shape_M(3) = [5, 4, 3]
    integer, parameter :: fill_at_M_pass(3) = [2, 1, 3]
    integer, parameter :: fill_at_M_fail(3) = [2, 3, 3]

    integer :: i, j
    complex(kind=real32) :: s, M(shapeM(1), shapeM(2), shapeM(3))
    complex(kind=real32), allocatable :: A(:,:)

    ! check for scalar
    s = identity( like=zero )
    call check( error, s == one, &
      '`identity` failed for scalar of type `complex(kind=real32)`.', &
      'Returns incorrect value.' )
    if (allocated(error)) return
    ! check for allocatable rank 2
    A = identity( shapeA(1), shapeA(2), like=zero )
    call check( error, all( shape(A) == shapeA ), &
      '`identity` failed for allocatable rank 2 array of type `complex(kind=real32)`.', &
      'Returns incorrect shape.' )
    if (allocated(error)) return
    call check( error, all( [(A(i, i) == one, i=1, minval(shapeA))] ), &
      '`identity` failed for allocatable rank 2 array of type `complex(kind=real32)`.', &
      'Returns non unit diagonal element.' )
    if (allocated(error)) return
    do i = 1, minval( shapeA )
      A(i, i) = zero
    end do
    call check( error, all( A == zero ), &
      '`identity` failed for allocatable rank 2 array of type `complex(kind=real32)`.', &
      'Returns non zero off-diagonal element.' )
    if (allocated(error)) return
    ! check for rank 3
    call to_identity( M(:,1,1), shapeM, fill_shape=fill_shape_M, fill_at=fill_at_M_pass )
    call check( error, all( [(M(fill_at_M_pass(1)+i, fill_at_M_pass(2)+i, fill_at_M_pass(3)+i) == one, i=0, minval(fill_shape_M)-1)] ), &
      '`identity` failed for rank 3 array of type `complex(kind=real32)`.', &
      'Returns non unit diagonal element.' )
    if (allocated(error)) return
    do i = 0, minval( fill_shape_M ) - 1
      M(fill_at_M_pass(1)+i, fill_at_M_pass(2)+i, fill_at_M_pass(3)+i) = zero
    end do
    call check( error, all( M(fill_at_M_pass(1):fill_shape_M(1)+fill_at_M_pass(1)-1, fill_at_M_pass(2):fill_shape_M(2)+fill_at_M_pass(2)-1, fill_at_M_pass(3):fill_shape_M(3)+fill_at_M_pass(3)-1) == zero ), &
      '`identity` failed for rank 3 array of type `complex(kind=real32)`.', &
      'Returns non zero off-diagonal element.' )
    if (allocated(error)) return
    ! check violation of boundary check
    M = 2 * one
    call to_identity( M(:,1,1), shapeM, fill_shape=fill_shape_M, fill_at=fill_at_M_fail )
    call check( error, all( M == 2*one ), &
      '`identity` failed for rank 3 array of type `complex(kind=real32)`.', &
      'Changed array even if boundaries do not conform. Array should be left unchanged.' )
    if (allocated(error)) return

    if (allocated(A)) deallocate( A )
  end subroutine identity_complex32_test
  
  !> Unit tests for [[m_identity(module):identity(interface)]] and [[m_identity(module):to_identity(interface)]]
  !> with arrays of `type complex(kind=real64)`.
  subroutine identity_complex64_test( error )
    use math_constants, only : zero => ZERO_COMPLEX64, one => ONE_COMPLEX64
    !> Error object
    type(error_type), allocatable, intent(out) :: error

    integer, parameter :: shapeA(2) = [5, 3]
    integer, parameter :: shapeM(3) = [8, 5, 9]
    integer, parameter :: fill_shape_M(3) = [5, 4, 3]
    integer, parameter :: fill_at_M_pass(3) = [2, 1, 3]
    integer, parameter :: fill_at_M_fail(3) = [2, 3, 3]

    integer :: i, j
    complex(kind=real64) :: s, M(shapeM(1), shapeM(2), shapeM(3))
    complex(kind=real64), allocatable :: A(:,:)

    ! check for scalar
    s = identity( like=zero )
    call check( error, s == one, &
      '`identity` failed for scalar of type `complex(kind=real64)`.', &
      'Returns incorrect value.' )
    if (allocated(error)) return
    ! check for allocatable rank 2
    A = identity( shapeA(1), shapeA(2), like=zero )
    call check( error, all( shape(A) == shapeA ), &
      '`identity` failed for allocatable rank 2 array of type `complex(kind=real64)`.', &
      'Returns incorrect shape.' )
    if (allocated(error)) return
    call check( error, all( [(A(i, i) == one, i=1, minval(shapeA))] ), &
      '`identity` failed for allocatable rank 2 array of type `complex(kind=real64)`.', &
      'Returns non unit diagonal element.' )
    if (allocated(error)) return
    do i = 1, minval( shapeA )
      A(i, i) = zero
    end do
    call check( error, all( A == zero ), &
      '`identity` failed for allocatable rank 2 array of type `complex(kind=real64)`.', &
      'Returns non zero off-diagonal element.' )
    if (allocated(error)) return
    ! check for rank 3
    call to_identity( M(:,1,1), shapeM, fill_shape=fill_shape_M, fill_at=fill_at_M_pass )
    call check( error, all( [(M(fill_at_M_pass(1)+i, fill_at_M_pass(2)+i, fill_at_M_pass(3)+i) == one, i=0, minval(fill_shape_M)-1)] ), &
      '`identity` failed for rank 3 array of type `complex(kind=real64)`.', &
      'Returns non unit diagonal element.' )
    if (allocated(error)) return
    do i = 0, minval( fill_shape_M ) - 1
      M(fill_at_M_pass(1)+i, fill_at_M_pass(2)+i, fill_at_M_pass(3)+i) = zero
    end do
    call check( error, all( M(fill_at_M_pass(1):fill_shape_M(1)+fill_at_M_pass(1)-1, fill_at_M_pass(2):fill_shape_M(2)+fill_at_M_pass(2)-1, fill_at_M_pass(3):fill_shape_M(3)+fill_at_M_pass(3)-1) == zero ), &
      '`identity` failed for rank 3 array of type `complex(kind=real64)`.', &
      'Returns non zero off-diagonal element.' )
    if (allocated(error)) return
    ! check violation of boundary check
    M = 2 * one
    call to_identity( M(:,1,1), shapeM, fill_shape=fill_shape_M, fill_at=fill_at_M_fail )
    call check( error, all( M == 2*one ), &
      '`identity` failed for rank 3 array of type `complex(kind=real64)`.', &
      'Changed array even if boundaries do not conform. Array should be left unchanged.' )
    if (allocated(error)) return

    if (allocated(A)) deallocate( A )
  end subroutine identity_complex64_test
  
end module m_identity_test
