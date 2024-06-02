!FYPP HEADER====================================================================
!
! INFORMATION
! This file has been created by the fypp preprocessor. DO NOT MODIFY THIS FILE!
! Implement changes in the original preprocessed file.
! original file ::       test/array-math/generators/random-test.f90.fpp
! preprocessing time ::  2024-05-31 10:36:32 UTC+0200
!
!END FYPP HEADER================================================================
!> Unit tests for [[m_random(module)]].
module m_random_test
  use, intrinsic :: iso_fortran_env, only : int32, int64, real32, real64
  use testdrive, only : error_type, check
  use m_random
  implicit none
  private

  public :: random_integer32_test
  public :: random_integer64_test
  public :: random_real32_test
  public :: random_real64_test
  public :: random_complex32_test
  public :: random_complex64_test
  
contains

  !> Unit tests for [[m_random(module):identity(interface)]] and [[m_identity(module):fill_identity(interface)]]
  !> with arrays of `type integer(kind=int32)`.
  subroutine random_integer32_test( error )
    use math_constants, only : zero => ZERO_INTEGER32
    !> Error object
    type(error_type), allocatable, intent(out) :: error

    integer, parameter :: interval(4) = [2, 5, 4, 6]
    integer, parameter :: shapeA(2) = [5, 3]
    integer, parameter :: shapeM(3) = [8, 5, 9]
    integer, parameter :: sub_shape_M(3) = [5, 4, 3]
    integer, parameter :: sub_first_M_pass(3) = [2, 1, 3]
    integer, parameter :: sub_first_M_fail(3) = [2, 3, 3]
    integer, parameter :: fill_fill_M(3) = sub_first_M_pass + sub_shape_M - 1

    integer(kind=int32) :: intv(2), s, M(shapeM(1), shapeM(2), shapeM(3))
    integer(kind=int32), allocatable :: A(:,:), Msub(:,:,:)

    intv = interval(:2)
    

    ! check for scalar
    s = random( like=zero, interval=intv )
    call check( error, &
      (real(s - intv(1)) >= 0) .and. (real(s - intv(2)) <= 0), &
      '`random` failed for scalar of type `integer(kind=int32)`.', &
      'Returns value outside interval.' )
    if (allocated(error)) return
    ! check for allocatable rank 2
    A = random( shapeA(1), shapeA(2), like=zero, interval=intv )
    call check( error, all( shape(A) == shapeA ), &
      '`random` failed for allocatable rank 2 array of type `integer(kind=int32)`.', &
      'Returns incorrect shape.' )
    if (allocated(error)) return
    call check( error, &
      all(real(A - intv(1)) >= 0) .and. all(real(A - intv(2)) <= 0), &
      '`random` failed for allocatable rank 2 array of type `integer(kind=int32)`.', &
      'Returns values outside interval.' )
    if (allocated(error)) return
    ! check for rank 3
    M = -intv(1)
    call fill_random( M(:,1,1), shapeM, sub_shape=sub_shape_M, sub_first=sub_first_M_pass, interval=intv )
    Msub = M(sub_first_M_pass(1):fill_fill_M(1), sub_first_M_pass(2):fill_fill_M(2), sub_first_M_pass(3):fill_fill_M(3))
    call check( error, &
      all(real(Msub - intv(1)) >= 0) .and. all(real(Msub - intv(2)) <= 0), &
      '`random` failed for rank 3 array of type `integer(kind=int32)`.', &
      'Returns values outside interval.' )
    if (allocated(error)) return
    M(sub_first_M_pass(1):fill_fill_M(1), sub_first_M_pass(2):fill_fill_M(2), sub_first_M_pass(3):fill_fill_M(3)) = -intv(1)
    call check( error, &
      all( M == -intv(1) ), &
      '`random` failed for rank 3 array of type `integer(kind=int32)`.', &
      'Elements outside fill area were changed.' )
    if (allocated(error)) return
    ! check violation of boundary check
    call fill_random( M(:,1,1), shapeM, sub_shape=sub_shape_M, sub_first=sub_first_M_fail )
    call check( error, &
      all( M == -intv(1) ), &
      '`random` failed for rank 3 array of type `integer(kind=int32)`.', &
      'Changed array even if boundaries do not conform. Array should be left unchanged.' )
    if (allocated(error)) return

    if (allocated(A)) deallocate( A )
    if (allocated(Msub)) deallocate( Msub )
  end subroutine random_integer32_test
  
  !> Unit tests for [[m_random(module):identity(interface)]] and [[m_identity(module):fill_identity(interface)]]
  !> with arrays of `type integer(kind=int64)`.
  subroutine random_integer64_test( error )
    use math_constants, only : zero => ZERO_INTEGER64
    !> Error object
    type(error_type), allocatable, intent(out) :: error

    integer, parameter :: interval(4) = [2, 5, 4, 6]
    integer, parameter :: shapeA(2) = [5, 3]
    integer, parameter :: shapeM(3) = [8, 5, 9]
    integer, parameter :: sub_shape_M(3) = [5, 4, 3]
    integer, parameter :: sub_first_M_pass(3) = [2, 1, 3]
    integer, parameter :: sub_first_M_fail(3) = [2, 3, 3]
    integer, parameter :: fill_fill_M(3) = sub_first_M_pass + sub_shape_M - 1

    integer(kind=int64) :: intv(2), s, M(shapeM(1), shapeM(2), shapeM(3))
    integer(kind=int64), allocatable :: A(:,:), Msub(:,:,:)

    intv = interval(:2)
    

    ! check for scalar
    s = random( like=zero, interval=intv )
    call check( error, &
      (real(s - intv(1)) >= 0) .and. (real(s - intv(2)) <= 0), &
      '`random` failed for scalar of type `integer(kind=int64)`.', &
      'Returns value outside interval.' )
    if (allocated(error)) return
    ! check for allocatable rank 2
    A = random( shapeA(1), shapeA(2), like=zero, interval=intv )
    call check( error, all( shape(A) == shapeA ), &
      '`random` failed for allocatable rank 2 array of type `integer(kind=int64)`.', &
      'Returns incorrect shape.' )
    if (allocated(error)) return
    call check( error, &
      all(real(A - intv(1)) >= 0) .and. all(real(A - intv(2)) <= 0), &
      '`random` failed for allocatable rank 2 array of type `integer(kind=int64)`.', &
      'Returns values outside interval.' )
    if (allocated(error)) return
    ! check for rank 3
    M = -intv(1)
    call fill_random( M(:,1,1), shapeM, sub_shape=sub_shape_M, sub_first=sub_first_M_pass, interval=intv )
    Msub = M(sub_first_M_pass(1):fill_fill_M(1), sub_first_M_pass(2):fill_fill_M(2), sub_first_M_pass(3):fill_fill_M(3))
    call check( error, &
      all(real(Msub - intv(1)) >= 0) .and. all(real(Msub - intv(2)) <= 0), &
      '`random` failed for rank 3 array of type `integer(kind=int64)`.', &
      'Returns values outside interval.' )
    if (allocated(error)) return
    M(sub_first_M_pass(1):fill_fill_M(1), sub_first_M_pass(2):fill_fill_M(2), sub_first_M_pass(3):fill_fill_M(3)) = -intv(1)
    call check( error, &
      all( M == -intv(1) ), &
      '`random` failed for rank 3 array of type `integer(kind=int64)`.', &
      'Elements outside fill area were changed.' )
    if (allocated(error)) return
    ! check violation of boundary check
    call fill_random( M(:,1,1), shapeM, sub_shape=sub_shape_M, sub_first=sub_first_M_fail )
    call check( error, &
      all( M == -intv(1) ), &
      '`random` failed for rank 3 array of type `integer(kind=int64)`.', &
      'Changed array even if boundaries do not conform. Array should be left unchanged.' )
    if (allocated(error)) return

    if (allocated(A)) deallocate( A )
    if (allocated(Msub)) deallocate( Msub )
  end subroutine random_integer64_test
  
  !> Unit tests for [[m_random(module):identity(interface)]] and [[m_identity(module):fill_identity(interface)]]
  !> with arrays of `type real(kind=real32)`.
  subroutine random_real32_test( error )
    use math_constants, only : zero => ZERO_REAL32
    !> Error object
    type(error_type), allocatable, intent(out) :: error

    integer, parameter :: interval(4) = [2, 5, 4, 6]
    integer, parameter :: shapeA(2) = [5, 3]
    integer, parameter :: shapeM(3) = [8, 5, 9]
    integer, parameter :: sub_shape_M(3) = [5, 4, 3]
    integer, parameter :: sub_first_M_pass(3) = [2, 1, 3]
    integer, parameter :: sub_first_M_fail(3) = [2, 3, 3]
    integer, parameter :: fill_fill_M(3) = sub_first_M_pass + sub_shape_M - 1

    real(kind=real32) :: intv(2), s, M(shapeM(1), shapeM(2), shapeM(3))
    real(kind=real32), allocatable :: A(:,:), Msub(:,:,:)

    intv = interval(:2)
    

    ! check for scalar
    s = random( like=zero, interval=intv )
    call check( error, &
      (real(s - intv(1)) >= 0) .and. (real(s - intv(2)) <= 0), &
      '`random` failed for scalar of type `real(kind=real32)`.', &
      'Returns value outside interval.' )
    if (allocated(error)) return
    ! check for allocatable rank 2
    A = random( shapeA(1), shapeA(2), like=zero, interval=intv )
    call check( error, all( shape(A) == shapeA ), &
      '`random` failed for allocatable rank 2 array of type `real(kind=real32)`.', &
      'Returns incorrect shape.' )
    if (allocated(error)) return
    call check( error, &
      all(real(A - intv(1)) >= 0) .and. all(real(A - intv(2)) <= 0), &
      '`random` failed for allocatable rank 2 array of type `real(kind=real32)`.', &
      'Returns values outside interval.' )
    if (allocated(error)) return
    ! check for rank 3
    M = -intv(1)
    call fill_random( M(:,1,1), shapeM, sub_shape=sub_shape_M, sub_first=sub_first_M_pass, interval=intv )
    Msub = M(sub_first_M_pass(1):fill_fill_M(1), sub_first_M_pass(2):fill_fill_M(2), sub_first_M_pass(3):fill_fill_M(3))
    call check( error, &
      all(real(Msub - intv(1)) >= 0) .and. all(real(Msub - intv(2)) <= 0), &
      '`random` failed for rank 3 array of type `real(kind=real32)`.', &
      'Returns values outside interval.' )
    if (allocated(error)) return
    M(sub_first_M_pass(1):fill_fill_M(1), sub_first_M_pass(2):fill_fill_M(2), sub_first_M_pass(3):fill_fill_M(3)) = -intv(1)
    call check( error, &
      all( M == -intv(1) ), &
      '`random` failed for rank 3 array of type `real(kind=real32)`.', &
      'Elements outside fill area were changed.' )
    if (allocated(error)) return
    ! check violation of boundary check
    call fill_random( M(:,1,1), shapeM, sub_shape=sub_shape_M, sub_first=sub_first_M_fail )
    call check( error, &
      all( M == -intv(1) ), &
      '`random` failed for rank 3 array of type `real(kind=real32)`.', &
      'Changed array even if boundaries do not conform. Array should be left unchanged.' )
    if (allocated(error)) return

    if (allocated(A)) deallocate( A )
    if (allocated(Msub)) deallocate( Msub )
  end subroutine random_real32_test
  
  !> Unit tests for [[m_random(module):identity(interface)]] and [[m_identity(module):fill_identity(interface)]]
  !> with arrays of `type real(kind=real64)`.
  subroutine random_real64_test( error )
    use math_constants, only : zero => ZERO_REAL64
    !> Error object
    type(error_type), allocatable, intent(out) :: error

    integer, parameter :: interval(4) = [2, 5, 4, 6]
    integer, parameter :: shapeA(2) = [5, 3]
    integer, parameter :: shapeM(3) = [8, 5, 9]
    integer, parameter :: sub_shape_M(3) = [5, 4, 3]
    integer, parameter :: sub_first_M_pass(3) = [2, 1, 3]
    integer, parameter :: sub_first_M_fail(3) = [2, 3, 3]
    integer, parameter :: fill_fill_M(3) = sub_first_M_pass + sub_shape_M - 1

    real(kind=real64) :: intv(2), s, M(shapeM(1), shapeM(2), shapeM(3))
    real(kind=real64), allocatable :: A(:,:), Msub(:,:,:)

    intv = interval(:2)
    

    ! check for scalar
    s = random( like=zero, interval=intv )
    call check( error, &
      (real(s - intv(1)) >= 0) .and. (real(s - intv(2)) <= 0), &
      '`random` failed for scalar of type `real(kind=real64)`.', &
      'Returns value outside interval.' )
    if (allocated(error)) return
    ! check for allocatable rank 2
    A = random( shapeA(1), shapeA(2), like=zero, interval=intv )
    call check( error, all( shape(A) == shapeA ), &
      '`random` failed for allocatable rank 2 array of type `real(kind=real64)`.', &
      'Returns incorrect shape.' )
    if (allocated(error)) return
    call check( error, &
      all(real(A - intv(1)) >= 0) .and. all(real(A - intv(2)) <= 0), &
      '`random` failed for allocatable rank 2 array of type `real(kind=real64)`.', &
      'Returns values outside interval.' )
    if (allocated(error)) return
    ! check for rank 3
    M = -intv(1)
    call fill_random( M(:,1,1), shapeM, sub_shape=sub_shape_M, sub_first=sub_first_M_pass, interval=intv )
    Msub = M(sub_first_M_pass(1):fill_fill_M(1), sub_first_M_pass(2):fill_fill_M(2), sub_first_M_pass(3):fill_fill_M(3))
    call check( error, &
      all(real(Msub - intv(1)) >= 0) .and. all(real(Msub - intv(2)) <= 0), &
      '`random` failed for rank 3 array of type `real(kind=real64)`.', &
      'Returns values outside interval.' )
    if (allocated(error)) return
    M(sub_first_M_pass(1):fill_fill_M(1), sub_first_M_pass(2):fill_fill_M(2), sub_first_M_pass(3):fill_fill_M(3)) = -intv(1)
    call check( error, &
      all( M == -intv(1) ), &
      '`random` failed for rank 3 array of type `real(kind=real64)`.', &
      'Elements outside fill area were changed.' )
    if (allocated(error)) return
    ! check violation of boundary check
    call fill_random( M(:,1,1), shapeM, sub_shape=sub_shape_M, sub_first=sub_first_M_fail )
    call check( error, &
      all( M == -intv(1) ), &
      '`random` failed for rank 3 array of type `real(kind=real64)`.', &
      'Changed array even if boundaries do not conform. Array should be left unchanged.' )
    if (allocated(error)) return

    if (allocated(A)) deallocate( A )
    if (allocated(Msub)) deallocate( Msub )
  end subroutine random_real64_test
  
  !> Unit tests for [[m_random(module):identity(interface)]] and [[m_identity(module):fill_identity(interface)]]
  !> with arrays of `type complex(kind=real32)`.
  subroutine random_complex32_test( error )
    use math_constants, only : zero => ZERO_COMPLEX32
    !> Error object
    type(error_type), allocatable, intent(out) :: error

    integer, parameter :: interval(4) = [2, 5, 4, 6]
    integer, parameter :: shapeA(2) = [5, 3]
    integer, parameter :: shapeM(3) = [8, 5, 9]
    integer, parameter :: sub_shape_M(3) = [5, 4, 3]
    integer, parameter :: sub_first_M_pass(3) = [2, 1, 3]
    integer, parameter :: sub_first_M_fail(3) = [2, 3, 3]
    integer, parameter :: fill_fill_M(3) = sub_first_M_pass + sub_shape_M - 1

    complex(kind=real32) :: intv(2), s, M(shapeM(1), shapeM(2), shapeM(3))
    complex(kind=real32), allocatable :: A(:,:), Msub(:,:,:)

    intv = interval(:2)
    intv = intv + cmplx( 0, interval(3:), kind=real32 )

    ! check for scalar
    s = random( like=zero, interval=intv )
    call check( error, &
      (real(s - intv(1)) >= 0) .and. (real(s - intv(2)) <= 0) .and. (aimag(s - intv(1)) >= 0) .and. (aimag(s - intv(2)) <= 0), &
      '`random` failed for scalar of type `complex(kind=real32)`.', &
      'Returns value outside interval.' )
    if (allocated(error)) return
    ! check for allocatable rank 2
    A = random( shapeA(1), shapeA(2), like=zero, interval=intv )
    call check( error, all( shape(A) == shapeA ), &
      '`random` failed for allocatable rank 2 array of type `complex(kind=real32)`.', &
      'Returns incorrect shape.' )
    if (allocated(error)) return
    call check( error, &
      all(real(A - intv(1)) >= 0) .and. all(real(A - intv(2)) <= 0) .and. all(aimag(A - intv(1)) >= 0) .and. all(aimag(A - intv(2)) <= 0), &
      '`random` failed for allocatable rank 2 array of type `complex(kind=real32)`.', &
      'Returns values outside interval.' )
    if (allocated(error)) return
    ! check for rank 3
    M = -intv(1)
    call fill_random( M(:,1,1), shapeM, sub_shape=sub_shape_M, sub_first=sub_first_M_pass, interval=intv )
    Msub = M(sub_first_M_pass(1):fill_fill_M(1), sub_first_M_pass(2):fill_fill_M(2), sub_first_M_pass(3):fill_fill_M(3))
    call check( error, &
      all(real(Msub - intv(1)) >= 0) .and. all(real(Msub - intv(2)) <= 0) .and. all(aimag(Msub - intv(1)) >= 0) .and. all(aimag(Msub - intv(2)) <= 0), &
      '`random` failed for rank 3 array of type `complex(kind=real32)`.', &
      'Returns values outside interval.' )
    if (allocated(error)) return
    M(sub_first_M_pass(1):fill_fill_M(1), sub_first_M_pass(2):fill_fill_M(2), sub_first_M_pass(3):fill_fill_M(3)) = -intv(1)
    call check( error, &
      all( M == -intv(1) ), &
      '`random` failed for rank 3 array of type `complex(kind=real32)`.', &
      'Elements outside fill area were changed.' )
    if (allocated(error)) return
    ! check violation of boundary check
    call fill_random( M(:,1,1), shapeM, sub_shape=sub_shape_M, sub_first=sub_first_M_fail )
    call check( error, &
      all( M == -intv(1) ), &
      '`random` failed for rank 3 array of type `complex(kind=real32)`.', &
      'Changed array even if boundaries do not conform. Array should be left unchanged.' )
    if (allocated(error)) return

    if (allocated(A)) deallocate( A )
    if (allocated(Msub)) deallocate( Msub )
  end subroutine random_complex32_test
  
  !> Unit tests for [[m_random(module):identity(interface)]] and [[m_identity(module):fill_identity(interface)]]
  !> with arrays of `type complex(kind=real64)`.
  subroutine random_complex64_test( error )
    use math_constants, only : zero => ZERO_COMPLEX64
    !> Error object
    type(error_type), allocatable, intent(out) :: error

    integer, parameter :: interval(4) = [2, 5, 4, 6]
    integer, parameter :: shapeA(2) = [5, 3]
    integer, parameter :: shapeM(3) = [8, 5, 9]
    integer, parameter :: sub_shape_M(3) = [5, 4, 3]
    integer, parameter :: sub_first_M_pass(3) = [2, 1, 3]
    integer, parameter :: sub_first_M_fail(3) = [2, 3, 3]
    integer, parameter :: fill_fill_M(3) = sub_first_M_pass + sub_shape_M - 1

    complex(kind=real64) :: intv(2), s, M(shapeM(1), shapeM(2), shapeM(3))
    complex(kind=real64), allocatable :: A(:,:), Msub(:,:,:)

    intv = interval(:2)
    intv = intv + cmplx( 0, interval(3:), kind=real64 )

    ! check for scalar
    s = random( like=zero, interval=intv )
    call check( error, &
      (real(s - intv(1)) >= 0) .and. (real(s - intv(2)) <= 0) .and. (aimag(s - intv(1)) >= 0) .and. (aimag(s - intv(2)) <= 0), &
      '`random` failed for scalar of type `complex(kind=real64)`.', &
      'Returns value outside interval.' )
    if (allocated(error)) return
    ! check for allocatable rank 2
    A = random( shapeA(1), shapeA(2), like=zero, interval=intv )
    call check( error, all( shape(A) == shapeA ), &
      '`random` failed for allocatable rank 2 array of type `complex(kind=real64)`.', &
      'Returns incorrect shape.' )
    if (allocated(error)) return
    call check( error, &
      all(real(A - intv(1)) >= 0) .and. all(real(A - intv(2)) <= 0) .and. all(aimag(A - intv(1)) >= 0) .and. all(aimag(A - intv(2)) <= 0), &
      '`random` failed for allocatable rank 2 array of type `complex(kind=real64)`.', &
      'Returns values outside interval.' )
    if (allocated(error)) return
    ! check for rank 3
    M = -intv(1)
    call fill_random( M(:,1,1), shapeM, sub_shape=sub_shape_M, sub_first=sub_first_M_pass, interval=intv )
    Msub = M(sub_first_M_pass(1):fill_fill_M(1), sub_first_M_pass(2):fill_fill_M(2), sub_first_M_pass(3):fill_fill_M(3))
    call check( error, &
      all(real(Msub - intv(1)) >= 0) .and. all(real(Msub - intv(2)) <= 0) .and. all(aimag(Msub - intv(1)) >= 0) .and. all(aimag(Msub - intv(2)) <= 0), &
      '`random` failed for rank 3 array of type `complex(kind=real64)`.', &
      'Returns values outside interval.' )
    if (allocated(error)) return
    M(sub_first_M_pass(1):fill_fill_M(1), sub_first_M_pass(2):fill_fill_M(2), sub_first_M_pass(3):fill_fill_M(3)) = -intv(1)
    call check( error, &
      all( M == -intv(1) ), &
      '`random` failed for rank 3 array of type `complex(kind=real64)`.', &
      'Elements outside fill area were changed.' )
    if (allocated(error)) return
    ! check violation of boundary check
    call fill_random( M(:,1,1), shapeM, sub_shape=sub_shape_M, sub_first=sub_first_M_fail )
    call check( error, &
      all( M == -intv(1) ), &
      '`random` failed for rank 3 array of type `complex(kind=real64)`.', &
      'Changed array even if boundaries do not conform. Array should be left unchanged.' )
    if (allocated(error)) return

    if (allocated(A)) deallocate( A )
    if (allocated(Msub)) deallocate( Msub )
  end subroutine random_complex64_test
  
end module m_random_test
