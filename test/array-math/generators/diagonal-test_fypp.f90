!FYPP HEADER====================================================================
!
! INFORMATION
! This file has been created by the fypp preprocessor. DO NOT MODIFY THIS FILE!
! Implement changes in the original preprocessed file.
! original file ::       test/array-math/generators/diagonal-test.f90.fpp
! preprocessing time ::  2024-06-04 18:46:12 UTC+0200
!
!END FYPP HEADER================================================================
!> Unit tests for [[m_diagonal(module)]].
module m_diagonal_test
  use, intrinsic :: iso_fortran_env, only : int32, int64, real32, real64
  use testdrive, only : error_type, check
  use m_diagonal
  implicit none
  private

  public :: diagonal_integer32_test
  
contains

  !> Unit tests for [[m_diagonal(module):diagonal(interface)]], [[m_diagonal(module):fill_diagonal(interface)]]
  !> and [[m_diagonal(module):get_diagonal(interface)]] with arrays of `type integer(kind=int32)`.
  subroutine diagonal_integer32_test( error )
    use math_constants, only : zero => ZERO_INTEGER32, one => ONE_INTEGER32
    !> Error object
    type(error_type), allocatable, intent(out) :: error

    integer, parameter :: n = 5
    integer, parameter :: sub_diag(3) = [-2, 0, 3]
    integer, parameter :: shapeM(3) = [8, 5, 9]
    integer, parameter :: sub_shape_M(3) = [5, 4, 3]
    integer, parameter :: sub_first_M_pass(3) = [2, 1, 3]
    integer, parameter :: sub_first_M_fail(3) = [2, 3, 3]
    integer, parameter :: fill_fill_M(3) = sub_first_M_pass + sub_shape_M - 1

    integer :: isub, i
    integer(kind=int32) :: diag_put(n), M(shapeM(1), shapeM(2), shapeM(3))
    integer(kind=int32), allocatable :: diag_get(:), A(:,:), Msub(:,:,:)

    do isub = 1, size( sub_diag )
      ! check for allocatable rank 2
      diag_put = one * [(i, i=1, n)]
      A = diagonal( diag_put, sub_diag=sub_diag(isub) )
      diag_get = diagonal( A, sub_diag=sub_diag(isub) )
      call check( error, &
        all( shape(A) == [n+abs(sub_diag(isub)), n+abs(sub_diag(isub))] ), &
        '`diagonal` failed for allocatable rank 2 array of type `integer(kind=int32)`.', &
        'Returns incorrect shape.' )
      if (allocated(error)) return
      if (sub_diag(isub) >= 0) then
        call check( error, &
          all( [(A(i, i+sub_diag(isub)) == diag_put(i), i=1, n)] ), &
          '`diagonal` failed for allocatable rank 2 array of type `integer(kind=int32)`.', &
          'Returns incorrect diagonal element.' )
        do i = 1, n
          A(i, i+sub_diag(isub)) = zero
        end do
      else
        call check( error, &
          all( [(A(i-sub_diag(isub), i) == diag_put(i), i=1, n)] ), &
          '`diagonal` failed for allocatable rank 2 array of type `integer(kind=int32)`.', &
          'Returns incorrect diagonal element.' )
        do i = 1, n
          A(i-sub_diag(isub), i) = zero
        end do
      end if
      if (allocated(error)) return
      call check( error, &
        all( A == zero ), &
        '`diagonal` failed for allocatable rank 2 array of type `integer(kind=int32)`.', &
        'Returns non zero off-diagonal element.' )
      if (allocated(error)) return
      ! check for allocatable rank 1
      call check( error, &
        all( diag_put == diag_get ), &
        '`diagonal` failed for allocatable rank 1 array of type `integer(kind=int32)`.', &
        'Returns incorrect diagonal element.' )
      if (allocated(error)) return
    end do
    ! check for rank 3
    M = -one
    call fill_diagonal( M(:,1,1), shapeM, diag_put, sub_shape=sub_shape_M, sub_first=sub_first_M_pass )
    Msub = M(sub_first_M_pass(1):fill_fill_M(1), sub_first_M_pass(2):fill_fill_M(2), sub_first_M_pass(3):fill_fill_M(3))
    call check( error, &
      all( [(Msub(i, i, i) == diag_put(i), i=1, minval(sub_shape_M))] ), &
      '`diagonal` failed for rank 3 array of type `integer(kind=int32)`.', &
      'Returns incorrect diagonal element.' )
    if (allocated(error)) return
    !do i = 1, minval( sub_shape_M )
    !  Msub(i, i, i) = zero
    !end do
    !call check( error, &
    !  all( Msub == zero ), &
    !  '`diagonal` failed for rank 3 array of type `integer(kind=int32)`.', &
    !  'Returns non zero off-diagonal element.' )
    !if (allocated(error)) return
    !M(sub_first_M_pass(1):fill_fill_M(1), sub_first_M_pass(2):fill_fill_M(2), sub_first_M_pass(3):fill_fill_M(3)) = -one
    !call check( error, &
    !  all( M == -one ), &
    !  '`diagonal` failed for rank 3 array of type `integer(kind=int32)`.', &
    !  'Elements outside fill area were changed.' )
    !if (allocated(error)) return
    !! check violation of boundary check
    !call fill_diagonal( M(:,1,1), shapeM, sub_shape=sub_shape_M, sub_first=sub_first_M_fail )
    !call check( error, &
    !  all( M == -one ), &
    !  '`diagonal` failed for rank 3 array of type `integer(kind=int32)`.', &
    !  'Changed array even if boundaries do not conform. Array should be left unchanged.' )
    !if (allocated(error)) return

    !if (allocated(A)) deallocate( A )
    !if (allocated(Msub)) deallocate( Msub )
  end subroutine diagonal_integer32_test
  
end module m_diagonal_test
