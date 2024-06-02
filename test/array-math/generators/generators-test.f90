!> Unit tests for [[array_generators(module)]].
module array_generators_test
  use, intrinsic :: iso_fortran_env, only : int32, int64, real32, real64
  use testdrive, only : error_type, check

  use array_generators, only : &
    identity, fill_identity, &
    random, fill_random

  implicit none
  private

  public :: collect

contains

  !> Collect all exported unit tests
  subroutine collect( testsuite )
    use testdrive, only : unittest_type, new_unittest
    !> Collection of tests
    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    ! add new tests here
    testsuite = [ &
      new_unittest( 'identity', test_identity ), &
      new_unittest( 'random matrix', test_random ) &
    ]
  end subroutine collect

  !> Test module [[m_identity(module)]].
  subroutine test_identity( error )
    use m_identity_test
    !> error handling
    type(error_type), allocatable, intent(out) :: error

    call identity_integer32_test( error )
    call identity_integer64_test( error )
    call identity_real32_test( error )
    call identity_real64_test( error )
    call identity_complex32_test( error )
    call identity_complex64_test( error )
  end subroutine test_identity
  
  !> Test module [[m_random(module)]].
  subroutine test_random( error )
    use m_random_test
    !> error handling
    type(error_type), allocatable, intent(out) :: error

    call random_integer32_test( error )
    call random_integer64_test( error )
    call random_real32_test( error )
    call random_real64_test( error )
    call random_complex32_test( error )
    call random_complex64_test( error )
  end subroutine test_random
  
end module array_generators_test
