program tester
  use, intrinsic :: iso_fortran_env, only : error_unit
  use testdrive, only : run_testsuite, new_testsuite, testsuite_type
  ! add new test suite here
  use array_generators_test, only : array_generators_test_collect => collect
  implicit none

  character(len=*), parameter :: fmt = '("#", *(1x, a))'

  integer :: is, stat
  type(testsuite_type), allocatable :: testsuites(:)
  
  ! add new test suite here
  testsuites = [ &
    new_testsuite( 'array generators', array_generators_test_collect ) &
  ]

  stat = 0
  do is = 1, size(testsuites)
    write( error_unit, '(a)' ) repeat( '#', 80 )
    write( error_unit, fmt ) 'Testing:', testsuites(is)%name
    write( error_unit, '(a)' ) repeat( '#', 80 )
    call run_testsuite( testsuites(is)%collect, error_unit, stat )
  end do

  if (stat > 0) then
    write( error_unit, '(i0, 1x, a)' ) stat, 'test(s) failed!'
    error stop
  end if
end program tester
