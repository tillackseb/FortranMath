!> description
module arrays
  use, intrinsic :: iso_fortran_env, only : real32, real64
  use, intrinsic :: iso_c_binding
  implicit none
  private
  
  !> description
  type, abstract :: Array
    integer :: rank = 0
    integer, allocatable :: shape(:)
    type(c_ptr) :: ptr = c_null_ptr
  end type Array

  !> description
  type, extends(Array) :: RealArray
    integer :: kind = real64
  contains
    procedure :: bind => bind_real32
    procedure :: get_element => get_element_real32
  end type RealArray

  public :: RealArray

contains

  !> description
  pure subroutine bind_real32( self, T, shapeT )
    class(RealArray), intent(inout) :: self
    real(kind=real32), target, intent(in) :: T
    integer, intent(in) :: shapeT(:)

    self%shape = shapeT
    self%rank = size( self%shape )
    self%ptr = c_loc( T )
  end subroutine bind_real32

  !> description
  pure function get_element_real32( self, indices ) result( e )
    class(RealArray), intent(in) :: self
    integer, intent(in) :: indices(self%rank)
    real(kind=real32) :: e
  end function get_element_real32
  
end module arrays
