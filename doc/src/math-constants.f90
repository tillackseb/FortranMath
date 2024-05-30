!> Collection of important constants.
module math_constants
  use, intrinsic :: iso_fortran_env, only : int32, int64, real32, real64
  implicit none

  ! zero
  integer(kind=int32), parameter    :: ZERO_INTEGER32   = int( 0, kind=int32 )
  integer(kind=int64), parameter    :: ZERO_INTEGER64   = int( 0, kind=int64 )
  real(kind=real32), parameter      :: ZERO_REAL32      = real( 0, kind=real32 )
  real(kind=real64), parameter      :: ZERO_REAL64      = real( 0, kind=real64 )
  complex(kind=real32), parameter   :: ZERO_COMPLEX32   = cmplx( 0, 0, kind=real32 )
  complex(kind=real64), parameter   :: ZERO_COMPLEX64   = cmplx( 0, 0, kind=real64 )

  ! one
  integer(kind=int32), parameter    :: ONE_INTEGER32    = int( 1, kind=int32 )
  integer(kind=int64), parameter    :: ONE_INTEGER64    = int( 1, kind=int64 )
  real(kind=real32), parameter      :: ONE_REAL32       = real( 1, kind=real32 )
  real(kind=real64), parameter      :: ONE_REAL64       = real( 1, kind=real64 )
  complex(kind=real32), parameter   :: ONE_COMPLEX32    = cmplx( 1, 0, kind=real32 )
  complex(kind=real64), parameter   :: ONE_COMPLEX64    = cmplx( 1, 0, kind=real64 )

  ! imaginary unit
  complex(kind=real32), parameter :: I_COMPLEX32 = cmplx( 0, 1, kind=real32 )
  complex(kind=real64), parameter :: I_COMPLEX64 = cmplx( 0, 1, kind=real64 )

  ! Euler's number
  real(kind=real32), parameter :: E_REAL32 = exp( ONE_REAL32 )
  real(kind=real64), parameter :: E_REAL64 = exp( ONE_REAL64 )

  ! Pi
  real(kind=real32), parameter :: PI_REAL32 = 4 * atan( ONE_REAL32 )
  real(kind=real64), parameter :: PI_REAL64 = 4 * atan( ONE_REAL64 )

end module math_constants
