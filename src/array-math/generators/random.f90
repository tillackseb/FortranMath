!> description
! NOTE: Procedures in this module cannot be pure, because intrinsic `random_number` is not pure.
module m_random
  use, intrinsic :: iso_fortran_env, only : int32, int64, real32, real64
  use m_random_templates
  implicit none
  private
  
  public :: random, fill_random

  !> description
  interface random
    module procedure :: random_rank0_integer32, random_rank2_integer32, random_square_integer32
    module procedure :: random_rank0_integer64, random_rank2_integer64, random_square_integer64
    module procedure :: random_rank0_real32, random_rank2_real32, random_square_real32
    module procedure :: random_rank0_real64, random_rank2_real64, random_square_real64
    module procedure :: random_rank0_complex32, random_rank2_complex32, random_square_complex32
    module procedure :: random_rank0_complex64, random_rank2_complex64, random_square_complex64
  end interface random

  !> description
  interface fill_random
    module procedure :: random_integer32, random_real32, random_complex32
    module procedure :: random_integer64, random_real64, random_complex64
  end interface fill_random

contains

  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  !:: integer32
  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  function random_rank0_integer32( like, interval ) result( s )
    integer(kind=int32), intent(in) :: like
    integer(kind=kind(like)), optional, intent(in) :: interval(2)
    integer(kind=kind(like)) :: s
    integer(kind=kind(like)) :: mat(1)
    call random_integer32( mat, [1], interval=interval )
    s = mat(1)
  end function random_rank0_integer32
  function random_rank2_integer32( m, n, like, interval ) result( mat )
    integer, intent(in) :: m, n
    integer(kind=int32), intent(in) :: like
    integer(kind=kind(like)), optional, intent(in) :: interval(2)
    integer(kind=kind(like)), allocatable :: mat(:,:)
    allocate( mat(m, n) )
    call random_integer32( mat, shape(mat), interval=interval )
  end function random_rank2_integer32
  function random_square_integer32( n, like, interval ) result( mat )
    integer, intent(in) :: n
    integer(kind=int32), intent(in) :: like
    integer(kind=kind(like)), optional, intent(in) :: interval(2)
    integer(kind=kind(like)), allocatable :: mat(:,:)
    mat = random_rank2_integer32( n, n, like, interval=interval )
  end function random_square_integer32

  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  !:: integer64
  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  function random_rank0_integer64( like, interval ) result( s )
    integer(kind=int64), intent(in) :: like
    integer(kind=kind(like)), optional, intent(in) :: interval(2)
    integer(kind=kind(like)) :: s
    integer(kind=kind(like)) :: mat(1)
    call random_integer64( mat, [1], interval=interval )
    s = mat(1)
  end function random_rank0_integer64
  function random_rank2_integer64( m, n, like, interval ) result( mat )
    integer, intent(in) :: m, n
    integer(kind=int64), intent(in) :: like
    integer(kind=kind(like)), optional, intent(in) :: interval(2)
    integer(kind=kind(like)), allocatable :: mat(:,:)
    allocate( mat(m, n) )
    call random_integer64( mat, shape(mat), interval=interval )
  end function random_rank2_integer64
  function random_square_integer64( n, like, interval ) result( mat )
    integer, intent(in) :: n
    integer(kind=int64), intent(in) :: like
    integer(kind=kind(like)), optional, intent(in) :: interval(2)
    integer(kind=kind(like)), allocatable :: mat(:,:)
    mat = random_rank2_integer64( n, n, like, interval=interval )
  end function random_square_integer64

  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  !:: real32
  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  function random_rank0_real32( like, interval ) result( s )
    real(kind=int32), intent(in) :: like
    real(kind=kind(like)), optional, intent(in) :: interval(2)
    real(kind=kind(like)) :: s
    real(kind=kind(like)) :: mat(1)
    call random_real32( mat, [1], interval=interval )
    s = mat(1)
  end function random_rank0_real32
  function random_rank2_real32( m, n, like, interval ) result( mat )
    integer, intent(in) :: m, n
    real(kind=int32), intent(in) :: like
    real(kind=kind(like)), optional, intent(in) :: interval(2)
    real(kind=kind(like)), allocatable :: mat(:,:)
    allocate( mat(m, n) )
    call random_real32( mat, shape(mat), interval=interval )
  end function random_rank2_real32
  function random_square_real32( n, like, interval ) result( mat )
    integer, intent(in) :: n
    real(kind=int32), intent(in) :: like
    real(kind=kind(like)), optional, intent(in) :: interval(2)
    real(kind=kind(like)), allocatable :: mat(:,:)
    mat = random_rank2_real32( n, n, like, interval=interval )
  end function random_square_real32

  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  !:: real64
  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  function random_rank0_real64( like, interval ) result( s )
    real(kind=int64), intent(in) :: like
    real(kind=kind(like)), optional, intent(in) :: interval(2)
    real(kind=kind(like)) :: s
    real(kind=kind(like)) :: mat(1)
    call random_real64( mat, [1], interval=interval )
    s = mat(1)
  end function random_rank0_real64
  function random_rank2_real64( m, n, like, interval ) result( mat )
    integer, intent(in) :: m, n
    real(kind=int64), intent(in) :: like
    real(kind=kind(like)), optional, intent(in) :: interval(2)
    real(kind=kind(like)), allocatable :: mat(:,:)
    allocate( mat(m, n) )
    call random_real64( mat, shape(mat), interval=interval )
  end function random_rank2_real64
  function random_square_real64( n, like, interval ) result( mat )
    integer, intent(in) :: n
    real(kind=int64), intent(in) :: like
    real(kind=kind(like)), optional, intent(in) :: interval(2)
    real(kind=kind(like)), allocatable :: mat(:,:)
    mat = random_rank2_real64( n, n, like, interval=interval )
  end function random_square_real64

  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  !:: complex32
  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  function random_rank0_complex32( like, interval ) result( s )
    complex(kind=int32), intent(in) :: like
    complex(kind=kind(like)), optional, intent(in) :: interval(2)
    complex(kind=kind(like)) :: s
    complex(kind=kind(like)) :: mat(1)
    call random_complex32( mat, [1], interval=interval )
    s = mat(1)
  end function random_rank0_complex32
  function random_rank2_complex32( m, n, like, interval ) result( mat )
    integer, intent(in) :: m, n
    complex(kind=int32), intent(in) :: like
    complex(kind=kind(like)), optional, intent(in) :: interval(2)
    complex(kind=kind(like)), allocatable :: mat(:,:)
    allocate( mat(m, n) )
    call random_complex32( mat, shape(mat), interval=interval )
  end function random_rank2_complex32
  function random_square_complex32( n, like, interval ) result( mat )
    integer, intent(in) :: n
    complex(kind=int32), intent(in) :: like
    complex(kind=kind(like)), optional, intent(in) :: interval(2)
    complex(kind=kind(like)), allocatable :: mat(:,:)
    mat = random_rank2_complex32( n, n, like, interval=interval )
  end function random_square_complex32

  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  !:: complex64
  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  function random_rank0_complex64( like, interval ) result( s )
    complex(kind=int64), intent(in) :: like
    complex(kind=kind(like)), optional, intent(in) :: interval(2)
    complex(kind=kind(like)) :: s
    complex(kind=kind(like)) :: mat(1)
    call random_complex64( mat, [1], interval=interval )
    s = mat(1)
  end function random_rank0_complex64
  function random_rank2_complex64( m, n, like, interval ) result( mat )
    integer, intent(in) :: m, n
    complex(kind=int64), intent(in) :: like
    complex(kind=kind(like)), optional, intent(in) :: interval(2)
    complex(kind=kind(like)), allocatable :: mat(:,:)
    allocate( mat(m, n) )
    call random_complex64( mat, shape(mat), interval=interval )
  end function random_rank2_complex64
  function random_square_complex64( n, like, interval ) result( mat )
    integer, intent(in) :: n
    complex(kind=int64), intent(in) :: like
    complex(kind=kind(like)), optional, intent(in) :: interval(2)
    complex(kind=kind(like)), allocatable :: mat(:,:)
    mat = random_rank2_complex64( n, n, like, interval=interval )
  end function random_square_complex64

end module m_random
