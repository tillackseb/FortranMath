!> description
module m_allsame
  use, intrinsic :: iso_fortran_env, only : int32, int64, real32, real64
  use m_allsame_templates
  implicit none
  private
  
  public :: allsame, fill_allsame, zeros, ones

  !> description
  interface allsame
    module procedure :: allsame_rank2_integer32, allsame_square_integer32
    module procedure :: allsame_rank2_integer64, allsame_square_integer64
    module procedure :: allsame_rank2_real32, allsame_square_real32
    module procedure :: allsame_rank2_real64, allsame_square_real64
    module procedure :: allsame_rank2_complex32, allsame_square_complex32
    module procedure :: allsame_rank2_complex64, allsame_square_complex64
  end interface allsame

  !> description
  interface fill_allsame
    module procedure :: allsame_integer32, allsame_real32, allsame_complex32
    module procedure :: allsame_integer64, allsame_real64, allsame_complex64
  end interface fill_allsame

  !> description
  interface zeros
    module procedure :: zeros_rank2_integer32, zeros_square_integer32
  end interface zeros

  !> description
  interface ones
    module procedure :: ones_rank2_integer32, ones_square_integer32
  end interface ones

contains

  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  !:: integer32
  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  pure function allsame_rank2_integer32( m, n, val ) result( mat )
    integer, intent(in) :: m, n
    integer(kind=int32), intent(in) :: val
    integer(kind=kind(val)), allocatable :: mat(:,:)
    allocate( mat(m, n), source=val )
  end function allsame_rank2_integer32
  pure function allsame_square_integer32( n, val ) result( mat )
    integer, intent(in) :: n
    integer(kind=int32), intent(in) :: val
    integer(kind=kind(val)), allocatable :: mat(:,:)
    mat = allsame_rank2_integer32( n, n, val )
  end function allsame_square_integer32
  pure function zeros_rank2_integer32( m, n, like ) result( mat )
    use math_constants, only : zero => ZERO_INTEGER32
    integer, intent(in) :: m, n
    integer(kind=int32), intent(in) :: like
    integer(kind=kind(like)), allocatable :: mat(:,:)
    mat = allsame_rank2_integer32( m, n, zero )
  end function zeros_rank2_integer32
  pure function zeros_square_integer32( n, like ) result( mat )
    integer, intent(in) :: n
    integer(kind=int32), intent(in) :: like
    integer(kind=kind(like)), allocatable :: mat(:,:)
    mat = zeros_rank2_integer32( n, n, like )
  end function zeros_square_integer32
  pure function ones_rank2_integer32( m, n, like ) result( mat )
    use math_constants, only : one => ONE_INTEGER32
    integer, intent(in) :: m, n
    integer(kind=int32), intent(in) :: like
    integer(kind=kind(like)), allocatable :: mat(:,:)
    mat = allsame_rank2_integer32( m, n, one )
  end function ones_rank2_integer32
  pure function ones_square_integer32( n, like ) result( mat )
    integer, intent(in) :: n
    integer(kind=int32), intent(in) :: like
    integer(kind=kind(like)), allocatable :: mat(:,:)
    mat = ones_rank2_integer32( n, n, like )
  end function ones_square_integer32

  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  !:: integer64
  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  pure function allsame_rank2_integer64( m, n, val ) result( mat )
    integer, intent(in) :: m, n
    integer(kind=int64), intent(in) :: val
    integer(kind=kind(val)), allocatable :: mat(:,:)
    allocate( mat(m, n), source=val )
  end function allsame_rank2_integer64
  pure function allsame_square_integer64( n, val ) result( mat )
    integer, intent(in) :: n
    integer(kind=int64), intent(in) :: val
    integer(kind=kind(val)), allocatable :: mat(:,:)
    mat = allsame_rank2_integer64( n, n, val )
  end function allsame_square_integer64
  pure function zeros_rank2_integer64( m, n, like ) result( mat )
    use math_constants, only : zero => ZERO_INTEGER64
    integer, intent(in) :: m, n
    integer(kind=int64), intent(in) :: like
    integer(kind=kind(like)), allocatable :: mat(:,:)
    mat = allsame_rank2_integer64( m, n, zero )
  end function zeros_rank2_integer64
  pure function zeros_square_integer64( n, like ) result( mat )
    integer, intent(in) :: n
    integer(kind=int64), intent(in) :: like
    integer(kind=kind(like)), allocatable :: mat(:,:)
    mat = zeros_rank2_integer64( n, n, like )
  end function zeros_square_integer64
  pure function ones_rank2_integer64( m, n, like ) result( mat )
    use math_constants, only : one => ONE_INTEGER64
    integer, intent(in) :: m, n
    integer(kind=int64), intent(in) :: like
    integer(kind=kind(like)), allocatable :: mat(:,:)
    mat = allsame_rank2_integer64( m, n, one )
  end function ones_rank2_integer64
  pure function ones_square_integer64( n, like ) result( mat )
    integer, intent(in) :: n
    integer(kind=int64), intent(in) :: like
    integer(kind=kind(like)), allocatable :: mat(:,:)
    mat = ones_rank2_integer64( n, n, like )
  end function ones_square_integer64

  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  !:: real32
  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  pure function allsame_rank2_real32( m, n, val ) result( mat )
    integer, intent(in) :: m, n
    real(kind=real32), intent(in) :: val
    real(kind=kind(val)), allocatable :: mat(:,:)
    allocate( mat(m, n), source=val )
  end function allsame_rank2_real32
  pure function allsame_square_real32( n, val ) result( mat )
    integer, intent(in) :: n
    real(kind=real32), intent(in) :: val
    real(kind=kind(val)), allocatable :: mat(:,:)
    mat = allsame_rank2_real32( n, n, val )
  end function allsame_square_real32
  pure function zeros_rank2_real32( m, n, like ) result( mat )
    use math_constants, only : zero => ZERO_REAL32
    integer, intent(in) :: m, n
    real(kind=real32), intent(in) :: like
    real(kind=kind(like)), allocatable :: mat(:,:)
    mat = allsame_rank2_real32( m, n, zero )
  end function zeros_rank2_real32
  pure function zeros_square_real32( n, like ) result( mat )
    integer, intent(in) :: n
    real(kind=real32), intent(in) :: like
    real(kind=kind(like)), allocatable :: mat(:,:)
    mat = zeros_rank2_real32( n, n, like )
  end function zeros_square_real32
  pure function ones_rank2_real32( m, n, like ) result( mat )
    use math_constants, only : one => ONE_REAL32
    integer, intent(in) :: m, n
    real(kind=real32), intent(in) :: like
    real(kind=kind(like)), allocatable :: mat(:,:)
    mat = allsame_rank2_real32( m, n, one )
  end function ones_rank2_real32
  pure function ones_square_real32( n, like ) result( mat )
    integer, intent(in) :: n
    real(kind=real32), intent(in) :: like
    real(kind=kind(like)), allocatable :: mat(:,:)
    mat = ones_rank2_real32( n, n, like )
  end function ones_square_real32

  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  !:: real64
  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  pure function allsame_rank2_real64( m, n, val ) result( mat )
    integer, intent(in) :: m, n
    real(kind=real64), intent(in) :: val
    real(kind=kind(val)), allocatable :: mat(:,:)
    allocate( mat(m, n), source=val )
  end function allsame_rank2_real64
  pure function allsame_square_real64( n, val ) result( mat )
    integer, intent(in) :: n
    real(kind=real64), intent(in) :: val
    real(kind=kind(val)), allocatable :: mat(:,:)
    mat = allsame_rank2_real64( n, n, val )
  end function allsame_square_real64
  pure function zeros_rank2_real64( m, n, like ) result( mat )
    use math_constants, only : zero => ZERO_REAL64
    integer, intent(in) :: m, n
    real(kind=real64), intent(in) :: like
    real(kind=kind(like)), allocatable :: mat(:,:)
    mat = allsame_rank2_real64( m, n, zero )
  end function zeros_rank2_real64
  pure function zeros_square_real64( n, like ) result( mat )
    integer, intent(in) :: n
    real(kind=real64), intent(in) :: like
    real(kind=kind(like)), allocatable :: mat(:,:)
    mat = zeros_rank2_real64( n, n, like )
  end function zeros_square_real64
  pure function ones_rank2_real64( m, n, like ) result( mat )
    use math_constants, only : one => ONE_REAL64
    integer, intent(in) :: m, n
    real(kind=real64), intent(in) :: like
    real(kind=kind(like)), allocatable :: mat(:,:)
    mat = allsame_rank2_real64( m, n, one )
  end function ones_rank2_real64
  pure function ones_square_real64( n, like ) result( mat )
    integer, intent(in) :: n
    real(kind=real64), intent(in) :: like
    real(kind=kind(like)), allocatable :: mat(:,:)
    mat = ones_rank2_real64( n, n, like )
  end function ones_square_real64

  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  !:: complex32
  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  pure function allsame_rank2_complex32( m, n, val ) result( mat )
    integer, intent(in) :: m, n
    complex(kind=real32), intent(in) :: val
    complex(kind=kind(val)), allocatable :: mat(:,:)
    allocate( mat(m, n), source=val )
  end function allsame_rank2_complex32
  pure function allsame_square_complex32( n, val ) result( mat )
    integer, intent(in) :: n
    complex(kind=real32), intent(in) :: val
    complex(kind=kind(val)), allocatable :: mat(:,:)
    mat = allsame_rank2_complex32( n, n, val )
  end function allsame_square_complex32
  pure function zeros_rank2_complex32( m, n, like ) result( mat )
    use math_constants, only : zero => ZERO_COMPLEX32
    integer, intent(in) :: m, n
    complex(kind=real32), intent(in) :: like
    complex(kind=kind(like)), allocatable :: mat(:,:)
    mat = allsame_rank2_complex32( m, n, zero )
  end function zeros_rank2_complex32
  pure function zeros_square_complex32( n, like ) result( mat )
    integer, intent(in) :: n
    complex(kind=real32), intent(in) :: like
    complex(kind=kind(like)), allocatable :: mat(:,:)
    mat = zeros_rank2_complex32( n, n, like )
  end function zeros_square_complex32
  pure function ones_rank2_complex32( m, n, like ) result( mat )
    use math_constants, only : one => ONE_COMPLEX32
    integer, intent(in) :: m, n
    complex(kind=real32), intent(in) :: like
    complex(kind=kind(like)), allocatable :: mat(:,:)
    mat = allsame_rank2_complex32( m, n, one )
  end function ones_rank2_complex32
  pure function ones_square_complex32( n, like ) result( mat )
    integer, intent(in) :: n
    complex(kind=real32), intent(in) :: like
    complex(kind=kind(like)), allocatable :: mat(:,:)
    mat = ones_rank2_complex32( n, n, like )
  end function ones_square_complex32

  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  !:: complex64
  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  pure function allsame_rank2_complex64( m, n, val ) result( mat )
    integer, intent(in) :: m, n
    complex(kind=real64), intent(in) :: val
    complex(kind=kind(val)), allocatable :: mat(:,:)
    allocate( mat(m, n), source=val )
  end function allsame_rank2_complex64
  pure function allsame_square_complex64( n, val ) result( mat )
    integer, intent(in) :: n
    complex(kind=real64), intent(in) :: val
    complex(kind=kind(val)), allocatable :: mat(:,:)
    mat = allsame_rank2_complex64( n, n, val )
  end function allsame_square_complex64
  pure function zeros_rank2_complex64( m, n, like ) result( mat )
    use math_constants, only : zero => ZERO_COMPLEX64
    integer, intent(in) :: m, n
    complex(kind=real64), intent(in) :: like
    complex(kind=kind(like)), allocatable :: mat(:,:)
    mat = allsame_rank2_complex64( m, n, zero )
  end function zeros_rank2_complex64
  pure function zeros_square_complex64( n, like ) result( mat )
    integer, intent(in) :: n
    complex(kind=real64), intent(in) :: like
    complex(kind=kind(like)), allocatable :: mat(:,:)
    mat = zeros_rank2_complex64( n, n, like )
  end function zeros_square_complex64
  pure function ones_rank2_complex64( m, n, like ) result( mat )
    use math_constants, only : one => ONE_COMPLEX64
    integer, intent(in) :: m, n
    complex(kind=real64), intent(in) :: like
    complex(kind=kind(like)), allocatable :: mat(:,:)
    mat = allsame_rank2_complex64( m, n, one )
  end function ones_rank2_complex64
  pure function ones_square_complex64( n, like ) result( mat )
    integer, intent(in) :: n
    complex(kind=real64), intent(in) :: like
    complex(kind=kind(like)), allocatable :: mat(:,:)
    mat = ones_rank2_complex64( n, n, like )
  end function ones_square_complex64

end module m_allsame
