!> description
module m_identity
  use, intrinsic :: iso_fortran_env, only : int32, int64, real32, real64
  use m_identity_templates
  implicit none
  private
  
  public :: identity, fill_identity

  !> description
  interface identity
    module procedure :: identity_rank0_integer32, identity_rank2_integer32, identity_square_integer32
    module procedure :: identity_rank0_integer64, identity_rank2_integer64, identity_square_integer64
    module procedure :: identity_rank0_real32, identity_rank2_real32, identity_square_real32
    module procedure :: identity_rank0_real64, identity_rank2_real64, identity_square_real64
    module procedure :: identity_rank0_complex32, identity_rank2_complex32, identity_square_complex32
    module procedure :: identity_rank0_complex64, identity_rank2_complex64, identity_square_complex64
  end interface identity

  !> description
  interface fill_identity
    module procedure :: identity_integer32, identity_real32, identity_complex32
    module procedure :: identity_integer64, identity_real64, identity_complex64
  end interface fill_identity

contains

  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  !:: integer32
  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  pure function identity_rank0_integer32( like ) result( s )
    integer(kind=int32), intent(in) :: like
    integer(kind=kind(like)) :: s
    integer(kind=kind(like)) :: mat(1)
    call identity_integer32( mat, [1] )
    s = mat(1)
  end function identity_rank0_integer32
  pure function identity_rank2_integer32( m, n, like ) result( mat )
    integer, intent(in) :: m, n
    integer(kind=int32), intent(in) :: like
    integer(kind=kind(like)), allocatable :: mat(:,:)
    allocate( mat(m, n) )
    call identity_integer32( mat, shape(mat) )
  end function identity_rank2_integer32
  pure function identity_square_integer32( n, like ) result( mat )
    integer, intent(in) :: n
    integer(kind=int32), intent(in) :: like
    integer(kind=kind(like)), allocatable :: mat(:,:)
    mat = identity_rank2_integer32( n, n, like )
  end function identity_square_integer32

  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  !:: integer64
  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  pure function identity_rank0_integer64( like ) result( s )
    integer(kind=int64), intent(in) :: like
    integer(kind=kind(like)) :: s
    integer(kind=kind(like)) :: mat(1)
    call identity_integer64( mat, [1] )
    s = mat(1)
  end function identity_rank0_integer64
  pure function identity_rank2_integer64( m, n, like ) result( mat )
    integer, intent(in) :: m, n
    integer(kind=int64), intent(in) :: like
    integer(kind=kind(like)), allocatable :: mat(:,:)
    allocate( mat(m, n) )
    call identity_integer64( mat, shape(mat) )
  end function identity_rank2_integer64
  pure function identity_square_integer64( n, like ) result( mat )
    integer, intent(in) :: n
    integer(kind=int64), intent(in) :: like
    integer(kind=kind(like)), allocatable :: mat(:,:)
    mat = identity_rank2_integer64( n, n, like )
  end function identity_square_integer64

  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  !:: real32
  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  pure function identity_rank0_real32( like ) result( s )
    real(kind=real32), intent(in) :: like
    real(kind=kind(like)) :: s
    real(kind=kind(like)) :: mat(1)
    call identity_real32( mat, [1] )
    s = mat(1)
  end function identity_rank0_real32
  pure function identity_rank2_real32( m, n, like ) result( mat )
    integer, intent(in) :: m, n
    real(kind=real32), intent(in) :: like
    real(kind=kind(like)), allocatable :: mat(:,:)
    allocate( mat(m, n) )
    call identity_real32( mat, shape(mat) )
  end function identity_rank2_real32
  pure function identity_square_real32( n, like ) result( mat )
    integer, intent(in) :: n
    real(kind=real32), intent(in) :: like
    real(kind=kind(like)), allocatable :: mat(:,:)
    mat = identity_rank2_real32( n, n, like )
  end function identity_square_real32

  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  !:: real64
  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  pure function identity_rank0_real64( like ) result( s )
    real(kind=real64), intent(in) :: like
    real(kind=kind(like)) :: s
    real(kind=kind(like)) :: mat(1)
    call identity_real64( mat, [1] )
    s = mat(1)
  end function identity_rank0_real64
  pure function identity_rank2_real64( m, n, like ) result( mat )
    integer, intent(in) :: m, n
    real(kind=real64), intent(in) :: like
    real(kind=kind(like)), allocatable :: mat(:,:)
    allocate( mat(m, n) )
    call identity_real64( mat, shape(mat) )
  end function identity_rank2_real64
  pure function identity_square_real64( n, like ) result( mat )
    integer, intent(in) :: n
    real(kind=real64), intent(in) :: like
    real(kind=kind(like)), allocatable :: mat(:,:)
    mat = identity_rank2_real64( n, n, like )
  end function identity_square_real64

  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  !:: complex32
  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  pure function identity_rank0_complex32( like ) result( s )
    complex(kind=real32), intent(in) :: like
    complex(kind=kind(like)) :: s
    complex(kind=kind(like)) :: mat(1)
    call identity_complex32( mat, [1] )
    s = mat(1)
  end function identity_rank0_complex32
  pure function identity_rank2_complex32( m, n, like ) result( mat )
    integer, intent(in) :: m, n
    complex(kind=real32), intent(in) :: like
    complex(kind=kind(like)), allocatable :: mat(:,:)
    allocate( mat(m, n) )
    call identity_complex32( mat, shape(mat) )
  end function identity_rank2_complex32
  pure function identity_square_complex32( n, like ) result( mat )
    integer, intent(in) :: n
    complex(kind=real32), intent(in) :: like
    complex(kind=kind(like)), allocatable :: mat(:,:)
    mat = identity_rank2_complex32( n, n, like )
  end function identity_square_complex32

  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  !:: complex64
  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  pure function identity_rank0_complex64( like ) result( s )
    complex(kind=real64), intent(in) :: like
    complex(kind=kind(like)) :: s
    complex(kind=kind(like)) :: mat(1)
    call identity_complex64( mat, [1] )
    s = mat(1)
  end function identity_rank0_complex64
  pure function identity_rank2_complex64( m, n, like ) result( mat )
    integer, intent(in) :: m, n
    complex(kind=real64), intent(in) :: like
    complex(kind=kind(like)), allocatable :: mat(:,:)
    allocate( mat(m, n) )
    call identity_complex64( mat, shape(mat) )
  end function identity_rank2_complex64
  pure function identity_square_complex64( n, like ) result( mat )
    integer, intent(in) :: n
    complex(kind=real64), intent(in) :: like
    complex(kind=kind(like)), allocatable :: mat(:,:)
    mat = identity_rank2_complex64( n, n, like )
  end function identity_square_complex64

end module m_identity
