!> description
module m_diagonal
  use, intrinsic :: iso_fortran_env, only : int32, int64, real32, real64
  use m_diagonal_templates
  implicit none
  private
  
  public :: diagonal, fill_diagonal, get_diagonal

  !> description
  interface diagonal
    module procedure :: diagonal_square_integer32, get_diagonal_rank2_integer32
  end interface diagonal

  !> description
  interface fill_diagonal
    module procedure :: diagonal_integer32, diagonal_real32, diagonal_complex32
    module procedure :: diagonal_integer64, diagonal_real64, diagonal_complex64
  end interface fill_diagonal

  !> description
  interface get_diagonal
    module procedure :: get_diagonal_integer32, get_diagonal_real32, get_diagonal_complex32
    module procedure :: get_diagonal_integer64, get_diagonal_real64, get_diagonal_complex64
  end interface get_diagonal

contains

  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  !:: integer32
  !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  pure function diagonal_square_integer32( diag, sub_diag ) result( mat )
    use math_constants, only : zero => ZERO_INTEGER32
    use m_allsame, only : zeros
    integer(kind=int32), intent(in) :: diag(:)
    integer, optional, intent(in) :: sub_diag
    integer(kind=kind(diag)), allocatable :: mat(:,:)
    integer :: n, off
    off = 0
    if (present(sub_diag)) off = sub_diag
    n = size( diag )
    mat = zeros( n+abs(off), like=zero )
    call fill_diagonal( mat(:, 1), shape(mat), diag, sub_shape=[n, n], sub_first=1+[0, off]-min(0, off) )
  end function diagonal_square_integer32
  !> description
  pure function get_diagonal_rank2_integer32( mat, sub_diag ) result( diag )
    integer(kind=int32), intent(in) :: mat(:,:)
    integer, optional, intent(in) :: sub_diag
    integer(kind=int32), allocatable :: diag(:)
    integer :: n, off
    off = 0
    if (present(sub_diag)) off = sub_diag
    n = max( 0, minval( shape(mat) - ([0, off] - min(0, off)) ) )
    allocate( diag(n) )
    if (n > 0) &
      call get_diagonal( mat(:, 1), shape(mat), diag, sub_shape=[n, n], sub_first=1+[0, off]-min(0, off) )
  end function get_diagonal_rank2_integer32
  
end module m_diagonal
