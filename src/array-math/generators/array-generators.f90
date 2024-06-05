!> Collection of various array generator procedures.
!>
!>## List of Procedures
!>{!./array_generators.md!}
module array_generators
  use m_allsame, only : allsame, fill_allsame, zeros, ones
  use m_identity, only : identity, fill_identity, &
    eye => identity, fill_eye => fill_identity
  use m_random, only : random, fill_random, &
    rand => random, fill_rand => fill_random
  use m_diagonal, only : diagonal, fill_diagonal, get_diagonal, &
    diag => diagonal, fill_diag => fill_diagonal, get_diag => get_diagonal
  implicit none
  private

  public :: allsame, fill_allsame, zeros, ones
  public :: identity, eye, fill_identity, fill_eye
  public :: random, rand, fill_random, fill_rand
  public :: diagonal, diag, fill_diagonal, fill_diag, get_diagonal, get_diag
end module array_generators

