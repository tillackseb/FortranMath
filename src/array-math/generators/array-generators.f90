!> Collection of various array generator procedures.
!>
!>## List of Procedures
!>{!./array_generators.md!}
module array_generators
  use m_identity, only : identity, fill_identity, &
    eye => identity, to_eye => fill_identity
  use m_random, only : random, fill_random, &
    rand => random, to_rand => fill_random
  implicit none
  private

  public :: identity, eye, fill_identity, to_eye
  public :: random, rand, fill_random, to_rand
end module array_generators

