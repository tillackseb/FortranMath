!> Collection of various array generator procedures.
!>
!>## List of Procedures
!>{!./array_generators.md!}
module array_generators
  use m_identity, only : identity, to_identity, &
    eye => identity, to_eye => to_identity
  !use m_random, only : random, to_random, &
  !  rand => random, to_rand => to_random
  implicit none
  private

  public :: identity, eye, to_identity, to_eye
  !public :: random, rand, to_random, to_rand
end module array_generators

