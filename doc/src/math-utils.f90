module math_utils
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, math-utils!"
  end subroutine say_hello
end module math_utils
