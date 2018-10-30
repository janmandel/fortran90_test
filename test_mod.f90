  module test_mod
  ! variables
  type mg
     double precision, pointer, dimension(:,:):: C_h_stencil
     double precision, pointer, dimension(:,:):: I_2h_h_stencil
     double precision, pointer, dimension(:,:):: I_h_2h_stencil
     double precision, pointer, dimension(:,:):: u_h, f_h, r_h
  end type mg

  contains
  ! subroutines and functions
  subroutine sub(y)
  ! purpose: test assignment
  ! input: none
  ! output: y
  implicit none
  real, intent(out):: y
  y=1
  end subroutine sub
  end module test_mod

