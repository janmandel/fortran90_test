  program test
  use test_mod
  implicit none
! *** local ***
  integer:: level, levels
  parameter(levels=5)
  real:: x,xx

! *** executable ***  
  type(mg), dimension(levels) :: p
  do level=1,levels
     allocate(p(level)%u_h(10,10))
  enddo
   
  call sub(x)
  print *,'hi there', x
  end

