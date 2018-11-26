program project
! skeleton of a  2d laplace iteration on rectangular domain to demonstrate MPI 
! incomplete - as a guidance to writing your own only
! Jan Mandel, November 26, 2018

use mpi
! who am I? what is my k
! compute my ib,jb,its,ite,jts,jte

! set ims,ime,jms,jme
call work(.....)

end program project

subroutine work(ib,jb,k,its,ite,jts,jte,ims,ime,jms,jme)

integer:: its,ite,jts,jte     ! starting and ending indices of the submatrix I am updating
integer:: ims,ime,jms,jme     ! starting and ending indices of the submatrix I have in memory - at least by 1 larger on each side

double precision, dimension(ims:ime,jms:jme):: u,f,v ! dynamic memory allocation

!  assign subdomains to processors in M by N grid

!                   1               N 
!        1       |----------------------|
!                | 1    M+1             | 
!                | 2    M+2             | 
!                |                      |
!                | M    M+M  ....   M*N |
!        M       |-----------------------


print *,ib,jb
! I am subdomain ib,jb on processor k

! initialize u,f
! do the following in iteration loop

! x: if jb>1 then receive u(its:ite,jts-1) from subdomain ib,jb-1 on processor k-M 
!    if jb<N then send u(its:ite,jte+1) to subdomain ib,jb+1 on processor k+M
! x: if jb=1 then assume that u(its:ite,jts-1) has boundary values - set them first!
! y: do not need to worry about corner because I have only 5 point difference scheme
! z: etc.

!                  jts                jte
!               y zzzzzzzzzzzzzzzzzzzzzzzz 
!         its   x |----------------------|
! ib,jb-1       x |        ib,jb         | 
!               x |                      |
!               x |                      |
!         ite   x |----------------------|




! main work loop - compute  v = u - omega * (f - A*u)
! need also values with i=its-1 and ite+1 and j=jts-1 and jte+1
do j=jts,jte
    do i=its,ite
        v(i,j) = u(i,j) - omega * (f(i,j)*h*h - (4*u(u,j)-u(i+1,j) - u(i-1,j) - u(i,j+1) - u(i,j-1)))
    enddo
enddo

! u = v
do j=jts,jte
    do i=its,ite
        u(i,j) = u(i,j)
    enddo
enddo

! end iteration loop here

! think how to get the results out to a file!

end subroutine work
