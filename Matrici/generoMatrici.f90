program generoMatrici
implicit none
!lavoro in doppia precisione
integer, parameter :: dp=kind(0.d0)
real(dp), dimension(:,:), allocatable :: T,S
real(dp), dimension(:), allocatable :: v 
integer :: n, i, j
real(dp) :: rnd, machinePrecision, count, max, e1, e2
integer,parameter :: seed = 86456

!!!
!Fine dichiarazioni
!!!

machinePrecision=epsilon(1.d0)

!scelgo la dimensione
n=8

!alloco memoria
allocate( T(n,n), S(n,n) )

!!!
!ATTENZIONE: mi devo ricordare di generale pencil (T,S)
!che siano "non riducibili", ovvero t.c.
!T(i,i+1)^2 + S(i,i+1)^2 \neq 0 per i=1,...,n-1
!!!



!!$!!!
!!$!Matrici tridiagonali simmetriche random, 
!!$!ma dominanti diagonali (autovalori "distanti")
!!$!Per i teoremi di Gershgorin T ed S sono
!!$!definite positive
!!$!!!
!!$do i=1,n
!!$   do j=1,i
!!$      call random_number(rnd)
!!$      if (i == j) then
!!$         !aggiungo n+1, cosi' sono sicuro che 
!!$         !T(i,i)> somma_su_j_di T(i,j)
!!$         T(i,j) = 1.d0
!!$      else
!!$         if ( abs(i-j) == 1 ) then
!!$            T(i,j) = rnd*1.d-3
!!$            T(j,i) = T(i,j)
!!$         else
!!$            T(i,j)=0.d0
!!$            T(j,i)=T(i,j)
!!$         end if
!!$      end if
!!$   end do
!!$end do
!!$
!!$!do i=1,n
!!$!   rnd = sum(T(i,:))
!!$!   T(i,i) = 2.d0*rnd
!!$!end do
!!$
!!$do i=1,n
!!$   do j=1,i
!!$      call random_number(rnd)
!!$      if (i == j) then
!!$         !aggiungo n+1, cosi' sono sicuro che 
!!$         !S(i,i)> somma_su_j_di S(i,j)
!!$         S(i,j) = 0.d0
!!$      else
!!$         if ( abs(i-j) == 1 ) then
!!$            S(i,j) = rnd*1.d-1
!!$            S(j,i) = S(i,j)
!!$         else
!!$            S(i,j)=0.d0
!!$            S(j,i)=S(i,j)
!!$         end if
!!$      end if
!!$   end do
!!$end do
!!$
!!$do i=1,n
!!$   rnd = sum(S(i,:))
!!$   S(i,i) = i*1.d-1 + rnd
!!$end do




do i=1,n
   do j=1,n
      if ( i==j ) then
         T(i,j) = 1.d0
      else
         T(i,j) = 0.d0
      end if
   end do
end do

e1=1.d-1
e2=1.d-1 + 5.d-2

do i=1,n
   do j=1,n
      if ( i==j ) then
         S(i,j) = 0.d0
      end if
      if ( abs(i-j)==1 .AND. j>i ) then
         rnd=rand(seed)
         S(i,j) = rnd*1.d-3
         S(j,i) = S(i,j)
      end if
   end do
end do

do i=1,n-1
   rnd = 2.d0*S(i,i+1)
   S(i,i) = ( (e2-e1)*(i-1) )/( e1*e2*(n-1) ) + 1/e2  + abs(rnd)
end do

S(n,n) = ( (e2-e1)*(n-1) )/( e1*e2*(n-1) ) + 1/e2






!!$!!!
!!$!Matrici Problema Sturm-Liouville
!!$!!!
!!$do i=1,n
!!$   do j=1,i
!!$      if (i == j) then
!!$         !T(i,j) = 2.d0*(n+1) + ( (3.d0*i**2 + 2.d0) * 8.d0 )/(n+1)
!!$         T(i,j) = -6.d0/(n+1) * ( 2.d0*i**2 - 2.d0*i -1.d0 )
!!$      end if
!!$      if ( abs(i-j) == 1 .AND. j>i ) then
!!$         !T(i,j) = -1.d0*(n+1) + ( -1.d0 * (2.d0 -3.d0*(2.d0*i+1) +6.d0*i*(i+1)) )/(n+1)
!!$         T(i,j) = -1.d0/(n+1) + (n+1)*1.d0
!!$         T(j,i) = T(i,j)
!!$      end if
!!$      if ( abs(i-j) > 1 ) then
!!$         T(i,j)=0.d0
!!$         T(j,i)=T(i,j)
!!$      end if
!!$   end do
!!$end do
!!$
!!$do i=1,n
!!$   do j=1,i
!!$      if (i == j) then
!!$         S(i,j) = 4.d0/(6.d0*(n+1))
!!$      end if
!!$      if ( abs(i-j) == 1 ) then
!!$         S(i,j) = 1.d0/(6.d0*(n+1))
!!$         S(j,i) = S(i,j)
!!$      end if
!!$      if ( abs(i-j) > 1) then
!!$         S(i,j)=0.d0
!!$         S(j,i)=S(i,j)
!!$      end if
!!$   end do
!!$end do





!!$!!!
!!$!ATTENZIONE: ...
!!$!S=I;T=trid(-1,2,-1)
!!$!!!
!!$do i=1,n
!!$   do j=1,i
!!$      if (i == j) then
!!$         T(i,j) = 2.d0
!!$         !T(i,j) = - S(i,j)
!!$      else
!!$         if ( abs(i-j) == 1 ) then
!!$            T(i,j) = -1.d0
!!$            !T(i,j) = - S(i,j)
!!$            T(j,i) = T(i,j)
!!$            !T(j,i) = - S(i,j)
!!$         else
!!$            T(i,j) = 0.d0
!!$            !T(i,j) = - S(i,j)
!!$            T(j,i) = T(i,j)
!!$            !T(j,i) = - S(i,j)
!!$         end if
!!$      end if
!!$   end do
!!$end do
!!$
!!$do i=1,n
!!$   do j=1,i
!!$      if (i == j) then
!!$         S(i,j) = 1.d0
!!$      else
!!$         S(i,j) = 0.d0
!!$         S(j,i) = S(i,j)
!!$      end if
!!$   end do
!!$end do


!!!
!ATTENZIONE: mi devo ricordare di generare pencil (T,S)
!che siano "non riducibili", ovvero t.c.
!T(i,i+1)^2 + S(i,i+1)^2 \neq 0 per i=1,...,n-1
!!!
do i=1,n-1
   if ( abs(T(i,i+1)**2 + S(i,i+1)**2) <= 10*machinePrecision ) then
      write(*,*) "PENCIL NON RIDUCIBILE"
   end if
end do


!!!
!Calcolo norma infinito (utile per il corollario 4.3)
!!!
max = 0
do i=1,n
   count = 0
   do j=1,n
      count = count + abs(S(i,j))
   end do
   if ( count > max ) then
      max = count
   end if
end do

write(*,*)"||S||_{\infty}=",max

!!!
!Stapo a video le matrici
!!!
write(*,*)"T="
write(*,*)"diag"
do i=1,n
   write(*,*) T(i,i)
end do
write(*,*)"super"
do i=1,n-1
   write(*,*) T(i,i+1)
end do

write(*,*)"S="
write(*,*)"diag"
do i=1,n
   write(*,*) S(i,i)
end do
write(*,*)"super"
do i=1,n-1
   write(*,*) S(i,i+1)
end do

!!!
!Scrivo su file
!!!
open(unit=2, file="T_Sturm_8.txt")
open(unit=3, file="S_Sturm_8.txt")

write(2,*) n
write(3,*) n

do j=1,n
   do i=1,n
      write(2,*) T(i,j)
      write(3,*) S(i,j)
   end do
end do

end program generoMatrici
