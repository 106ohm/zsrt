program generoMatrici
implicit none
!lavoro in doppia precisione
integer, parameter :: dp=kind(0.d0)
real(dp), dimension(:,:), allocatable :: T,S
real(dp), dimension(:), allocatable :: v 
integer :: n, i, j
real(dp) :: rnd, machinePrecision


!!!
!Fine dichiarazioni
!!!

machinePrecision=epsilon(1.d0)

!scelgo la dimensione
n=16

!alloco memoria
allocate( T(n,n), S(n,n) )

!!!
!ATTENZIONE: mi devo ricordare di generale pencil (T,S)
!che siano "non riducibili", ovvero t.c.
!T(i,i+1)^2 + S(i,i+1)^2 \neq 0 per i=1,...,n-1
!!!



!!!
!Matrici tridiagonali simmetriche random, 
!ma dominanti diagonali (autovalori "distanti")
!Per i teoremi di Gershgorin T ed S sono
!definite positive
!!!
do i=1,n
   do j=1,i
      call random_number(rnd)
      if (i == j) then
         !aggiungo n+1, cosi' sono sicuro che 
         !T(i,i)> somma_su_j_di T(i,j)
         T(i,j) = 1.d0
      else
         if ( abs(i-j) == 1 ) then
            T(i,j) = rnd*1.d-3
            T(j,i) = T(i,j)
         else
            T(i,j)=0.d0
            T(j,i)=T(i,j)
         end if
      end if
   end do
end do

!do i=1,n
!   rnd = sum(T(i,:))
!   T(i,i) = 2.d0*rnd
!end do

do i=1,n
   do j=1,i
      call random_number(rnd)
      if (i == j) then
         !aggiungo n+1, cosi' sono sicuro che 
         !S(i,i)> somma_su_j_di S(i,j)
         S(i,j) = 0.d0
      else
         if ( abs(i-j) == 1 ) then
            S(i,j) = rnd*1.d-3
            S(j,i) = S(i,j)
         else
            S(i,j)=0.d0
            S(j,i)=S(i,j)
         end if
      end if
   end do
end do

do i=1,n
   rnd = sum(S(i,:))
   S(i,i) = i*1.d0 + rnd
end do

!!$do i=1,n
!!$   do j=1,i
!!$      call random_number(rnd)
!!$      if (i == j) then
!!$         !aggiungo n+1, cosi' sono sicuro che 
!!$         !T(i,i)> somma_su_j_di T(i,j)
!!$         T(i,j) = rnd+n+1
!!$      else
!!$         if ( abs(i-j) == 1 ) then
!!$            T(i,j) = rnd
!!$            T(j,i) = T(i,j)
!!$         else
!!$            T(i,j)=0.d0
!!$            T(j,i)=T(i,j)
!!$         end if
!!$      end if
!!$   end do
!!$end do
!!$
!!$
!!$do i=1,n
!!$   do j=1,i
!!$      call random_number(rnd)
!!$      if (i == j) then
!!$         !aggiungo n+1, cosi' sono sicuro che 
!!$         !S(i,i)> somma_su_j_di S(i,j)
!!$         S(i,i) = rnd+n+1
!!$      else
!!$         if ( abs(i-j) == 1 ) then
!!$            S(i,j) = rnd
!!$            S(j,i) = S(i,j)
!!$         else
!!$            S(i,j)=0.d0
!!$            S(j,i)=S(i,j)
!!$         end if
!!$      end if
!!$   end do
!!$end do


!!$!!!
!!$!Matrici tridiagonali simmetriche random, 
!!$!ma dominanti diagonali (per poco)
!!$!Per i teoremi di Gershgorin T ed S sono
!!$!definite positive
!!$!!!
!!$do i=1,n
!!$   do j=1,i
!!$      call random_number(rnd)
!!$      if (i /= j) then
!!$         if ( abs(i-j) == 1 ) then
!!$            T(i,j) = rnd
!!$            T(j,i) = T(i,j)
!!$         else
!!$            T(i,j)=0.d0
!!$            T(j,i)=T(i,j)
!!$         end if
!!$      end if
!!$   end do
!!$end do
!!$do i=1,n
!!$   T(i,i) = sum(T(i,:)) + machinePrecision
!!$end do
!!$
!!$do i=1,n
!!$   do j=1,i
!!$      call random_number(rnd)
!!$      if (i /= j) then
!!$         if ( abs(i-j) == 1 ) then
!!$            S(i,j) = rnd
!!$            S(j,i) = S(i,j)
!!$         else
!!$            S(i,j)=0.d0
!!$            S(j,i)=S(i,j)
!!$         end if
!!$      end if
!!$   end do
!!$end do
!!$do i=1,n
!!$   S(i,i) = sum(S(i,:)) + machinePrecision
!!$end do



!!$!!!
!!$!Matrici AD HOC
!!$!!!
!!$n=4
!!$T(1,1)= 1.d0
!!$T(2,2)= 2.d0
!!$T(3,3)= 3.d0
!!$T(4,4)= 4.d0
!!$
!!$T(1,2)= 0.d0
!!$T(1,3)= 0.d0
!!$T(1,4)= 0.d0
!!$T(2,1)= 0.d0
!!$T(2,3)= 0.d0
!!$T(2,4)= 0.d0
!!$T(3,1)= 0.d0
!!$T(3,2)= 0.d0
!!$T(3,4)= 0.d0
!!$T(4,1)= 0.d0
!!$T(4,2)= 0.d0
!!$T(4,3)= 0.d0
!!$!!!
!!$S(1,1)= 1.d0
!!$S(2,2)= 1.d0
!!$S(3,3)= 1.d0
!!$S(4,4)= 1.d0
!!$
!!$S(1,2)= 0.d0
!!$S(1,3)= 0.d0
!!$S(1,4)= 0.d0
!!$S(2,1)= 0.d0
!!$S(2,3)= 0.d0
!!$S(2,4)= 0.d0
!!$S(3,1)= 0.d0
!!$S(3,2)= 0.d0
!!$S(3,4)= 0.d0
!!$S(4,1)= 0.d0
!!$S(4,2)= 0.d0
!!$S(4,3)= 0.d0


!!$!!!
!!$!ATTENZIONE: ...
!!$!S=I,T=diag(1,...,n)
!!$!!!
!!$do i=1,n
!!$   do j=1,i
!!$      if (i == j) then
!!$         T(i,j) = 2.d0
!!$      else
!!$         if ( abs(i-j) == 1 ) then
!!$            T(i,j) = -1.d0
!!$            T(j,i) = T(i,j)
!!$         else
!!$            T(i,j) = 0.d0
!!$            T(j,i) = T(i,j)
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
open(unit=2, file="T.txt")
open(unit=3, file="S.txt")

write(2,*) n
write(3,*) n

do j=1,n
   do i=1,n
      write(2,*) T(i,j)
      write(3,*) S(i,j)
   end do
end do

end program generoMatrici
