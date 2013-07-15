program generoMatrici
implicit none
!lavoro in doppia precisione
integer, parameter :: dp=kind(0.d0)
real(dp), dimension(:,:), allocatable :: T,S
real(dp), dimension(:), allocatable :: v 
integer :: n, i, j
real(dp) :: rnd


!scelgo la dimensione
n=2

!alloco memoria
allocate( T(n,n), S(n,n) )


!!!
!Matrici per prime verifiche
!!!
open(unit=2, file="T.txt")
write(2,*) n
do i=1,n
   do j=1,n
      if (i == j) then
         T(i,j) = 1.d0
         write(2,*) T(i,j)
      else
         T(i,j) = 0.d0
         write(2,*) T(i,j)
      end if
   end do
end do

open(unit=3, file="S.txt")
write(3,*) n
do i=1,n
   do j=1,n
      if (i == j) then
         S(i,j) = 1.d0
         write(3,*) S(i,j)
      else
         S(i,j) = 0.d0
         write(3,*) S(i,j)
      end if
   end do
end do

!!$!!!
!!$!matrici random
!!$!!!
!!$open(unit=2, file="T.txt")
!!$write(2,*)n
!!$do i=1,n
!!$   do j=1,n
!!$      call random_number(rnd)
!!$      write(2,*) rnd
!!$   end do
!!$end do
!!$
!!$open(unit=3, file="S.txt")
!!$write(3,*)n
!!$do i=1,n
!!$   do j=1,n
!!$      call random_number(rnd)
!!$      write(3,*) rnd
!!$   end do
!!$end do




!!$!!!
!!$!matrici random, ma dominanti diagonali 
!!$!(autovalori abbastanda distanti)
!!$!!!
!!$open(unit=2, file="T.txt")
!!$write(2,*)n
!!$do i=1,n
!!$   do j=1,n
!!$      call random_number(rnd)
!!$      if (i == j) then
!!$         !aggiungo n+1, cosi' sono sicuro che 
!!$         !A(i,i)> somma_su_j_di A(i,j)
!!$         write(2,*) rnd+n+1
!!$      else
!!$         write(2,*) rnd
!!$      end if
!!$   end do
!!$end do
!!$
!!$open(unit=3, file="S.txt")
!!$write(3,*)n
!!$do i=1,n
!!$   do j=1,n
!!$      call random_number(rnd)
!!$      if (i == j) then
!!$         !aggiungo n+1, cosi' sono sicuro che 
!!$         !B(i,i)> somma_su_j_di B(i,j)
!!$         write(3,*) rnd+n+1
!!$      else
!!$         write(3,*) rnd
!!$      end if
!!$   end do
!!$end do


!!$!!!
!!$!matrici random, "poco" dominanti diagonali 
!!$!!!
!!$
!!$
!!$do i=1,n
!!$   do j=1,n
!!$      if (i == j) then
!!$         T(i,j) = 0
!!$      else
!!$         call random_number(rnd)
!!$         T(i,j) = rnd
!!$      end if
!!$   end do
!!$end do
!!$do i=1,n
!!$   T(i,i) = sum(T(i,:)) + 1.0d-015
!!$end do
!!$open(unit=2, file="T.txt")
!!$write(2,*)n
!!$do i=1,n
!!$   do j=1,n
!!$      write(2,*) T(i,j)
!!$   end do
!!$end do
!!$
!!$do i=1,n
!!$   do j=1,n
!!$      if (i == j) then
!!$         S(i,j) = 0
!!$      else
!!$         call random_number(rnd)
!!$         S(i,j) = rnd
!!$      end if
!!$   end do
!!$end do
!!$do i=1,n
!!$   S(i,i) = sum(S(i,:)) + 1.0d-015
!!$end do
!!$open(unit=3, file="S.txt")
!!$write(3,*)n
!!$do i=1,n
!!$   do j=1,n
!!$      write(3,*) S(i,j)
!!$   end do
!!$end do



!!!
!stapo a video le matrici
!!!
write(*,*)"T="
do i=1,n
   write(*,*) T(i,:)
end do

write(*,*)"S="
do i=1,n
   write(*,*) S(i,:)
end do



end program generoMatrici
