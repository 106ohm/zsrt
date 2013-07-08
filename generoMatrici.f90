program generoMatrici
implicit none
!lavoro in doppia precisione (usero' la libreria LAPACK)
integer, parameter :: dp=kind(0.d0)
real(dp), dimension(:,:), allocatable :: A, B, C
real(dp), dimension(:), allocatable :: v 
integer :: n, i, j
real(dp) :: rnd


!scelgo la dimensione
n=2

!creo le matrici A, B e C
allocate(A(n,n),B(n,n),C(n,n))

!!!
!matrici random
!!!
!open(unit=2, file="A.txt")
!write(2,*)n
!do i=1,n
!	do j=1,n
!		call random_number(rnd)
!		write(2,*) rnd
!	end do
!end do
!
!open(unit=3, file="B.txt")
!write(3,*)n
!do i=1,n
!	do j=1,n
!		call random_number(rnd)
!		write(3,*) rnd
!	end do
!end do
!
!open(unit=4, file="C.txt")
!write(4,*)n
!do i=1,n
!	do j=1,n
!		call random_number(rnd)
!		write(4,*) rnd
!	end do
!end do




!!!
!matrici random, ma dominanti diagonali (autovalori abbastanda distanti)
!!!


open(unit=2, file="A.txt")
write(2,*)n
do i=1,n
	do j=1,n
		call random_number(rnd)
		if (i == j) then
		!aggiungo n+1, cosi' sono sicuro che A(i,i)> somma_su_j_di A(i,j)
			write(2,*) rnd+n+1
		else
			write(2,*) rnd
		end if
	end do
end do

open(unit=3, file="B.txt")
write(3,*)n
do i=1,n
	do j=1,n
		call random_number(rnd)
		if (i == j) then
		!aggiungo n+1, cosi' sono sicuro che B(i,i)> somma_su_j_di B(i,j)
			write(3,*) rnd+n+1
		else
			write(3,*) rnd
		end if
	end do
end do

open(unit=4, file="C.txt")
write(4,*)n
do i=1,n
	do j=1,n
		call random_number(rnd)
		if (i == j) then		
		!aggiungo n+1, cosi' sono sicuro che C(i,i)> somma_su_j_di C(i,j)
			write(4,*) rnd+n+1
		else
			write(4,*) rnd
		end if
	end do
end do



!!$!!!
!!$!matrici random, "poco" dominanti diagonali 
!!$!!!
!!$
!!$
!!$do i=1,n
!!$   do j=1,n
!!$      if (i == j) then
!!$         A(i,j) = 0
!!$      else
!!$         call random_number(rnd)
!!$         A(i,j) = rnd
!!$      end if
!!$   end do
!!$end do
!!$do i=1,n
!!$   A(i,i) = sum(A(i,:)) + 1.0E-015
!!$end do
!!$open(unit=2, file="A.txt")
!!$write(2,*)n
!!$do i=1,n
!!$   do j=1,n
!!$      write(2,*)A(i,j)
!!$   end do
!!$end do
!!$
!!$do i=1,n
!!$   do j=1,n
!!$      if (i == j) then
!!$         B(i,j) = 0
!!$      else
!!$         call random_number(rnd)
!!$         B(i,j) = rnd
!!$      end if
!!$   end do
!!$end do
!!$do i=1,n
!!$   B(i,i) = sum(B(i,:)) + 1.0E-015
!!$end do
!!$open(unit=3, file="B.txt")
!!$write(3,*)n
!!$do i=1,n
!!$   do j=1,n
!!$      write(3,*)B(i,j)
!!$   end do
!!$end do
!!$
!!$do i=1,n
!!$   do j=1,n
!!$      if (i == j) then
!!$         C(i,j) = 0
!!$      else
!!$         call random_number(rnd)
!!$         C(i,j) = rnd
!!$      end if
!!$   end do
!!$end do
!!$do i=1,n
!!$   C(i,i) = sum(C(i,:)) + 1.0E-015
!!$end do
!!$open(unit=4, file="C.txt")
!!$write(4,*)n
!!$do i=1,n
!!$   do j=1,n
!!$      write(4,*)C(i,j)
!!$   end do
!!$end do
!!$
!!$!stapo a video le tre matrici
!!$
!!$write(*,*)"A="
!!$do i=1,n
!!$   write(*,*)A(i,:)
!!$end do
!!$
!!$write(*,*)"B="
!!$do i=1,n
!!$   write(*,*)B(i,:)
!!$end do
!!$
!!$write(*,*)"C="
!!$do i=1,n
!!$   write(*,*)C(i,:)
!!$end do






!!!
!TEST:
!con A=I, B=(-2)*I e C=I ho (lambda - 1)^2=0 e quindi lambda=1 con molteplicita' 2*n
!!!

!open(unit=2, file="A.txt")
!write(2,*)n
!do i=1,n
!	do j=1,n
!		if (i == j) then
!			write(2,*) 1
!		else
!			write(2,*) 0
!		end if
!	end do
!end do

!open(unit=3, file="B.txt")
!write(3,*)n
!do i=1,n
!	do j=1,n
!		if (i == j) then
!			write(3,*) -2
!		else
!			write(3,*) 0
!		end if
!	end do
!end do
!
!open(unit=4, file="C.txt")
!write(4,*)n
!$do i=1,n
!$	do j=1,n
!$		if (i == j) then		
!$			write(4,*) 1
!$		else
!$			write(4,*) 0
!$		end if
!$	end do
!$end do



!!!
!TEST: matrici A, B e C t.c. ci sia autovalore 1 con autovettore, con componenti uguali a 1, associato
!!!
!!$open(unit=2, file="A.txt")
!!$write(2,*)n
!!$write(2,*)-1
!!$write(2,*)-2
!!$write(2,*)0
!!$write(2,*)-3
!!$
!!$open(unit=3, file="B.txt")
!!$write(3,*)n
!!$write(3,*)6
!!$write(3,*)-1
!!$write(3,*)-2
!!$write(3,*)7
!!$
!!$open(unit=4, file="C.txt")
!!$write(4,*)n
!!$write(4,*)-1
!!$write(4,*)-1
!!$write(4,*)-1
!!$write(4,*)-1



end program generoMatrici
