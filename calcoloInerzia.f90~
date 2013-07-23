!!!
!PROGRAMMA PER IL CONFRONTO DEI RISULTATI
!!!

program sperimentazione

implicit none

integer, parameter :: dp = kind(1.d0)

integer :: n, i, j, nm, ierr, matz

real(dp), dimension(:,:), allocatable :: T, S, Z

real(dp), dimension(:), allocatable :: alfr, alfi, beta

real(dp), dimension(:), allocatable :: eigenvalues

real(dp) :: machinePrecision

!!!
!FINE DICHIARAZIONI
!!!

machinePrecision=epsilon(1.d0)

!leggo, per colonne, il contenuto dei file "T.txt" ed "S.txt",
!alloco la memoria e carico le matrici T ed S;
!cosi` avro` a disposizione la pencil (T,S).

open(unit=1, file="T.txt")
open(unit=2, file="S.txt")

read(1,*) n
read(2,*) n

nm = n

allocate( T(n,n),S(n,n) )

allocate( alfr(n), alfi(n), beta(n), z(nm,n) )

allocate( eigenvalues(n) )

do j=1,n
   do i=1,n
      read(1,*) T(i,j)
      read(2,*) S(i,j)
   end do
end do

!voglio solo gli autovalori:
matz = 0

call rgg(nm,n,T,S,alfr,alfi,beta,matz,Z,ierr)

!Per comprendere ierr si veda la documentazione della
!subroutine QZIT.
write(*,*)"IERR=",ierr

!So che gli autovalori sono reali, dunque
!(dopo un controllo) ignoro alfi e salvo
!il valore che cerco in eigenvalues.
do i=1,n
   if ( abs( alfi(i) ) >= 10.d0*machinePrecision ) then
      write(*,*)"ERRORE: alcuni autovalori non sono reali."
      exit
   end if
   if ( abs( beta(i) ) <= 10.0*machinePrecision ) then
      write(*,*)"ERRORE: il denominatore e` troppo piccolo."
      exit
   end if
   !Se sono qui allora non ho incontrato errori
   eigenvalues(i) = alfr(i)/beta(i)
end do



Open(unit=3,file="ris_eispack.txt")

write(3,*) n

do i=1,n
   write(3,*) eigenvalues(i)
end do

write(*,*) "FINE CALCOLO AUTOVALORI!"

end program sperimentazione
