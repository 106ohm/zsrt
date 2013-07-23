!!!
!PROGRAMMA PROVEINERZIA
!!!
!Al momento funziona solo per "n=2^numero",
!fra poco estendero` il calcolo al caso
!"n > 1".

program proveInerzia

implicit none

integer, parameter :: dp = kind(1.d0)

integer :: n, i, j

integer :: numAut

real(dp) :: x

real(dp), dimension(:,:), allocatable :: T,S

real(dp), dimension(:), allocatable :: autovalori

!!!
!FINE DICHIARAZIONI
!!!


!leggo, per colonne, il contenuto dei file "T.txt" ed "S.txt",
!alloco la memoria e carico le matrici T ed S.

open(unit=1, file="T.txt")
open(unit=2, file="S.txt")


read(1,*) n
read(2,*) n

allocate( T(n,n),S(n,n) )

allocate( autovalori(n) )

do j=1,n
   do i=1,n
      read(1,*) T(i,j)
      read(2,*) S(i,j)
   end do
end do


open(unit=3, file="Eispack/ris_eispack.txt")

read(3,*) n


do i=1,n

   read(3,*) autovalori(i)

end do

call quick_sort(autovalori,n)


!Testo la subroutine per il calcolo del numero di autovalori
!che precedono un certo valore

do j=1,n

   write(*,*)"j=",j
   write(*,*)"j-esimo autovalore=", autovalori(j)

   x = autovalori(j) + 2.d-1

   call numAutovaloriPrimaDiX(x,n,T,S,numAut)

   write(*,*)"numAut=",numAut

end do

end program proveInerzia
