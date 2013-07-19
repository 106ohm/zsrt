!!!
!PROGRAMMA PROVECALCOLI
!!!
!Al momento funziona solo per "n=2^numero",
!fra poco estendero` il calcolo al caso
!"n > 1".

program proveCalcoli

implicit none

integer, parameter :: dp = kind(1.d0)

integer :: n, i, j, dim, Tinizio, Tfine, Sinizio, Sfine, kappa

real(dp) :: x, lettore1, lettore2, fPrimo, fSecondo

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

allocate( T(1:n,0:1),S(1:n,0:1) )

do j=1,n
   do i=1,n
      read(1,*) lettore1
      read(2,*) lettore2
      if ( i == j ) then
         T(i,0) = lettore1
         S(i,0) = lettore2
      end if
      if ( abs(i-j)==1 .AND. j>i ) then
         T(i,1) = lettore1
         S(i,1) = lettore2
      end if
   end do
end do

T(n,1)=-100.d0
S(n,1)=-100.d0

do i=1,n
   write(*,*)"S(i,0)=",S(i,0)
end do

do i=1,n
   write(*,*)"S(i,1)=",S(i,1)
end do


dim= 8
Tinizio= 1
Tfine= 8
Sinizio= 1
Sfine= 8


open(unit=3, file="Eispack/ris_eispack.txt")

read(3,*) n

allocate(autovalori(n))

do i=1,n

   read(3,*) autovalori(i)

end do

call quick_sort(autovalori,n)

do i=1,n

   write(*,*)"i=",i
   write(*,*)"i-esimo autovalore=", autovalori(i)

   x = autovalori(i) + 2.d-2

   call calcoli(x, T, S, n, dim, Tinizio, Tfine, Sinizio, &
        Sfine, fPrimo, fSecondo, kappa)

   write(*,*)"x=",x,"fPrimo=",fPrimo,"fSecondo=",fSecondo,"kappa=",kappa

end do

end program proveCalcoli
