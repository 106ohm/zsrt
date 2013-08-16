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

integer :: dim, Tinizio, Tfine, Sinizio, Sfine, kappa

integer :: numAut

real(dp) :: x

real(dp), dimension(:,:), allocatable :: T,S

real(dp), dimension(:,:), allocatable :: Tcorta, Scorta

real(dp) :: fPrimo, fSecondo

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

allocate( Tcorta(1:n,0:1), Scorta(1:n,0:1) )

allocate( autovalori(n) )

do j=1,n
   do i=1,n
      
      read(1,*) T(i,j)
      read(2,*) S(i,j)

      if ( i == j ) then
         Tcorta(i,0)=T(i,j)
         Scorta(i,0)=S(i,j)
      end if
      if ( abs(i-j)==1 .AND. j>i ) then
         Tcorta(j,1) = T(i,j)
         Scorta(j,1) = S(i,j)
      end if

   end do
end do

!!$write(*,*)"T="
!!$do i=1,n
!!$   write(*,*) T(i,:)
!!$end do
!!$write(*,*)"T diag="
!!$write(*,*) Tcorta(:,0)
!!$write(*,*)"T super="
!!$write(*,*) Tcorta(:,1)
!!$
!!$write(*,*)"S="
!!$do i=1,n
!!$   write(*,*) S(i,:)
!!$end do
!!$write(*,*)"S diag="
!!$write(*,*) Scorta(:,0)
!!$write(*,*)"S super="
!!$write(*,*) Scorta(:,1)

open(unit=3, file="Eispack/ris_eispack.txt")

read(3,*) n


do i=1,n

   read(3,*) autovalori(i)

end do

call quick_sort(autovalori,n)


dim= n
Tinizio= 1
Tfine= n
Sinizio= 1
Sfine= n


write(*,*)"n=",n

write(*,*)"********************"

do j=1,n

   write(*,*)"j=",j
   write(*,*)"j-esimo autovalore=", autovalori(j)

   if ( j<n ) then
      x = ( autovalori(j)+autovalori(j+1) )/2.d0
      else
         x = autovalori(j) + 1.d0
      end if

   write(*,*)"x=",x

   call numAutovaloriPrimaDiX(x,n,Tcorta,Scorta,numAut)

   write(*,*)"numAut=",numAut

   call calcoli(x, Tcorta, Scorta, n, dim, Tinizio, Tfine, Sinizio, &
        Sfine, fPrimo, fSecondo, kappa)

   write(*,*)"kappa=",kappa

   write(*,*)"fPrimo=",fPrimo

   write(*,*)"fSecondo=",fSecondo

   write(*,*)"********************"

end do

end program proveInerzia
