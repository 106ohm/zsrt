!!!
!PROGRAMMA PRINCIPALE
!!!
program proveCalcoli

implicit none

integer, parameter :: dp = kind(1.d0)

integer :: n, i, j, em, en, dim

integer :: Tinizio,Tfine,Sinizio,Sfine

integer :: T0inizio,T0fine,T1inizio,T1fine

integer :: S0inizio, S0fine, S1inizio, S1fine

integer :: kappa, k1, k2

real(dp) :: fPrimo, fSecondo

real(dp) :: a,b

real(dp), dimension(:,:), allocatable :: T,S, Eigenvalues

!!!
!FINE DICHIARAZIONI
!!!

!leggo, per colonne, il contenuto dei file "T.txt" ed "S.txt",
!alloco la memoria e carico le matrici T ed S;
!cosi` avro` a disposizione la pencil (T,S).

open(unit=1, file="T.txt")
open(unit=2, file="S.txt")

read(1,*) n
read(2,*) n

allocate( T(n,n),S(n,n) )

do j=1,n
   do i=1,n
      read(1,*) T(i,j)
      read(2,*) S(i,j)
   end do
end do

!scelgo le dimensioni di Eigenvalues, alloco memoria
!ed inizializzo i suoi valori a zero

em=n
!ATTENIONE: il "logarithmus dualis", ovvero in base 2,
!lo calcoliamo tramite ld(n)=log(n)/log(2)
en=int( log(n*1.d0)/log(2.d0) ) + 2

write(*,*)"n=",n

Tinizio=1
Tfine=n
Sinizio=1
Sfine=n

a=-100.d0
b=100.d0

!ATTENZIONE: numCol=0 poiche' l'indice di colonna di Eigenvalues parte da 1.
!ATTENZIONE: la prima chiamata della subroutine e` con flag=0, le altre no.

dim = Tfine - Tinizio + 1

write(*,*)"dim=",dim

write(*,*)"PREPARO LA CHIAMATA RICORSIVA"

!divisione intera per due
dim = dim/2

write(*,*)"(per la ricorsione) dim=",dim

!Le nuove pencil, (T0, S0) e (T1, S1), sono identificate 
!dagli indici di inizio/fine delle rispettive diagonali
T0inizio=Tinizio
T0fine=T0inizio+dim-1
T1inizio=T0fine+1
T1fine=Tfine
S0inizio=Sinizio
S0fine=S0inizio+dim-1
S1inizio=S0fine+1
S1fine=Sfine

write(*,*)"T0inizio=",T0inizio,"T0fine=",T0fine
write(*,*)"T1inizio=",T1inizio,"T1fine=",T1fine
write(*,*)"S0inizio=",S0inizio,"S0fine=",S0fine
write(*,*)"S1inizio=",S1inizio,"S1fine=",S1fine

write(*,*)"FINE CHIAMATA RICORSIVA"

!torno alla dim che mi occorre
dim = Tfine - Tinizio + 1

write(*,*)"dim=",dim

call calcoli(a, T, S, n, dim, Tinizio, Tfine, Sinizio, Sfine, fPrimo, fSecondo, kappa)

write(*,*)"a=",a,"fPrimo=",fPrimo,"fSecondo=",fSecondo

k1=kappa+1


call calcoli(b, T, S, n, dim, Tinizio, Tfine, Sinizio, Sfine, fPrimo, fSecondo, kappa)

write(*,*)"b=",b,"fPrimo",fPrimo,"fSecondo",fSecondo

k2=kappa

write(*,*)"k1=",k1,"k2=",k2

end program proveCalcoli
