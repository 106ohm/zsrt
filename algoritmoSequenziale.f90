!!!
!PROGRAMMA PRINCIPALE
!!!
program tuttiAutovalori

implicit none

end program tuttiAutovalori


!!!
!Fissato l'intervallo I, calcolo gli autovalori della pencil (T,S)
!in esso contenuti. L'intervallo I e` generalizzato,
!cioe` puo` essere della forma [a,b] o  (-infty,b] o [a,+infty) od 
!anche (-infty,+infty)
!!!
recursive subroutine calcoloAutovaloriDentroI(aj, bj, n, T, S)

implicit none

!dichiaro la precisione di macchina,
!in questo caso doppia,
integer, parameter :: dp = kind(1.d0)

integer :: n

real(dp) :: aj, bj

real(dp), dimension(:,:), allocatable :: T, S

!Adesso le variabili che non provengono dall'esterno

integer :: i, j, k

real(dp), dimension(:,:), allocatable :: T0, T1, S0, S1

!!!
!FINE DICHIARAZIONI
!!!

allocate( T(n,n), S(n,n) )

!!!
!SPLIT
!!!

!SE n=2 CALCOLO DIRETTAMENTE GLI AUTOVALORI

if (n == 2) then
   !da scrivere
   exit
end if

!ALTRIMENTI PREPARO LE PENCIL PER LA RICORSIONE

!divisione intera
k = n/2

allocate( T0(k,k), T1(k,k), S0(k,k), S1(k,k) )
!adesso ho a disposizione le nuove pencil: (T0, S0) e (T1, S1).



!!!
!EVALUATE: usiamo la ricorsione.
!Questo e` il cuore del calcolo.
!!!


!!!
!MERGE, iterazione di Laguerre
!!!


end subroutine calcoloAutovaloriDentroI


!!!
!Descrive l'intervallo Ij=[aj, bj]
!!!
subroutine selezionaIntervallo()

implicit none

end subroutine selezionaIntervallo

!!!
!Stima la molteplicita` dell'autovalore
!!!
subroutine EstMlt()

implicit none

end subroutine EstMlt

!!!
!Calcola j-esimo autovalore con l'iterazione di Laguerre
!!!
subroutine LagIt()

implicit none

end subroutine LagIt
