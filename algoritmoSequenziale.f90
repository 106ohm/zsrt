!!!
!PROGRAMMA PRINCIPALE
!!!
program sperimentazione

implicit none

end program sperimentazione


!!!
!Fissato l'intervallo I, calcolo gli autovalori della pencil (T,S)
!in esso contenuti. L'intervallo I e` generalizzato,
!cioe` puo` essere della forma [a,b] o  (-infty,b] o [a,+infty) od 
!anche (-infty,+infty).
!Gli autovalori sono salvati nella matrice chiamata "Eigenvalues".
!La i-esima colonna contiene dall'alto verso il basso gli autovalori
!della pensil (T^,S^).
!l'ultima colonna contiene gli autovalori del problema di partenza. 
!!!
recursive subroutine calcoloAutovaloriDentroI(a, b, n, T, S, Tinizio, Tfine, Sinizio, Sfine, en, em, Eigenvalues, numCol)
!L'intervallo I=[a,b] e` identificato dai suoi estremi

implicit  none

!dichiaro la precisione di macchina (doppia):
integer, parameter :: dp = kind(1.d0)

integer, save :: n, en, em

integer :: numCol

real(dp), save :: a, b

real(dp), dimension(:,:), allocatable, save :: T, S, Eigenvalues

!indici per identificare la T e la S in input:                                                                                   
integer :: Tinizio, Tfine, Sinizio, Sfine
!ATTENZIONE: voglio siano variabili locali, dunque non aggiungo SAVE!!!

!Di seguito le variabili che non provengono dall'esterno

!indici per identificare le due nuove pencil:
integer :: T0inizio, T0fine, T1inizio, T1fine, S0inizio, S0fine,S1inizio, S1fine
!Anche qui uso variabili locali

integer :: i, j, k, dim, kappaX, k1, k2

real(dp) :: machinePrecision, x, aj, bj

!!!
!FINE DICHIARAZIONI
!!!

machinePrecision=epsilon(1.d0)

allocate( T(n,n), S(n,n), Eigenvalues(en,em) )

!!!
!SPLIT
!!!

dim = Tfine - Tinizio

!SE dim=2 oppure dim=1 ALLORA CALCOLO DIRETTAMENTE GLI AUTOVALORI
if (dim <= 2) then
   !da scrivere
   return
end if

!ALTRIMENTI PREPARO LE PENCIL PER LA RICORSIONE

!divisione intera per due
dim = dim/2
!le nuove pencil, (T0, S0) e (T1, S1), sono identificate 
!dagli indici di inizio/fine delle rispettive diagonali
T0inizio=Tinizio
T0fine=T0inizio+dim
T1inizio=T0fine+1
T1fine=Tfine
S0inizio=Sinizio
S0fine=S0inizio+dim
S1inizio=S0fine+1
S1fine=Sfine

!!!
!EVALUATE: usiamo la ricorsione.
!Questo e` il cuore del calcolo.
!!!

call calcoloAutovaloriDentroI(a, b, n, T, S, T0inizio, T0fine, S0inizio, S0fine, en, em, Eigenvalues, numCol+1)

call calcoloAutovaloriDentroI(a, b, n, T, S, T1inizio, T1fine, S1inizio, S1fine, en, em, Eigenvalues, numCol+1)

!!!
!MERGE, iterazione di Laguerre (pagina 17 dell'articolo)
!!!

!In questo momento ho a disposizione \hat\lambda_{k+1}, ..., \hat\lambda_{k+m}, salvati nella numCol-esima colonna di Eigenvalues.
!Mi devo ricordare sempre che \hat\lambda_{k}=a e \hat\lambda_{k+m+1}=b.
!Calcolo kappa(a) e kappa(b), e da questi ricavo k1 e k2
!OSS: in questo caso considero kappa(a)=0 e kappa(b)=n
k1=0+1
k2=n

do j=k1:k2


   !Determino l'intervallo Ij=(aj, bj) in cui ho convergenza cubica
   !nel ricercare \lambda_j
   aj=a
   bj=b
   if ( bj-aj > max(aj,bj)*machinePrecision ) then
      x = Eigenvalues(j,numCol)
      !cioe` x=\hat\lambda_j.
      !Adesso chiamo la subroutine per il calcolo di (12), (13) e (14).
      !100 call calcoli(...)
      !ATTENZIONE ho messo una etichetta!!!
      if ( kappaX < j ) then
         aj = x
      else
         bJ = x
      end if
      !Adesso cerco un nuovo x
      if ( () .OR. () ) then
         x = (aj+bj)/2.d0
         !ripeto il calcolo fatto alla etichetta 100:
         GOTO 100
      end if
      !Chiamo EstMlt e LagIt
      !call ...
      !call ...
   else
      Eigenvalues(j,numCol+1) = (aj+bj)/2.d0
   end if
end do



end subroutine calcoloAutovaloriDentroI


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

!!!                                                                    
!Calcola (12), (13) e (14)               
!!!                                                                    
subroutine calcoli()

implicit none

end subroutine calcoli
