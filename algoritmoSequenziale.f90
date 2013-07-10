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
!Gli autovalori sono salvati nella matrice chiamata "Eigenvalues",
!che ha dimensioni en*em.
!La numCol-esima colonna contiene dall'alto verso il basso 
!gli autovalori della pensil (T^,S^).
!l'ultima colonna contiene gli autovalori del problema di partenza. 
!!!
recursive subroutine calcoloAutovaloriDentroI(a, b, n, T, S, Tinizio, Tfine, Sinizio, Sfine, en, em, Eigenvalues, numCol)
!L'intervallo I=[a,b] e` identificato dai suoi estremi

implicit  none

!dichiaro la precisione di macchina (doppia):
integer, parameter :: dp = kind(1.d0)

integer, intent(IN) :: n

integer, intent(IN) :: en, em

integer, save :: numCol

real(dp), intent(IN) :: a, b

real(dp), dimension(n,n), save, intent(IN) :: T, S
real(dp), dimension(en,em), save, intent(INOUT) :: Eigenvalues

!indici per identificare la T e la S in input:                                                                                   
integer, save :: Tinizio, Tfine, Sinizio, Sfine
!ATTENZIONE: voglio siano variabili locali, dunque non aggiungo SAVE!!!

!Di seguito le variabili che non provengono dall'esterno

!indici per identificare le due nuove pencil:
integer, save :: T0inizio, T0fine, T1inizio, T1fine, S0inizio, S0fine,S1inizio, S1fine
!Anche qui uso variabili locali

integer :: i, j, k, dim, kappa, k1, k2

real(dp) :: machinePrecision, x, aj, bj, sign, mlt, fPrimo, fSecondo, lambdaJ

!!!
!FINE DICHIARAZIONI
!!!

machinePrecision=epsilon(1.d0)

!allocate( T(n,n), S(n,n), Eigenvalues(en,em) )

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

call calcoloAutovaloriDentroI(a, b, n, T, S, T0inizio, T0fine, &
S0inizio, S0fine, en, em, Eigenvalues, numCol+1)

call calcoloAutovaloriDentroI(a, b, n, T, S, T1inizio, T1fine, &
S1inizio, S1fine, en, em, Eigenvalues, numCol+1)

!!!
!MERGE, pagina 17 dell'articolo
!!!

!Ho a disposizione \hat\lambda_{k+1}, ..., \hat\lambda_{k+m}, 
!salvati nella numCol-esima colonna di Eigenvalues.
!Mi devo ricordare sempre che \hat\lambda_{k}=a e 
!\hat\lambda_{k+m+1}=b.
!Calcolo kappa(a) e kappa(b), e da questi ricavo k1 e k2
!OSS: in questo caso considero kappa(a)=0 e kappa(b)=n
k1=0+1
k2=dim

do j=k1:k2


   !Determino l'intervallo Ij=(aj, bj) in cui ho convergenza cubica
   !nel ricercare \lambda_j
   aj=a
   bj=b
   if ( bj-aj > max(aj,bj)*machinePrecision ) then
      x = Eigenvalues(j,numCol)
      !cioe` x=\hat\lambda_j.
      !Adesso chiamo la subroutine per il calcolo di (12), (13) e (14).
      100 call calcoli(x, T, S, n, fPrimo, fSecondo, kappa)
      !ATTENZIONE ho messo una etichetta!!!
      if ( kappa < j ) then
         aj = x
      else
         bJ = x
      end if
      !Adesso cerco un nuovo x
      if ( kappa /= j-1 .AND. kappa /= j ) then
         x = (aj+bj)/2.d0
         !ripeto il calcolo fatto alla etichetta 100:
         GOTO 100
      end if
      !Chiamo EstMlt e LagIt
      sign = sign( - fPrimo )
      !vedi meta` p. 14
      call EstMlt(x, sign, en, em, Eigenvalues, numCol, j, mlt)
      call LagIt(x, mlt, aj, bj, en, em, Eigenvalues, numCol, j, &
      fPrimo, fSecondo, kappa, lambdaJ)
      Eigenvalues(j,numCol+1) = lambdaJ
   else
      Eigenvalues(j,numCol+1) = (aj+bj)/2.d0
   end if
end do



end subroutine calcoloAutovaloriDentroI


!!!
!Stima la molteplicita` dell'autovalore
!!!
subroutine EstMlt(x, sign, en, em, Eigenvalues, numCol, j, mlt)

implicit none

integer, parameter :: dp=kind(1.d0)

real(dp), intent(IN) :: x

integer, intent(IN) :: sign, en, em, numCol, j

integer, intent(OUT) :: mlt

real(dp), dimension(en,em), intent(INOUT) :: Eigenvalues

integer :: k, m

!FINE DICHIARAZIONI

mlt=1

k=1

do while ( .TRUE. )

   m = j + k*sign

   if ( abs( Eigenvalues(j, numCol)-Eigenvalues(m, numCol) ) < 0.01 * abs( Eigenvalues(j, numCol) - x ) ) then
      mlt = mlt + 1
   else
      GOTO 10
   end if

   k = k+1

10 end do

!OSS: incrementare k non modifica il calcolo, ma fornisce indicazioni
!sull'avanzamento dello stesso

end subroutine EstMlt

!!!
!Calcola j-esimo autovalore con l'iterazione di Laguerre
!!!
subroutine LagIt(x, mlt, aj, bj, en, em, Eigenvalues, numCol, j, fPrimo, fSecondo, kappa, lambdaJ)

implicit none

integer, parameter :: dp=kind(1.d0)

real(dp), intent(IN) :: x 

real(dp), intent(INOUT) :: aj, bj

integer, intent(IN) :: en, em, numCol, j

integer, intent(IN) :: mlt

integer, intento(INOUT) :: kappa

real(dp), intent(INOUT) :: fPrimo, fSecondo

real(dp), dimension(en,em), intent(INOUT) :: Eigenvalues

integer :: i, j, k, l, exKappa

real(dp) :: deltaL

real(dp), dimension(-2:0) :: xl

real(dp), intent(OUT) :: lambdaJ

!vedi p. 16

lambdaJ = 0.d0
!Se uscendo dallla subroutine questo valore non e`
!cambiato allora il calcolo e` errato.

xl(-2) = x
xl(-1) = x
xl(0) = x

l = 2

do while ( .TRUE. )

   exKappa = kappa

   !aggiorno le variabili
   xl(-2) = xl(-1)
   xl(-1) = xl(0)

   if ( kappa < j ) then
      !Calcolo xl(0) = L_{mlt +}(xl(-1))
      xl(0) = xl(-1) + (n*1.d0)/(-fPrimo + sqrt( ((n-mlt)/(mlt*1.d0))* &
     ( (n-1)*fPrimo**2 - n*fSecondo ) ) )
   else
      !Calcolo x_l(0) = L_{mlt -}(xl(-1))
      xl(0) = xl(-1) + (n*1.d0)/(-fPrimo - sqrt( ((n-mlt)/(mlt*1.d0))* &
     ( (n-1)*fPrimo**2 - n*fSecondo ) ) )
   end if

   deltal = xl(0) - xl(-1)

  ! condizione (24)
  if ( ( abs(xl(0)-xl(-1)) <= machinePrecision*abs(xl(0)) ) .OR. & 
  ( abs(xl(0)-xl(-1)) >= abs(xl(-1)-xl(-2)) ) .OR. &
  ( ((xl(0)-xl(-1))**2)/(abs(xl(-1)-xl(-2))-xl(0)-xl(-1)) <= machinePrecision*abs(xl(0)) )  ) then
     GOTO 30
  end if

  !calcolo (12), (13) e (14)
  20 call  calcoli(xl(0), T, S, n, fPrimo, fSecondo, kappa)

  !aggiorno [aj, bj] secondo il nuovo kappa
  if ( ( mlt > 1 ) .AND. ( abs(kappa-exKappa) > 1 ) ) then
     mlt = abs(kappa-exKappa)
     xl(0) = (xl(0)+xl(-1))/2.d0
     GOTO 20
  end if

  l = l+1

end do

30 lambdaJ = xl(0)

!OSS: incrementare l non modifica il calcolo ma indica l'avanzamento
!dello stesso

end subroutine LagIt

!!!                                                                    
!Calcola (12), (13) e (14)               
!!!                                                                    
subroutine calcoli(x, T, S, n, fPrimo, fSecondo, kappa)

implicit none

integer, parameter :: dp=kind(1.d0)

real(dp), intent(IN) :: x

integer, intent(IN) :: n

integer, intent(OUT) :: kappa

real(dp), intent(OUT) :: fPrimo, fSecondo

real(dp), dimension(n,n), intent(IN) :: T, S

integer :: i, j, k, l

real(dp), dimension(-2:0) :: xi, eta, zeta

!FINE DICHIARAZIONI

!OSS: ro_i=prodotto di xi_k per k=1, ..., i
!OSS: necessito in ogni momento di xi_{i-1}, zeta_{i-1}, zeta_{i-2}, eta_{i-1} ed eta_{i-2}

xi(-2) = 0.d0

xi(-1) = T(1,1) - x * S(1,1)
if ( xi(-1) == 0 ) then
   xi(-1) = T(1,1)* machinePrecision**2
end if

eta(-2) = 0.d0
eta(-1) = S(1,1)/xi(0)

zeta(-1) = zeta(-2) = 0.d0

kappa = 0

do i=2,n

!mi occupo di xi

xi(0) = T(i,i) - x*S(i,i) - (( T(i-1,i)-x*S(i-1,i) )**2)/xi(-1)

if ( xi(0) == 0 ) then
   xi(0) = ( (abs(T(i-1,i))+abs(x*S(i-1,i)))**2 * machinePrecision**2 )/xi(-1)
end if

!mi occupo di eta

eta(0) = ( (T(i,i)-x*S(i,i))*eta(-1) + S(i,i) - ( 2*(T(i-1,i)-x*S(i-1,i))*S(i-1,i) + (T(i-1,i)-x*S(i-1,i))**2 * eta(-2) )/xi(-1) )/xi(0)

!mi occupo di zeta

zeta(0) = ( - ( 2*S(i-1,i)**2 + 4*(T(i-1,i)-x*S(i-1,i))*S(i-1,i)*eta(-2) - (T(i-1,i)-x*S(i-1,i))**2*zeta(-2) )/xi(-1) )/xi(0)

!tengo conto di quanti termini negativi compaiono nella successione degli xi

if ( xi(0) < 0.d0 ) then
   kappa = kappa+1
end if

!aggiiorno le variabili

xi(-2)=xi(-1)
xi(-1)=xi(0)

eta(-2)=eta(-1)
eta(-1)=eta(0)

zeta(-2) = zeta(-1)
zeta(-1) = zeta(0)

end do

!immagazzino i risultati in variabili dal nome piu` evocativo
fPrimo = - eta(0)
fSecondo = zeta(0)

end subroutine calcoli
