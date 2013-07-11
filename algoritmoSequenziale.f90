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
!La nuova pensil (\hatT,\hupS) e` composta da sue sotto-pensil,
!(T0,S0) e (T1,S1), dove \hatT e` diagonale a blocchi T0 eT1 (
!similmente \hatS). Dunque gli autovalori di (\hatT,\hatS) sono
!l'unione degli autovaliri di (T0,S0) e di quelli di (T1,S1).  
!La subroutine calcoloAutovaloriDentroI chiama se stessa ricorsivamente
!due volte. Quando siamo alla "profondita` di ricorsione" identificata
!dal numero "numCol" vengono salvati nella colonna numCol-esima di 
!Eigenvalues gli autovalori delle pencil (T0,S0) e (T1,S1); 
!questi vengono successivamente ordinati e vanno a formare
!gli autovalori della pencil (\hatT, \hatS).
!La prima colonna di Eigenvalues contiene gli autovalori del problema 
!di partenza.  
!Si e` scelto di immagazzinare gli autovalori di (T0,S0) dall'alto
!verso il basso e quelli di (T1,S1) dal basso verso l'alto,
!per questo una bandiera "flag", positiva o negativa, orienta
!la subroutine stessa nella scrittura all'interno di Eigenvalues.
!!!
recursive subroutine calcoloAutovaloriDentroI(flag, a, b, n, T, S, Tinizio, Tfine, Sinizio, Sfine, en, em, Eigenvalues, numCol)
!L'intervallo I=[a,b] e` identificato dai suoi estremi

implicit  none

!dichiaro la precisione di macchina (doppia):
integer, parameter :: dp = kind(1.d0)

integer, intent(IN) :: flag

integer, intent(IN) :: n

integer, intent(IN) :: en, em

integer, save :: numCol

real(dp), intent(IN) :: a, b

real(dp), dimension(n,n), save, intent(IN) :: T, S
real(dp), dimension(em,en), save, intent(INOUT) :: Eigenvalues

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

call calcoloAutovaloriDentroI(1, a, b, n, T, S, T0inizio, T0fine, &
S0inizio, S0fine, en, em, Eigenvalues, numCol+1)

call calcoloAutovaloriDentroI(-1, a, b, n, T, S, T1inizio, T1fine, &
S1inizio, S1fine, en, em, Eigenvalues, numCol+1)

!!!
!MERGE, pagina 17 dell'articolo
!!!

!Ho a disposizione \hat\lambda_{k+1}, ..., \hat\lambda_{k+m}, 
!salvati nella numCol-esima colonna di Eigenvalues.
!Mi devo ricordare sempre che \hat\lambda_{k}=a e 
!\hat\lambda_{k+m+1}=b.
!Calcolo kappa(a) e kappa(b), e da questi ricavo k1 e k2
call calcoli(a, T, S, n, dim, Tinizio, Tfine, Sinizio, Sfine, fPrimo, fSecondo, kappa)
k1=kappa+1
call calcoli(b, T, S, n, dim, Tinizio, Tfine, Sinizio, Sfine, fPrimo, fSecondo, kappa)
k2=kappa

!OSS: nel caso del calcolo di tutti gli autovalori ho
!kappa(a)=0 e kappa(b)=n e dunque k1=0+1 e k2=dim

do j=k1:k2


   !Determino l'intervallo Ij=(aj, bj) in cui ho convergenza cubica
   !nel ricercare \lambda_j
   aj=a
   bj=b
   if ( bj-aj > max(aj,bj)*machinePrecision ) then
      x = Eigenvalues(j,numCol)
      !cioe` x=\hat\lambda_j.
      !Adesso chiamo la subroutine per il calcolo di (12), (13) e (14).
      100 call calcoli(x, T, S, n, dim, Tinizio, Tfine, Sinizio, Sfine, fPrimo, fSecondo, kappa)
  
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
      call LagIt(x, mlt, aj, bj, en, em, Eigenvalues, numCol, &
      j, fPrimo, fSecondo, kappa, lambdaJ)
      if ( flag > 0 ) then
         !altro verso basso
         Eigenvalues(j,numCol+1) = lambdaJ
      else
         !basso verso alto
         Eigenvalues(em-j,numCol+1) = lambdaJ
      end if
   else
      if ( flag > 0 ) then
         !alto verso basso
         Eigenvalues(j,numCol+1) = (aj+bj)/2.d0
      else
         !basso verso alto
         Eigenvalues(em-j,numCol+1) = (aj+bj)/2.d0
      end if
   end if
end do

!Unisco e riordino gli autovalori di (T0,S0) e (T1,S1) negli
!autovalori di (\hatT,\hatS)
call quick_sort(Eigenvalues, en, em, numCol+1)

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

real(dp), dimension(em,en), intent(INOUT) :: Eigenvalues

integer :: k, m

real(dp) :: machinePrecision

!FINE DICHIARAZIONI

machinePrecision=epsilon(1.d0)

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

real(dp), dimension(em,en), intent(INOUT) :: Eigenvalues

integer :: i, j, k, l, exKappa

real(dp) :: deltaL

real(dp), dimension(-2:0) :: xl

real(dp), intent(OUT) :: lambdaJ

real(dp) :: machinePrecision

!vedi p. 16

machinePrecision=epsilon(1.d0)

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
  ( ((xl(0)-xl(-1))**2)/(abs(xl(-1)-xl(-2))-xl(0)-xl(-1)) <= &
  machinePrecision*abs(xl(0)) )  ) then
     GOTO 30
  end if

  !calcolo (12), (13) e (14)
  20 call  calcoli(xl(0), T, S, n, dim, Tinizio, Tfine, Sinizio, &
  Sfine, fPrimo, fSecondo, kappa)

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
subroutine calcoli(x, T, S, n, dim, Tinizio, Tfine, Sinizio, Sfine, &
 fPrimo, fSecondo, kappa)

implicit none

integer, parameter :: dp=kind(1.d0)

real(dp), intent(IN) :: x

integer, intent(IN) :: n

integer, intent(IN) :: dim, Tinizio, Tfine, Sinizio, Sfine

integer, intent(OUT) :: kappa

real(dp), intent(OUT) :: fPrimo, fSecondo

real(dp), dimension(n,n), intent(IN) :: T, S

integer :: i, j, k, l

real(dp), dimension(-2:0) :: xi, eta, zeta

real(dp) :: machinePrecision

!FINE DICHIARAZIONI

machinePrecision=epsilon(1.d0)


!ATTENZIONE: tutte le formule che coinvolgono T o S DEVONO partire
!da Tinizio e da Sinizio. T ed S sono state definite con indici che
!vanno da 1 a n, dunque per iniziare a contare da 1 ri-definisco
!Tinizio, Tfine, Sinizio ed Sfine.
Tinizio = Tinizio-1
Tfine = Tfine-1
Sinizio = Sinizio-1
Sfine = Sfine-1 

!OSS: ro_i=prodotto di xi_k per k=1, ..., i
!OSS: necessito in ogni momento di xi_{i-1}, zeta_{i-1}, zeta_{i-2}, eta_{i-1} ed eta_{i-2}

xi(-2) = 0.d0

xi(-1) = T(Tinizio+1,Tinizio+1) - x * S(Sinizio+1,Sinizio+1)
if ( xi(-1) == 0 ) then
   xi(-1) = T(Tinizio+1,Tinizio+1)* machinePrecision**2
end if

eta(-2) = 0.d0
eta(-1) = S(Sinizio+1,Sinizio+1)/xi(0)

zeta(-1) = zeta(-2) = 0.d0

kappa = 0

do i=2,dim

   !mi occupo di xi

   xi(0) = T(Tinizio+i,Tinizio+i) - x*S(Sinizio+i,Sinizio+i) - &
   (( T(Tinizio+i-1,Tinizio+i)-x*S(Sinizio+i-1,Sinizio+i) )**2)/xi(-1)

   if ( xi(0) == 0 ) then
      xi(0) = ( (abs(T(Tinizio+i-1,Tinizio+i))+ & 
      abs(x*S(Sinizio+i-1,Sinizio+i)))**2 * machinePrecision**2 )/ &
      xi(-1)
   end if

   !mi occupo di eta

   eta(0) = ( (T(Tinizio+i,Tinizio+i)-x*S(Sinizio+i,Sinizio+i))* &
   eta(-1) + S(Sinizio+i,Sinizio+i) - ( 2*(T(Tinizio+i-1,Tinizio+i)- &
   x*S(Sinizio+i-1,Sinizio+i))*S(Sinizio+i-1,Sinizio+i) + & 
   (T(Tinizio+i-1,Tinizio+i)-x*S(Sinizio+i-1,Sinizio+i))**2 * &
    eta(-2) )/xi(-1) )/xi(0)

   !mi occupo di zeta

   zeta(0) = ( - ( 2*S(Sinizio+i-1,Sinizio+i)**2 + &
   4*(T(Tinizio+i-1,Tinizio+i)- x*S(Sinizio+i-1,Sinizio+i))* &
   S(Sinizio+i-1,Sinizio+i)*eta(-2) - (T(Tinizio+i-1,Tinizio+i)- &
   x*S(Sinizio+i-1,Sinizio+i))**2* & 
   zeta(-2) )/xi(-1) )/xi(0)

   !tengo conto di quanti termini negativi compaiono 
   !nella successione degli xi

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


recursive subroutine quick_sort(Eigenvalues, en, em, numCol+1)

  implicit none

  integer, parameter :: dp=kind(1.d0)

  integer, intent(IN) :: en, em, numCol

  real(dp), dimension(em,en), intent(INOUT) :: Eigenvalues

  integer :: i, j, k

  !da scrivere

end subroutine quick_sort
