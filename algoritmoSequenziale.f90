!!!
!PROGRAMMA PRINCIPALE
!!!
program sperimentazione

implicit none

integer, parameter :: dp = kind(1.d0)

integer :: n, i, j, em, en

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

allocate( Eigenvalues(em,en) )

do j=1,en
   do i=1,em
      Eigenvalues(i,j) = 0.d0
   end do
end do

!Chiamo la subroutine che trova gli autovalori nell'intervallo
![a,b] e li salva nella prima colonna della matrice Eigenvalues
!(le altre colonne di Eigenvalues vengono utilizzate nella
!ricorsione e possono fornire informazioni sui calcoli).
!Per guidare la scrittura nella matrice utiliziamo una bandiera:
!se flag>0 allora scriviamo dall'alto verso il basso,
!se flag<0 allora scriviamo dal basso verso l'alto,
!se flag=0 allora siamo nella prima chiamata e dunque e` indifferente;
!abbiamo scelto di scrivere dall'alto verso il basso.

a=-100.d0
b=100.d0

!ATTENZIONE: numCol=0 poiche' l'indice di colonna di Eigenvalues parte da 1.
!ATTENZIONE: la prima chiamata della subroutine e` con flag=0, le altre no.

call calcoloAutovaloriDentroI(0, a, b, n, T, S, 1, n, 1, n, en, em, Eigenvalues, 0)

!Scrivo la prima colonna della matrice Eigenvalues sul file "risultato.txt"

open(unit=3,file="risultato.txt")

write(3,*) em

do i=1,em
   write(3,*) Eigenvalues(i,1)
end do

write(*,*) "FINE CALCOLO AUTOVALORI!"

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
recursive subroutine calcoloAutovaloriDentroI(flag, a, b, n, T, S, & 
Tinizio, Tfine, Sinizio, Sfine, en, em, Eigenvalues, numCol)
!L'intervallo I=[a,b] e` identificato dai suoi estremi

implicit  none

!dichiaro la precisione di macchina (doppia):
integer, parameter :: dp = kind(1.d0)

integer, intent(IN) :: flag

integer, intent(IN) :: n

integer, intent(IN) :: en, em

integer :: numCol

real(dp), intent(IN) :: a, b

real(dp), dimension(n,n), intent(IN) :: T, S
real(dp), dimension(em,en), intent(INOUT) :: Eigenvalues

!indici per identificare la T e la S in input:                                                                                   
integer :: Tinizio, Tfine, Sinizio, Sfine
!ATTENZIONE: voglio siano variabili locali, dunque non aggiungo SAVE!!!

!Di seguito le variabili che non provengono dall'esterno

!indici per identificare le due nuove pencil:
integer :: T0inizio, T0fine, T1inizio, T1fine, S0inizio, S0fine,S1inizio, S1fine
!Anche qui uso variabili locali

integer :: i, j, k, dim, kappa, k1, k2, segno, mlt

real(dp) :: machinePrecision, x, aj, bj, fPrimo, fSecondo, lambdaJ

real(dp) :: menoBeqSecGrado, cEqSecGrado

!$
real(dp), dimension(em) :: vettoreAusiliario
!$

!!!
!FINE DICHIARAZIONI
!!!


machinePrecision=epsilon(1.d0)

!!!
!SPLIT
!!!

!T... e S... si muovono nello stesso modo,
!prendo quindi come dimensione uno dei due:
dim = Tfine - Tinizio

!SE dim=1, (ovvero se le matrici sono 2x2), oppure 
!dim=0, (ovvero le matrici sono 1x1), 
!ALLORA CALCOLO DIRETTAMENTE GLI AUTOVALORI
if (dim <= 1) then
   if (dim == 0) then
      !non puo` accadere che S(Tinizio,Tinizio)=0
      !poiche' S e` non singolare; comunque controllo
      !che non sia "numericamente zero"
      if ( abs(S(Sinizio,Sinizio)) <= 10.d0*machinePrecision ) then
         !immagazzino zero, cioe` non aggiorno Eigenvalues
         return
      else
         if ( flag >= 0 ) then
            !altro verso basso
            Eigenvalues(1,numCol+1) = T(Tinizio,Tfine)/ &
            S(Sinizio,Sfine)
         else
            !basso verso alto
            Eigenvalues(em-1,numCol+1) = T(Tinizio,Tfine)/ &
            S(Sinizio,Sfine)
         end if
      end if
   else
      !se mi trovo qui allora dim=1
      !risolvo a mano S^{-1}T x = \lambda x.
      !Dopo aver controllato che S sia
      !"numericamente definita positiva", trascrivo
      !i risultati.

      if ( abs(S(Sinizio,Sinizio)*S(Sfine,Sfine)- &
      S(Sinizio,Sfine)**2)<= 10.d0*machinePrecision ) return

      menoBeqSecGrado = ( S(Sinizio,Sfine)*T(Tinizio,Tfine)- &
      S(Sinizio,Sinizio)*T(Tfine,Tfine)- &
      S(Sfine,Sfine)*T(Tinizio,Tinizio)+ &
      S(Sinizio,Sfine)*T(Tinizio,Tfine) )/ &
      ( -S(Sinizio,Sinizio)*S(Sfine,Sfine)+S(Sinizio,Sfine)**2 )

      cEqSecGrado = ( S(Sinizio,Sfine)**2*T(Tinizio,Tfine)**2 - &
      S(Sinizio,Sfine)*S(Sfine,Sfine)*T(Tinizio,Tinizio)* &
      T(Tinizio,Tfine) + &
      S(Sinizio,Sinizio)*S(Sfine,Sfine)*T(Tinizio,Tinizio)* &
      T(Tfine,Tfine) - &
      S(Sinizio,Sinizio)*S(Sinizio,Sfine)*T(Tinizio,Tfine)* &
      T(Tfine,Tfine) + &
      S(Sinizio,Sfine)*S(Sfine,Sfine)*T(Tinizio,Tinizio)* &
      T(Tinizio,Tfine) - &
      S(Sinizio,Sinizio)*S(Sfine,Sfine)*T(Tinizio,Tfine)**2 - &
      S(Sinizio,Sfine)**2*T(Tinizio,Tfine)*T(Tfine,Tfine) + &
      S(Sinizio,Sinizio)*S(Sinizio,Sfine)*T(Tinizio,Tfine)* &
      T(Tfine,Tfine) )/ &
      ( S(Sinizio,Sinizio)*S(Sfine,Sfine)-S(Sinizio,Sfine)**2 )
      
      !\lambda = \frac{ menoBeqSecGrado \pm
      !\sqrt{menoBeqSecGrad^2 -4*cEqSecGrado} }{2}
      
      !immagazzino i risultati
      if ( flag >= 0 ) then
         !altro verso basso
         Eigenvalues(1,numCol+1) = 5.d-1*(menoBeqSecGrado + &
         sqrt( menoBeqSecGrado**2 -4.d0*cEqSecGrado ))
         Eigenvalues(2,numCol+1) = 5.d-1*(menoBeqSecGrado - &
         sqrt( menoBeqSecGrado**2 -4.d0*cEqSecGrado ))
      else
         !basso verso alto
         Eigenvalues(em-1,numCol+1) = 5.d-1*(menoBeqSecGrado + &
         sqrt( menoBeqSecGrado**2 -4.d0*cEqSecGrado ))
         Eigenvalues(em-2,numCol+1) = 5.d-1*(menoBeqSecGrado - &
         sqrt( menoBeqSecGrado**2 -4.d0*cEqSecGrado ))
      end if

   end if
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

do j=k1,k2


   !Determino l'intervallo Ij=(aj, bj) in cui ho convergenza cubica
   !nel ricercare \lambda_j
   aj=a
   bj=b
   if ( bj-aj > max(aj,bj)*machinePrecision ) then
      x = Eigenvalues(j,numCol+1)
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
      if ( -fPrimo >= 0.d0 ) then
         segno = 1
      else
         segno = -1
      end if
      !segno = sign( - fPrimo )
      !vedi meta` p. 14

      call EstMlt(x, segno, en, em, Eigenvalues, numCol+1, j, mlt)
      call LagIt(x, mlt, aj, bj, n, dim, T, S, Tinizio, Tfine, Sinizio, &
      Sfine, en, em, Eigenvalues, numCol+1,j, fPrimo, fSecondo, kappa, lambdaJ)
      !immagazzino i risultati
      if ( flag >= 0 ) then
         !altro verso basso
         Eigenvalues(j,numCol+1) = lambdaJ
      else
         !basso verso alto
         Eigenvalues(em-j,numCol+1) = lambdaJ
      end if
   else
      if ( flag >= 0 ) then
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

!do i=1,em
!   vettoreAusiliario(i) = Eigenvalues(i,numCol+1)
!end do

!call quick_sort(vettoreAusiliario)

!do i=1,em
!   Eigenvalues(i,numCol+1) = vettoreAusiliario(i)
!end do

call quick_sort( Eigenvalues(:,numCol+1), em )

!P.S. cosi` come e` scritta la chiamata al quick sort
!necessita la creazione di un array, chiamato "a", non
!necessario: si potrebbe lavorare direttamente sulla colonna
!numCol di Eigenvalues...

end subroutine calcoloAutovaloriDentroI


!!!
!Stima la molteplicita` dell'autovalore
!!!
subroutine EstMlt(x, segno, en, em, Eigenvalues, numCol, j, mlt)

implicit none

integer, parameter :: dp=kind(1.d0)

real(dp), intent(IN) :: x

integer, intent(IN) :: segno, en, em, numCol, j

integer, intent(OUT) :: mlt

real(dp), dimension(em,en), intent(INOUT) :: Eigenvalues

integer :: k, m

real(dp) :: machinePrecision

!FINE DICHIARAZIONI

machinePrecision=epsilon(1.d0)

mlt=1

k=1

do while ( .TRUE. )

   m = j + k*segno

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
subroutine LagIt(x, mlt, aj, bj, n, dim, T, S, Tinizio, Tfine, Sinizio, &
Sfine, en, em, Eigenvalues, numCol, j, fPrimo, fSecondo, kappa, lambdaJ)

implicit none

integer, parameter :: dp=kind(1.d0)

real(dp), intent(IN) :: x

integer, intent(IN) :: n

real(dp), dimension(n,n), intent(IN) :: T, S 

integer, intent(INOUT) :: Tinizio, Tfine, Sinizio, Sfine

real(dp), intent(INOUT) :: aj, bj

integer, intent(IN) :: en, em, numCol, j

integer, intent(INOUT) :: mlt

integer, intent(INOUT) :: dim

integer, intent(INOUT) :: kappa

real(dp), intent(INOUT) :: fPrimo, fSecondo

real(dp), dimension(em,en), intent(INOUT) :: Eigenvalues

integer :: i, k, l, exKappa

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

integer, intent(INOUT) :: dim, Tinizio, Tfine, Sinizio, Sfine

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

zeta(-2) = 0.d0
zeta(-1) = zeta(-2)

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


recursive subroutine quick_sort(a, n)
!!!
!Copiata dal libro p.282
!!!

  implicit none

  integer, parameter :: dp=kind(1.d0)


  real(dp), dimension(n), intent(INOUT) :: a

  integer :: i

  integer :: n

  !n = size(a)

  if (n>1) then
     call partition(a,i)
     call quick_sort( a(:i-1), i-1 )
     call quick_sort( a(i+1:), n-i )
  end if

  contains

    subroutine partition(a,j)

      implicit none

      integer, parameter :: dp=kind(1.d0)

      real(dp), dimension(:), intent(INOUT) :: a

      integer, intent(OUT) :: j

      integer :: i,temp

      i=1
      j=size(a)

      do
         do
            if (i>j) exit
            if (a(j)>a(1)) exit
            i=i+1
         end do
         
         do
            if ( (j<i) .OR. (a(j)<=a(1)) ) exit
            j = j-1
         end do

         if (i>=j) exit

         temp=a(i)
         a(i)=a(j)
         a(j)=temp

      end do

      temp = a(j)
      a(j)=a(1)
      a(1)=temp

    end subroutine partition

end subroutine quick_sort
