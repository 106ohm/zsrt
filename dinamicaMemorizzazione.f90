!!!
!PROGRAMMA PRINCIPALE
!!!
!Al momento funziona solo per "n=2^numero",
!fra poco estendero` il calcolo al caso
!"n > 1".

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
en=int( log(n*1.d0)/log(2.d0) )

write(*,*) "en=",en

allocate( Eigenvalues(em,en) )

do j=1,en
   do i=1,em
      Eigenvalues(i,j) = 0.d0
   end do
end do

write(*,*) "Eigenvalues prima del calcolo:"
do i=1,em
   write(*,*) Eigenvalues(i,:)
end do

!Chiamo la subroutine che trova gli autovalori nell'intervallo
![a,b] e li salva nella prima colonna della matrice Eigenvalues
!(le altre colonne di Eigenvalues vengono utilizzate nella
!ricorsione e possono fornire informazioni sui calcoli).
!Per guidare la scrittura nella matrice utiliziamo una bandiera:
!se flag>0 allora scriviamo dall'alto verso il basso,
!se flag<0 allora scriviamo dal basso verso l'alto.

a=0.d0
b=5.d0


call calcoloAutovaloriDentroI(a, b, n, T, S, en, em, Eigenvalues)

!Scrivo la prima colonna della matrice Eigenvalues sul file 
!"risultato.txt"

write(*,*) "Eigenvalues dopo il calcolo:"
do i=1,n
   write(*,*) Eigenvalues(i,:)
end do

Open(unit=3,file="risultato.txt")

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
!Quando siamo alla "profondita`" identificata
!dal numero "numCol" vengono salvati nella colonna numCol-esima di 
!Eigenvalues gli autovalori delle pencil (T0,S0) e (T1,S1); 
!questi vengono successivamente ordinati e vanno a formare
!gli autovalori della pencil (\hatT, \hatS).
!La prima colonna di Eigenvalues contiene gli autovalori del problema 
!di partenza.  
!Si e` scelto di immagazzinare gli autovalori di (T0,S0) dall'alto
!verso il basso e quelli di (T1,S1) dal basso verso l'alto,
!per questo una bandiera "flag", positiva o negativa, orienta
!la scrittura all'interno di Eigenvalues.
!!!
subroutine calcoloAutovaloriDentroI(a, b, n, T, S, en, em, Eigenvalues)
!L'intervallo I=[a,b] e` identificato dai suoi estremi

implicit  none

!dichiaro la precisione di macchina (doppia):
integer, parameter :: dp = kind(1.d0)

integer, intent(IN) :: n

integer, intent(IN) :: en, em

real(dp), intent(IN) :: a, b

real(dp), dimension(n,n), intent(IN) :: T, S

real(dp), dimension(em,en), intent(INOUT) :: Eigenvalues

!indici per identificare la T e la S:                       
                                                          
integer :: Tinizio, Tfine, Sinizio, Sfine

integer :: flag

integer :: numCol

integer :: i, j, k, dim, kappa, k1, k2, segno, mlt


real(dp) :: machinePrecision, x, aj, bj, fPrimo, fSecondo, lambdaJ

real(dp) :: alphaSecGrado, betaSecGrado, deltaSecGrado, gammaSecGrado

!!!
!FINE DICHIARAZIONI
!!!


machinePrecision=epsilon(1.d0)

!T... e S... si muovono nello stesso modo,
!prendo quindi come dimensione uno dei due:

dim=2

numCol=en

do while (dim <= n)
   
   do k=0, n/dim - 1

      Tinizio = 1 + k * dim
      Tfine = (1 + k) * dim

      Sinizio = 1 + k * dim
      Sfine = (1 + k) * dim

      !SE dim=2, (ovvero se le matrici sono 2x2), oppure 
      !dim=1, (ovvero le matrici sono 1x1), 
      !ALLORA CALCOLO DIRETTAMENTE GLI AUTOVALORI
      if (dim <= 2) then

      
         if (dim == 1) then
            !non puo` accadere che S(Tinizio,Tinizio)=0
            !poiche' S e` non singolare; comunque controllo
            !che non sia "numericamente zero"
            if ( abs(S(Sinizio,Sinizio)) <= 10.d0*machinePrecision ) then
               !immagazzino zero, cioe` non aggiorno Eigenvalues
               return
            else
               Eigenvalues(Tinizio,numCol) = T(Tinizio,Tfine)/ &
                    S(Sinizio,Sfine)
            end if
         else
            !se mi trovo qui allora dim=2
            !risolvo a mano S^{-1}T x = \lambda x.
            
            deltaSecGrado = abs(S(Sinizio,Sinizio)*S(Sfine,Sfine)- &
                 S(Sinizio,Sfine)**2)

            if ( deltaSecGrado <= 10.d0*machinePrecision ) then 
               write(*,*) "evitata divisione per zero: detaSecGrado troppo piccolo!"
               return
            end if

            !                              |\alpha  \gamma|
            !chiamo S^{-1}T = delta^{-1} * |              | e risolvo
            !                              |\gamma  \beta |
            !
            !det[ S^{-1}T - \lambdaI ]=0.
            !
            ! alla fine ottengo \lambda = frac{1}{2*\delta} * 
            ![ \alpha+\beta \pm \sqrt{ \alpha^2 + \beta^2 + 
            !\gamma^2 - \alpha*\beta } ]
         
            alphaSecGrado = S(Sfine,Sfine)*T(Tinizio,Tinizio) - &
                 S(Sinizio,Sfine)*T(Tinizio,Tfine)

            betaSecGrado = -S(Sinizio,Sfine)*T(Tinizio,Tfine) + &
                 S(Sinizio,Sinizio)*T(Tfine,Tfine)

            gammaSecGrado = -S(Sinizio,Sfine)*T(Tinizio,Tfine) + &
                 S(Sinizio,Sinizio)*T(Tinizio,Tfine)

            !controllo di non dover estrarre una radice negativa:
            if ( alphaSecGrado**2 + betaSecGrado**2 + gammaSecGrado**2 - &
                 2.d0*alphaSecGrado*betaSecGrado < 0.d0 ) then
               write(*,*) "radice negativa!"
               return
            end if

            !!!
            !immagazzino i risultati
            !!!

            flag = 1
            !altro verso basso
         
            Eigenvalues(Tinizio,numCol) = ( 1.d0/(2.d0*deltaSecGrado) ) * & 
                 ( alphaSecGrado + betaSecGrado + sqrt( alphaSecGrado**2 + &
                 betaSecGrado**2 + gammaSecGrado**2 -2.d0*alphaSecGrado* &
                 betaSecGrado ) ) 

            write(*,*),"(",Tinizio,",",numCol,")=",Eigenvalues(Tinizio,numCol)
            
            Eigenvalues(Tinizio+1,numCol) = ( 1.d0/(2.d0*deltaSecGrado) ) * & 
                 ( alphaSecGrado + betaSecGrado - sqrt( alphaSecGrado**2 + &
                 betaSecGrado**2 + gammaSecGrado**2 -2.d0*alphaSecGrado* &
                 betaSecGrado ) )

            write(*,*),"(",Tinizio+1,",",numCol,")=",Eigenvalues(Tinizio+1,numCol)

            flag = -1
            !basso verso alto
         
            Eigenvalues(Tfine,numCol) = ( 1.d0/(2.d0*deltaSecGrado) ) * & 
                 ( alphaSecGrado + betaSecGrado + sqrt( alphaSecGrado**2 + &
                 betaSecGrado**2 + gammaSecGrado**2 -2.d0*alphaSecGrado* &
                 betaSecGrado ) )

            write(*,*),"(",Tfine,",",numCol,")=",Eigenvalues(Tfine,numCol)

            Eigenvalues(Tfine-1,numCol) = ( 1.d0/(2.d0*deltaSecGrado) )* &
                 ( alphaSecGrado + betaSecGrado - sqrt( alphaSecGrado**2 + &
                 betaSecGrado**2 + gammaSecGrado**2 -2.d0*alphaSecGrado* &
                 betaSecGrado ) )

            write(*,*),"(",Tfine-1,",",numCol,")=",Eigenvalues(Tfine-1,numCol)

         end if

         
      GOTO 200
      !cioe` riparti dal "do k=... end do" con k+1

      end if



      !!!
      !Adesso dim>2 e numCol=numCol-1
      !!!


      !Riordino gli autovalori di (T0,S0) e (T1,S1) negli
      !autovalori di (\hatT,\hatS).
      call quick_sort( Eigenvalues(:,numCol+1), em )


      !cerco k1 e k2

      call calcoli(a, T, S, n, dim, Tinizio, Tfine, Sinizio, Sfine, &
           fPrimo, fSecondo, kappa)
      k1=kappa+1
      call calcoli(b, T, S, n, dim, Tinizio, Tfine, Sinizio, Sfine, &
           fPrimo, fSecondo, kappa)
      k2=kappa+1

      write(*,*)"k1=",k1,"k2=",k2


      !OSS: nel caso del calcolo di tutti gli autovalori ho,
      !nella 0-esima chiamata ricorsiva,
      !kappa(a)=0 e kappa(b)=n e dunque k1=0+1 e k2=dim
      
      do j=k1,k2

         !write(*,*)"j=",j

         !Determino l'intervallo Ij=(aj, bj) in cui ho convergenza cubica
         !nel ricercare \lambda_j
         aj=a
         bj=b
         if ( bj-aj > max(aj,bj)*machinePrecision ) then

            x = Eigenvalues(j,numCol+1)
            !cioe` x=\hat\lambda_j.
            !Chiamo la subroutine per il calcolo di (12), (13) e (14).
100         call calcoli(x, T, S, n, dim, Tinizio, Tfine, &
                 Sinizio, Sfine, fPrimo, fSecondo, kappa)

            if ( kappa < j ) then
               aj = x
            else
               bJ = x
            end if
      
            !write(*,*)"x=",x

            !Adesso cerco un nuovo x
            if ( kappa /= j-1 .AND. kappa /= j ) then
               x = (aj+bj)/2.d0
               !ripeto il calcolo fatto alla etichetta 100:
               !Write(*,*)"GOTO 100"
               GOTO 100
            end if

            !Chiamo EstMlt e LagIt, ma prima mi occupo del segno
            if ( -fPrimo >= 0.d0 ) then
               segno = 1
            else
               segno = -1
            end if
      
            !segno = sign( - fPrimo )
            !vedi meta` p. 14

            !write(*,*)"inizio EstMlt"
            call EstMlt(x, segno, en, em, Eigenvalues, numCol+1, j, mlt)
            !write(*,*)"fine EstMlt"

            !write(*,*)"j=",j,"mlt=",mlt
            !write(*,*)"aj=",aj,"bj=",bj

            !write(*,*)"inizio LagIt"
            call LagIt(x, mlt, aj, bj, n, dim, T, S, Tinizio, Tfine, &
                 Sinizio, Sfine, en, em, Eigenvalues, numCol+1, j, &
                 fPrimo, fSecondo, kappa, lambdaJ)
            !write(*,*)"fine LagIt"

            !immagazzino i risultati
            
            flag = 1
            !altro verso basso
            Eigenvalues(Tinizio+j-1,numCol) = lambdaJ
            write(*,*),"(",j,",",numCol,")=",Eigenvalues(j,numCol)

            flag = -1
            !basso verso alto
            Eigenvalues(Tfine-j+1,numCol) = lambdaJ
            write(*,*),"(",Tfine-j+1,",",numCol,")=",Eigenvalues(Tfine-j+1,numCol)
      
         else
      
            !in questo caso a e b distano solo "un passo macchina"
            write(*,*)"a e b distano pochissimo!"

            flag = 1
            !alto verso basso
            Eigenvalues(Tinizio+j-1,numCol) = (aj+bj)/2.d0
            write(*,*),"(",Tinizio+j-1,",",numCol,")=",Eigenvalues(Tinizio+j-1,numCol)
      
            flag = -1
            !basso verso alto
            Eigenvalues(Tfine-j+1,numCol) = (aj+bj)/2.d0
            write(*,*),"(",Tfine-j+1,",",numCol,")=",Eigenvalues(Tfine-j+1,numCol)

         end if
   
      end do

200   write(*,*)""

   end do

   dim = dim * 2
   
   numCol = numCol-1

end do

!ordino la prima colonna di Eigenvalues
call quick_sort( Eigenvalues(:,1), em )

end subroutine calcoloAutovaloriDentroI


!!!
!Stima la molteplicita` dell'autovalore
!!!
subroutine EstMlt(x, segno, en, em, Eigenvalues, numCol, j, mlt)

implicit none

integer, parameter :: dp=kind(1.d0)

real(dp), intent(INOUT) :: x

integer, intent(IN) :: segno, en, em, numCol, j

integer, intent(INOUT) :: mlt

real(dp), dimension(em,en), intent(INOUT) :: Eigenvalues

integer :: k, m

real(dp) :: machinePrecision

!FINE DICHIARAZIONI

machinePrecision=epsilon(1.d0)

mlt=1

k=1

do while ( .TRUE. )

   m = j + k*segno

   if ( abs( Eigenvalues(j, numCol)-Eigenvalues(m, numCol) ) < & 
   0.01 * abs( Eigenvalues(j, numCol) - x ) ) then
      mlt = mlt + 1
   else
      GOTO 10
   end if

   k = k+1

end do

10 write(*,*)""

!OSS: incrementare k non modifica il calcolo, ma fornisce indicazioni
!sull'avanzamento dello stesso

end subroutine EstMlt

!!!
!Calcola j-esimo autovalore con l'iterazione di Laguerre
!!!
subroutine LagIt(x, mlt, aj, bj, n, dim, T, S, Tinizio, Tfine, &
Sinizio, Sfine, en, em, Eigenvalues, numCol, j, &
fPrimo, fSecondo, kappa, lambdaJ)

implicit none

integer, parameter :: dp=kind(1.d0)

real(dp), intent(INOUT) :: x

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

real(dp) :: deltaL, exDeltaL

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

exDeltaL=10.d0
deltaL=10.d0

l = 2

!write(*,*)"fPrimo=",fPrimo,"fSecondo=",fSecondo

do while ( .TRUE. )

   if ( l >= 10000 ) exit

   !write(*,*)"xl(-2)=",xl(-2),"xl(-1)=",xl(-1),"xl(0)=",xl(0)
   !write(*,*)"exDeltaL=",exDeltaL,"deltaL=",deltaL

   exKappa = kappa

   !aggiorno le variabili
   xl(-2) = xl(-1)
   xl(-1) = xl(0)

   if ( abs(fPrimo) <= machinePrecision .OR. &
   abs(fSecondo) <= machinePRecision ) then
      write(*,*)"fPrimo oppure fSecondo sono troppo piccoli!"
      !esco con xl(0) precedentemente calcolato
      GOTO 30
   end if

   if ( kappa < j ) then
      !Calcolo xl(0) = L_{mlt +}(xl(-1))
      xl(0) = xl(-1) + (n*1.d0)/(-fPrimo + &
      sqrt( ((n-mlt)/(mlt*1.d0))* ( (n-1)*fPrimo**2 - n*fSecondo ) ) )
   else
      !Calcolo x_l(0) = L_{mlt -}(xl(-1))
      xl(0) = xl(-1) + (n*1.d0)/(-fPrimo - &
      sqrt( ((n-mlt)/(mlt*1.d0))* ( (n-1)*fPrimo**2 - n*fSecondo ) ) )
   end if

   if ( isnan(xl(0)) ) then
      write(*,*)"condizione di arresto particolare: xl(0) e` NaN!"
      xl(0)=xl(-1)
      GOTO 30
   end if

   exDeltaL = xl(-1) - xl(-2)
   deltaL = xl(0) - xl(-1)

   ! condizione (24)

   if ( abs(deltaL) <= machinePrecision*abs(xl(0)) ) then
      write(*,*)"condizione di arresto (24) del primo tipo"
      GOTO 30
   end if

   if ( abs(deltaL) >= abs(exDeltaL) ) then
      write(*,*)"condizione di arresto (24) del secondo tipo"
      GOTO 30
   end if

   if ( deltaL**2/( abs(exDeltaL)-xl(0)-xl(-1) ) <= &
   machinePrecision*abs(xl(0)) ) then
      write(*,*)"condizione di arresto (24) del terzo tipo"
      GOTO 30
   end if

   !calcolo (12), (13) e (14)
20 call  calcoli(xl(0), T, S, n, dim, Tinizio, Tfine, Sinizio, &
   Sfine, fPrimo, fSecondo, kappa)

   !aggiorno [aj, bj] secondo il nuovo kappa
   if ( ( mlt > 1 ) .AND. ( abs(kappa-exKappa) > 1 ) ) then
      mlt = abs(kappa-exKappa)
      xl(0) = (xl(0)+xl(-1))/2.d0
      !write(*,*)"GOTO 20"
      GOTO 20
   end if

   l = l+1

end do

30 write(*,*) "l=",l 

lambdaJ = xl(0)

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


!OSS: ro_i=prodotto di xi_k per k=1, ..., i
!OSS: necessito in ogni momento di xi_{i-1}, zeta_{i-1}, 
!zeta_{i-2}, eta_{i-1} ed eta_{i-2}


!write(*,*)"dim=",dim

xi(-2) = 0.d0

xi(-1) = T(Tinizio,Tinizio) - x * S(Sinizio,Sinizio)
if ( xi(-1) == 0 ) then
   xi(-1) = T(Tinizio,Tinizio)* machinePrecision**2
end if

xi(0)=xi(-1)

eta(-2) = 0.d0
eta(-1) = S(Sinizio,Sinizio)/xi(0)
eta(0) = eta(-1)

zeta(-2) = 0.d0
zeta(-1) = zeta(-2)
zeta(0) = zeta(-1)

kappa = 0

do i=1,dim-1
      
   !ATTENZIONE: qui ho i=1,dim-1
   
   !mi occupo di xi
   
   xi(0) = T(Tinizio+i,Tinizio+i) - x*S(Sinizio+i,Sinizio+i) - &
   (( T(Tinizio+i-1,Tinizio+i)-x*S(Sinizio+i-1,Sinizio+i) )**2)/&
   xi(-1)

   if ( abs(xi(0)) <= machinePrecision ) then
      xi(0) = ( (abs(T(Tinizio+i-1,Tinizio+i))+ & 
      abs(x*S(Sinizio+i-1,Sinizio+i)))**2 * machinePrecision**2 )/&
      xi(-1)
   end if

   !mi occupo di eta:
   !divido in due tappe il calcolo.

   eta(0) =2.d0*(T(Tinizio+i,Tinizio+i)-x*S(Sinizio+i,Sinizio+i))*&
   S(Sinizio+i-1,Sinizio+i) + (T(Tinizio+i-1,Tinizio+i)- &
   x*S(Sinizio+i-1,Sinizio+i))**2*eta(-2)
      
   !if ( abs(eta(0))<= machinePrecision ) then
   !   write(*,*)"primo pezzo di eta e` zero!"
   !end if

   eta(0) = ( (T(Tinizio+i,Tinizio+i)-x*S(Sinizio+i,Sinizio+i))* &
   eta(-1) + S(Sinizio+i,Sinizio+i) - eta(0)/xi(-1) )/xi(0)

   if ( abs(eta(0))<= machinePrecision ) then
      write(*,*)"secondo -ed ultimo- pezzo di eta e` zero!"
   end if
   
   !mi occupo di zeta: 
   !divido in due tappe il calcolo.
      
   zeta(0) = 2.d0*S(Sinizio+i-1,Sinizio+i)**2 + &
   4.d0*( T(Tinizio+i-1,Tinizio+i)-x*S(Sinizio+i-1,Sinizio+i) )*&
   S(Sinizio+i-1,Sinizio+i)*eta(-2) - &
   ( T(Tinizio+i-1,Tinizio+i)-x*S(Sinizio+i-1,Sinizio+i) )**2*&
   zeta(-2)

   !if ( abs(zeta(0))<= machinePrecision ) then
   !   write(*,*)"primo pezzo di zeta e` zero!"
   !end if

   zeta(0) = (( T(Tinizio+i,Tinizio+i) -x*S(Sinizio+i,Sinizio+i))*&
   zeta(-1) + 2.d0*S(Sinizio+i,Sinizio+i)*eta(-1) - &
   zeta(0)/xi(-1) )/xi(0)
   
   if ( abs(zeta(0))<= machinePrecision ) then
      write(*,*)"secondo -ed ultimo- pezzo di zeta e` zero!"
   end if

   !tengo conto di quanti termini negativi compaiono 
   !nella successione degli xi
      
   if ( xi(0) <= 0.d0 ) then
      kappa = kappa+1
   end if
      
   !aggiiorno le variabili
      
   xi(-2)=xi(-1)
   xi(-1)=xi(0)

   eta(-2)=eta(-1)
   eta(-1)=eta(0)

   zeta(-2) = zeta(-1)
   zeta(-1) = zeta(0)

   !write(*,*)"xi(-2)=",xi(-2),"xi(-1)=",xi(-1),"xi(0)=",xi(0)
   !write(*,*)"eta(-2)=",eta(-2),"eta(-1)=",eta(-1),"eta(0)=",eta(0)
   !write(*,*)"zeta(-2)=",zeta(-2),"zeta(-1)=",zeta(-1),"zeta(0)=", zeta(0)
   !write(*,*)"kappa=",kappa
   
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

  integer, intent(IN) :: n

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

      integer :: i

      real(dp) :: temp

      i=1
      j=size(a)

      do
         do
            if ( i>j ) exit
            if ( a(j) > a(1) ) exit
            i=i+1
         end do
         
         do
            if ( (j<i) .OR. ( a(j) <= a(1) ) ) exit
            j = j-1
         end do

         if ( i >= j ) exit

         temp = a(i)
         a(i) = a(j)
         a(j) = temp

      end do

      temp = a(j)
      a(j) = a(1)
      a(1) = temp

    end subroutine partition

end subroutine quick_sort
