!!!
!PROGRAMMA PRINCIPALE
!!!
!Al momento funziona solo per "n=2^numero",
!fra poco estendero` il calcolo al caso
!"n > 1".

program sperimentazione

implicit none

integer, parameter :: dp = kind(1.d0)

integer :: n, i, j, em, en, numAut

integer :: verbose

real(dp) :: a,b, lettore1, lettore2, machinePrecision

real(dp), dimension(:,:), allocatable :: T, S

real(dp), dimension(:,:), allocatable :: Eigenvalues

!!!
!FINE DICHIARAZIONI
!!!

machinePrecision=epsilon(1.d0)

!verbose=
!0) non stampo a video informazioni
!1) stapo Eigenvalues prima e dopo il calcolo
!2) stampo k1, k2 e le condizioni di arresto
!3) stampo (per ogni j=k1,k2) l'intervallo
!chiamato [aj, bj], il numero x iniziale,
! con la sua mlt, ed il numero di iterazioni
!4) stampo le informazioni dentro i cicli. 
verbose = 3

!leggo, per colonne, il contenuto dei file "T.txt" ed "S.txt",
!alloco la memoria e carico le matrici T ed S;
!cosi` avro` a disposizione la pencil (T,S).

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
         T(j,1) = lettore1
         S(j,1) = lettore2
      end if
   end do
end do

T(1,1)=0.d0
S(1,1)=0.d0

!ATTENZIONE!
!Il contenuto della sopradiagonale di T (e similmente di S)
!lo leggo in T(2:n,1), cioe` partendo dal secondo elemento di T(:,1)

do i=1,n
   write(*,*)"S(",i,",0)=",S(i,0)
end do

do i=1,n
   write(*,*)"S(",i,",1)=",S(i,1)
end do

!scelgo le dimensioni di Eigenvalues, alloco memoria
!ed inizializzo i suoi valori a zero

em=n
!ATTENIONE: il "logarithmus dualis", ovvero in base 2,
!lo calcoliamo tramite ld(n)=log(n)/log(2)
en=int( log(n*1.d0)/log(2.d0) )

allocate( Eigenvalues(em,en) )

do j=1,en
   do i=1,em
      Eigenvalues(i,j) = 0.d0
   end do
end do

if ( verbose >= 1 ) then

   write(*,*) "en=",en

   write(*,*) "Eigenvalues prima del calcolo:"
   do i=1,em
      write(*,*) Eigenvalues(i,:)
   end do
end if


!Chiamo la subroutine che trova gli autovalori nell'intervallo
![a,b] e li salva nella prima colonna della matrice Eigenvalues.


!!!
!i)
!!!
write(*,*)""
write(*,*)""
write(*,*)"i)"
write(*,*)""
write(*,*)""

a=0.d0
!b=1.d0 + machinePrecision
b=1.d0

call calcoloAutovaloriDentroI(a, b, n, T, S, en, em, Eigenvalues, verbose)

!Scrivo la prima colonna della matrice Eigenvalues sul file 
!"risultato.txt"

if ( verbose >= 1 ) then
   write(*,*) "Eigenvalues dopo il calcolo:"
   do i=1,n
      write(*,*) Eigenvalues(i,:)
   end do
end if

Open(unit=3,file="risultato_i.txt")

write(3,*) em

do i=1,em
   write(3,*) Eigenvalues(i,1)
end do


!!!
!ii)
!!!
write(*,*)""
write(*,*)""
write(*,*)"ii)"
write(*,*)""
write(*,*)""

do j=1,en
   do i=1,em
      Eigenvalues(i,j) = 0.d0
   end do
end do

a=0.d0
b=1.d0


call calcoloAutovaloriDentroI(a, b, n, S, T, en, em, Eigenvalues, verbose)

!Scrivo la prima colonna della matrice Eigenvalues sul file 
!"risultato.txt"

if ( verbose >= 1 ) then
   write(*,*) "Eigenvalues dopo il calcolo:"
   do i=1,n
      write(*,*) Eigenvalues(i,:)
   end do
end if

Open(unit=4,file="risultato_ii.txt")

write(4,*) em

do i=1,em
   if ( abs(Eigenvalues(i,1)) > machinePrecision ) then
      write(4,*) 1.d0/Eigenvalues(i,1)
   else
      write(4,*) 0.d0
   end if
end do


!!!
!iii)
!!!
write(*,*)""
write(*,*)""
write(*,*)"iii)"
write(*,*)""
write(*,*)""

do j=1,en
   do i=1,em
      Eigenvalues(i,j) = 0.d0
   end do
end do

a=0.d0
b=1.d0


call calcoloAutovaloriDentroI(a, b, n, S, -T, en, em, Eigenvalues, verbose)

!Scrivo la prima colonna della matrice Eigenvalues sul file 
!"risultato.txt"

if ( verbose >= 1 ) then
   write(*,*) "Eigenvalues dopo il calcolo:"
   do i=1,n
      write(*,*) Eigenvalues(i,:)
   end do
end if

Open(unit=5,file="risultato_iii.txt")

write(5,*) em

do i=1,em
   if ( abs(Eigenvalues(i,1)) > machinePrecision ) then
      write(5,*) -1.d0/Eigenvalues(i,1)
   else
      write(5,*) 0.d0
   end if
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
subroutine calcoloAutovaloriDentroI(a, b, n, T, S, en, em, Eigenvalues, verbose)
!L'intervallo I=[a,b] e` identificato dai suoi estremi

implicit  none

!dichiaro la precisione di macchina (doppia):
integer, parameter :: dp = kind(1.d0)

integer, intent(IN) :: n

integer, intent(IN) :: en, em

real(dp), intent(IN) :: a, b

integer, intent(IN) :: verbose

real(dp), dimension(1:n,0:1), intent(IN) :: T, S

real(dp), dimension(em,en), intent(INOUT) :: Eigenvalues

!indici per identificare la T e la S:                       
                                                          
integer :: Tinizio, Tfine, Sinizio, Sfine

integer :: numCol, countSubInterval

integer :: i, j, k, h, dim, kappa, kappaA, kappaB, k1, k2, segno, mlt

integer :: numAut

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
            if ( abs(S(Sinizio,0)) <= 10.d0*machinePrecision ) then
               !immagazzino zero, cioe` non aggiorno Eigenvalues
               return
            else
               Eigenvalues(Tinizio,numCol) = T(Tinizio,1)/ &
                    S(Sinizio,1)
            end if
         else
            !se mi trovo qui allora dim=2
            !risolvo a mano S^{-1}T x = \lambda x.
            
            deltaSecGrado = abs(S(Sinizio,0)*S(Sfine,0)- &
                 S(Sinizio,1)**2)

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
         
            alphaSecGrado = S(Sfine,0)*T(Tinizio,0) - &
                 S(Sinizio,1)*T(Tinizio,1)

            betaSecGrado = -S(Sinizio,1)*T(Tinizio,1) + &
                 S(Sinizio,0)*T(Tfine,0)

            gammaSecGrado = -S(Sinizio,1)*T(Tinizio,1) + &
                 S(Sinizio,0)*T(Tinizio,1)

            !controllo di non dover estrarre una radice negativa:
            if ( alphaSecGrado**2 + betaSecGrado**2 + gammaSecGrado**2 - &
                 2.d0*alphaSecGrado*betaSecGrado < 0.d0 ) then
               write(*,*) "radice negativa!"
               return
            end if

            !!!
            !immagazzino i risultati,
            !d'altro verso basso
            !!!

            Eigenvalues(Tinizio,numCol) = ( 1.d0/(2.d0*deltaSecGrado) ) * & 
                 ( alphaSecGrado + betaSecGrado + sqrt( alphaSecGrado**2 + &
                 betaSecGrado**2 + gammaSecGrado**2 -2.d0*alphaSecGrado* &
                 betaSecGrado ) ) 

            !write(*,*),"(",Tinizio,",",numCol,")=",Eigenvalues(Tinizio,numCol)
            
            Eigenvalues(Tinizio+1,numCol) = ( 1.d0/(2.d0*deltaSecGrado) ) * & 
                 ( alphaSecGrado + betaSecGrado - sqrt( alphaSecGrado**2 + &
                 betaSecGrado**2 + gammaSecGrado**2 -2.d0*alphaSecGrado* &
                 betaSecGrado ) )

            !write(*,*),"(",Tinizio+1,",",numCol,")=",Eigenvalues(Tinizio+1,numCol)


         end if

         
      GOTO 200
      !cioe` riparti dal "do k=... end do" con k+1

      end if

      !!!
      !Adesso dim>2 e numCol=numCol-1
      !!!


      !Riordino gli autovalori di (T0,S0) e (T1,S1) negli
      !autovalori di (\hatT,\hatS).
      !call quick_sort( Eigenvalues(:,numCol+1), em )


      !cerco k1 e k2

      call calcoli(a, T, S, n, dim, Tinizio, Tfine, Sinizio, Sfine, &
           fPrimo, fSecondo, kappa)
      !call numAutovaloriPrimaDiX(a,dim,T(Tinizio:Tfine,:),S(Sinizio:Sfine,:),numAut)
      !if ( verbose >= 3 .AND. kappa /= numAut ) then
      !   write(*,*)"kappa e` diverso da numAut!"
      !   write(*,*)"kappa(a)=",kappa,"numAut=",numAut
      !end if
      k1 = kappa+1
      !k1 = numAut+1
      call calcoli(b, T, S, n, dim, Tinizio, Tfine, Sinizio, Sfine, &
           fPrimo, fSecondo, kappa)
      
      !call numAutovaloriPrimaDiX(b,dim,T(Tinizio:Tfine,:),S(Sinizio:Sfine,:),numAut)
      !if ( verbose >= 3 .AND. kappa /= numAut ) then
      !   write(*,*)"kappa e` diverso da numAut!"
      !   write(*,*)"kappa(a)=",kappa,"numAut=",numAut
      !end if
      k2=kappa
      !k2=numAut
      
      if (verbose >= 2) then
         write(*,*)"-------------------------------------"
         write(*,*)"|Adesso dim=",dim
         write(*,*)"|Tinizio=",Tinizio,"Tfine=",Tfine
         write(*,*)"|k1=",k1,"k2=",k2
         write(*,*)"-------------------------------------"
      End if


      !OSS: nel caso del calcolo di tutti gli autovalori ho,
      !nella 0-esima chiamata ricorsiva,
      !kappa(a)=0 e kappa(b)=n e dunque k1=0+1 e k2=dim
      
      
      if ( k1 > k2 ) then
         write(*,*)"NON CALCOLO"
         GOTO 200
      end if

      do j = k1, k2

         !Determino l'intervallo Ij=(aj, bj) in cui ho convergenza cubica
         !nel ricercare \lambda_j
         aj=a
         bj=b

         if ( bj-aj > max(aj,bj)*machinePrecision ) then
            
            x = Eigenvalues(Tinizio+j-1,numCol+1)
            !cioe` x=\hat\lambda_j.

            !if ( dim > 4 .AND. x == 0.d0 ) cycle
            !Se x == 0.d0 non e` un candidato

            if ( verbose >= 3 ) then
               write(*,*)"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               write(*,*)"Inizio a scegliere l'intervallo [aj, bj]:"
            end if


            countSubInterval=0

100         write(*,*)""

            countSubInterval = countSubInterval + 1

            !Chiamo la subroutine per il calcolo di (12), (13) e (14).
            if ( bj-aj <= max(aj,bj)*machinePrecision ) then
               write(*,*)"a e b distano pochissimo!"

               !immagazzino i risultati
               Eigenvalues(Tinizio+j-1,numCol) = (aj+bj)/2.d0
               !write(*,*),"(",Tinizio+j-1,",",numCol,")=",Eigenvalues(Tinizio+j-1,numCol)
               
               cycle
               
            end if
            
            call calcoli(x, T, S, n, dim, Tinizio, Tfine, &
                 Sinizio, Sfine, fPrimo, fSecondo, kappa)
            !call numAutovaloriPrimaDiX(x,dim,T(Tinizio:Tfine,:),S(Sinizio:Sfine,:),numAut)
            !if ( verbose >= 3 .AND. kappa /= numAut ) then
            !   write(*,*)"kappa e` diverso da numAut!"
            !   write(*,*)"kappa(a)=",kappa,"numAut=",numAut
            !end if
            !kappa=numAut

            if ( verbose >= 4 ) then
               write(*,*)"work in progres..."
               write(*,*)"x=",x,"j=",j,"kappa(x)=",kappa
            end if

            if ( kappa < j ) then
               aj = x
               kappaA = kappa
            else
               bJ = x
               kappaB = kappa
            end if
 

            !Definisco segno
            if ( -fPrimo >= 0.d0 ) then
               segno = 1
            else
               segno = -1
            end if

            !segno = sign( - fPrimo ) ed adesso dovrebbe coincidere
            !con sign( \lambda_j - x )
            !vedi meta` p. 14
     
            !Se il segno di -fPrimo non coincide con quello di
            !\lambda_j-x ( il fatto che questa condizione
            !coincida con quella scritta sotto e` da
            !ricercarsi a p. 17 dell'articolo)
            !allora procedo con la bisezione
            if (kappa+1 < j) then
               write(*,*)"UNO"
               x = (aj+bj)/2.d0
               GOTO 100
            end if

            if (j < kappa) then
               write(*,*)"DUE"
               x = (aj+bj)/2.d0
               GOTO 100
            end if

            if (kappa >= j .AND. segno >= 0) then
               write(*,*)"TRE"
               x = (aj+bj)/2.d0
               GOTO 100
            end if

            if (kappa < j .AND. segno < 0) then
               write(*,*)"QUATTRO"
               x = (aj+bj)/2.d0
               GOTO 100
            end if

            if (kappa == 0 .AND. segno <0) then
               write(*,*)"CINQUE"
               x = (aj+bj)/2.d0
               GOTO 100
            end if

            if (kappa == dim .AND. segno >=0) then
               write(*,*)"SEI"
               x = (aj+bj)/2.d0
               GOTO 100
            end if

            if ( verbose >= 4 ) then
               write(*,*)"countSubInterval=", countSubInterval
            end if


            if ( verbose >= 3 ) then
               write(*,*)"Ho scelto l'intervallo [aj, bj]:"
               write(*,*)"x=",x,"j=",j,"kappa(x)=",kappa
               write(*,*)"aj=",aj,"bj=",bj
               call calcoli(aj, T, S, n, dim, Tinizio, Tfine, &
                 Sinizio, Sfine, fPrimo, fSecondo, kappa)
               kappaA=kappa
               call calcoli(bj, T, S, n, dim, Tinizio, Tfine, &
                 Sinizio, Sfine, fPrimo, fSecondo, kappa)
               kappaB=kappa
               call calcoli(x, T, S, n, dim, Tinizio, Tfine, &
                 Sinizio, Sfine, fPrimo, fSecondo, kappa)
               write(*,*)"kappa(aj)=",kappaA,"kappa(bj)=",kappaB
            end if
            
            !!!
            !Chiamo EstMlt e LagIt
            !!!

            !call EstMlt(x, segno, en, em, Eigenvalues, en, j, mlt)
            call EstMlt(x, segno, en, em, Eigenvalues, numCol+1, j, mlt)

            if (verbose >= 3) then
               write(*,*)"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
               write(*,*)"Entro in LagIt"
               write(*,*)"la molteplicita` del j-esimo autovalore"
               write(*,*)"e` stimata essere mlt=",mlt,"(j=",j,")"
            end if


            call LagIt(x, mlt, aj, bj, n, dim, T, S, Tinizio, Tfine, &
                 Sinizio, Sfine, j, &
                 fPrimo, fSecondo, kappa, lambdaJ, verbose)

            if (verbose >= 3) then
               write(*,*)"Esco da LagIt"
            end if

            !immagazzino i risultati.
            Eigenvalues(Tinizio+j-1,numCol) = lambdaJ
            !write(*,*),"(",j,",",numCol,")=",Eigenvalues(j,numCol)

         else
      
            !in questo caso a e b distano solo "un passo macchina"
            write(*,*)"a e b distano pochissimo!"

            !immagazzino i risultati
            Eigenvalues(Tinizio+j-1,numCol) = (aj+bj)/2.d0
            !write(*,*),"(",Tinizio+j-1,",",numCol,")=",Eigenvalues(Tinizio+j-1,numCol)

         end if

   
      end do

200   write(*,*)""

   end do

   dim = dim * 2
   
   numCol = numCol-1

end do

!ordino la prima colonna di Eigenvalues
!call quick_sort( Eigenvalues(:,1), em )

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

!write(*,*)"segno=", segno

!k=1,...,numeroGrande (ho scelto 2*em)
do k=1,em*2

   m = j + k*segno

   !write(*,*)"m=",m

   if ( m <= 0 ) then
      write(*,*)"EstMlt sovrastima la molteplicita`."
      GOTO 10
   end if

   if ( abs( Eigenvalues(j, numCol)-Eigenvalues(m, numCol) ) < & 
   0.01 * abs( Eigenvalues(j, numCol) - x ) ) then
      mlt = mlt + 1
   else
      GOTO 10
   end if

end do

10 write(*,*)""

!OSS: incrementare k non modifica il calcolo, ma fornisce indicazioni
!sull'avanzamento dello stesso

end subroutine EstMlt


!!!
!Calcola j-esimo autovalore con l'iterazione di Laguerre
!!!
subroutine LagIt(x, mlt, aj, bj, n, dim, T, S, Tinizio, Tfine, &
Sinizio, Sfine, j, &
fPrimo, fSecondo, kappa, lambdaJ, verbose)

implicit none

integer, parameter :: dp=kind(1.d0)

real(dp), intent(INOUT) :: x

integer, intent(IN) :: n

real(dp), dimension(1:n,0:1), intent(IN) :: T, S 

integer, intent(INOUT) :: Tinizio, Tfine, Sinizio, Sfine

real(dp), intent(INOUT) :: aj, bj

integer, intent(IN) :: verbose

integer, intent(IN) :: j

integer, intent(INOUT) :: mlt

integer, intent(INOUT) :: dim

integer, intent(INOUT) :: kappa

real(dp), intent(INOUT) :: fPrimo, fSecondo


integer :: i, k, l, exKappa, kappaA, kappaB, numAut

real(dp) :: deltaL, exDeltaL

real(dp), dimension(-2:0) :: xl

real(dp), intent(OUT) :: lambdaJ

real(dp) :: machinePrecision

!vedi p. 16

machinePrecision=epsilon(1.d0)

lambdaJ = 0.d0
!Se uscendo dallla subroutine questo valore non e`
!cambiato allora il calcolo e` errato.

xl(0) = x

l = 2

!write(*,*)"fPrimo=",fPrimo,"fSecondo=",fSecondo

do while ( .TRUE. )

   if ( l >= 1000 ) then
      write(*,*)"LagIt impiega troppo iterazioni (piu` di mille)."
      exit
   end if

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


   !calcolo x_l(0) a tappe:
   !questa e` la parte comune a + e -
   xl(0) = (n-1)*fPrimo**2 - n*fSecondo
   xl(0) = abs( ( (n-mlt) * xl(0) )/ (mlt*1.d0) )

   if ( kappa < j ) then

      if ( verbose >=3 ) then
         write(*,*)"mi muovo verso destra"
      end if
      !Calcolo xl(0) = L_{mlt +}(xl(-1))

      xl(0) = -fPrimo + sqrt(xl(0))

      xl(0) = xl(-1) + (n*1.d0)/xl(0)


      !xl(0) = xl(-1) + (n*1.d0)/(-fPrimo + &
      !sqrt( (((n-mlt)*1.d0)/(mlt*1.d0))* ( (n-1)*fPrimo**2 - n*fSecondo ) ) )
   else
      if ( verbose >=3 ) then
         write(*,*)"mi muovo verso sinistra"
      end if
      !Calcolo x_l(0) = L_{mlt -}(xl(-1))

      xl(0) = -fPrimo - sqrt(xl(0))
      
      xl(0) = xl(-1) + (n*1.d0)/xl(0)

      !xl(0) = xl(-1) + (n*1.d0)/(-fPrimo - &
      !sqrt( (((n-mlt)*1.d0)/(mlt*1.d0))* ( (n-1)*fPrimo**2 - n*fSecondo ) ) )
   end if

   if ( isnan(xl(0)) ) then
      if (verbose >= 2) then
         write(*,*)"condizione di arresto particolare: xl(0) e` NaN!"
      end if
      xl(0)=xl(-1)
      GOTO 30
   end if

   if ( verbose >= 3 ) then
      write(*,*)"xl(0)=",xl(0)
   end if

   exDeltaL = xl(-1) - xl(-2)
   deltaL = xl(0) - xl(-1)

   ! condizione (24)

   !write(*,*)"deltaL=",deltaL

   if ( abs(deltaL) <= machinePrecision*abs(xl(0)) ) then
      if (verbose >= 2) then
         write(*,*)"condizione di arresto (24) del primo tipo"
      end if
      GOTO 30
   end if

   if ( abs(deltaL) >= abs(exDeltaL) ) then
      if (verbose >= 2) then
         write(*,*)"condizione di arresto (24) del secondo tipo"
      end if
      GOTO 30
   end if

   if ( (deltaL**2)/( abs(exDeltaL)-abs(deltaL) ) <= &
   machinePrecision*abs(xl(0)) ) then
      if (verbose >= 2) then
         write(*,*)"condizione di arresto (24) del terzo tipo"
      end if
      GOTO 30
   end if

   if ( abs(xl(0))<machinePrecision ) then
      if (verbose >= 2) then
         write(*,*)"xl(0) troppo piccolo"
      end if
      GOTO 30
   end if

   !calcolo (12), (13) e (14)
20 call  calcoli(xl(0), T, S, n, dim, Tinizio, Tfine, Sinizio, &
   Sfine, fPrimo, fSecondo, kappa)
   !call numAutovaloriPrimaDiX(xl(0),dim,T(Tinizio:Tfine,:),S(Sinizio:Sfine,:),numAut)
   !if ( verbose >= 3 .AND. kappa /= numAut ) then
   !   write(*,*)"kappa e` diverso da numAut!"
   !   write(*,*)"kappa(a)=",kappa,"numAut=",numAut
   !end if
   !kappa=numAut

   !aggiorno [aj, bj] secondo il nuovo kappa
   if (  mlt > 1  .AND.  abs(kappa-exKappa) > 1  ) then

      mlt = abs(kappa-exKappa)

      xl(-2) = xl(0)
      xl(0) = (xl(0)+xl(-1))/2.d0
      xl(-1) = xl(-2)
      !non sono in grado di fare un corretto
      !xl(-2)=xl(-1)
      !ma non mi interessa poiche'
      !appena ri-entro nel ciclo lo perco comunque
      
      GOTO 20
   end if

   l = l+1

   !OSS: incrementare l non modifica il calcolo ma indica l'avanzamento
   !dello stesso

end do

30 write(*,*) ""

if ( verbose >= 3 ) then
   write(*,*) "LagIt compie ",l," iterazioni."
end if

lambdaJ = xl(0)

end subroutine LagIt


!!!                                                                   
!Calcola (12), (13) e (14). Nell'altro articolo
!questo algoritmo viene chiamato DetEvl               
!!!                                                                 
  
subroutine calcoli(x, T, S, n, dim, Tinizio, Tfine, Sinizio, Sfine, &
fPrimo, fSecondo, kappa)

implicit none

integer, parameter :: dp=kind(1.d0)

!integer, parameter :: dq=16

real(dp), intent(IN) :: x

integer, intent(IN) :: n

integer, intent(INOUT) :: dim, Tinizio, Tfine, Sinizio, Sfine

integer, intent(OUT) :: kappa

real(dp), intent(OUT) :: fPrimo, fSecondo

real(dp), dimension(1:n,0:1), intent(IN) :: T, S

integer :: i, j, k, l

integer :: verboseCalcoli

real(dp), dimension(:), allocatable :: xi
real(dp), dimension(:), allocatable ::  eta, zeta

real(dp) :: machinePrecision

!FINE DICHIARAZIONI

!verboseCalcoli = 6
verboseCalcoli=0

!zero= z'00000000000000000000000000000000'

!uno = z'3fff0000000000000000000000000000'

!due = uno+uno

!quattro = due + due

!machinePrecision=epsilon(uno)
machinePrecision = epsilon(1.d0)


allocate( xi(1:dim), eta(0:dim), zeta(0:dim) )



!OSS: ro_i=prodotto di xi_k per k=1, ..., i
!OSS: necessito in ogni momento di xi_{i-1}, zeta_{i-1}, 
!zeta_{i-2}, eta_{i-1} ed eta_{i-2}

!FONDAMENTALE:
!tengo conto di quanti termini negativi compaiono 
!nella successione degli xi; questo sara` kappa!

if ( verboseCalcoli >= 4 ) then
   write(*,*)"x=",x
end if

kappa = 0

do i=0,dim

   if ( i==0 ) then
      eta(0)=0.d0
      zeta(0)=0.d0
   end if
   
   if ( i==1 ) then
      xi(1)=T(Tinizio,0)-x*S(Sinizio,0)
      
      if ( abs(xi(1)) <= machinePrecision ) then
         xi(1) = T(Tinizio,0)*machinePrecision**2
      end if

      eta(1)=S(Sinizio,0)/xi(1)
      zeta(1)=0.d0
   end if
   

   if ( i>=2 ) then
      !Se mi trovo qui allora i>=2
      xi(i) = T(Tinizio-1+i,0) - x*S(Sinizio-1+i,0) - (T(Tinizio-1+i,1) - x*S(Sinizio-1+i,1) )**2/xi(i-1)

      if ( abs(xi(i)) <= machinePrecision ) then
         xi(i) = ( (abs(T(Tinizio-1+i,1))+abs(x*S(Sinizio-1+i,1)))**2 * machinePrecision**2  )/xi(i-1)
      end if

      eta(i) = 2.d0 * (T(Tinizio-1+i,1) - x*S(Sinizio-1+i,1)) * S(Sinizio-1+i,1) + &
           (T(Tinizio-1+i,1) - x*S(Sinizio-1+i,1))**2 * eta(i-2)
      eta(i) = -eta(i)/xi(i-1)
      eta(i) = ( eta(i) + (T(Tinizio-1+i,0) - x*S(Sinizio-1+i,0))*eta(i-1) + S(Sinizio-1+i,0) )/xi(i)

      zeta(i) = 2.d0*S(Sinizio-1+i,1)**2 * 4.d0*(T(Tinizio-1+i,1) - x*S(Sinizio-1+i,1))*S(Sinizio-1+i,1)*eta(i-2) - &
           (T(Tinizio-1+i,1) - x*S(Sinizio-1+i,1))**2 * zeta(i-2)
      zeta(i) = -zeta(i)/xi(i-1)
      zeta(i) = ( zeta(i) + (T(Tinizio-1+i,0) - x*S(Sinizio-1+i,0))*zeta(i-1) + 2.d0*S(Sinizio-1+i,0)*eta(i-1) )/xi(i)

   end if

   !Adesso aggiorno il conteggio degli xi negativi
   if ( xi(i) < 0.d0 ) then
      kappa = kappa + 1
   end if

   if ( verboseCalcoli >= 5 ) then
      write(*,*)"i=",i,"kappa=",kappa
   end if

end do

if ( verboseCalcoli >= 6 ) then
   write(*,*)"ecco il vettore xi:"
   write(*,*)xi(:)
   write(*,*)"ecco il vettore eta:"
   write(*,*)eta(:)
   write(*,*)"ecco il vettore zeta:"
   write(*,*)zeta(:)
end if


!immagazzino i risultati in variabili dal nome piu` evocativo
fPrimo = - eta(dim)
fSecondo = zeta(dim)

if ( verboseCalcoli >= 4 ) then
   write(*,*)"fPrimo=",fPrimo,"fSecondo=",fSecondo
end if

end subroutine calcoli


!!$!!!                                                                   
!!$!Calcola (12), (13) e (14). Nell'altro articolo
!!$!questo algoritmo viene chiamato DetEvl               
!!$!!!                                                                 
!!$  
!!$subroutine calcoli(x, T, S, n, dim, Tinizio, Tfine, Sinizio, Sfine, &
!!$fPrimo, fSecondo, kappa)
!!$
!!$implicit none
!!$
!!$integer, parameter :: dp=kind(1.d0)
!!$
!!$!integer, parameter :: dq=16
!!$
!!$real(dp), intent(IN) :: x
!!$
!!$integer, intent(IN) :: n
!!$
!!$integer, intent(INOUT) :: dim, Tinizio, Tfine, Sinizio, Sfine
!!$
!!$integer, intent(OUT) :: kappa
!!$
!!$real(dp), intent(OUT) :: fPrimo, fSecondo
!!$
!!$real(dp), dimension(1:n,0:1), intent(IN) :: T, S
!!$
!!$integer :: i, j, k, l
!!$
!!$integer :: verboseCalcoli
!!$
!!$real(dp), dimension(:), allocatable :: xi
!!$real(dp), dimension(:), allocatable ::  eta, zeta
!!$
!!$real(dp) :: machinePrecision
!!$
!!$!FINE DICHIARAZIONI
!!$
!!$verboseCalcoli = 6
!!$
!!$machinePrecision = epsilon(1.d0)
!!$
!!$
!!$!allocate( xi(1:dim), eta(0:dim), zeta(0:dim) )
!!$allocate( xi(0:1), eta(0:2), zeta(0:2) )
!!$
!!$
!!$!OSS: ro_i=prodotto di xi_k per k=1, ..., i
!!$!OSS: necessito in ogni momento di xi_{i-1}, zeta_{i-1}, 
!!$!zeta_{i-2}, eta_{i-1} ed eta_{i-2}
!!$
!!$!FONDAMENTALE:
!!$!tengo conto di quanti termini negativi compaiono 
!!$!nella successione degli xi; questo sara` kappa!
!!$
!!$if ( verboseCalcoli >= 4 ) then
!!$   write(*,*)"x=",x
!!$end if
!!$
!!$kappa = 0
!!$
!!$do i=0,dim
!!$
!!$   if ( i==0 ) then
!!$      eta(0)=0.d0
!!$      zeta(0)=0.d0
!!$   end if
!!$   
!!$   if ( i==1 ) then
!!$      xi(1)=T(Tinizio,0)-x*S(Sinizio,0)
!!$      
!!$      if ( abs(xi(1)) <= machinePrecision ) then
!!$         xi(1) = T(Tinizio,0)*machinePrecision**2
!!$      end if
!!$
!!$      eta(1)=S(Sinizio,0)/xi(0)
!!$      zeta(1)=0.d0
!!$   end if
!!$   
!!$
!!$   if ( i>=2 ) then
!!$      !Se mi trovo qui allora i>=2
!!$
!!$      xi(mod(i,2)) = T(Tinizio-1+i,0) - x*S(Sinizio-1+i,0) - (T(Tinizio-1+i,1) - x*S(Sinizio-1+i,1) )**2/xi(mod(i-1,2))
!!$
!!$      if ( abs(xi(mod(i,2))) <= machinePrecision ) then
!!$         xi(mod(i,2)) = ( (abs(T(Tinizio-1+i,1))+abs(x*S(Sinizio-1+i,1)))**2 * machinePrecision**2  )/xi(mod(i-1,2))
!!$      end if
!!$
!!$      eta(mod(i,3)) = 2.d0 * (T(Tinizio-1+i,1) - x*S(Sinizio-1+i,1)) * S(Sinizio-1+i,1) + &
!!$           (T(Tinizio-1+i,1) - x*S(Sinizio-1+i,1))**2 * eta(mod(i-2,3))
!!$      eta(mod(i,3)) = -eta(mod(i,3))/xi(mod(i-1,2))
!!$      eta(mod(i,3)) = ( eta(mod(i,3)) + (T(Tinizio-1+i,0) - x*S(Sinizio-1+i,0))*eta(mod(i-1,3)) + S(Sinizio-1+i,0) )/xi(mod(i,2))
!!$
!!$      zeta(mod(i,3)) = 2.d0*S(Sinizio-1+i,1)**2 * 4.d0*(T(Tinizio-1+i,1) - x*S(Sinizio-1+i,1))*S(Sinizio-1+i,1)*eta(mod(i-2,3)) - &
!!$           (T(Tinizio-1+i,1) - x*S(Sinizio-1+i,1))**2 * zeta(mod(i-2,3))
!!$      zeta(mod(i,3)) = -zeta(mod(i,3))/xi(mod(i-1,2))
!!$      zeta(mod(i,3)) = ( zeta(mod(i,3)) + (T(Tinizio-1+i,0) - x*S(Sinizio-1+i,0))*zeta(mod(i-1,3)) + &
!!$           2.d0*S(Sinizio-1+i,0)*eta(mod(i-1,3)) )/xi(mod(i,2))
!!$
!!$   end if
!!$
!!$   !Adesso aggiorno il conteggio degli xi negativi
!!$   if ( xi(mod(i,2)) < 0.d0 ) then
!!$      kappa = kappa + 1
!!$   end if
!!$
!!$   if ( verboseCalcoli >= 5 ) then
!!$      write(*,*)"i=",i,"kappa=",kappa
!!$   end if
!!$
!!$end do
!!$
!!$if ( verboseCalcoli >= 6 ) then
!!$   write(*,*)"ecco il vettore xi:"
!!$   write(*,*)xi(:)
!!$   write(*,*)"ecco il vettore eta:"
!!$   write(*,*)eta(:)
!!$   write(*,*)"ecco il vettore zeta:"
!!$   write(*,*)zeta(:)
!!$end if
!!$
!!$
!!$!immagazzino i risultati in variabili dal nome piu` evocativo
!!$fPrimo = - eta(mod(dim,3))
!!$fSecondo = zeta(mod(dim,3))
!!$
!!$if ( verboseCalcoli >= 4 ) then
!!$   write(*,*)"fPrimo=",fPrimo,"fSecondo=",fSecondo
!!$end if
!!$
!!$end subroutine calcoli


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




