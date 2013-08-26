!!!
!PROGRAMMA PROVEINERZIA
!!!
!Al momento funziona solo per "n=2^numero",
!fra poco estendero` il calcolo al caso
!"n > 1".

program proveInerzia

implicit none

integer, parameter :: dp = kind(1.d0)

integer :: n, i, j, verbose, dim, soloScrittura

integer :: numAut

real(dp) :: x

real(dp) :: fPrimo, fSecondo

integer :: kappa, Tinizio, Tfine, Sinizio, Sfine

integer :: IER, pgbeg

real(dp), dimension(:,:), allocatable :: Teispack, Seispack, T, S

real(dp), dimension(:), allocatable :: autovalori

real, dimension(:), allocatable :: xPlot, yPlot
real :: maxDiff

!!!
!FINE DICHIARAZIONI
!!!

IER = PGBEG(0,'ConfrontoInerziaKappa.ps/PS',1,1)
if (IER.ne.1) stop


n=256

verbose=0

soloScrittura=0

allocate( Teispack(n,n), Seispack(n,n) )
allocate( T(1:n,0:1), S(1:n,0:1) )
allocate( autovalori(n) )

allocate( xPlot(16), yPlot(16) )

call generoMatrici(n,T(1:n,0:1),S(1:n,0:1), soloScrittura)


do i=1,n
   do j=1,n
      if ( i==j ) then
         Teispack(i,j)=T(i,0)
         Seispack(i,j)=S(i,0)
      end if
      if ( abs(i-j)==1 .AND. i<j ) then
         Teispack(i,j)=T(j,1)
         Teispack(j,i)=Teispack(i,j)
         Seispack(i,j)=S(j,1)
         Seispack(j,i)=Seispack(i,j)
      end if
   end do
end do

write(*,*)"n=",n

if ( verbose >= 3 ) then
   write(*,*)"T dig="
   write(*,*)T(1:n,0)
   !write(*,*)"Teispack diag="
   !do i=1,n
   !   write(*,*) Teispack(i,i)
   !end do
   write(*,*)"T super="
   write(*,*)T(1:n,1)
   !write(*,*)"Teispack super="
   !do i=1,n-1
   !   write(*,*) Teispack(i,i+1)
   !end do
   
   write(*,*)"n=",n
   
   write(*,*)"S dig="
   write(*,*)S(1:n,0)
   !write(*,*)"Seispack diag="
   !do i=1,n
   !   write(*,*) Seispack(i,i)
   !end do
   write(*,*)"S super="
   write(*,*)S(1:n,1)
   !write(*,*)"Seispack super="
   !do i=1,n-1
   !   write(*,*) Seispack(i,i+1)
   !end do
end if

if ( soloScrittura > 0 ) then
   stop
end if

open(unit=3, file="ris_eispack.txt")

read(3,*) n


do i=1,n

   read(3,*) autovalori(i)

end do

call quick_sort(autovalori,n)


!Testo la subroutine per il calcolo del numero di autovalori
!che precedono un certo valore

!write(*,*)"n=",n

!Fisso un autovalore
j=32

dim=64
Tinizio=193
Tfine=4*dim
Sinizio=193
Sfine=4*dim

write(*,*)"dim=",dim
write(*,*)"j=",j
write(*,*)"j-esimo autovalore=", autovalori(j)

write(*,*)"-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-."

!Testo l'efficacia dell'inerzia e del kappa per x_i che si
!avvicina sempre piu` all'autovalore \lambda_j

maxDiff=0.0

do i=1,16

   !write(*,*)"j=",j
   write(*,*)"i=",i
   !write(*,*)"j-esimo autovalore=", autovalori(j)

   !x = autovalori(j) + 1.d0*10**(-i)
   x = autovalori(j) - 10.d0**(-i)

   write(*,*)"x=",x

   call numAutovaloriPrimaDiX(x,n,dim,Teispack,Seispack,Tinizio,Tfine,Sinizio,Sfine,numAut)

   call calcoli(x, T, S, n, dim, Tinizio, Tfine, Sinizio, Sfine, &
        fPrimo, fSecondo, kappa)

   write(*,*)"numAut=",numAut
   write(*,*)"kappa=", kappa

   !ATTENZIONE: faccio il grafico con la differenza SENZA
   !valore assoluto
   xPlot(i)=1.0*i
   yPlot(i)=1.0*(numAut-kappa)

   if ( maxDiff < yPlot(i) ) then
      maxDiff=yPlot(i)
   end if

end do

maxDiff = abs(maxDiff)

if ( maxDiff == 0.0 ) then
   maxDiff=1.0
end if

CALL PGENV(0.0,16.0,-maxDiff,maxDiff,0,1)
CALL PGPT(16,xPlot,yPlot,7)
CALL PGLINE(16,xPlot,yPlot)
CALL PGEND

end program proveInerzia


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

character(len=1) :: null

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


!!!
!SUBROUTINE PER IL CALCOLO INERZA
!T ed S QUI sono matrici nxn
!!!

subroutine numAutovaloriPrimaDiX(x,n,dim,Teispack,Seispack,Tinizio,Tfine,Sinizio,Sfine,numAut)

implicit none

integer, parameter :: dp = kind(1.d0)

integer, intent(IN) :: n

integer, intent(IN) :: dim

integer, intent(IN) :: Tinizio, Tfine, Sinizio, Sfine

real(dp), intent(IN) :: x

real(dp), dimension(n,n), intent(IN) :: Teispack

real(dp), dimension(n,n), intent(IN) :: Seispack

real(dp), dimension(dim,dim) :: T,S

integer, intent(OUT) :: numAut

!!!

character :: UPLO

integer :: i, j, k, LWORK

integer, dimension(:), allocatable :: IPIV

integer :: INFO

real(dp), dimension(:), allocatable :: WORK

real(dp), dimension(:,:), allocatable :: A, xI

real(dp) :: machinePrecision

!!!
!FINE DICHIARAZIONI
!!!

machinePrecision=epsilon(1.d0)


!allocate( A(n,n), xI(n,n) )
!allocate( IPIV(n) )

allocate( A(dim,dim), xI(dim,dim) )
allocate( IPIV(dim) )

!compongo T,S
do i=1,dim
   do j=1,dim
      T(i,j) = Teispack(Tinizio+i-1,Tinizio+j-1)
      S(i,j) = Seispack(Sinizio+i-1,Sinizio+j-1)
   end do
end do

!compongo xI
do i=1,dim
   do j=1,dim
      if ( i==j ) then
         xI(i,j)=x
      else
         xI(i,j)=0.d0
      end if
   end do
end do

UPLO = "L"


A = T - matmul(xI,S)

do i=1,dim
   do j=1,dim
      if ( abs(A(i,j))<=machinePrecision ) then
         A(i,j)=0.d0
      end if
   end do
end do

!stampo la matrice A
!write(*,*)"matrice A"
!do i=1,n
!   write(*,*)A(i,:)
!end do

!cerco il miglior valor per LWORK:
allocate( WORK(dim) )
call DSYTRF(UPLO, dim, A, dim, IPIV, WORK, -1, INFO)
LWORK=WORK(1)
deallocate( WORK )
!write(*,*)"LWORK=",LWORK
allocate( WORK(LWORK) )

!calcolo la fattorizzazione A=LDL^T
call DSYTRF(UPLO, dim, A, dim, IPIV, WORK, LWORK, INFO)

!write(*,*)"INFO=", INFO
!write(*,*)"IPIV=",IPIV(:)

!write(*,*)"matrice A dopo DSYTRF"
!do i=1,n
!   write(*,*)A(i,:)
!end do

numAut=0
k=1
do while( k <= dim )
   !write(*,*)"k=",k
   if ( IPIV(k) > 0 ) then
      !A(k,k) e` l'inizio di un blocco 1x1
      !write(*,*)"A(k,k)=",A(k,k)
      if ( A(k,k) < 0.d0 ) then
         numAut = numAut + 1
         !write(*,*)"numAut=",numAut
      end if
      k = k+1
   else
      !A(k,k) e` l'inizio del blocco 2x2 simmetrico:
      !|A(k,k)   A(k+1,k)  |
      !|A(k+1,k) A(k+1,k+1)|
      !calcolo quindi la fattorizzazione A(k:k+1,k:k+1)=LDL^T 
      call DSYTRF(UPLO, 2, A(k:k+1,k:k+1), 2, IPIV, WORK, -1, INFO)
      LWORK=WORK(1)
      call DSYTRF(UPLO, 2, A(k:k+1,k:k+1), 2, IPIV, WORK, LWORK, INFO)
      !controllo dunque il segno di A(k,k) e di A(k+1,k+1),
      !che compongono l'attuale D
      do j=0,1
         if ( A(k+j,k+j) < 0.d0 ) then
            numAut = numAut + 1
         end if
      end do
      k = k+2
   end if
end do

end subroutine numAutovaloriPrimaDiX



subroutine generoMatrici(n, T, S, soloScrittura)

implicit none

!lavoro in doppia precisione
integer, parameter :: dp=kind(0.d0)

integer, intent(IN) :: n

integer, intent(IN) :: soloScrittura

real(dp), dimension(1:n,0:1), intent(OUT) :: T,S

real(dp), dimension(:,:), allocatable :: Teispack, Seispack

integer,parameter :: seed = 86456

!real(dp), dimension(:), allocatable :: v 

integer :: i, j

real(dp) :: rnd, machinePrecision, count, max, e1, e2


!!!
!Fine dichiarazioni
!!!

machinePrecision=epsilon(1.d0)


!!!
!ATTENZIONE: mi devo ricordare di generale pencil (T,S)
!che siano "non riducibili", ovvero t.c.
!T(i,i+1)^2 + S(i,i+1)^2 \neq 0 per i=1,...,n-1
!!!



!!$!!!
!!$!Matrici tridiagonali simmetriche random, 
!!$!ma dominanti diagonali (autovalori "distanti")
!!$!Per i teoremi di Gershgorin T ed S sono
!!$!definite positive
!!$!!!
!!$do i=1,n
!!$   !call random_number(rnd)
!!$   rnd=rand(seed)
!!$   T(i,0) = 1.d0
!!$   T(i,1) = rnd*1.d-3
!!$end do
!!$
!!$T(1,1)=0.d0
!!$
!!$do i=1,n
!!$   !call random_number(rnd)
!!$   rnd=rand(seed)
!!$   S(i,0) = 0.d0
!!$   S(i,1) = rnd*1.d-1
!!$end do
!!$
!!$do i=1,n-1
!!$   rnd = 2.d0*S(i+1,1)
!!$   S(i,0) = i*1.d-1 + abs(rnd)
!!$end do
!!$
!!$S(n,0) = n*1.d-1
!!$
!!$S(1,1)=0.d0




!!!
!Matrici tridiagonali simmetriche random, 
!ma dominanti diagonali (autovalori "distanti")
!Per i teoremi di Gershgorin T ed S sono
!definite positive
!Impongo T=I e gli autovalori di S
!compresi fra e1 ed e2
!!!
do i=1,n
   !call random_number(rnd)
   rnd=rand(seed)
   T(i,0) = 1.d0
   !T(i,1) = rnd*1.d-3
   T(i,1) = 0.d0
end do

T(1,1)=0.d0

e1=1.d-1
e2=1.d-1 + 5.d-2

do i=1,n
   !call random_number(rnd)
   rnd=rand(seed)
   S(i,0) = 0.d0
   S(i,1) = rnd*1.d-3
end do

do i=0,n-1
   rnd = 2.d0*S(i+1,1)
   S(i+1,0) = ( (e2-e1)*i )/( e1*e2*(n-1) ) + 1/e2  + abs(rnd)
end do

S(1,1) = 0.d0





!!$!!!
!!$!Matrici Problema Sturm-Liouville
!!$!!!
!!$do i=1,n
!!$   do j=1,i
!!$      if (i == j) then
!!$         T(i,j) = 2.d0*(n+1) + ( (3.d0*i**2 + 2.d0) * 8.d0 )/(n+1)
!!$      end if
!!$      if ( abs(i-j) == 1 ) then
!!$         T(i,j) = -1.d0*(n+1) + ( -1.d0 * (2.d0 -3.d0*(2.d0*i+1) +6.d0*i*(i+1)) )/(n+1) 
!!$         T(j,i) = T(i,j)
!!$      end if
!!$      if ( abs(i-j) > 1 ) then
!!$         T(i,j)=0.d0
!!$         T(j,i)=T(i,j)
!!$      end if
!!$   end do
!!$end do
!!$
!!$do i=1,n
!!$   do j=1,i
!!$      if (i == j) then
!!$         S(i,j) = 4.d0/(6.d0*(n+1))
!!$      end if
!!$      if ( abs(i-j) == 1 ) then
!!$         S(i,j) = 1.d0/(6.d0*(n+1))
!!$         S(j,i) = S(i,j)
!!$      end if
!!$      if ( abs(i-j) > 1) then
!!$         S(i,j)=0.d0
!!$         S(j,i)=S(i,j)
!!$      end if
!!$   end do
!!$end do


!SCRIVO LE MATRICI



!!!
!ATTENZIONE: mi devo ricordare di generare pencil (T,S)
!che siano "non riducibili", ovvero t.c.
!T(i,i+1)^2 + S(i,i+1)^2 \neq 0 per i=1,...,n-1
!!!
do i=2,n
   if ( abs(T(i,1)**2 + S(i,1)**2) <= 10*machinePrecision ) then
      write(*,*) "PENCIL NON RIDUCIBILE"
   end if
end do



if ( soloScrittura > 0 ) then

   allocate( Teispack(n,n), Seispack(n,n) )

   open(unit=1, file="T.txt")
   open(unit=2, file="S.txt")

   write(1,*)n
   write(2,*)n

   do j=1,n
      do i=1,n
         if ( i==j ) then
            Teispack(i,j)=T(i,0)
            Seispack(i,j)=S(i,0)
         end if
         if ( abs(i-j)==1 .AND. i>j ) then
            Teispack(i,j)=T(i,1)
            Teispack(j,i)=Teispack(i,j)
            Seispack(i,j)=S(i,1)
            Seispack(j,i)=Seispack(i,j)
         end if
         write(1,*) Teispack(i,j)
         write(2,*) Seispack(i,j)
      end do
   end do


   !Calcolo norma infinito (utile per il corollario 4.3)
   max = 0.d0
   do i=1,n
      count = 0.d0
      do j=1,n
         count = count + abs(Seispack(i,j))
      end do
      if ( count > max ) then
         max = count
      end if
   end do
   
   write(*,*)"||S||_{\infty}=",max
   
end if

end subroutine generoMatrici
