!!!
!SUBROUTINE PER IL CALCOLO INERZA
!!!

subroutine numAutovaloriPrimaDiX(x,n,T,S,numAut)

implicit none

integer, parameter :: dp = kind(1.d0)

integer, intent(IN) :: n

real(dp), intent(IN) :: x

real(dp), dimension(N,N), intent(IN) :: T

real(dp), dimension(N,N), intent(INOUT) :: S

real(dp), intent(OUT) :: numAut

!!!

character :: UPLO

integer :: i, j, k, LWORK, LDA

integer :: IPIV

integer :: INFO

real(dp), dimension(:), allocatable :: WORK

real(dp), dimension(n,n) :: A, xI

real(dp) :: machinePrecision

!!!
!FINE DICHIARAZIONI
!!!

machinePrecision=epsilon(1.d0)


!compongo xI
do i=1,n
   do j=1,n
      if ( i==j ) then
         xI(i,j)=x
      else
         xI(i,j)=0.d0
      end if
   end do
end do

UPLO = "L"
LDA = n

!fattorizzazione di Cholesky S=LL^T
call DPOTRF(UPLO, n, S, LDA, INFO)

!cancello la parte sopradiagonale
do i=1,n
   do j=1,n
      if ( i<j ) then
         S(i,j) = 0.d0
      end if
   end do
end do

!inverto L. Il risultato e` salvato in S
call DTRTRI(UPLO, "N", n, S, LDA, INFO)

!Compongo la matrice A
A = matmul(matmul(S,T),transpose(S)) - xI

!cerco il miglior valor per LWORK:
call DSYTRF(UPLO, n, A, LDA, IPIV, WORK, -1, INFO)
LWORK=WORK(1)

allocate( WORK(LWORK) )

!calcolo la fattorizzazione A=LDL^T
call DSYTRF(UPLO, n, A, LDA, IPIV, WORK, LWORK, INFO)

numAut=0
k=1
do while( k <= n )
   if ( IPIV(k) > 0 ) then
      !A(k,k) e` l'inizio di un blocco 1x1
      if ( A(k,k) > 0.d0 ) then
         numAut = numAut + 1
      end if
      k = k+1
   else
      !A(k,k) e` l'inizio del blocco 2x2 simmetrico:
      !|A(k,k)   A(k+1,k)  |
      !|A(k+1,k) A(k+1,k+1)|
      !calcolo quindi la fattorizzazione A(k:k+1,k:k+1)=LDL^T 
      call DSYTRF(UPLO, n, A(k:k+1,k:k+1), LDA, IPIV, WORK, -1, INFO)
      LWORK=WORK(1)
      call DSYTRF(UPLO, n, A(k:k+1,k:k+1), LDA, IPIV, WORK, LWORK, INFO)
      !controllo dunque il segno di A(k,k) e di A(k+1,k+1),
      !che compongono l'attuale D
      do j=0,1
         if ( A(k+j,k+j) > 0.d0 ) then
            numAut = numAut + 1
         end if
      end do
      k = k+2
   end if
end do

end subroutine numAutovaloriPrimaDiX
