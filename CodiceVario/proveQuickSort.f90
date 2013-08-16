program proveQuickSort

implicit none

integer, parameter :: dp = kind(1.d0)

integer :: n, i, j, em, en

real(dp) :: rnd

real(dp), dimension(:,:), allocatable :: Eigenvalues

!!!
!FINE DICHIARAZIONI
!!!

n=4

em=n

en=int( log(n*1.d0)/log(2.d0) ) + 2

allocate( Eigenvalues(em,en) )

do j=1,en
   do i=1,em
      Eigenvalues(i,j) = 0.d0
   end do
end do

!Adesso riempio una colonna con numeri reali disordinati

do i=1,em
   call random_number(rnd)
   Eigenvalues(i,2) = rnd*10.d0
end do

write(*,*) "Eigenvalues prima del calcolo:"
do i=1,em
   write(*,*) Eigenvalues(i,:)
end do

!Chiamo l'ordinamento
call quick_sort( Eigenvalues(:,2), em )

write(*,*) "Eigenvalues dopo il calcolo:"
do i=1,em
   write(*,*) Eigenvalues(i,:)
end do

end program proveQuickSort
