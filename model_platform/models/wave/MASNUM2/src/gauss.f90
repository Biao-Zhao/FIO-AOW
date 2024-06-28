subroutine  gauss_distribution
use control_wave,only : output
use const_wave, only : zpi
use all_var_wave,only :gau,zz
 implicit none
 integer, parameter :: num=30001
 real, parameter :: dz=0.0001
 integer :: i
 real :: z
 real,allocatable,dimension(:) :: gauss

 allocate(gauss(num))
 gauss=0.0
!!
 do i=2,num
   z=(i-1)*dz
   gauss(i)=gauss(i-1)+exp(-z**2/2.0)*dz
 end do
!!
! open(23,file=trim(output)//'gauss_test.dat',status='unknown')
!!
 do i=1,3001
   !gauss(i)=gauss(i)/sqrt(zpi)
   gau(i) = gauss((i-1)*10+1)
   zz(i)  = (i-1)*10*dz
!   write(23,'(f6.3,f10.6)') zz(i),gau(i)
 end do
!!
deallocate(gauss)

return
end subroutine gauss_distribution
