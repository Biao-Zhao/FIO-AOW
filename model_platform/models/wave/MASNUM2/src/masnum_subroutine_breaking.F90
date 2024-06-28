subroutine breaking

use lims_wave, only : myid
use const_wave, only : pi,zpi,g
use control_wave,only : islon,ielon,islat,ielat,input
use all_var_wave,only :wx,wy,nsp,h1_3,tpf,ape,aet,gau,ra,hen,pt,kt,et,bbr,mt,ust


!=======================================================================================================
!                              Main program                                                            =
!     This subroutine is incoporated into MASNUM-Wave model  by Zhao Biao based on the paper of        =
! Yuan Yeli et al.(2009JPO). The original source codes are from Yang Yongzeng. While  this             =
! version of subroutine is modified with the help of Han Lei and Ren Danqin. Currently we only         =
! use the variable ra (Whilecape coverage) in our model.                                               =
!                                                                                                      =
!                                                                          Zhao Biao 2016.05.24        =
!=======================================================================================================
  implicit none

  real :: w10, cd, angwi, angwa
  real :: currbias,rlim, gaubr, expo, thetco, thetbr
  real, dimension(islon:ielon,islat:ielat) :: spwid, spro,rph,wavlen
  real,dimension(60) :: wi1,spwid1
  integer :: ia,ic,i,numint
  real,parameter :: abr=1
  real,parameter :: conbr=-1.5574   !conbr=-1.9006  ???????????????? 
  real,parameter :: cen=0.0144      !cen=0.002451   ????????????????
  real,parameter :: ub2=0.25
  real,parameter :: rlam=2.0/3.
  real,parameter :: ften=1.0/2.
  real,parameter :: row=1023.
  real,parameter :: eps=1.0e-10
 
!  write(6,*) 'call subroutine breaking'
!==================================== initialization ======================================
  ra       = 0.0 
  hen      = 0.0
  pt       = 0.0 
  kt       = 0.0
  et       = 0.0 
  bbr      = 0.0
  mt       = 0.0
  
!==========================================================================================
!  open(11,file=trim(input)//'rho_hl.dat',status='old')
!  do i=1,60
!     read(11,*)wi1(i),spwid1(i)
!  end do
!  close(11)
!==========================================================================================
  do ic=islat,ielat
  do ia=islon,ielon
    if(nsp(ia,ic).eq.0) cycle
    w10=sqrt(wx(ia,ic)**2+wy(ia,ic)**2)
! ------------------------ for ro, c0 and wave length -------------------------
!    if(w10.le.wi1(1)) spwid(ia,ic)=spwid1(1)
!    if(w10.gt.wi1(60)) spwid(ia,ic)=spwid1(60)
!    if(w10.gt.wi1(1).and.w10.le.wi1(60))then      
!      do i=1,59
!        if(w10.gt.wi1(i).and.w10.le.wi1(i+1))then
!           spwid(ia,ic)=spwid1(i)*(wi1(i+1)-w10)+spwid1(i+1)*(w10-wi1(i))
!        end if
!      end do
!    end if
!    spro(ia,ic)=sqrt(1.0-spwid(ia,ic)**2)
!    if(w10.le.40) spro(ia,ic)=0.4
!------------------------------------------------------------------------------
    spro(ia,ic)=0.4
    if((h1_3(ia,ic).le.0).or.(isnan(h1_3(ia,ic)))) h1_3(ia,ic)=0.5
    if((ape(ia,ic).lt.1.0).or.(isnan(ape(ia,ic)))) ape(ia,ic)=1.0
    wavlen(ia,ic)=g*rlam*ape(ia,ic)**2/zpi                 ! wave length

!   cd=1.5*0.001
!   cd=(0.80+0.065*w10)*0.001                          ! drag coefficient  why not use this formula ???????????????????
    cd=(ust(ia,ic)/(w10+eps))**2                             ! use friction velocity  from atm for coupling zhaobiao 

    angwi=atan2d(wy(ia,ic),wx(ia,ic))
    if(angwi.lt.0.0) angwi = angwi+360.0
    angwa    = aet(ia,ic)
    

    currbias = 0.55*sqrt(2*pi*abr*rlam*cd)/spro(ia,ic)*sqrt(w10**2/(g*wavlen(ia,ic)))*cosd(angwa-angwi)
        


    if(currbias.gt.1.0) currbias=1.0
    rph(ia,ic)=(1.0-currbias)**2
    rlim = spro(ia,ic)/(pi*abr*rlam)*wavlen(ia,ic)/h1_3(ia,ic)*rph(ia,ic)
    if(rlim.gt.3.0) rlim=3.0

    if(rlim.lt.0.or.isnan(rlim)) then                
    write(6,*) myid,'error:rlim',rlim,wavlen(ia,ic),h1_3(ia,ic),rph(ia,ic),ia,ic
    end if
    
    numint= int(rlim/0.001)+1.0


    gaubr = sqrt(zpi)/2-gau(numint)
    
    expo  = spro(ia,ic)**2/(2.*(pi*abr*rlam)**2)*(h1_3(ia,ic)/wavlen(ia,ic))**(-2.)*rph(ia,ic)**2
    

    if(expo .gt.100.) expo  = 100.
    thetco=spro(ia,ic)/(pi*abr*rlam)*wavlen(ia,ic)/h1_3(ia,ic)*gaubr
    thetbr=thetco*exp(expo)
    if(thetbr.gt.100.0)thetbr=100.0
!------------------------------------------------------------------------------
!
!                        sea surface whitecap coverage
!                             Yuan Yeli(2009JPO)
!
!------------------------------------------------------------------------------
     ra(ia,ic)  = cen*ften/ub2*spro(ia,ic)/(4*abr*pi)                  &
                *sqrt(g*wavlen(ia,ic)/(rlam*pi))                       &
                *((1.0+thetbr)*pi**2*abr**2*rlam**2/(4*spro(ia,ic)**2) &
                *(h1_3(ia,ic)/wavlen(ia,ic))**2)**conbr                &
                *exp(-1.*expo)
!------------------------------------------------------------------------------
!             
!                         breaking entrainment depth
!
!------------------------------------------------------------------------------
     hen(ia,ic) = cen*spro(ia,ic)**2/(2*pi*abr*rlam)*wavlen(ia,ic)     &
                *((1.0+thetbr)*pi**2*abr**2*rlam**2/(4*spro(ia,ic)**2) &
                *(h1_3(ia,ic)/wavlen(ia,ic))**2)**conbr
!------------------------------------------------------------------------------
! 
!                   potential energy loss of the waves 
!           due to wave breaking per unit time per unit sea surface area      
!
!------------------------------------------------------------------------------
    pt(ia,ic)  = row*g**(3.0/2)*rlam**(1.0/2)/(32.*pi**(1.0/2))            &
               * spro(ia,ic)                                               &
               *h1_3(ia,ic)**(3.0/2)*(h1_3(ia,ic)/wavlen(ia,ic))**(1.0/2)  &
               *exp(-1.0*expo)
!------------------------------------------------------------------------------
!
!                   kinetic energy loss of the waves                                                
!           due to wave breaking per unit time per unit sea surface area      
!
!------------------------------------------------------------------------------
    kt(ia,ic)   = row*g**(3.0/2)/(32.*pi**(3.0/2)*abr*rlam**0.5)           &
                *spro(ia,ic)**2                                        & 
                *wavlen(ia,ic)**(3.0/2)*(h1_3(ia,ic)/wavlen(ia,ic))*gaubr
!------------------------------------------------------------------------------
!
!             mechanical energy loss of the waves at wave breaking
!
!------------------------------------------------------------------------------
    et(ia,ic)  = pt(ia,ic)+kt(ia,ic)
    if(et(ia,ic).gt.3000.0) et(ia,ic)=3000.0



!------------------------------------------------------------------------------

    bbr(ia,ic) = 2*hen(ia,ic)*sqrt(et(ia,ic)/(2*row*ub2))

!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
!
!               momentum loss of the waves at wave breaking
!
!------------------------------------------------------------------------------
    mt(ia,ic)  = row*g/(8.*sqrt(2.*abr)*pi) &
               * spro(ia,ic)*h1_3(ia,ic)*gaubr


!------------------------------------------------------------------------------
    if(h1_3(ia,ic).le.0.5)then
      ra(ia,ic)   = 0.
      hen(ia,ic)  = 0.
      pt(ia,ic)   = 0.
      kt(ia,ic)   = 0.
      et(ia,ic)   = 0.
      bbr(ia,ic)  = 0.
      mt(ia,ic)   = 0.
    endif
    if(isnan(ra(ia,ic)))  ra(ia,ic)=0
    if(isnan(hen(ia,ic))) hen(ia,ic)=0
    if(isnan(pt(ia,ic)))  pt(ia,ic)=0
    if(isnan(kt(ia,ic)))  kt(ia,ic)=0
    if(isnan(et(ia,ic)))  et(ia,ic)=0
    if(isnan(bbr(ia,ic))) bbr(ia,ic)=0
    if(isnan(mt(ia,ic)))  mt(ia,ic)=0

!------------------------------------------------------------------------------
  enddo
  enddo

!  write(6,*) 'call subroutine breaking succeed'
  return

! -----------------------------------------------------------------------------

end subroutine breaking
