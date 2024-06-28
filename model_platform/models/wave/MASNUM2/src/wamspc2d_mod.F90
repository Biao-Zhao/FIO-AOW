!--------------------------------------------------------------------
! Author: Jianting Du
! This module calculate two-dimensional wave spectrum from wave parameters including Hm0, Fp, and DIR
! Usage:
! call spectra2d(hs,1.d0/tp,th,ipx,ipy,ee2(:,:,i),2)
!--------------------------------------------------------------------   
module wamspc2d_mod
   use all_var_wave
   use data_kind_mod_wave
   implicit none
   public :: spectra2d
   private
   contains
!--------------------------------------------------------------------   
    subroutine spectra2d(hm0,    &! Significant wave height
                         fp,     &! Peak frequency
                         thetin, &! Mean wave direction
                         idx,idy,    &! Point index
                         eeout,  &! Wave spectrum for output
                         iflag   )! Method: 0 Donelan 1985; 1 JONSWAP 1973; 2 Tsagareli 2008
      implicit none

      real(kind_r4),intent(in) :: hm0,fp,thetin
      integer,intent(in) :: idx,idy
      real(kind_r4),intent(inout):: eeout(:,:)
      integer,optional   :: iflag
      real(kind_r4) :: u10,fpnd,fm,zpi4,ef1d
      real(kind_r4) :: alpha,gama,sigma,theta0,thetj
!     real(kind_r4) :: sumk,ef1d0,ef1d1,sumk1
      real(kind_r4) :: aths,athc,ath,sinth,costh
      real(kind_r4) :: f,ft,fmf,uccp,dthet,sumth
      real(kind_r4) :: S_thet(jl)
      integer :: j,k
      
      if (.not.present(iflag)) iflag = 2
      dthet  = thet(2)-thet(1)
      zpi4   = zpi**4
      !Step 1, calculate the relative u10 and nan-dimensional fp
      !!call FetchCurve(hm0,fp,u10,fpnd,1.25d0)
      call FetchCurve(hm0,fp,u10,fpnd,1.25)
      !Step 2, calculate one-dimensional wave spectrum
      fm     = fpnd*g/u10
      uccp   = fpnd*zpi  !u10*zpi*fm/g
      theta0 = 1.5d0*pi-thetin*pi/180.d0
      if (theta0.lt.0) theta0 = zpi+theta0
!      theta0 = zpi
!      write(*,*) 'theta0:',theta0*180/pi,thetin
      !alpha,gamma,and sigma according to Donelan (1985)
      alpha  = 0.006*uccp**0.55
      gama   = 1.7d0+6.d0*log10(uccp)
      if(gama < 1.7d0) gama = 1.7d0
      sigma  = 0.032d0/uccp**3+0.08d0
      ft     = 2.5*g/pi/u10
      do k = 1,kl
        f     = wf(k,idx,idy)
        fmf   = fm/f
        select case(iflag)
        case(0) !Donelan (1985)
          ef1d = alpha*gg/zpi4/fm/f**4 * exp(-fmf**4)       &
  &            * gama**exp(-(f-fm)**2/(2*(sigma*fm)**2))
        case(1) !JONSWAP (1973)
          ef1d = alpha*gg/zpi4/f**5 * exp(-1.25*fmf**4)     &
  &            * gama**exp(-(f-fm)**2/(2*(sigma*fm)**2))
        case(2) !Tsagareli(2008)
          if ( f<=ft ) then
            ef1d = alpha*gg/zpi4/fm/f**4 * exp(-fmf**4)     &
  &              * gama**exp(-(f-fm)**2/(2*(sigma*fm)**2))
          else
            ef1d = alpha*gg/zpi4/fm*ft/f**5 * exp(-fmf**4)  &
  &              * gama**exp(-(f-fm)**2/(2*(sigma*fm)**2))
          endif
        end select
        ! Step 3, add directional distribution to make 2D spectrum
        do j = 1,jl
          thetj = theta0-thet(j)
!          if (thetj < -pi) thetj = zpi - thetj
!          if (thetj > pi)  thetj = thetj - zpi
!          write(*,*) 'Direc:',thet(j),theta0,f,fm,uccp
          call DirectDistri(thetj,f,fm,uccp,S_thet(j),2)
        enddo
        sumth = sum(S_thet)*dthet
        S_thet = S_thet/sumth
!        write(*,*) 'S_thet:',theta0,thet(j),thetj,S_thet
!        write(*,*) '----k:',k,sumth,'----'
!        write(*,'(10f6.2)') S_thet
        do j = 1,jl
          !E(f,theta) = E(k,theta)*2pi/cg/k
          eeout(k,j) = ef1d*ccg(k,idx,idy)/zpi/wk(k) * S_thet(j)
        enddo
      enddo
      return
!      sumk = 0.d0; ef1d0 = 0.d0
!      sumk1 = 0.d0
       aths=0.d0; athc=0.d0;ath=0.d0
      do k = 1,kl-1
!        ef1d1 = 0.d0
        do j = 1,jl
           sinth=sin(thet(j));costh=cos(thet(j))
!          ef1d1 = ef1d1 + eeout(k,j)*dthet
!          sumk1 = sumk1 + (eeout(k,j)+eeout(k+1,j))*(wk(k+1)-wk(k))*0.5*dthet
!          write(*,*) 'dwk:',(wk(k+1)-wk(k)),(pwk-1.d0)*wk(k)**2,wk(k+1),wk(k)
           aths=aths+(eeout(k,j)+eeout(k+1,j))*(wk(k+1)-wk(k))*0.5*dthet*sinth
           athc=athc+(eeout(k,j)+eeout(k+1,j))*(wk(k+1)-wk(k))*0.5*dthet*costh
        enddo
!        sumk = sumk + (ef1d0+ef1d1) * (wk(k+1)-wk(k))*0.5
!        ef1d0 = ef1d1
      enddo
      ath = atan2(aths,athc)*180.d0/pi
      if(ath.lt.0) ath=360.+ath
!      write(*,*) 'ath:',ath
!      write(*,*) 'Hm0:',4*sqrt(sumk),4*sqrt(sumk1),dthet,pwk
 
 
    end subroutine spectra2d
!--------------------------------------------------------------------   
    subroutine FetchCurve(HM0,FP,U10,FPND,fac)
      !Calculate U10 and FETCH according to the Fetch-limited wave growth curves 
      !of E-FETCH, FP-FETCH
      !U10: relative 10 meters wind speed
      !FPND: nan-dimensional peak frequency
      implicit none
      real(kind_r4),intent(in)  :: HM0,FP,fac
      real(kind_r4),intent(out) :: U10,FPND
      real(kind_r4) :: A,B,C,D,M0,M0ND

      !Kahma and Calkoen (1992) (composite condition)
      A    = 5.2e-7; B = 0.9d0; C = 2.1804d0; D = -0.27d0
      M0   = HM0**2.d0/16.d0
      U10  = 5.4115e7*M0**1.5d0*FP**5.d0/g**2.d0
      U10  = max(U10, 0.13d0*g/FP) !PM Limit
      M0ND = M0*g**2.d0/U10**4.d0 * fac**2
      FPND = 0.0284/M0ND**0.3
!      FPND = FP*U10/g
!      FPND = max(FPND,0.13) !PM Limit
!      XND  = (M0ND/A)**(1.0/B)
!      TND  = zpi*C/(0.4*(D+1))*XND**(D+1)
!      XM   = XND*U10**2/g

    end subroutine FetchCurve
!--------------------------------------------------------------------   
    subroutine DirectDistri(theta0,f,fm,uccp,S_thet,iflag)
      !Directional distribution of wave spectrum
      implicit none
      real(kind_r4),intent(in)  :: theta0,f,fm,uccp
      real(kind_r4),intent(out) :: S_thet
      integer,optional    :: iflag
      real(kind_r4) :: ffm
 
      ffm = f/fm

      if (.not.present(iflag)) iflag = 2

      select case(iflag)
      case(0) !Donelan (1985)
        call DL85(theta0,ffm,S_thet)
      case(1) !Longuet Higgins (1963),Babanin and Soloviev (1998)
        call BS98_LH(theta0,f,fm,ffm,uccp,S_thet)
      case(2) !Donelan (1985),Babanin and Soloviev (1998)
        call BS98_DL(theta0,f,fm,ffm,uccp,S_thet)
      case(3) !Mitsuyasu (1975)
        call MIT75(theta0,f,fm,ffm,uccp,S_thet)
      case(4) !Hasselmann (1980)
        call HASLM80(theta0,f,fm,ffm,uccp,S_thet)
      case(5) !Komen (1984)
        call Kom84(theta0,f,fm,ffm,uccp,S_thet)
      case(6) !cos^n(theta)
        call cos_dir(theta0,2,S_thet)
      end select
  
      return
    end subroutine DirectDistri
!--------------------------------------------------------------------   
!==================================================
!Donelan 1985 directional distribution
    subroutine DL85(theta0,wwp,S_thet)
      implicit none
      real(kind_r4),intent(in ) :: theta0,wwp
      real(kind_r4),intent(out) :: S_thet
      real(kind_r4) :: thetj,beta_D,sech_bt
      ! ---------------------
      !------beta-------------
!      if(0.56d0<wwp .and. wwp<0.95d0)then
      if( wwp<=0.95d0)then
        beta_D = 2.16d0*wwp**1.3d0
      elseif(0.95d0<wwp .and. wwp<1.6d0)then
        beta_D = 2.28d0/wwp**1.3d0
      elseif(wwp>=1.6d0)then
        beta_D = 10.d0**(-0.4d0+0.8393d0*exp(-0.567d0*log(wwp**2.d0)))
      else
        beta_D = 1.24d0
      endif
      ! ------directional distribution
      sech_bt = 1.d0/cosh(beta_D*theta0)
      S_thet  = 0.5d0*beta_D*sech_bt**2.d0
      return
    end subroutine DL85

!==================================================
!Longuet-Higgins-1963,Babanin and Soloviev-1998
    subroutine BS98_LH(theta0,ff,fm,ffm,uccp,S_thet)
      implicit none
      real(kind_r4), intent(in ) :: theta0,ff,fm,ffm,uccp
      real(kind_r4), intent(out) :: S_thet
      real(kind_r4) :: A,A0,s,K_fthet
      !--------------------------
      A0 = 1.18d0/sqrt(uccp) + 1.d0/zpi
      if(ff>=0.95d0*fm)then
        A = 1.12d0/sqrt(uccp)/ffm**0.95d0 + 1.d0/zpi
      elseif(ff<0.95d0*fm)then
        A = 2.05d0*ffm**exp(1.39d0-uccp) - 1.05d0
        A = A*A0
      endif
      s = 24.1d0*A - 11.4d0
      K_fthet = cos(theta0/2.d0)**(2.d0*s)
      S_thet = A*K_fthet
      return
    end subroutine BS98_LH

!==================================================
!Donelan (1985),Babanin and Soloviev (1998)
    subroutine BS98_DL(theta0,ff,fm,ffm,uccp,S_thet)
      implicit none
      real(kind_r4), intent(in ) :: theta0,ff,fm,ffm,uccp
      real(kind_r4), intent(out) :: S_thet
      real(kind_r4) :: A,A0,beta_D,K_fthet
      !--------------------------
      A0 = 1.18d0/uccp**0.5d0 + 1.d0/zpi
      if(ff>=0.95d0*fm)then
        A = 1.12d0/uccp**0.5d0/ffm**0.95d0 + 1.d0/zpi
      elseif(ff<0.95d0*fm)then
        A = 2.05d0*ffm**exp(1.39d0-uccp) - 1.05d0
        A = A*A0
      endif
      beta_D = 2.d0*A
      K_fthet = 0.5d0*beta_D*(1.d0/cosh(beta_D*theta0))**2.d0
      S_thet = A*K_fthet
!      write(*,*) 'A,Beta_D,K_fthet:',A,beta_D,K_fthet
      return
    end subroutine BS98_DL

!==================================================
!Mitsuyasu-1975
    subroutine MIT75(theta0,ff,fm,ffm,uccp,S_thet)
      implicit none
      real(kind_r4), intent(in ) :: theta0,ff,fm,ffm,uccp
      real(kind_r4), intent(out) :: S_thet
      real(kind_r4) :: A,s,sp,K_fthet
      !--------------------------
      sp = 11.5d0/uccp**2.5d0
      if(ff<fm)then
        s = sp*ffm**5.d0
      else
        s = sp/ffm**2.5d0
      endif
      !A = (s+11.4)/24.1
      A = gamma(s+1.d0)/(2.d0*sqrt(pi)*gamma(s+0.5d0))
      K_fthet = cos(theta0/2.d0)**(2.d0*s)
      S_thet = A*K_fthet
      return
   end subroutine MIT75

!==================================================
!Hasselmann-1980
    subroutine HASLM80(theta0,ff,fm,ffm,uccp,S_thet)
      implicit none
      real(kind_r4),  intent(in ) :: theta0,ff,fm,ffm,uccp
      real(kind_r4), intent(out) :: S_thet
      real(kind_r4) :: A,s,mue,K_fthet
      !--------------------------
      if(ff<1.05*fm)then
        s = 6.97*ffm**4.06
      else
        mue = -2.33-1.45*(uccp-1.17)
        s = 9.77*ffm**mue
      endif
      !A = (s+11.4)/24.1
      A = gamma(s+1.d0)/(2.d0*sqrt(pi)*gamma(s+0.5d0))
      K_fthet = cos(theta0/2.d0)**(2.d0*s)
      S_thet = A*K_fthet
      return
    end subroutine HASLM80
!==================================================
!Komen-1984
    subroutine Kom84(theta0,ff,fm,ffm,uccp,S_thet)
      implicit none
      real(kind_r4), intent(in ) :: theta0,ff,fm,ffm,uccp
      real(kind_r4), intent(out) :: S_thet
      real(kind_r4) :: Np,s,mue,K_fthet
      !--------------------------
      if(ff<fm)then
        mue = 4.06
      else
        mue = -2.34
      endif
      s = 9.77*ffm**mue
      Np = 2**(1-2*s)*pi*gamma(2*s+1)/gamma(s+1)**2
      K_fthet = cos(theta0/2.)**(2*s)
      S_thet = K_fthet/Np
      return
    end subroutine Kom84

!==================================================
!cos(thet) directional distribution
    subroutine cos_dir(theta0,n,S_thet)
      implicit none
      real(kind_r4), intent(in ) :: theta0
      integer,intent(in ) :: n
      real(kind_r4), intent(out) :: S_thet
      if (abs(theta0)>0.5d0*pi) then
        S_thet = 0.d0
      else
        S_thet = 2.d0/pi*cos(theta0)**n
      endif
      return
   end subroutine cos_dir
!==================================================
!--------------------------------------------------------------------   
end module wamspc2d_mod
