 SUBROUTINE MEAN_SWELL
!------------------------------------------added by wgs----------------------------------------------------------------------
 USE data_kind_mod_wave
 USE CONTROL_WAVE, ONLY : ISLAT,IELAT,ISLON,IELON
 USE CONST_WAVE, ONLY : G,PI,ZERO,KL,ZPI,JL,TZTZ,DELTTH
 USE ALL_VAR_WAVE, ONLY : NSP,DWK,DWF,WF,WK,WKH,EE,THET,CCG,wx,wy,   &
                     swell_hs,swell_tp,swell_th,swell_tz,windy_hs,windy_tp,windy_th,windy_tz, &
                     D
 IMPLICIT NONE
 INTEGER(kind_in) :: IA,IC,IAHM,ICHM,K,K1,I,I1,J
 REAL(kind_r4) :: HMAX,PID180,DWKK,DWFK,WFK,WFK1,TEST,DTTEST,beta,wt
 REAL(kind_r4) :: WSK,WSK1,WKK,WKK1,EEF0,EEKJ,EEKJ1,EEKJTH
 REAL(kind_r4) :: SINTH,COSTH,AETT,CHBH,THS                             !added by zhaobiao
 REAL(kind_r4) :: WFK2,WFK12,WSK2,WSK3,WSK12,WSK13,G2
 REAL(kind_r4) :: SINTHJL(JL),COSTHJL(JL)
 REAL(kind_r4),DIMENSION(ISLON:IELON,ISLAT:IELAT) :: HB,HBB,    &
               AE,ASI,AWF,AWK,AETS,AETC,AE1,ASI1,AWF1,AETS1,AETC1,APE1,APE2                         ! added by zhaobiao 
!
 HMAX=0.
 IAHM=1
 ICHM=1
 PID180=PI/180.
 DTTEST=0.01d0
 beta=1.2d0
 !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 DO J=1,JL
   SINTHJL(J)=SIN(THET(J))
   COSTHJL(J)=COS(THET(J))
 END DO
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

 AE=ZERO;ASI=ZERO;AWF=ZERO;AWK=ZERO
!ARK=ZERO
 APE2=ZERO;HB=ZERO
 HBB=ZERO;
 AETS=ZERO;AETC=ZERO
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! add by wgs for swell calculate 20190926
ae1=zero;asi1=zero;awf1=zero
aets1=zero;aetc1=zero;ape1=zero

DO IC=ISLAT,IELAT
DO IA=ISLON,IELON
 DO K=1,KL+1
   K1=K+1
   I=K-KL+1
   I1=I+1
   DWKK=DWK(K)
   WFK=WF(K,IA,IC)
   WFK2=1./(WFK*WFK)
   WFK1=WF(K1,IA,IC)
   WFK12=1./(WFK1*WFK1)
 
   WSK=ZPI*WFK
   WSK2=WSK*WSK
   WSK1=ZPI*WFK1
   WSK12=WSK1*WSK1
   WKK=WK(K)
   WKK1=WK(K1)

   DO J=1,JL
     IF (K<KL) THEN
         EEKJ =EE(K,J,IA,IC)
         EEKJ1=EE(K1,J,IA,IC)
      ELSE
         EEKJ =EE(KL,J,IA,IC)*WKH(I)
         EEKJ1=EE(KL,J,IA,IC)*WKH(I1)
      ENDIF
      !SINTH=SIN(THET(J))
      !COSTH=COS(THET(J))
       SINTH=SINTHJL(J)
       COSTH=COSTHJL(J)

      TEST=beta*wkk*(wx(ia,ic)*costh+wy(ia,ic)*sinth)/wsk-1.d0
      if(test+dttest<0.d0)then
        wt=1.d0   ! swell
      elseif(test-dttest>0.d0)then
        wt=0.d0 ! wind-wave
      else
        wt=(dttest-test)/(dttest*2.d0)
      end if

      AE(IA,IC)=AE(IA,IC)+wt*(EEKJ+EEKJ1)*DWKK
      !ASI(IA,IC)=ASI(IA,IC)+wt*(EEKJ/WFK**2+EEKJ1/WFK1**2)*DWKK
      !APE(IA,IC)=APE(IA,IC)+wt*(EEKJ*WSK**2+EEKJ1*WSK1**2)*DWKK
      ASI(IA,IC)=ASI(IA,IC)+wt*(EEKJ*WFK2+EEKJ1*WFK12)*DWKK
      APE2(IA,IC)=APE2(IA,IC)+wt*(EEKJ*WSK2+EEKJ1*WSK12)*DWKK
      AWF(IA,IC)=AWF(IA,IC)+wt*(EEKJ*WFK+EEKJ1*WFK1)*DWKK
      AETS(IA,IC)=AETS(IA,IC)+wt*(EEKJ+EEKJ1)*DWKK*sinth
      AETC(IA,IC)=AETC(IA,IC)+wt*(EEKJ+EEKJ1)*DWKK*costh

      AE1(IA,IC)=AE1(IA,IC)+(1.d0-wt)*(EEKJ+EEKJ1)*DWKK
      ASI1(IA,IC)=ASI1(IA,IC)+(1.d0-wt)*(EEKJ*WFK2+EEKJ1*WFK12)*DWKK
      APE1(IA,IC)=APE1(IA,IC)+(1.d0-wt)*(EEKJ*WSK2+EEKJ1*WSK12)*DWKK
      AWF1(IA,IC)=AWF1(IA,IC)+(1.d0-wt)*(EEKJ*WFK+EEKJ1*WFK1)*DWKK
      AETS1(IA,IC)=AETS1(IA,IC)+(1.d0-wt)*(EEKJ+EEKJ1)*DWKK*sinth
      AETC1(IA,IC)=AETC1(IA,IC)+(1.d0-wt)*(EEKJ+EEKJ1)*DWKK*costh
   END DO
 END DO

 swell_hs(ia,ic)=4.d0*sqrt(ae(ia,ic))
 swell_tp(ia,ic)=(asi(ia,ic)/ae(ia,ic))*(awf(ia,ic)/ae(ia,ic))
 swell_th(ia,ic)=atan2(aets(ia,ic),aetc(ia,ic))*180.d0/pi
 swell_tz(ia,ic)=zpi/sqrt(ape2(ia,ic)/ae(ia,ic));if(swell_tz(ia,ic)<0)swell_tz(ia,ic)=360.+swell_tz(ia,ic)

 windy_hs(ia,ic)=4.d0*sqrt(ae1(ia,ic))
 windy_tp(ia,ic)=(asi1(ia,ic)/ae1(ia,ic))*(awf1(ia,ic)/ae1(ia,ic))
 windy_th(ia,ic)=atan2(aets1(ia,ic),aetc1(ia,ic))*180.d0/pi
 windy_tz(ia,ic)=zpi/sqrt(ape1(ia,ic)/ae1(ia,ic));if(windy_tz(ia,ic)<0)windy_tz(ia,ic)=360.+windy_tz(ia,ic)
END DO
END DO
!
 RETURN
 END
