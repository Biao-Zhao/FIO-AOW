 SUBROUTINE MIXTURE_COUPLE
 USE data_kind_mod_wave
 USE LIMS_WAVE, ONLY : KB
 USE CONTROL_WAVE, ONLY : ISLON,IELON,ISLAT,IELAT
 USE ALL_VAR_WAVE, ONLY : NSP,WF,DWK,WK,ZBV,EE,D,BV,WKH
 USE CONST_WAVE, ONLY : ZERO,KLD,JL,KL,ZPI
 IMPLICIT NONE
 INTEGER IA,IC,KH,K1,I,I1,K,J
 REAL(kind_r4) DWKK,WFK,WFK1,WSK,WSK1,WKK,WKK1,EKJ,EKJ1
 REAL(kind_r4),DIMENSION(ISLON:IELON,ISLAT:IELAT,KB) :: BV1,BV2,BV3
!=======================================================================================
! write(6,*) 'call subroutine Bv'
 
 BV1=ZERO;BV2=ZERO;BV3=ZERO;BV=0.
!
 ZBV=-1*ZBV
 DO KH=1,KB
 DO IC=ISLAT,IELAT
 DO IA=ISLON,IELON
    IF(NSP(IA,IC)==0)CYCLE
    DO K=1,KLD
       K1=K+1
       I=K-KL+1
       I1=I+1
       DWKK=DWK(K)
       WFK=WF(K,IA,IC)
       WFK1=WF(K1,IA,IC)
       WSK=ZPI*WFK
       WSK1=ZPI*WFK1
       WKK=WK(K)
       WKK1=WK(K1)
       DO J=1,JL
          IF (K<KL) THEN
             EKJ=EE(K,J,IA,IC)
             EKJ1=EE(K1,J,IA,IC)
          ELSE
             EKJ=EE(KL,J,IA,IC)*WKH(I)
             EKJ1=EE(KL,J,IA,IC)*WKH(I1)
          ENDIF
          BV1(IA,IC,KH)=BV1(IA,IC,KH)+(EKJ+EKJ1)*EXP(2*WKK*ZBV(KH))*DWKK
              !   +(EKJ+EKJ1)*EXP(2*WKK*100.0*ZYYZ(KH))*DWKK
          BV2(IA,IC,KH)=BV2(IA,IC,KH)+WSK**2*(EKJ+EKJ1)*EXP(2*WKK*ZBV(KH))*DWKK
              !   +WSK**2*(EKJ+EKJ1)*EXP(2*WKK*100.0*ZYYZ(KH))*DWKK
          BV3(IA,IC,KH)=BV3(IA,IC,KH)+WKK*WSK**2*(EKJ+EKJ1)*EXP(2*WKK*ZBV(KH))*DWKK
              !   +WKK*WSK**2*(EKJ+EKJ1)*EXP(2*WKK*100.0*ZYYZ(KH))*DWKK
       END DO
    END DO

    IF(ABS(BV2(IA,IC,KH)*BV3(IA,IC,KH))<1.E-10)THEN
    BV(IA,IC,KH)=0.
    ELSE
    BV(IA,IC,KH)=BV1(IA,IC,KH)/SQRT(BV2(IA,IC,KH))*BV3(IA,IC,KH)
    END IF
    IF(BV(IA,IC,KH)>1.)BV(IA,IC,KH)=1.
    IF(ISNAN(BV(IA,IC,KH)))BV(IA,IC,KH)=0
    IF(ABS(ZBV(KH))>D(IA,IC))THEN
      BV(IA,IC,KH)=ZERO
      CYCLE
    ENDIF
 END DO
 END DO
 END DO
 ZBV=-1*ZBV
 !========================================================================================================
 !write(6,*) 'call subroutine Bv succeed'

 RETURN
 END  SUBROUTINE MIXTURE_COUPLE 
