 SUBROUTINE INTACT
 USE data_kind_mod_wave
 USE ALL_VAR_WAVE, ONLY : TAUBB11,TAUBB12,TAUBB22,TAUBB33,NSP,              &
                     DWK,WF,WK,THET,EA,WKH,ZBV,D
 USE LIMS_WAVE, ONLY : KB
 USE CONTROL_WAVE, ONLY : ISLON,IELON,ISLAT,IELAT
 USE CONST_WAVE, ONLY : ZERO,KLD,JL,ZPI,KL
 IMPLICIT NONE
 INTEGER IA,IC,KH,K,J,K1,I,I1
 REAL(kind_r4) :: DWKK,WFK,WFK1,WSK,WSK1,WKK,WKK1,THETA0,SINTH,COSTH,  &
                  EKJ,EKJ1
 
 TAUBB11=ZERO;TAUBB12=ZERO
 TAUBB22=ZERO;TAUBB33=ZERO
  
!
 DO IC=ISLAT,IELAT
 DO IA=ISLON,IELON
    IF(NSP(IA,IC)/=1)CYCLE
    DO KH=1,KB
       IF(ABS(ZBV(KH))>D(IA,IC))THEN
         TAUBB11(IA,IC,KH)=ZERO
         TAUBB12(IA,IC,KH)=ZERO
         TAUBB22(IA,IC,KH)=ZERO
         TAUBB33(IA,IC,KH)=ZERO
         CYCLE
       END IF
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
             THETA0=THET(J)
             SINTH=SIN(THETA0)
             COSTH=COS(THETA0) 
             IF (K.LT.KL) THEN
             EKJ=EA(K,J,IA,IC)
             EKJ1=EA(K1,J,IA,IC)
             ELSE
             EKJ=EA(KL,J,IA,IC)*WKH(I)
             EKJ1=EA(KL,J,IA,IC)*WKH(I1)
             ENDIF
             TAUBB11(IA,IC,KH)=TAUBB11(IA,IC,KH)-WSK**2*COSTH**2*(EKJ+EKJ1)*EXP(2*WKK*ZBV(KH))*DWKK
                 !             *EXP(2*WKK*100.0*ZBV(KH))*DWKK
             TAUBB12(IA,IC,KH)=TAUBB12(IA,IC,KH)-WSK**2*SINTH*COSTH*(EKJ+EKJ1)*EXP(2*WKK*ZBV(KH))*DWKK
                 !             *EXP(2*WKK*100.0*ZBV(KH))*DWKK
             TAUBB22(IA,IC,KH)=TAUBB22(IA,IC,KH)-WSK**2*SINTH**2*(EKJ+EKJ1)*EXP(2*WKK*ZBV(KH))*DWKK
                 !             *EXP(2*WKK*100.0*ZBV(KH))*DWKK
             TAUBB33(IA,IC,KH)=TAUBB33(IA,IC,KH)-WSK**2*(EKJ+EKJ1)*EXP(2*WKK*ZBV(KH))*DWKK 
           !                   *EXP(2*WKK*100.0*ZBV(KH))*DWKK
          END DO
       END DO
    END DO   
 END DO
 END DO
!
 RETURN
 END
