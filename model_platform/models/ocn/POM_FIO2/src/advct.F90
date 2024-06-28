 SUBROUTINE ADVCT
!======================================================================
! THIS ROUTINE CALCULATES THE HORIZONTAL PORTIONS OF MOMENTUM ADVECTION
! WELL IN ADVANCE OF THEIR USE IN ADVU AND ADVV SO THAT THEIR VERTICAL 
! INTEGRALS (CREATED IN MAIN) MAY BE USED IN THE EXTERNAL MODE CALCULATION.
!======================================================================
 USE ALL_VAR, ONLY : ADVX,ADVY,U,V,UB,VB,AAM,DT,DX,DY,ARU,ARV,A,C,EE
 USE CONST, ONLY : ZERO
 USE CONTROL
 USE LIMS
 IMPLICIT NONE
! LOCAL
 REAL DTAAM
! XFLUX >>> A
! YFLUX >>> C
! CURV >>>> EE
 
 EE = ZERO
 ADVX = ZERO
 A = ZERO
 C = ZERO
! LOOP 60
! DO 60 J=2,JMM1
! DO 60 I=2,IMM1
 DO J=LSLAT,LELAT
 DO I=LSLON,LELON
 DO K=1,KBM1
 EE(I,J,K)=                                                    &
 +.25E0*((V(I,J+1,K)+V(I,J,K))*(DY(I+1,J)-DY(I-1,J))             &
          -(U(I+1,J,K)+U(I,J,K))*(DX(I,J+1)-DX(I,J-1)) )         &
           /(DX(I,J)*DY(I,J))
 END DO
 END DO
 END DO

!-----------------------------------------------------------------
! CALCULATE X-COMPONENT OF VELOCITY ADVECTION                 
!-----------------------------------------------------------------
!
!******** HORIZONTAL ADVECTION FLUXES ****************************
! LOOP 100
 DO K=1,KBM1
! DO 100 J=1,JM
! DO 100 I=2,IMM1
 DO J=ISLAT-1,IELAT+1
 DO I=LSLON,LELON
 A(I,J,K)=.125E0*((DT(I+1,J)+DT(I,J))*                       &
               U(I+1,J,K)+(DT(I,J)+DT(I-1,J))                    &
              *U(I,J,K))*(U(I+1,J,K)+U(I,J,K))
 END DO             
 END DO             
 END DO
! LOOP 120             
 DO K=1,KBM1
! DO 120 J=2,JM
! DO 120 I=2,IM
 DO J=LSLAT,LELAT
 DO I=LSLON,LELON
 C(I,J,K)=.125E0*((DT(I,J)+DT(I,J-1))                        &
        *V(I,J,K)+(DT(I-1,J)+DT(I-1,J-1))                        &
        *V(I-1,J,K))*(U(I,J,K)+U(I,J-1,K))
 END DO
 END DO
 END DO

!****** ADD HORIZONTAL DIFFUSION FLUXES ****************************
! LOOP 130
 DO K=1,KBM1
! DO  130 J=2,JM
! DO  130 I=2,IMM1
 DO J=LSLAT,LELAT
 DO I=LSLON,LELON
 A(I,J,K)=A(I,J,K)                                       &
         -DT(I,J)*AAM(I,J,K)*2.E0*(UB(I+1,J,K)-UB(I,J,K))/DX(I,J)
 DTAAM=.25E0*(DT(I,J)+DT(I-1,J)+DT(I,J-1)+DT(I-1,J-1))           &
       *(AAM(I,J,K)+AAM(I-1,J,K)+AAM(I,J-1,K)+AAM(I-1,J-1,K))
 C(I,J,K)=C(I,J,K)                                       &
           -DTAAM*((UB(I,J,K)-UB(I,J-1,K))                       &
                     /(DY(I,J)+DY(I-1,J)+DY(I,J-1)+DY(I-1,J-1))  &
                     +(VB(I,J,K)-VB(I-1,J,K))                    &
                     /(DX(I,J)+DX(I-1,J)+DX(I,J-1)+DX(I-1,J-1)))
!
 A(I,J,K)=DY(I,J)*A(I,J,K)
 C(I,J,K)=                                                   &   
      .25E0*(DX(I,J)+DX(I-1,J)+DX(I,J-1)+DX(I-1,J-1))*C(I,J,K)
 END DO
 END DO
 END DO
  
!
!******** DO HORIZ. ADVECTION *******
! LOOP 146 
 DO K=1,KBM1
! DO 146 J=2,JMM1
! DO 146 I=2,IMM1
 DO J=JSLAT,JELAT
 DO I=JSLON,JELON
 ADVX(I,J,K)=                                                    & 
             +A(I,J,K)-A(I-1,J,K)                        &
             +C(I,J+1,K)-C(I,J,K)
 END DO 
 END DO 
 END DO
! LOOP 150 	
 DO K=1,KBM1
! DO 150 J=3,JMM2
! DO 150 I=3,IMM2
 DO J=KSLAT,KELAT
 DO I=KSLON,KELON
 ADVX(I,J,K)=ADVX(I,J,K)                                         &
    -ARU(I,J)*.25*(EE(I,J,K)*DT(I,J)*(V(I,J+1,K)+V(I,J,K))     &
              +EE(I-1,J,K)*DT(I-1,J)*(V(I-1,J+1,K)+V(I-1,J,K)))
 END DO
 END DO
 END DO
!
!-----------------------------------------------------------------
!      CALCULATE Y-COMPONENT OF VELOCITY ADVECTION                 
!-----------------------------------------------------------------
 ADVY = ZERO
 A = ZERO
 C = ZERO
!
!********** HORIZONTAL ADVECTION FLUXES **************************
! LOOP 300 
 DO K=1,KBM1
! DO 300 J=2,JM
! DO 300 I=2,IM
 DO J=LSLAT,LELAT
 DO I=LSLON,LELON
 A(I,J,K)=.125E0*((DT(I,J)+DT(I-1,J))*U(I,J,K)               &
                   +(DT(I,J-1)+DT(I-1,J-1))*U(I,J-1,K))          &
                         *(V(I,J,K)+V(I-1,J,K))
 END DO                        
 END DO                        
 END DO
! LOOP 320                        
 DO K=1,KBM1
! DO 320 J=2,JMM1
! DO 320 I=1,IM
 DO J=LSLAT,LELAT
 DO I=ISLON-1,LELON
 C(I,J,K)=.125E0*((DT(I,J+1)+DT(I,J))*V(I,J+1,K)             &
                   +(DT(I,J)+DT(I,J-1))*V(I,J,K))                &
                         *(V(I,J+1,K)+V(I,J,K))
 END DO
 END DO
 END DO
!******* ADD HORIZONTAL DIFFUSION FLUXES **************************
! LOOP 700
 DO K=1,KBM1
! DO  700 J=2,JMM1
! DO  700 I=2,IM
 DO J=LSLAT,LELAT
 DO I=LSLON,LELON
 DTAAM=.25E0*(DT(I,J)+DT(I-1,J)+DT(I,J-1)+DT(I-1,J-1))           &
            *(AAM(I,J,K)+AAM(I-1,J,K)+AAM(I,J-1,K)+AAM(I-1,J-1,K))
 A(I,J,K)=A(I,J,K)                                       &
      -DTAAM*((UB(I,J,K)-UB(I,J-1,K))                            &                                      
          /(DY(I,J)+DY(I-1,J)+DY(I,J-1)+DY(I-1,J-1))             &
                +(VB(I,J,K)-VB(I-1,J,K))                         &
          /(DX(I,J)+DX(I-1,J)+DX(I,J-1)+DX(I-1,J-1)))
 C(I,J,K)=C(I,J,K)                                       &
         -DT(I,J)*AAM(I,J,K)*2.E0*(VB(I,J+1,K)-VB(I,J,K))/DY(I,J)
!
 A(I,J,K)                                                    &
     =.25E0*(DY(I,J)+DY(I-1,J)+DY(I,J-1)+DY(I-1,J-1))*A(I,J,K)
 C(I,J,K)=DX(I,J)*C(I,J,K)
 END DO
 END DO
 END DO

!********** DO HORIZ. ADVECTION ************
! LOOP 400
 DO K=1,KBM1
! DO 400 J=2,JMM1
! DO 400 I=2,IMM1
 DO J=JSLAT,JELAT
 DO I=JSLON,JELON
 ADVY(I,J,K)=                                                    &
             +A(I+1,J,K)-A(I,J,K)                        &
             +C(I,J,K)-C(I,J-1,K)
 END DO
 END DO
 END DO
! LOOP 410 
 DO K=1,KBM1
! DO 410 J=2,JMM1
! DO 410 I=2,IMM1
 DO J=JSLAT,JELAT
 DO I=JSLON,JELON
 ADVY(I,J,K)=ADVY(I,J,K)                                         &
   +ARV(I,J)*.25*(EE(I,J,K)*DT(I,J)*(U(I+1,J,K)+U(I,J,K))      &
             +EE(I,J-1,K)*DT(I,J-1)*(U(I+1,J-1,K)+U(I,J-1,K)))
 END DO
 END DO
 END DO
!
 RETURN
 END
