 SUBROUTINE ADVU
 USE LIMS
 USE CONTROL
 USE CONST, ONLY : ZERO,GRAV
 USE ALL_VAR, ONLY : DRHOX,ADVX,UF,W,U,V,UB,DT,EGF,EGB,ETF,ETB,ARU,DZ,COR,DY,H
 IMPLICIT NONE
!  Do vertical advection
 UF(:,:,:)=ZERO
! LOOP 140
! DO 140 J=1,JM
! DO 140 I=2,IM
 DO K=2,KBM1
 DO J=ISLAT,IELAT
 DO I=JSLON,IELON
 UF(I,J,K)=.25E0*(W(I,J,K)+W(I-1,J,K))*(U(I,J,K)+U(I,J,K-1))
 END DO
 END DO
 END DO

!****COMBINE HOR. and VERT. ADVECTION with
!           -FVD + GDEG/DX + BAROCLINIC TERM **********************
! LOOP 150
 DO K=1,KBM1
! DO 150 J=2,JMM1
! DO 150 I=2,IMM1
 DO J=JSLAT,JELAT
 DO I=JSLON,JELON 
 UF(I,J,K)=ADVX(I,J,K)+(UF(I,J,K)-UF(I,J,K+1))*ARU(I,J)/DZ(K)         &
    -ARU(I,J)*.25*(COR(I,J)*DT(I,J)*(V(I,J+1,K)+V(I,J,K))             &
              +COR(I-1,J)*DT(I-1,J)*(V(I-1,J+1,K)+V(I-1,J,K)))        &
         +GRAV*.25E0*(DT(I,J)+DT(I-1,J))                              &
         *.5*(EGF(I,J)-EGF(I-1,J)+EGB(I,J)-EGB(I-1,J))                &
         *(DY(I,J)+DY(I-1,J))                                         &
         +DRHOX(I,J,K)
 END DO
 END DO
 END DO
!******* STEP FORWARD IN TIME ***********************************
! LOOP 190
! DO 190 J=2,JMM1
! DO 190 I=2,IMM1
 DO K=1,KBM1
 DO J=JSLAT,JELAT
 DO I=JSLON,JELON
 UF(I,J,K)=                                                           &
       ((H(I,J)+ETB(I,J)+H(I-1,J)+ETB(I-1,J))*ARU(I,J)*UB(I,J,K)      &
          -2.E0*DTI2*UF(I,J,K))                                       &
      /((H(I,J)+ETF(I,J)+H(I-1,J)+ETF(I-1,J))*ARU(I,J))
 END DO
 END DO
 END DO
 RETURN
 END
