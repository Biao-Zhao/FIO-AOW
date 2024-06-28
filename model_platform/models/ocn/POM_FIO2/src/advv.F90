 SUBROUTINE ADVV
 USE LIMS
 USE CONTROL
 USE CONST, ONLY : ZERO,GRAV
 USE ALL_VAR, ONLY : DRHOY,ADVY,VF,W,U,V,VB,DT,EGF,EGB,ETF,ETB,ARV,DZ,COR,DX,H
 IMPLICIT NONE
 
! Do vertical advection
 VF(:,:,:)=ZERO
! LOOP 140
! DO 140 J=2,JM
! DO 140 I=1,IM
 DO K=2,KBM1
 DO J=JSLAT,IELAT
 DO I=ISLON,IELON
 VF(I,J,K)=.25*(W(I,J,K)+W(I,J-1,K))*(V(I,J,K)+V(I,J,K-1))
 END DO
 END DO
 END DO
!****COMBINE HOR. and VERT. ADVECTION with
!           +FUD + GDEG/DY + BAROCLINIC TERM **********************
! LOOP 340
 DO K=1,KBM1
! DO 340 J=2,JMM1
! DO 340 I=2,IMM1
 DO J=JSLAT,JELAT
 DO I=JSLON,JELON
 VF(I,J,K)=ADVY(I,J,K)+(VF(I,J,K)-VF(I,J,K+1))*ARV(I,J)/DZ(K)         &
    +ARV(I,J)*.25*(COR(I,J)*DT(I,J)*(U(I+1,J,K)+U(I,J,K))             &
              +COR(I,J-1)*DT(I,J-1)*(U(I+1,J-1,K)+U(I,J-1,K)))        &
        +GRAV*.25E0*(DT(I,J)+DT(I,J-1))                               &
        *.5*(EGF(I,J)-EGF(I,J-1)+EGB(I,J)-EGB(I,J-1))                 &
        *(DX(I,J)+DX(I,J-1))                                          &
        +DRHOY(I,J,K)
 END DO
 END DO
 END DO
!******* STEP FORWARD IN TIME ***************************************
! LOOP 390
! DO 390 J=2,JMM1
! DO 390 I=2,IMM1
 DO K=1,KBM1
 DO J=JSLAT,JELAT
 DO I=JSLON,JELON
 VF(I,J,K)=                                                           &
      ((H(I,J)+ETB(I,J)+H(I,J-1)+ETB(I,J-1))*ARV(I,J)*VB(I,J,K)       &
           -2.E0*DTI2*VF(I,J,K))                                      &
     /((H(I,J)+ETF(I,J)+H(I,J-1)+ETF(I,J-1))*ARV(I,J))
 END DO
 END DO
 END DO
 RETURN
 END
