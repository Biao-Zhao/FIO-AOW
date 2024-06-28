 SUBROUTINE ADVT(FB,F,FCLIM,FF)
!
! THIS SUBROUTINE INTEGRATES CONSERVATIVE SCALAR EQUATIONS
!
 USE LIMS
 USE CONTROL
 USE CONST, ONLY : LE,TPRNI
 USE ALL_VAR, ONLY : AAM,U,V,W,DT,ETF,ETB,A,C,H,DUM,DVM,DX,DY,ART,DZ
 IMPLICIT NONE
! XFLUX >>> A
! YFLUX >>> C
 REAL,DIMENSION(ISLON-LE:IELON+LE,ISLAT-LE:IELAT+LE,KB) ::            &
               FB,F,FCLIM,FF
! LOOP 529
 !F(:,:,KB)=F(:,:,KBM1)
 !FB(:,:,KB)=FB(:,:,KBM1)
!******* DO ADVECTION FLUXES **************************************
! LOOP 530
! DO 530 J=2,JM
! DO 530 I=2,IM
 DO K=1,KBM1
 DO J=LSLAT,LELAT
 DO I=LSLON,LELON
 A(I,J,K)=.25E0*((DT(I,J)+DT(I-1,J))                              &
             *(F(I,J,K)+F(I-1,J,K))*U(I,J,K))
 C(I,J,K)=.25E0*((DT(I,J)+DT(I,J-1))                              &
             *(F(I,J,K)+F(I,J-1,K))*V(I,J,K))
 END DO
 END DO
 END DO
!******  ADD DIFFUSIVE FLUXES *************************************
! LOOP 99
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! DELETE THE LOOP 99 FOR PARALLEL CODE
!      DO 99 K=1,KB
!      DO 99 J=1,JM
!      DO 99 I=1,IM
!  99  FB(I,J,K)=FB(I,J,K)-FCLIM(I,J,K)
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! LOOP 100
! DO 100 J=2,JM
! DO 100 I=2,IM
 DO K=1,KBM1      
 DO J=LSLAT,LELAT
 DO I=LSLON,LELON
 A(I,J,K)=A(I,J,K)                                            &
     -.5E0*(AAM(I,J,K)+AAM(I-1,J,K))*(H(I,J)+H(I-1,J))*TPRNI          &
     *(FB(I,J,K)-FB(I-1,J,K))*DUM(I,J)/(DX(I,J)+DX(I-1,J))
 C(I,J,K)=C(I,J,K)                                            &
     -.5E0*(AAM(I,J,K)+AAM(I,J-1,K))*(H(I,J)+H(I,J-1))*TPRNI          &
     *(FB(I,J,K)-FB(I,J-1,K))*DVM(I,J)/(DY(I,J)+DY(I,J-1))
 A(I,J,K)=.5E0*(DY(I,J)+DY(I-1,J))*A(I,J,K)
 C(I,J,K)=.5E0*(DX(I,J)+DX(I,J-1))*C(I,J,K)
 END DO
 END DO
 END DO
! LOOP 101
! DO 101 J=1,JM
! DO 101 I=1,IM
 DO K=1,KB 
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
 FB(I,J,K)=FB(I,J,K)+FCLIM(I,J,K)
 END DO
 END DO
 END DO
!
!****** DO VERTICAL ADVECTION *************************************
! LOOP 505
! DO 505 J=2,JMM1
! DO 505 I=2,IMM1
 DO J=JSLAT,JELAT
 DO I=JSLON,JELON
 FF(I,J,1)=-.5*(F(I,J,1)+F(I,J,2))*W(I,J,2)*ART(I,J)/DZ(1)
 END DO
 END DO
! LOOP 520
! DO 520 J=2,JMM1
! DO 520 I=2,IMM1
 DO K=2,KBM1
 DO J=JSLAT,JELAT
 DO I=JSLON,JELON
 FF(I,J,K)=.5*((F(I,J,K-1)+F(I,J,K))*W(I,J,K)                         &
              -(F(I,J,K)+F(I,J,K+1))*W(I,J,K+1))*ART(I,J)/DZ(K)
 END DO
 END DO
 END DO
!****** ADD NET HORIZONTAL FLUXES; THEN STEP FORWARD IN TIME **********
! LOOP 120
! DO 120 J=2,JMM1
! DO 120 I=2,IMM1
 DO K=1,KBM1
 DO J=JSLAT,JELAT
 DO I=JSLON,JELON
 FF(I,J,K)=FF(I,J,K)                                                  &
               +A(I+1,J,K)-A(I,J,K)                           &
               +C(I,J+1,K)-C(I,J,K)
 FF(I,J,K)=(FB(I,J,K)*(H(I,J)+ETB(I,J))*ART(I,J)-DTI2*FF(I,J,K))      &
                     /((H(I,J)+ETF(I,J))*ART(I,J))
 END DO
 END DO
 END DO 
 RETURN
 END                                                                                                                                    
