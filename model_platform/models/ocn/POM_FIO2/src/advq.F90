 SUBROUTINE ADVQ(QB,Q,QF)
! **********************************************************************
! *                                                                    *
! * FUNCTION    :  Calculates horizontal advection and diffusion, and  *
! *                vertical advection for turbulent quantities.        *
! *                                                                    *
! **********************************************************************
 USE DATA_KIND_MOD
 USE LIMS
 USE CONTROL
 USE CONST, ONLY : LE
 USE ALL_VAR, ONLY : A,C,V,DT,U,V,W,AAM,DX,DY,H,DUM,DVM,DZ,ART,ETB,ETF
 IMPLICIT NONE
 REAL(kind_r4),DIMENSION(ISLON-LE:IELON+LE,ISLAT-LE:IELAT+LE,KB) :: QB,Q,QF
     
! XFLUX >>> A
! YFLUX >>> C              
!******* HORIZONTAL ADVECTION ************************************
! LOOP 110
! DO 110 J=2,JM
! DO 110 I=2,IM
 DO K=2,KBM1
 DO J=LSLAT,LELAT
 DO I=LSLON,LELON
 A(I,J,K)=.125E0*(Q(I,J,K)+Q(I-1,J,K))                       &
              *(DT(I,J)+DT(I-1,J))*(U(I,J,K)+U(I,J,K-1))
 C(I,J,K)=.125E0*(Q(I,J,K)+Q(I,J-1,K))*(DT(I,J)+DT(I,J-1))   &
              *(V(I,J,K)+V(I,J,K-1))
 END DO        
 END DO        
 END DO        
!******* HORIZONTAL DIFFUSION ************************************
! LOO 315
! DO 315 J=2,JM
! DO 315 I=2,IM
 DO K=2,KBM1
 DO J=LSLAT,LELAT
 DO I=LSLON,LELON
 A(I,J,K)=A(I,J,K)                                       &
     -.25*(AAM(I,J,K)+AAM(I-1,J,K)+AAM(I,J,K-1)+AAM(I-1,J,K-1))  &
      *(H(I,J)+H(I-1,J))*(QB(I,J,K)-QB(I-1,J,K))*DUM(I,J)        &
            /(DX(I,J)+DX(I-1,J))
 C(I,J,K)=C(I,J,K)                                       &
     -.25*(AAM(I,J,K)+AAM(I,J-1,K)+AAM(I,J,K-1)+AAM(I,J-1,K-1))  &
      *(H(I,J)+H(I,J-1))*(QB(I,J,K)-QB(I,J-1,K))*DVM(I,J)        &
            /(DY(I,J)+DY(I,J-1))
 A(I,J,K)=.5E0*(DY(I,J)+DY(I-1,J))*A(I,J,K)
 C(I,J,K)=.5E0*(DX(I,J)+DX(I,J-1))*C(I,J,K)
 END DO
 END DO
 END DO
!****** VERTICAL ADVECTION; ADD FLUX TERMS ;THEN STEP FORWARD IN TIME
! LOOP 230
! DO 230 J=2,JMM1
! DO 230 I=2,IMM1
 DO K=2,KBM1
 DO J=JSLAT,JELAT
 DO I=JSLON,JELON
 QF(I,J,K)=(W(I,J,K-1)*Q(I,J,K-1)-W(I,J,K+1)*Q(I,J,K+1))         &
                      /(DZ(K)+DZ(K-1))*ART(I,J)                  &
                       +A(I+1,J,K)-A(I,J,K)              &
                       +C(I,J+1,K)-C(I,J,K)
 QF(I,J,K)=((H(I,J)+ETB(I,J))*ART(I,J)*QB(I,J,K)-DTI2*QF(I,J,K)) &
              /((H(I,J)+ETF(I,J))*ART(I,J))
 END DO              
 END DO             
 END DO             
!
 RETURN
 END               
