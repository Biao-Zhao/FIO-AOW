  SUBROUTINE SETSPEC(N)
  USE data_kind_mod_wave
  USE CONST_WAVE,ONLY : G,PI,JL,KL,ZPI
  USE CONTROL_WAVE,ONLY : ISLON,IELON,ISLAT,IELAT
  USE ALL_VAR_WAVE,ONLY : NSP,WX,WY,THET,WK,WF,EE
  IMPLICIT NONE
! --- Usage: if n = 1, this subroutine will prepare for initial conditions
!            if n = 2, this subroutine will prepare for OBC.
  INTEGER,INTENT(IN) :: N
! LOCAL
  REAL(kind_r4),PARAMETER :: GAMA = 3.3
!  REAL(kind_r4),PARAMETER :: SQ3 = SQRT(3.) 
  REAL(kind_r4),PARAMETER :: SQ3 = 1.732051 
!  REAL(kind_r4),PARAMETER :: XJ0 = 50*1000. 
  REAL(kind_r4),PARAMETER :: XJ0 = 300*1000. 
  INTEGER IA,IC,J,K
  REAL(kind_r4) :: VX,VY,WW,XJ,ARLFA,WSJ,WKJ,WL
  REAL(kind_r4) :: THETA0,COSTH,SINTH,WK0,WF0,WS0
  REAL(kind_r4) :: SIGMA,ALPHA 
!
  DO IA=ISLON,IELON
  DO IC=ISLAT,IELAT
!
  IF(NSP(IA,IC)<N)CYCLE 
!
  VX=WX(IA,IC)
  VY=WY(IA,IC)
  WW=VX**2+VY**2
  WW=SQRT(WW)
  IF (WW<=0.) WW=0.9

  XJ=G*XJ0/(WW**2)
  ARLFA=(0.076*(XJ**(-0.4)))/PI
  WSJ=22.*(XJ**(-0.33))*G/WW
  WKJ=WSJ**2/G
!
   DO J=1,JL
      THETA0=THET(J)
      COSTH=COS(THETA0)
      SINTH=SIN(THETA0)
      DO K=1,KL
         WK0=WK(K)
         WF0=WF(K,IA,IC)
         WS0=ZPI*WF0
         WL=VX*COSTH+VY*SINTH
!  
        IF (WL>0) THEN
           IF (WS0<=WSJ) THEN
              SIGMA=0.07
           ELSE
              SIGMA=0.09
           ENDIF
           ALPHA=ARLFA/WK0**4*EXP(-1.25*(WKJ/WK0)**2)            &
             *GAMA**(EXP(-0.5*((1.-WS0/WSJ)/SIGMA)**2))          &
             *(WL/WW)**2
        ELSE
           ALPHA=0.0
        END IF
!  
        EE(K,J,IA,IC)=ALPHA
      END DO !500
   END DO
  END DO
  END DO
!
  RETURN
  END
