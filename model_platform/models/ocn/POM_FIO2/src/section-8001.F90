 SUBROUTINE SECTION_8001
 USE LIMS
 USE CONTROL
 USE CONST, ONLY : ZERO,HORCON
 USE ALL_VAR, ONLY : AAM,DX,DY,U,V,ADX2D,ADY2D,DRX2D,DRY2D,         &
                     AAM2D,ADVX,ADVY,AAM,DZ,DRHOX,DRHOY,ADVUA,ADVVA
 IMPLICIT NONE 
 CALL ADVCT
 CALL BAROPG
!**********************************************************************
! HOR VISC = HORCON*DX*DY*SQRT((DU/DX)**2+(DV/DY)**2
!                               +.5*(DU/DY+DV/DX)**2)
!**********************************************************************
!  IF MODE.EQ.2 THEN INITIAL VALUES OF AAM2D ARE USED. IF ONE WISHES 
!  SMAGORINSKY LATERAL VISCOSITY AND DIFFUSION FOR AN EXTERNAL MODE 
!  CALCULATION, THEN APPROPIATE CODE CAN BE ADAPTED FROM THAT BELOW
!  AND INSTALLED AFTER S.N 102 AND BEFORE S.N. 5000 IN SUBROUTINE ADVAVE.
 DO K=1,KBM1
!   DO J=2,JMM1
!   DO I=2,IMM1
 DO J=NSLAT,NELAT
 DO I=NSLON,NELON
 AAM(I,J,K)=HORCON*DX(I,J)*DY(I,J)                                  &
           *SQRT( ((U(I+1,J,K)-U(I,J,K))/DX(I,J))**2                &
                 +((V(I,J+1,K)-V(I,J,K))/DY(I,J))**2                &
 +.5E0*(.25E0*(U(I,J+1,K)+U(I+1,J+1,K)-U(I,J-1,K)-U(I+1,J-1,K))     &
              /DY(I,J)                                              &
 +.25E0*(V(I+1,J,K)+V(I+1,J+1,K)-V(I-1,J,K)-V(I-1,J+1,K))           &
              /DX(I,J)) **2)
 END DO
 END DO
 END DO
!FORM VERTICAL AVERAGES OF 3-D FIELDS FOR USE IN EXTERNAL MODE --
 ADX2D = ZERO
 ADY2D = ZERO
 DRX2D = ZERO
 DRY2D = ZERO
 AAM2D = ZERO
! LOOP 100
!DO 100 J=1,JM
!DO 100 I=1,IM
 DO J=ISLAT-1,IELAT+1
 DO I=ISLON-1,IELON+1
 DO K=1,KBM1
 ADX2D(I,J)=ADX2D(I,J)+ADVX(I,J,K)*DZ(K)
 ADY2D(I,J)=ADY2D(I,J)+ADVY(I,J,K)*DZ(K)
 DRX2D(I,J)=DRX2D(I,J)+DRHOX(I,J,K)*DZ(K)
 DRY2D(I,J)=DRY2D(I,J)+DRHOY(I,J,K)*DZ(K)
 AAM2D(I,J)=AAM2D(I,J)+AAM(I,J,K)*DZ(K)
 END DO
 END DO
 END DO
! ----------------------------------------------------------------------
! OUTPUT WUBOT WVBOT CURV2D
 CALL ADVAVE
! LOOP 87
!DO 87 J=1,JM
!DO 87 I=1,IM
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
 ADX2D(I,J)=ADX2D(I,J)-ADVUA(I,J)
 ADY2D(I,J)=ADY2D(I,J)-ADVVA(I,J)
 END DO
 END DO
 RETURN
 END 
