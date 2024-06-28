 SUBROUTINE INTERNAL_MODE(IINT)
 USE LIMS
 USE CONTROL
 USE PARALLEL_MOD
 USE CONST, ONLY : ZERO,SMOTH
 USE ALL_VAR!, !ONLY : TPS,DZ,DT,U,UTB,UTF,V,VTF,VTB,EGF,ETF,UF,VF,     &
              !       KM,Q2,Q2L,Q2B,Q2LB,T,TB,S,SB,UB,VB,TCLIM,SCLIM,  &
              !       WSSURF,SSURF,WTSURF,TSURF,RHO
 IMPLICIT NONE
 INTEGER IINT
!---------------------------------------------------------------------
!      ADJUST U(Z) AND V(Z) SUCH THAT
!      VERTICAL AVERAGE OF (U,V) = (UA,VA)
!---------------------------------------------------------------------
 TPS = ZERO
! LOOP 300
! DO 300 J=1,JM
! DO 300 I=1,IM
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
    DO K=1,KBM1
    TPS(I,J)=TPS(I,J)+U(I,J,K)*DZ(K)
    END DO
 END DO
 END DO
! LOOP 302 	
! DO 302 J=1,JM
! DO 302 I=2,IM
 DO J=ISLAT,IELAT
 DO I=JSLON,IELON
 DO K=1,KBM1
   U(I,J,K)=(U(I,J,K)-TPS(I,J))+(UTB(I,J)+UTF(I,J))/(DT(I,J)+DT(I-1,J))
 END DO
 END DO
 END DO
!  
 TPS = ZERO
 DO K=1,KBM1
! LOOP 304 
! DO 304 J=1,JM
! DO 304 I=1,IM
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
    TPS(I,J)=TPS(I,J)+V(I,J,K)*DZ(K)
 END DO
 END DO
 END DO

! LOOP 306 	
! DO 306 J=2,JM
! DO 306 I=1,IM
 DO J=JSLAT,IELAT
 DO I=ISLON,IELON
   DO K=1,KBM1
   V(I,J,K)=(V(I,J,K)-TPS(I,J))+(VTB(I,J)+VTF(I,J))/(DT(I,J)+DT(I,J-1))
   END DO
 END DO
 END DO
!*************************************************
 CALL COMM_3D(U,2,2)
 CALL COMM_3D(V,2,2)
 CALL COMM_2D(EGF,1,1)
 CALL COMM_2D(ETF,1,1)
!*************************************************
!----------------------------------------------------------------
!  VERTVL INPUT = U,V,DT(=H+ET),ETF,ETB; OUTPUT = W
!  VERTVL CALCULATES W FROM U, V, DT (H+ET), ETF AND ETB:
!----------------------------------------------------------------
 CALL VERTVL
 CALL BCOND(5)
!
!
 UF = ZERO
 VF = ZERO
!----------------------------------------------------------------
!  COMPUTE Q2F AND Q2LF USING UF AND VF AS TEMPORARY VARIABLES
!  CALCULATE Q2F AND Q2LF USING UF, VF, A AND C AS TEMPORARY VARIABLES
!----------------------------------------------------------------
 CALL ADVQ(Q2B,Q2,UF)
 CALL ADVQ(Q2LB,Q2L,VF)
 IF(IINT/=1.and.BVTYPE)THEN
 write(6,*) 'BVTYPE:',BVTYPE
 DO K=1,KB
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
 KM(I,J,K)=KM(I,J,K)-BV(I,J,K)*FSM(I,J)
 KH(I,J,K)=KH(I,J,K)-BV(I,J,K)*FSM(I,J)
 END DO
 END DO
 END DO
 END IF

 CALL PROFQ(DTI2)
!----------------------------------------------------------------
! COUPLED WITH WAVE MODEL
!----------------------------------------------------------------
IF(BVTYPE)THEN
 write(6,*) 'BVTYPE:',BVTYPE
 DO K=1,KB
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
 KM(I,J,K)=KM(I,J,K)+BV(I,J,K)*FSM(I,J)
 KH(I,J,K)=KH(I,J,K)+BV(I,J,K)*FSM(I,J)
 END DO
 END DO
 END DO
END IF
!*************************************************
! CALL UPDATE_3D(W,1,1)
 CALL COMM_3D(KM,1,1)
!*************************************************
 CALL BCOND(6)
! LOOP 310
! DO 310 J=1,JM
! DO 310 I=1,IM
 DO K=1,KB
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
 Q2(I,J,K)=Q2(I,J,K)+.5*SMOTH*(UF(I,J,K)+Q2B(I,J,K)-2.*Q2(I,J,K))
 Q2L(I,J,K)=Q2L(I,J,K)+.5*SMOTH*(VF(I,J,K)+Q2LB(I,J,K)-2.*Q2L(I,J,K))
 Q2B(I,J,K)=Q2(I,J,K)
 Q2(I,J,K)=UF(I,J,K)
 Q2LB(I,J,K)=Q2L(I,J,K)
 Q2L(I,J,K)=VF(I,J,K)
 END DO
 END DO
 END DO
!----------------------------------------------------------------
! COMPUTE TF AND SF USING UF AND VF AS TEMPORARY VARIABLES
! CALCULATE TF AND SF USING UF, VF, A AND C AS TEMPORARY VARIABLES
!----------------------------------------------------------------
!  
 IF(MODE/=4)THEN
   CALL ADVT(SB,S,SCLIM,VF)
   CALL ADVT(TB,T,TCLIM,UF)
   CALL PROFT(VF,WSSURF,SSURF,1,DTI2)
   CALL PROFT(UF,WTSURF,TSURF,2,DTI2) !edited by zhaobiao
   CALL BCOND(4)
  ! LOOP 355	
  ! DO 355 J=1,JM
  ! DO 355 I=1,IM
   DO K=1,KB
   DO J=ISLAT,IELAT
   DO I=ISLON,IELON
   T(I,J,K)=T(I,J,K)+.5*SMOTH*(UF(I,J,K)+TB(I,J,K)-2.*T(I,J,K))
   S(I,J,K)=S(I,J,K)+.5*SMOTH*(VF(I,J,K)+SB(I,J,K)-2.*S(I,J,K))
   TB(I,J,K)=T(I,J,K)
   T(I,J,K)=UF(I,J,K)
   SB(I,J,K)=S(I,J,K)
   S(I,J,K)=VF(I,J,K)
   END DO
   END DO
   END DO
   CALL DENS(S,T,RHO,DT)
 END IF
! 360 CONTINUE
!
!----------------------------------------------------------------
! COMPUTE UF AND VF
!----------------------------------------------------------------
 CALL ADVU
 CALL ADVV

 CALL PROFU(DTI2)
 CALL PROFV(DTI2)
 CALL BCOND(3)
 TPS = ZERO
! LOOP 370 
! DO 370 J=1,JM
! DO 370 I=1,IM
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
 DO K=1,KBM1
 TPS(I,J)=TPS(I,J)+(UF(I,J,K)+UB(I,J,K)-2.E0*U(I,J,K))*DZ(K)
 END DO
 END DO
 END DO
! LOOP 372 	
 DO K=1,KBM1
! DO 372 J=1,JM
! DO 372 I=1,IM
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
 U(I,J,K)=U(I,J,K)+.5*SMOTH*(UF(I,J,K)+UB(I,J,K)-2.E0*U(I,J,K)-TPS(I,J))
 END DO
 END DO
 END DO

 TPS = ZERO
! LOOP 374 	
! DO 374 J=1,JM
! DO 374 I=1,IM
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
 DO K=1,KBM1
 TPS(I,J)=TPS(I,J)+(VF(I,J,K)+VB(I,J,K)-2.E0*V(I,J,K))*DZ(K)
 END DO
 END DO
 END DO
! LOOP 376
 DO K=1,KBM1
! DO 376 J=1,JM
! DO 376 I=1,IM
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
 V(I,J,K)=V(I,J,K)+.5*SMOTH*(VF(I,J,K)+VB(I,J,K)-2.E0*V(I,J,K)-TPS(I,J))
 END DO
 END DO
 END DO
! LOOP 377
! DO 377 J=1,JM 
! DO 377 I=1,IM  	
 DO K=1,KB
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
 UB(I,J,K)=U(I,J,K)
 U(I,J,K)=UF(I,J,K)
 VB(I,J,K)=V(I,J,K)
 V(I,J,K)=VF(I,J,K)
 END DO
 END DO
 END DO
!
 RETURN
 END
