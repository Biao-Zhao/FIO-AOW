 SUBROUTINE SETBC
 USE CONTROL
 USE LIMS
 USE ALL_VAR
 USE CONST
 IMPLICIT NONE

 INTEGER L1,L2,L5
 REAL(kind_r4) W1,W3,RDAY
 REAL(kind_r4) TDB(NUMTIDE)
 

 L1=1;L2=2
 RDAY=((HOUR)*3600+MINS*60+SEC)/86400.
 w3=RDAY
 w1=1.-w3
 !WRITE(6,*),"BC W1= ",W1,L1,"W3= ",W3,L2
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
! set south boundary condition for temperature and salinity 
 IF(RFS)THEN
 DO K=1,KB
 DO I=ISLON,IELON
 TBS(I,K)=(TSOUTH(I,K,L1)*W1+TSOUTH(I,K,L2)*W3)*FSM(I,1)
 SBS(I,K)=(SSOUTH(I,K,L1)*W1+SSOUTH(I,K,L2)*W3)*FSM(I,1)
 UBS(I,K)=(USOUTH(I,K,L1)*W1+USOUTH(I,K,L2)*W3)*DUM(I,1)
 VBS(I,K)=(VSOUTH(I,K,L1)*W1+VSOUTH(I,K,L2)*W3)*DVM(I,1) !added by zhaobiao
 END DO
 END DO
 END IF
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
! set north boundary condition for temperature and salinity 
 IF(RFN)THEN
 DO K=1,KB
 DO I=ISLON,IELON
 TBN(I,K)=(TNORTH(I,K,L1)*W1+TNORTH(I,K,L2)*W3)*FSM(I,JM)
 SBN(I,K)=(SNORTH(I,K,L1)*W1+SNORTH(I,K,L2)*W3)*FSM(I,JM)
 UBN(I,K)=(UNORTH(I,K,L1)*W1+UNORTH(I,K,L2)*W3)*DUM(I,JM) !added by zhaobiao
 VBN(I,K)=(VNORTH(I,K,L1)*W1+VNORTH(I,K,L2)*W3)*DVM(I,JM)
 END DO
 END DO
 END IF
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
! set east boundary condition for temperature and salinity 
 IF(RFE)THEN
 DO K=1,KB
 DO J=ISLAT,IELAT
 TBE(J,K)=(TEAST(J,K,L1)*W1+TEAST(J,K,L2)*W3)*FSM(IM,J)
 SBE(J,K)=(SEAST(J,K,L1)*W1+SEAST(J,K,L2)*W3)*FSM(IM,J)
 UBE(J,K)=(UEAST(J,K,L1)*W1+UEAST(J,K,L2)*W3)*DUM(IM,J) !added by zhaobiao
 VBE(J,K)=(VEAST(J,K,L1)*W1+VEAST(J,K,L2)*W3)*DVM(IM,J)
 END DO
 END DO
 END IF
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
! set west boundary condition for temperature and salinity 
 IF(RFW)THEN
 DO K=1,KB
 DO J=ISLAT,IELAT
 TBW(J,K)=(TWEST(J,K,L1)*W1+TWEST(J,K,L2)*W3)*FSM(1,J)
 SBW(J,K)=(SWEST(J,K,L1)*W1+SWEST(J,K,L2)*W3)*FSM(1,J)
 UBW(J,K)=(UWEST(J,K,L1)*W1+UWEST(J,K,L2)*W3)*DUM(1,J) !added by zhaobiao
 VBW(J,K)=(VWEST(J,K,L1)*W1+VWEST(J,K,L2)*W3)*DVM(1,J)
 END DO
 END DO
 END IF
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>      
!set south boundary condition for SEA LEVEL and V
 IF(RFS)THEN
 DO I=ISLON,IELON
 ELS(I)=(ETSOUTH(I,L1)*W1+ETSOUTH(I,L2)*W3)*FSM(I,1)
 VABS(I)=(VASOUTH(I,L1)*W1+VASOUTH(I,L2)*W3)*DVM(I,1)
! add  tide
 IF(TIDETYPE) then
 DO L5=1,NUMTIDE
 TDB(L5)=AMPS(L5,I)*COSD(TT*24*OMG(L5)-PHAS(L5,I))
 END DO
 ELS(I)=SUM(TDB)+ELS(I)
 VABS(I)=M2UVS(3,I)*COSD(TT*24*OMG(1)-M2UVS(4,I))+VABS(I)
 END IF
 END DO
 END IF
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
!set north boundary condition for SEA LEVEL and V
 IF(RFN)THEN
 DO I=ISLON,IELON
 ELN(I)=(ETNORTH(I,L1)*W1+ETNORTH(I,L2)*W3)*FSM(I,JM)
 VABN(I)=(VANORTH(I,L1)*W1+VANORTH(I,L2)*W3)*DVM(I,JM)
! add tide
 IF(TIDETYPE)then
 DO L5=1,NUMTIDE
 TDB(L5)=AMPN(L5,I)*COSD(TT*24*OMG(L5)-PHAN(L5,I))
 ENDDO
 ELN(I)=SUM(TDB)+ELN(I)
 VABN(I)=M2UVN(3,I)*COSD(TT*24*OMG(1)-M2UVN(4,I))+VABN(I)
 END IF
 END DO
 END IF
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>      
!set east boundary condition for SEA LEVEL and V
 IF(RFE)THEN

 DO J=ISLAT,IELAT
 ELE(J)=(ETEAST(J,L1)*W1+ETEAST(J,L2)*W3)*FSM(IM,J)
 UABE(J)=(UAEAST(J,L1)*W1+UAEAST(J,L2)*W3)*DUM(IM,J)
! add tide
 IF(TIDETYPE) then
 DO L5=1,NUMTIDE
 TDB(L5)=AMPE(L5,J)*COSD(TT*24*OMG(L5)-PHAE(L5,J))
 END DO
 ELE(J)=SUM(TDB)+ELE(J)
 UABE(J)=M2UVE(1,J)*COSD(TT*24*OMG(1)-M2UVE(2,J))+UABE(J)
 END IF
 END DO
 END IF
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>      
!set west boundary condition for SEA LEVEL and V
 IF(RFW)THEN
 DO J=ISLAT,IELAT
 ELW(J)=(ETWEST(J,L1)*W1+ETWEST(J,L2)*W3)*FSM(1,J)
 UABW(J)=(UAWEST(J,L1)*W1+UAWEST(J,L2)*W3)*DUM(1,J)
 END DO
 END IF
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>      
 RETURN
 END
