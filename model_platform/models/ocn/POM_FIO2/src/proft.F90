 SUBROUTINE PROFT(F,WFSURF,FSURF,NBC,DT2)
! **********************************************************************
! *                                                                    *
! * FUNCTION    :  Solves for vertical diffusion of temperature and    *
! *                salinity using method described by Richmeyer and    *
! *                Morton.                                             *
! *                                                                    *
! *                Irradiance parameters are from Paulson and Simpson. *
! *                                                                    *
! *                See:                                                *
! *                                                                    *
! *                Richtmeyer R.D., and K.W. Morton, 1967. Difference  *
! *                  Methods for Initial-Value Problems, 2nd edition,  *
! *                  Interscience, New York, 198-201.                  *
! *                                                                    *
! *                Paulson, C. A., and J. Simpson, 1977: Irradiance    *
! *                  measurements in the upper ocean, J. Phys.         *
! *                  Oceanogr., 7, 952-956.                            *
! *                                                                    *
! *                NOTES:                                              *
! *                                                                    *
! *                (1) wfsurf and swrad are negative values when water *
! *                    column is warming or salt is being added.       *
! *                                                                    *
! *                (2) nbc may only be 1 and 3 for salinity.           *
! *                                                                    *
! **********************************************************************
!
 USE DATA_KIND_MOD
 USE LIMS
 USE CONTROL
 USE CONST, ONLY : LE,ZERO,UMOL
 USE ALL_VAR, ONLY : A,C,KH,EE,GG,SWRAD,TPS,ETF,H,DZ,DZZ,Z
 IMPLICIT NONE
 REAL(kind_r4),DIMENSION(ISLON-LE:IELON+LE,ISLAT-LE:IELAT+LE,KB) :: F
 REAL(kind_r4),DIMENSION(ISLON-LE:IELON+LE,ISLAT-LE:IELAT+LE) :: WFSURF,FSURF
 REAL(kind_r4) DT2
 INTEGER(kind_in) NBC
! LOCAL
 REAL(kind_r4),DIMENSION(ISLON-LE:IELON+LE,ISLAT-LE:IELAT+LE,KB) :: RAD
 REAL(kind_r4),DIMENSION(5) :: R,AD1,AD2 
 REAL(kind_r4) UMOLPR
 INTEGER(kind_in) NTP,KI
! DH >>> TPS
! EQUIVALENCE (TPS,DH)
!
! Irradiance parameters after Paulson and Simpson, JPO, 1977, 952-956.
!
 NTP=2
!NTP         =     1      2       3       4       5
!JERLOV TYPE     =     I      IA      IB      II      III
 DATA R   /        .58 ,   .62  ,  .67  ,  .77  ,  .78   /
 DATA AD1 /        .35 ,   .60  ,  1.0  ,  1.5  ,  1.4   /
 DATA AD2 /        23. ,   20.  ,  17.  ,  14.  ,  7.9   /
!
 UMOLPR=UMOL
!***********************************************************************
!                                                                      *
! THE FOLLOWING SECTION SOLVES THE EQUATION                     *
!  DTI2*(KH*F')'-F=-FB                                          *
!                                                                      *
!***********************************************************************
! LOOP 10
! DO 10 J=1,JM
! DO 10 I=1,IM
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
 TPS(I,J)=H(I,J)+ETF(I,J)
 END DO
 END DO
 C=ZERO;A=ZERO
! LOOP 20
! DO 20 J=1,JM
! DO 20 I=1,IM
 DO K=2,KBM1
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
 A(I,J,K-1)=-DT2*(KH(I,J,K)+UMOLPR)/(DZ(K-1)*DZZ(K-1)*TPS(I,J)         &
      *TPS(I,J))
 C(I,J,K)=-DT2*(KH(I,J,K)+UMOLPR)/(DZ(K)*DZZ(K-1)*TPS(I,J)             &
      *TPS(I,J))
 END DO
 END DO
 END DO
!-----------------------------------------------------------------------
!   NBC=1: SURF. B.C. IS WFSURF. NO SW RADIATIVE PENETRATION.
!   NBC=2; SURF. B.C. IS WFSURF. SWRAD PENETRATES WATER COLUMN
!   NBC=3; SURF. B.C. IS TSURF. NO SW RADIATIVE PENETRATION.
!   NBC=4; SURF. B.C. IS TSURF. SWRAD PENETRATES WATER COLUMN
!
! NOTE THAT WTSURF (=WFSURF) AND SWRAD ARE NEG. VALUES WHEN WATER COLUMN IS
!            WARMING.
!-----------------------------------------------------------------------
!------------------------------------------------------------------
!     Penetrative Radiation Calculation. At the bottom any 
!     unattenuated radiation is deposited in the bottom layer.
!------------------------------------------------------------------
! LOOP 512
! DO 512 J=1,JM
! DO 512 I=1,IM
 RAD = ZERO

 IF(NBC==2 .OR. NBC==4)THEN   
   ! LOOP 511
   ! DO 511 J=1,JM
   ! DO 511 I=1,IM
   DO K=1,KBM1
   DO J=ISLAT,IELAT
   DO I=ISLON,IELON
   RAD(I,J,K)=SWRAD(I,J)                                              &
      *(   R(NTP)*EXP(Z(K)*TPS(I,J)/AD1(NTP))                          &
              +(1.-R(NTP))*EXP(Z(K)*TPS(I,J)/AD2(NTP))   )
   END DO
   END DO
   END DO
 ENDIF
 
 SELECT CASE(NBC)
!+++++++++++++++++++++++++++++++++++
 CASE(1) 
   ! LOOP 500
   ! DO 500 J=1,JM
   ! DO 500 I=1,IM
   DO J=ISLAT,IELAT
   DO I=ISLON,IELON
     EE(I,J,1)=A(I,J,1)/(A(I,J,1)-1.0)
     GG(I,J,1)=-DT2*WFSURF(I,J)/(-DZ(1)*TPS(I,J))-F(I,J,1)
     GG(I,J,1)=GG(I,J,1)/(A(I,J,1)-1.0)
   END DO
   END DO
!+++++++++++++++++++++++++++++++++++
 CASE(2)
  ! LOOP 510                        
  ! DO 510 J=1,JM                    
  ! DO 510 I=1,IM                    
  DO J=ISLAT,IELAT              
  DO I=ISLON,IELON              
    EE(I,J,1)=A(I,J,1)/(A(I,J,1)-1.0) 
    GG(I,J,1)=DT2*(WFSURF(I,J)                                        &  
          +RAD(I,J,1)-RAD(I,J,2))                                     &
         /(DZ(1)*TPS(I,J))-F(I,J,1)    
    GG(I,J,1)=GG(I,J,1)/(A(I,J,1)-1.0)
  END DO
  END DO
!+++++++++++++++++++++++++++++++++++
 CASE(3)
  ! LOOP 520
  ! DO 520 J=1,JM
  ! DO 520 I=1,IM
  DO J=ISLAT,IELAT
  DO I=ISLON,IELON
  EE(I,J,1)=ZERO
  GG(I,J,1)=FSURF(I,J)
  END DO
  END DO
!+++++++++++++++++++++++++++++++++++
 CASE(4)
  ! LOOP 520
  ! DO 520 J=1,JM
  ! DO 520 I=1,IM
  DO J=ISLAT,IELAT
  DO I=ISLON,IELON
  EE(I,J,1)=ZERO
  GG(I,J,1)=FSURF(I,J)
  END DO
  END DO
!+++++++++++++++++++++++++++++++++++
 END SELECT
 
!----------------------------------------------------------------------
! LOOP 101
! DO 101 J=1,JM
! DO 101 I=1,IM
 DO K=2,KBM2
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
 GG(I,J,K)=1./(A(I,J,K)+C(I,J,K)*(1.-EE(I,J,K-1))-1.)
 EE(I,J,K)=A(I,J,K)*GG(I,J,K)
 GG(I,J,K)=(C(I,J,K)*GG(I,J,K-1)-F(I,J,K)                             &
      +DT2*(RAD(I,J,K)-RAD(I,J,K+1))/(TPS(I,J)*DZ(K)))*GG(I,J,K)
 END DO
 END DO
 END DO
!-----  BOTTOM ADIABATIC B.C. ------------------------------------------
! LOOP 102
! DO 102 J=1,JM
! DO 102 I=1,IM
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
 F(I,J,KBM1)=((C(I,J,KBM1)*GG(I,J,KBM2)-F(I,J,KBM1)                   &
         +DT2*(RAD(I,J,KBM1)-RAD(I,J,KB))/(TPS(I,J)*DZ(KBM1)))         &
           /(C(I,J,KBM1)*(1.-EE(I,J,KBM2))-1.))
 END DO
 END DO
!- ---------------------------------------------------------------------
! LOOP 105
! DO 105 J=1,JM
! DO 105 I=1,IM
 DO K=2,KBM1
   KI=KB-K
   DO J=ISLAT,IELAT
   DO I=ISLON,IELON
   F(I,J,KI)=(EE(I,J,KI)*F(I,J,KI+1)+GG(I,J,KI))
   END DO
   END DO
 END DO
!
 RETURN
 END
