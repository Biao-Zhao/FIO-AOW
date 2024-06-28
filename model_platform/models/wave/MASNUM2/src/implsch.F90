 SUBROUTINE IMPLSCH
 USE data_kind_mod_wave
 USE CONTROL_WAVE, ONLY : ISLON,IELON,ISLAT,IELAT,DELTTM,DomainFig,OBCTYPE
 USE CONST_WAVE, ONLY : ALOG10PWK,GC2,CKSP,KL,KLP1,JL,ZPI,BETA10,          &
                   D1,D2,ADS,SBO,ABO,ACU,ZERO,CKSA,BETA1,G,RHOW
 USE ALL_VAR_WAVE, ONLY : NSP,WX,WY,WK,D,WP,WM,IKP,IKP1,IKM,IKM1,WKH,      &
                     JP1,JP2,JM1,JM2,E,AL31,AL11,AL21,AL13,AL23,W,    &
                     THET,WF,UXX,UXY,UYX,UYY,CCG,CONG,GROLIM,WKS17,   &
                     WINC,EE,UST,DWK,TAUINX,TAUINY,TAUDSX,TAUDSY,     &
                     SE,DSE,SEIN,SEDISS,EHS,ETP,ETH,WHS,WTP,WTH,NHS,NTP,NTH,SHS,STP,STH, &
                     RFE,RFW,RFN,RFS
 USE wamspc2d_mod 
 IMPLICIT NONE
 INTEGER(kind_in) :: IA,IC,KS1,KSP1,KPMT,KAKT,KH,KS,K
 INTEGER(kind_in) :: IP,IP1,IM,IM1,KP,KP1,KP2,KP3,MR
 INTEGER(kind_in) :: J,JS,J11,J12,J21,J22,I
 INTEGER(kind_in),DIMENSION(ISLON:IELON,ISLAT:IELAT) :: KS0,KPMT0,KAKT0 
 REAL(kind_r4) :: AWKSS,VX,VY,WW,WKPM,WKPMT,WAKT,DELTT,DELTT5,XX,CWKS17
 REAL(kind_r4) :: WP11,WP12,WP21,WP22,WM11,WM12,WM21,WM22,WP112,WP122
 REAL(kind_r4) :: WP212,WP222,WM112,WM122,WM212,WM222,FFACP,FFACP1
 REAL(kind_r4) :: EIJ,EA1,EA2,EA3,EA4,EA5,EA6,EA7,EA8,UP,UP1,UM,UM1,SAP,SAM 
 REAL(kind_r4) :: EIJ2,ZUA,EAD1,EAD2,FCEN,AD,ADP,ADM,DELAD,DELADP,DELADM
 REAL(kind_r4) :: CD,THETA0,COSTH,SINTH,WL,WLSTAR,WK0,WF0,WS0,BETT,BETA,DWKK 
 REAL(kind_r4) :: AWFSS,ASISS,ARKSS,AESS,EKS,EKSPM,SDS,SSDS,D0,DK,SSBO
 REAL(kind_r4) :: DUXDX0,DUXDY0,DUYDX0,DUYDY0,TH0,COST2,SINT2,CG,CP,CGDC
 REAL(kind_r4) :: CU1,CU2,CU3,WSTAR,SSCU,DELTEE,GADIAG,EEF,EEFAB,SIG
 REAL(kind_r4),DIMENSION(ISLON:IELON,ISLAT:IELAT) :: AE,ASI,AWF,AWK,ARK,ENH 
 REAL(kind_rn),DIMENSION(KL,ISLON:IELON,ISLAT:IELAT) :: FCONST0
 real,parameter :: eps=1.0e-10 
TAUINX = ZERO
TAUINY = ZERO
TAUDSX = ZERO
TAUDSY = ZERO
! ALOG10PWK=ALOG10(PWK)
 CALL MEAN2(AE,ASI,AWF,AWK,ARK)
!
 DO IC=ISLAT,IELAT
 DO IA=ISLON,IELON
    IF(NSP(IA,IC)/=1)CYCLE
    AWKSS=AWK(IA,IC)
    VX=WX(IA,IC)
    VY=WY(IA,IC)
    WW=VX**2+VY**2
    IF(WW<=ZERO)WW=0.05
    WKPM=GC2/WW
    WKPMT=CKSP*WKPM
    KPMT=ALOG10(WKPMT/WK(1))/ALOG10PWK+1
    WAKT=CKSA*AWKSS
    KAKT=ALOG10(WAKT/WK(1))/ALOG10PWK+1
    KS1=MAX0(KPMT,KAKT)
    KS=MIN0(KS1,KL)
    KSP1=KS+1
    KS0(IA,IC)=KS
!   >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!    PROGRAM NOT USED FOR EVER, ADDED BY Guanso.
    KPMT0(IA,IC)=KPMT
    KAKT0(IA,IC)=KAKT
!   >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!    DO 103 K=1,KS
!    FCONST0(K,IA,IC)=1.0
!   103 CONTINUE
!    DO 104 K=KSP1,KL
!    FCONST0(K,IA,IC)=0.0
!   104 CONTINUE
    FCONST0(1:KS,IA,IC)=1.0
    FCONST0(KSP1:KL,IA,IC)=0.0
 END DO
 END DO
!
 DELTT=DELTTM*60.
 DELTT5=DELTT*0.5
!
 DO IC=ISLAT,IELAT
 DO IA=ISLON,IELON
!
    IF(NSP(IA,IC)/=1)CYCLE
    SE=ZERO;DSE=ZERO
   !
   !*************************************
   !	CALL SNONLIN(E)
   !*************************************
   !
    XX=0.75*D(IA,IC)*AWK(IA,IC)
    IF (XX<0.5)XX=0.5
    ENH(IA,IC)=1.+(5.5/XX)*(1.-0.833*XX)*EXP(-1.25*XX)

    KH=0
    DO K=1,KL ! 1001
       KS=K
       WP11=WP(K,1,1)
       WP12=WP(K,1,2)
       WP21=WP(K,2,1)
       WP22=WP(K,2,2)
       WM11=WM(K,1,1)
       WM12=WM(K,1,2)
       WM21=WM(K,2,1)
       WM22=WM(K,2,2)
!      
       WP112=WP11**2
       WP122=WP12**2
       WP212=WP21**2
       WP222=WP22**2
       WM112=WM11**2
       WM122=WM12**2
       WM212=WM21**2
       WM222=WM22**2
!      
       FFACP=1.
       FFACP1=1.
       IP=IKP(K)
       IP1=IKP1(K)
       IM=IKM(K)
       IM1=IKM1(K)
       CWKS17=CONG*WKS17(K)
       KP=IP
       KP1=IP1
       KP2=KP
       KP3=KP1
       IF(KP>=KL)THEN
       KH=KH+1
       KP2=KL+1
       IF(KP==KL)KP2=KL
       KP=KL
       KP1=KL
       KP3=KL+1
       FFACP=WKH(KH)
       FFACP1=WKH(KH+1)
       END IF 
       DO MR=1,2 !1101
   !   
   !   *1.2 ANGULAR LOOP
   !   
   !   1200 CONTINUE
       DO J=1,JL ! 1201
          JS=J
          J11=JP1(MR,J)
          J12=JP2(MR,J)
          J21=JM1(MR,J)
          J22=JM2(MR,J)
   !      
   !      ****************************************************************
   !      
   !       DO 1202 IA=IX1,IX2
   !       DO 1202 IC=IY1,IY2
   !      
          EIJ=E(KS,JS,IA,IC)
          IF (EIJ<1.E-20)CYCLE
          EA1=E(KP ,J11,IA,IC)
          EA2=E(KP ,J12,IA,IC)
          EA3=E(KP1,J11,IA,IC)
          EA4=E(KP1,J12,IA,IC)
          EA5=E(IM ,J21,IA,IC)
          EA6=E(IM ,J22,IA,IC)
          EA7=E(IM1,J21,IA,IC)
          EA8=E(IM1,J22,IA,IC)
          UP =(WP11*EA1+WP12*EA2)*FFACP
          UP1=(WP21*EA3+WP22*EA4)*FFACP1
          UM =WM11*EA5+WM12*EA6
          UM1=WM21*EA7+WM22*EA8
          SAP=UP+UP1
          SAM=UM+UM1
          EIJ2=EIJ**2
          ZUA=2.*EIJ/AL31
          EAD1=SAP/AL11+SAM/AL21
          EAD2=-2.*SAP*SAM/AL31
   !      FCEN=FCNSS(K,IA,IC)*ENH(IA,IC)
          FCEN=FCONST0(K,IA,IC)*ENH(IA,IC)
          AD=CWKS17*(EIJ2*EAD1+EAD2*EIJ)*FCEN
          ADP=AD/AL13
          ADM=AD/AL23
          DELAD =CWKS17*(EIJ*2.*EAD1+EAD2) *FCEN
          DELADP=CWKS17*(EIJ2/AL11-ZUA*SAM)*FCEN/AL13
          DELADM=CWKS17*(EIJ2/AL21-ZUA*SAP)*FCEN/AL23
   !      
   !      *NONLINEAR TRANSFER
   !      
          SE(KS ,JS )= SE(KS ,JS )-2.0*AD
          SE(KP2,J11)= SE(KP2,J11)+ADP*WP11
          SE(KP2,J12)= SE(KP2,J12)+ADP*WP12
          SE(KP3,J11)= SE(KP3,J11)+ADP*WP21
          SE(KP3,J12)= SE(KP3,J12)+ADP*WP22
          SE(IM ,J21)= SE(IM ,J21)+ADM*WM11
          SE(IM ,J22)= SE(IM ,J22)+ADM*WM12
          SE(IM1,J21)= SE(IM1,J21)+ADM*WM21
          SE(IM1,J22)= SE(IM1,J22)+ADM*WM22
   !      
          DSE(KS ,JS )=DSE(KS ,JS )-2.0*DELAD
          DSE(KP2,J11)=DSE(KP2,J11)+DELADP*WP112
          DSE(KP2,J12)=DSE(KP2,J12)+DELADP*WP122
          DSE(KP3,J11)=DSE(KP3,J11)+DELADP*WP212
          DSE(KP3,J12)=DSE(KP3,J12)+DELADP*WP222
          DSE(IM ,J21)=DSE(IM ,J21)+DELADM*WM112
          DSE(IM ,J22)=DSE(IM ,J22)+DELADM*WM122
          DSE(IM1,J21)=DSE(IM1,J21)+DELADM*WM212
          DSE(IM1,J22)=DSE(IM1,J22)+DELADM*WM222
   !   
       END DO ! 1201 LEAP
       END DO ! 1101 LEAP
    END DO ! 1001 LEAP
  !*************************************
  !	CALL SINPUT(E)
  !*************************************
  !
    KS=KS0(IA,IC)
    VX=WX(IA,IC)
    VY=WY(IA,IC)
    WW=VX**2+VY**2
    W(IA,IC)=SQRT(WW)
    IF(UST(IA,IC).LE.0.0001) THEN
      CD=(0.80+0.065*W(IA,IC))*0.001                                       ! The initial value UST of WRF is 0.0001, it is not right
      IF(W(IA,IC).GE.31) CD=(3.366-0.03263*W(IA,IC))*0.001                 ! zhaobiao based on Makin,2005
      UST(IA,IC)=W(IA,IC)*SQRT(CD)
    ELSE
      CD=(UST(IA,IC)/(W(IA,IC)+eps))**2                                    ! zhaobiao ustar is from atm for coupling
      IF(W(IA,IC).GE.31) CD=(3.366-0.03263*W(IA,IC))*0.001                 ! zhaobiao based on Makin,2005
    END IF
    !
    DO J=1,JL  ! 2200
       THETA0=THET(J)
       COSTH=COS(THETA0)
       SINTH=SIN(THETA0)
       WL=VX*COSTH+VY*SINTH
       WLSTAR=WL*SQRT(CD)
       DO K=1,KS  ! 2500
          WK0=WK(K)
          WF0=WF(K,IA,IC)
          WS0=ZPI*WF0
          BETT=BETA10*(1.+BETA1*WINC(IA,IC))
          BETA=AMAX1(0.,BETT*(WK0*28.*WLSTAR-WS0))
          SEIN(K,J)=BETA*E(K,J,IA,IC)                                           ! zhaobiao,wind input source function
          SE(K,J)= SE(K,J)+BETA*E(K,J,IA,IC)
          DSE(K,J)=DSE(K,J)+BETA   
       END DO ! 2500 LEAP
    END DO ! 2200 LEAP
!   ***************************************************
!   	GOTO 3009
!   	IF (LOGSDISS.EQ.1) GOTO 3009
!   	CALL SDISSIP(E)--OLD
!   ***************************************************
!   3009	CONTINUE
!   	CALL SDISSIP(E)--NEW
!   ***************************************************
!
    KS=KS0(IA,IC)
    AWFSS=AWF(IA,IC)
    ASISS=ASI(IA,IC)
    ARKSS=ARK(IA,IC)
    AESS = AE(IA,IC)
    EKS=AESS*ARKSS*ARKSS
    EKSPM=EKS/0.0030162
!   SDS=2.36E-5*ASISS*ARKSS**3*AESS**2/ALPM2
!   2.36E-5/ALPM2=2.587605807
!   SDS=2.60*(ZPI*AWFSS)*ARKSS**3*AESS**2
    SDS=D1*ASISS/ARKSS*SQRT(EKSPM)*EXP(-D2*0.64/EKSPM)
!
    DO K=1,KS ! 3505
       WK0=WK(K)
       DO J=1,JL ! 3202
          SSDS=-ADS*SDS*WK0
          SEDISS(K,J)=SSDS*E(K,J,IA,IC)                                          !zhaobiao,dissipation source fucntion
          SE(K,J)= SE(K,J)+SSDS*E(K,J,IA,IC)
          DSE(K,J)=DSE(K,J)+SSDS
       END DO ! 3202 LEAP
    END DO ! 3505 LEAP
!
!  *************************************
!  	CALL SBOTTOM(E)
!  *************************************
!  
!    SBO=0.038*2./G 
    KS=KS0(IA,IC)
    D0=D(IA,IC)
    
    DO K=1,KS   ! 4500
       WK0=WK(K)
       DK=D0*WK0
       DO J=1,JL   ! 4200
          SSBO=0.
          IF(DK<30.)THEN
          SSBO=-ABO*SBO*WK0/SINH(2.*DK)
          ENDIF
          SE(K,J)= SE(K,J)+SSBO*E(K,J,IA,IC)
          DSE(K,J)=DSE(K,J)+SSBO
       END DO !   4200
    END DO !   4500
!   *************************************
!   	CALL SCURRENT(E)
!   *************************************
!   
    DUXDX0=UXX(IA,IC)
    DUXDY0=UXY(IA,IC)
    DUYDX0=UYX(IA,IC)
    DUYDY0=UYY(IA,IC)
!   
    KS=KS0(IA,IC)
    
    DO J=1,JL  ! 5200
       TH0=THET(J)
       SINTH=SIN(TH0)
       COSTH=COS(TH0)
       COST2=COSTH*COSTH
       SINT2=SINTH*SINTH
       DO K=1,KS ! 5500
          WK0=WK(K)
          WS0=ZPI*WF(K,IA,IC)
          CG=CCG(K,IA,IC)
          CP=WS0/WK0
          CGDC=CG/CP
          
          CU1=(CGDC*(1.+COST2)-0.5)*DUXDX0
          CU2= CGDC*SINTH*COSTH*(DUXDY0+DUYDX0)
          CU3=(CGDC*(1.+SINT2)-0.5)*DUYDY0
          SSCU=-ACU*(CU1+CU2+CU3)
          
          SE(K,J)= SE(K,J)+SSCU*E(K,J,IA,IC)
          DSE(K,J)=DSE(K,J)+SSCU
!         
       END DO   !5500
    END DO      !5200
!
!   *************************************
!   	CALL SOURCE TERMS END
!   *************************************
!
    KS=KS0(IA,IC)
    VX=WX(IA,IC)
    VY=WY(IA,IC)
    WW=VX**2+VY**2
    W(IA,IC)=SQRT(WW)
    IF(UST(IA,IC).LE.0.0001) THEN
      CD=(0.80+0.065*W(IA,IC))*0.001
      IF(W(IA,IC).GE.31) CD=(3.366-0.03263*W(IA,IC))*0.001                 ! zhaobiao based on Makin,2005
      UST(IA,IC)=W(IA,IC)*SQRT(CD) 
    ELSE
      CD=(UST(IA,IC)/(W(IA,IC)+eps))**2 
      IF(W(IA,IC).GE.31) CD=(3.366-0.03263*W(IA,IC))*0.001                 ! zhaobiao based on Makin,2005
    END IF
    WSTAR=W(IA,IC)*SQRT(CD)
    !zhaobiao ustar is from atm for coupling 
!
    DO J=1,JL  ! 6001
       THETA0=THET(J)         !zhaobiao
       COSTH=COS(THETA0)      !zhaobiao
       SINTH=SIN(THETA0)      !zhaobiao
    DO K=1,KS  ! 6002
       WK0=WK(K)              !zhaobiao 
       WF0=WF(K,IA,IC)        !zhaobiao
       DWKK=DWK(K)            !zhaobiao, 1/2*k*dk*dtheta
       WS0=ZPI*WF0            !zhaobiao

       DELTEE=WSTAR*GROLIM(K)
       GADIAG=1.-DELTT5*DSE(K,J)
       GADIAG=AMAX1(GADIAG,1.)
       EEF=DELTT*SE(K,J)/GADIAG
       EEFAB=ABS(EEF)
       EEFAB=AMAX1(EEFAB,0.1E-19)
       !--------------------------------------------------------------------------
       IF(EEFAB .LT. DELTEE)THEN
         TAUINX(IA,IC)=TAUINX(IA,IC)+RHOW*G*SEIN(K,J)*WK0*COSTH/WS0*DWKK*2.0       !zhaobiao,x-component wave-induced stress based on Janssen 2012
         TAUINY(IA,IC)=TAUINY(IA,IC)+RHOW*G*SEIN(K,J)*WK0*SINTH/WS0*DWKK*2.0       !zhaobiao,y-component
         TAUDSX(IA,IC)=TAUDSX(IA,IC)+RHOW*G*SEDISS(K,J)*WK0*COSTH/WS0*DWKK*2.0     !zhaobiao,x-component dissipation stress based on Janssen 2012
         TAUDSY(IA,IC)=TAUDSY(IA,IC)+RHOW*G*SEDISS(K,J)*WK0*SINTH/WS0*DWKK*2.0     !zhaobiao,y-component dissipation stress 
       ELSE
       END IF
       !--------------------------------------------------------------------------
       SIG=EEF/EEFAB
       EEFAB=AMIN1(EEFAB,DELTEE)
       EEF=E(K,J,IA,IC)+SIG*EEFAB
       EE(K,J,IA,IC)=AMAX1(EEF,0.)
    END DO ! 6002
    DO K=KS+1,KL ! 6012
       I=K-KS+1
       EE(K,J,IA,IC)=EE(KS,J,IA,IC)*WKH(I)
    END DO !6012
    END DO ! 6001
 END DO
 END DO ! 20000 leap
!
!********************************
!	SET WATER BOUNDARY
!********************************
!  program add by Guanso.
 IF(DomainFig)THEN
  IF(OBCTYPE==1)THEN      !use JONSWAP as the open boudary condition 
    CALL SETSPEC(2)
  ELSE IF(OBCTYPE==2)THEN !use global wave model as open boudary condtion based on Hs,Tp 
    DO IC=ISLAT,IELAT
       IF(RFW)THEN  ! WESTBDY
          IF(NSP(1,IC)==2)     CALL SPECTRA2D(WHS(ic),1./WTP(ic),WTH(ic),1,ic,ee(:,:,1,ic),2)
       ENDIF
       IF(RFE)THEN  ! EASTBDY
          IF(NSP(IELON,IC)==2) CALL SPECTRA2D(EHS(ic),1./ETP(ic),ETH(ic),IELON,ic,ee(:,:,IELON,ic),2)
       ENDIF
    ENDDO
    DO IA=ISLON,IELON
       IF(RFS)THEN  ! SOUTH BDY
          IF(NSP(IA,1)==2)     CALL SPECTRA2D(SHS(ia),1./STP(ia),STH(ia),IA,1,ee(:,:,IA,1),2)
       ENDIF
       IF(RFN)THEN  ! NORTH BDY 
          IF(NSP(IA,IELAT)==2) CALL SPECTRA2D(NHS(ia),1./NTP(ia),NTH(ia),IA,IELAT,ee(:,:,IA,IELAT),2)
       ENDIF
    ENDDO
  ELSE
    WRITE(6,*) "Wrong,OBCTYPE must be 1 or 2"
  ENDIF
 ENDIF
 
 RETURN
 END
