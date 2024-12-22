MODULE module_sf_seaspray
USE module_wrf_error
  IMPLICIT NONE
  REAL, PARAMETER,PRIVATE :: K=0.4
  REAL, PARAMETER,PRIVATE :: G=9.8
  REAL, PARAMETER,PRIVATE :: TK=273.15
  REAL, PARAMETER,PRIVATE :: PI=3.141593
  REAL, PARAMETER,PRIVATE:: RHO_W=1000.
  REAL, PARAMETER,PRIVATE:: CW=4000.
  REAL, PARAMETER,PRIVATE:: ALPHA=0.0185
  REAL,PRIVATE :: MS, MW, RHOSOL, MASSS, RGAS, DWP, KAP, LV, Y, YTERM1, YTERM2

!===========================================================================================
CONTAINS

SUBROUTINE FLUX_SPRAY(UR,TS,TR,sigwave,QR,P,S,USTAR,Z0,L,ref_ht,TAU_SP,hs_sp,hl_sp,qfx_sp)
!modefied by zhaobiao remove ref_ht,Qs,DEPTH


!  Version 3.1.
!  This subroutine computes the spray-mediated sensible and latent
!  heat fluxes based on my analysis of the HEXOS and FASTEX flux data with the
!  spray model described in Andreas (1992) and Andreas and DeCosmo
!  (2002) and the spray generation function of Fairall et al. (1994).
!  The spray sensible heat flux is predicted by
!
!	HS_SP = rho_w*cw*(Ts - Teq,100)*(2.30e-6*(USTAR**3)).
!
!  The spray latent heat flux is predicted by
!
!	HL_SP = rho_w*Lv*[1 - (r(Tau_f)/50)**3]*(1.10e-7*(USTAR**2.22))
!
!  where
!
!     r(Tau_f) = r0 - (r0 - req,50)[1 - exp(-Tau_f/Tau_r)]
!
!  where Tau_f is the time constant for 50 micrometer droplets to fall
!  back to the surface and Tau_r is the droplet's evaporation time
!  constant.
!
!  The routine also estimates the spray momentum flux as (Andreas and
!  Emanuel, 2001)
!
!	TAU_SP = 6.2E-5*rho_w*(USTAR**4).
!
!  USTAR is the friction velocity in m/s.
!  Z0 is the roughness length in meters.
!  L is the Obukhov length in meters.
!
!
!      REAL L, LV, LATENT
!
!      COMMON /CONSTANTS/ K, G
!      COMMON /CONDITIONS/ UR, RU, TR, RT, RH, RQ, QR, TS, P, S, H13,
!     A DEPTH
!	  UR is the wind speed (in m/s) at height RU (in m).
!	  TR is the temperature (in deg C) at height RT.
!	  RH is the relative humidity (in %) at height RQ.
!	  QR is the computed specific humidity at RQ.
!	  TS is the surface temperature (in deg C).
!	  P is the barometric pressure (in mb).
!	  S is the surface salintiy (in psu).
!	  H13 is significant wave height computed here.
!	  DEPTH is water depth in meters.
!	  K is the von Karman constant.
!	  G is the acceleration of gravity in m/s**2.
!
!      DATA R50/50.0/, R100/100.0/, TK/273.15/, RHO_W/1000.0/,
!     A CW/4000.0/, ALPHA/0.0185/
!         R50 is the key droplet radius for spray latent heat flux,
!         50 micrometers.
!         R100 is the key droplet radius for spray sensible heat flux,
!         100 micrometers.
!         TK is 0 deg C in kelvins.
!         RHO_W is the density of sea water in kg/m**3.
!         CW is the specific heat of seawater in J/kg-degC.
!         ALPHA is the Charnock constant.
!
!
 
IMPLICIT NONE
REAL, INTENT(IN) :: UR,TS,TR,QR,S,USTAR,Z0,L,sigwave,ref_ht !modefied by zhaobiao
REAL :: H13,P  !modefied by zhaobiao 
REAL, INTENT(OUT) :: tau_sp,HS_SP,HL_SP,QFX_SP
REAL :: RH, RU, RT, RQ, R50, R100, qrs
REAL :: S_FRAC,SALT_FAC, F, TINF, R50M, R100M, TEQ_100, TEQ50, REQM, TAU_R, REQ, & 
        US, USF, RU10, PSI, U10, A13, TAU_F, R_TAU_F,SALT_SP
!
R50=50.
R100=100.
RU=10   !modefied by zhaobiao  original is ref_ht
RT=2     !modefied by zhaobiao  original is ref_ht
RQ=2    !modefied by zhaobiao  original is ref_ht
qrs=(1.0007+3.46e-6*(p/100.))*6.1121*exp(17.502*tr/(240.97+tr))*0.98
qrs=0.62197*qrs/((p/100.)-0.378*qrs) !saturation specific humidity
rh=qr/qrs*100.
!
      S_FRAC = S/1000.0                 ! Fractional salinity
      SALT_FAC = S_FRAC/(1.0 - S_FRAC)  ! since latest version
      F = RH/100.0                      ! Fractional relative humidity
      IF(F .GT. 0.995) F = 0.995            ! Limit relative humidity
      IF(F .LT. 0.75) F = 0.75          ! Spray model can't handle lower RH
      TINF = TR + TK                    ! Kelvin temperature
      R100M = 1.0E-6*R100               ! Radius in meters
      R50M = 1.0E-6*R50                 ! Radius in meters
!
      CALL SUB_MOD_KEPERT(R100M,S_FRAC,TINF,F,P,TEQ_100)
!	  This returns Teq,100, the equilibrium temperature of 100-
!	  micrometer droplets.
!
!	  Compute spray heat fluxes.
!      HS_SP = 2.30E-6*RHO_W*CW*(TS - TEQ_100)*(USTAR**3)
      IF(USTAR .LT. 0.1480) THEN
         HS_SP = 3.92E-8*RHO_W*CW*(TS - TEQ_100)
      ELSE
         HS_SP = 5.02E-6*RHO_W*CW*(TS - TEQ_100)*(USTAR**2.54)
      END IF
!	  Spray sensible heat flux in W/m**2.
!
!	  Make calculations necessary to compute spray latent heat flux.
      CALL R_EQUILB(R50M,TEQ50,REQM,TAU_R,TR,RH,S,P)
!	  Returns equilibrium temperature (TEQ50), equilibrium radius
!	  (REQM, in meters), and evaporation time constant (TAU_R) for
!	  a droplet that started at 50 micrometers.
!
      REQ = 1.0E6*REQM                  ! In micrometers
      IF(REQ .GT. 50.0) REQ=50.0        ! modified by zhaobiao
!
      CALL LATENT(TEQ50,LV)                ! For 50 micron droplet
!
!	  Compute terminal fall speed of 50 micrometer droplets.
          CALL FALSPD(R50M,TR,S_FRAC,P,US,USF)
!	  USF is now terminal fall speed.
!
!	  Need U10 to estimate significant wave height.
      RU10 = 10.0                       ! 10 meter reference height
      call PSI_M(RU10,L,PSI)               ! Stability correction
      U10 = (USTAR/K)*(LOG(RU10/Z0) - PSI)
!
!	  Compute significant wave amplitude.
      !write(100,*) 'here1',sigwave
!      call SIG_WAVE(U10,3000.0,H13)! Find significant wave height.
      !write(100,*) 'here2',H13
        H13=sigwave
        IF(H13 .GT. 40.0) H13 = 40.0    ! Limit H13 to 40 meters
        A13 = H13/2.0                   ! Significant wave amplitude
!
      TAU_F = A13/USF! Time constant for fall to sea
!
!	  Compute radius when droplet returns to the sea.
      R_TAU_F = R50 - (R50 -REQ)*(1 - EXP(-(TAU_F/TAU_R)))! In micrometers
!	  Compute spray latent heat flux.
!      HL_SP = 1.10E-7*RHO_W*LV*(1 - ((R_TAU_F/R50)**3))*(USTAR**2.22)
     IF(USTAR .LT. 0.1358) THEN
        HL_SP  = 1.76E-9*RHO_W*LV*(1 - ((R_TAU_F/R50)**3))
        !QFX_SP = 1.76E-9*RHO_W*(1 - ((R_TAU_F/R50)**3))
     ELSE
        HL_SP  = 2.08E-7*RHO_W*LV*(1 - ((R_TAU_F/R50)**3))*(USTAR**2.39)
        !QFX_SP = 2.08E-7*RHO_W*(1 - ((R_TAU_F/R50)**3))*(USTAR**2.39)
     END IF 

!         compute  Spray salt flux in kg/m**2/s.
     IF(USTAR .LT. 0.1395) THEN
        SALT_SP = 1.53E-9*SALT_FAC*RHO_W*(1 - ((R_TAU_F/R50)**3))
     ELSE
        SALT_SP = 1.73E-7*SALT_FAC*RHO_W*(1 - ((R_TAU_F/R50)**3))*(USTAR**2.40)
     END IF
!         compute water flux from ocean from evaporating spray in kg/m**2/s.  
        QFX_SP=SALT_SP/SALT_FAC 
!
!	  Compute spray momentum flux.
      TAU_SP = 6.2E-5*RHO_W*(USTAR**4)
!	  Spray momentum flux in N/m**2.
!
      RETURN
      END SUBROUTINE FLUX_SPRAY 
!
!
SUBROUTINE SUB_MOD_KEPERT(R,S,TINF,F,P,T_EST)
!  Last modification 2/4/04.
!  This subroutine implements my modification of Jeff Kepert's (1996)
!  algorithm for estimating the temperature at which sea spray
!  droplets evaporate.
!  Kepert evaluated temperature derivatives of saturation vapor pressure
!  from the Clausius-Clapeyron equation.  I do a straightforward
!  differentiation of Buck's (1981) equation.
!  R is the droplet radius in meters.
!  S is the fractional salinity.
!  TINF is the air temperature in kelvins.
!  F is the fractional relative humidity.
!  P is barometric pressure.
!  T_EST is the estimated evaporating temperature.
!
!
!
!      REAL MW, MS, MASSS, KAP, LV, MY_ALPHA
!	  MW is the molecular weight of water.
!	  MS is the molecular weight of salt.
!	  MASSS is the mass of salt in the droplet.
!	  KAP is the modified thermal conductivity of heat in air.
!	  LV is the latent heat of vaporization of water.
!	  MY_ALPHA is analogous to Kepert's ALPHA.
!
!	  Microphyisical constants.
!      COMMON/M_CONSTANTS/ MS, MW, RHOSOL, MASSS, RGAS, DWP, KAP, LV,
!     A Y, YTERM1, YTERM2
!	  RGAS is the universal gas constant.
!	  DWP is the modified diffusivity of water vapor in air.
!	  KAP is the modified thermal conductivity of air.
!	  Y quantifies how curvature and salinity depress a droplet's vapor
!	  pressure.
!	  YTERM1 and YTERM2 are two terms in Y.
!
!
!      DATA IFLAG/1/, TK/273.15/, AA/17.502/, BB/240.97/
!	  IFLAG means find vapor pressure over water.
!	  TK is 0 deg C in kelvins.
!	  AA and BB are constants in Buck's (1981) equation for
!	  saturation vapor pressure.
!
!

IMPLICIT NONE
REAL, INTENT(IN) :: R,S,TINF,F,P
REAL, INTENT(OUT) :: T_EST
INTEGER :: IFLAG
REAL :: AA,BB
REAL :: TC,TW,MY_ALPHA,ALPHA0,ESAT,BETA,EY,A,B,C,DELTAT
!
IFLAG=1
AA=17.502
BB=240.97
!
      TC = TINF - TK! Air temperature in deg C
      TW = TINF! Water temperature in kelvins
!
      CALL SATVPR(P,TC,ESAT,IFLAG)
!	  ESAT is now saturation vapor pressure at TC.
!
      CALL SPRAY_CONSTANTS(R,S,P,TINF,TW)
!	  This fills COMMON/M_CONSTANTS/.
!
!	  Computed constants.
      MY_ALPHA = (AA*BB*TINF)/((BB + TC)**2)
      ALPHA0 = (MW*LV)/(RGAS*TINF)       ! Kepert's alpha
      BETA = ALPHA0*(100*ESAT*DWP)/KAP   ! Kepert's beta
!	  The 100 is necessary to convert ESAT to Pascals.
      EY = EXP(Y)
!
!	  Coefficients in the quadratic equation:
!	  A*(DELTAT**2)  +  B*DELTAT  +  C   =   0.
      A = (BETA/(TINF**2))*(0.5*(MY_ALPHA**2) &
          - MY_ALPHA*((2*TC + BB + TK)/(BB+TC)) + 1.0)*EY
      B = 1.0 + (BETA/TINF)*(MY_ALPHA - 1.0)*EY
      C = -BETA*(F - EY)
!
!	  Solve the quadratic equation.
      DELTAT = (2.0*C)/(-B - SQRT(B**2 - 4.0*A*C))
!
      T_EST = DELTAT + TC             ! Estimated evaporating temperature
!
RETURN
END SUBROUTINE SUB_MOD_KEPERT
!
!
SUBROUTINE SATVPR(P,T,E,IFLAG)
!  Last modification 6/22/00.
!  This subroutine uses Buck's (1981) equation to find the saturation vapor
!  pressure as temperature T and atmospheric pressure P.  The equation is
!
!               E = (PA*P + PB)*E0*EXP((A*T)/(B + T)
!
!  where P and E are in mb, and T is in degrees C.
!  IFLAG can suppress a calculation of E based on temperature.
!               If IFLAG = 0, compute E based on T.
!               If IFLAG = 1, compute saturation over water.
!               If IFLAG = 2, compute saturation over ice.
!
!
!      DIMENSION PA(2), PB(2), E0(2), A(2), B(2)
!
!      DATA PA/3.46E-6,4.18E-6/, PB/1.0007,1.0003/, E0/6.1121,6.1115/,
!     1 A/17.502,22.452/, B/240.97,272.55/
!         In each array, the first entry is for saturation with respect to
!         water, the second for ice.
!
IMPLICIT NONE
REAL, INTENT(IN) :: P,T
INTEGER, INTENT(IN) :: IFLAG
REAL, INTENT(OUT) :: E
REAL, DIMENSION(2) :: PA, PB, E0, A, B
INTEGER :: K
character*256 :: message   !added by zhao biao

DATA PA/3.46E-6,4.18E-6/, PB/1.0007,1.0003/, E0/6.1121,6.1115/, A/17.502,22.452/, B/240.97,272.55/
!
      K = 1                             ! SATURATION OVER WATER
      IF(IFLAG .EQ. 0) GO TO 70         ! T DETERMINES IF OVER ICE OR WATER
      IF(IFLAG .EQ. 1) GO TO 100        ! CHOOSE SATURATION OVER WATER
      IF(IFLAG .EQ. 2) GO TO 50         ! CHOOSE SATURATION OVER ICE
      write(message,*) 'seaspray subroutine error: IFLAG =',IFLAG,'IS OUTSIDE THE RANGE OF ACCEPTABLE VALUES. WE QUIT'
       CALL wrf_message(message)  ! added by zhaobiao
      PRINT 700, IFLAG
700   FORMAT(//5X,'IFLAG = ',I6,' IS OUTSIDE THE RANGE OF ACCEPTABLE ','VALUES.'/5X,'WE QUIT.'//)
      STOP 'BAD IFLAG seaspray subroutine'
!
50    K = 2                             ! SATURATION OVER ICE
      GO TO 100
!
70    IF(T .LT. 0.0) K = 2              ! SATURATION OVER ICE IF TRUE
100   E = (PA(K)*P + PB(K))*E0(K)*EXP((A(K)*T)/(B(K) + T))
!
RETURN
END SUBROUTINE SATVPR
!
!
SUBROUTINE SPRAY_CONSTANTS(R,S,P,TINF,TW)
!  Last modification 5/10/06.
!  This program computes constants that are used in various
!  microphysical computations involving spray.
!  R is the droplet radius in meters.
!  S is the fractional salinity.
!  P is the barometric pressure.
!  TINF and TW are air and water temperatures in kelvins.
!
!
!      REAL MW, MS, KAP, LATENT, LV, MASSS, MASSW, MASSD, MSMW
!
!		Microphysical constants.
!      COMMON/M_CONSTANTS/ MS, MW, RHOSOL, MASSS, RGAS, DWP, KAP, LV,
!     A Y, YTERM1, YTERM2
!
!         MW AND MS ARE THE MOLECULAR WEIGHTS OF WATER AND NACL IN KG/MOLE.
!		RHOSOL IS THE SOLUTIONS DENSITY OF THE DROPLET.
!         LV IS THE LATENT HEAT OF VAPORIZATION OF WATER IN J/KG.
!         DWP AND KAP ARE THE MOLECULAR DIFFUSIVITY OF WATER VAPOR IN M**2/S
!         AND THE THERMAL CONDUCTIVITY OF AIR IN W/M-K, ADJUSTED FOR
!         NON-CONTINUUM EFFECTS.
!         MASSS AND MASSW ARE THE MASSES (IN KG) OF THE SALT AND PURE WATER IN
!         THE SOLUTION DROPLET.
!         MASSD IS THE MASS OF THE DROPLET.
!         MSMW IS MASSS/MASSW.
!         RGAS is the universal gas constant.
!         Y IS A MESSY, NONDIMENSIONAL FUNCTION.
!
!
!      DATA RGAS/8.31447/, MW/18.0150E-3/, MS/58.443E-3/, TK/273.15/,
!     A PI/3.141593/, NU/2/
 
IMPLICIT NONE
REAL, INTENT(IN) :: R,S,P,TINF,TW
INTEGER :: NU
REAL :: TWC, MSMW, MASSD, RHOW, VA, MASSW, SIGMA, CURLYM, PHIS, YFAC1, YFAC2
!
RGAS=8.31447
MW=18.0150E-3
MS=58.443E-3
NU=2
!         RGAS IS THE UNIVERSAL GAS CONSTANT IN J/MOLE-K.
!         NU IS THE NUMBER OF IONS IN NACL.
!
!
!
      TWC = TW - TK                     ! TW IN CELSIUS
!
!         COMPUTE (CONSTANT) MASS OF SALT.
      MSMW = S/(1 - S)                  ! MASSS/MASSW INITIALLY
      MASSS = 1000.0*1.333333*PI*(R**3)*MSMW
!         APPROXIMATION NECESSARY FOR INITIAL CALCULATION.
      CALL SOLUTN(MSMW,R,TWC,MASSS,MS,RHOSOL,MASSD,RHOW,VA)
!         THIS COMPUTES DENSITY AND MASS OF DROPLET, AS WELL AS RHOW AND VA.
      MASSS = S*MASSD                 !MASS OF SALT
      IF(MSMW .NE. 0.0) GO TO 15        !OTHERWISE, SALINITY = 0
!	  DO NOT WANT TO DIVIDE BY ZERO
      MASSW = MASSD                   !MASS WATER = MASS DROP IF S = 0
      GO TO 20
15    MASSW = MASSS/MSMW              !INITIAL MASS OF PURE WATER
!
!
20    CALL LATENT(TWC,LV)                !LATENT HEAT OF VAPORIZATION
      CALL KAPDWP(R,TW,P)             !COMPUTES KAP AND DWP
      CALL TENSN(TWC,MSMW,SIGMA)         !COMPUTES SURFACE TENSION
!
!         COMPUTE PRACTICAL OSMOTIC COEFFICIENT, PHIS, WHICH IS DIMENSIONLESS.
      CURLYM = MSMW/MS                !THE MOLALITY
      PHIS = 0.92701 + CURLYM*(-2.1637E-2 + CURLYM*(3.4855E-2 &
        + CURLYM*(-5.9560E-3 + 3.9112E-4*CURLYM)))
!
!
60    YTERM1 = (2*MW*SIGMA)/(RGAS*TINF*RHOW)
      YTERM2 = NU*PHIS*MASSS*(MW/MS)
      YFAC1 = YTERM1/R
      YFAC2 = YTERM2/MASSW
      Y = YFAC1 - YFAC2
!
      RETURN
      END SUBROUTINE SPRAY_CONSTANTS
!
!
!
SUBROUTINE KAPDWP(R,TW,P)
!  Last modification 11/21/01.
!  THIS SUBROUTINE COMPUTES THE THERMAL CONDUCTIVITY OF AIR, KAP, AND
!  THE MOLECULAR DIFFUSIVITY OF WATER VAPOR IN AIR, DWP, ADJUSTED FOR
!  NON-CONTINUUM EFFECTS.
!  R IS THE DROPLET RADIUS IN M.
!  TW IS THE DROPLET TEMPERATURE IN KELVINS.
!  P IS THE ATMOSPHERIC PRESSURE IN MB.
!
!
!      REAL KAP, KA, LV, MW, MA, MS
!      COMMON /M_CONSTANTS/ MS, MW, DUM1, DUM2, RGAS, DWP, KAP, LV,
!     A Y, DUM3, DUM4
!         KA IS THE NORMAL THERMAL CONDUCTIVITY.
!         MW, MA, AND MS ARE THE MOLECULAR WEIGHTS OF WATER, AIR, AND
!		SALT IN KG/MOLE.
!         RGAS IS THE UNIVERSAL GASS CONSTANT.
!
!
!      DATA MA/28.9644E-3/, TK/273.15/, P0/1013.25/, DELT/2.16E-7/,
!     A DELW/8.0E-8/, ALPHAC/0.036/, ALPHAT/0.7/, PI/3.141593/,
!     B CP/1.006E3/
!         P0 IS ONE ATMOSPHERE IN MB.
!         DELT AND DELW ARE LENGTH SCALES IN M.  VALUES FROM P AND K, P. 416.
!         ALPHAC AND ALPHAT ARE DIMENSIONLESS CONSTANTS.  VALUES FROM P AND K,
!         P. 416.
!         CP IS THE SPECIFIC HEAT OF AIR NEAR 0 DEGR C IN J/KG-K.

IMPLICIT NONE
REAL, INTENT(IN) :: R,TW,P
REAL, EXTERNAL :: SPEC_HT_AIR
REAL :: MA,P0,DELT,DELW,ALPHAC,ALPHAT,CP
REAL :: TWC,KA,RHOA,DW,FAC1,FACK,FACD
!
MA=28.9644E-3
P0=1013.25
DELT=2.16E-7
DELW=8.0E-8
ALPHAC=0.036
ALPHAT=0.7
!
!
      TWC = TW - TK                   ! DROPLET TEMPERATURE IN CELSIUS
      KA = (2.411E-2)*(1 + TWC*(3.309E-3 - 1.441E-6*TWC))
!         BULK THERMAL CONDUCTIVITY OF AIR IN W/M-K.
!
      RHOA = 1.2922*(TK/TW)*(P/P0)    ! AIR DENSITY IN KG/M**3
!
      DW = (2.11E-5)*((TW/TK)**1.94)*(P0/P)
!         BULK MOLECULAR DIFFUSIVITY IN M**2/S.
!
!
      FAC1 = SQRT((2*PI)/(RGAS*TW))
      FACK = (SQRT(MA)*FAC1*KA)/(R*ALPHAT*RHOA*SPEC_HT_AIR(TWC))
      KAP = KA/((R/(R + DELT)) + FACK)  ! ADJUSTED THERMAL CONDUCTIVITY
!
!
      FACD = (SQRT(MW)*FAC1*DW)/(R*ALPHAC)
      DWP = DW/((R/(R + DELW)) + FACD)  ! ADJUSTED MOLECULAR DIFFUSIVITY
!
      RETURN
      END SUBROUTINE KAPDWP
!
!
!
SUBROUTINE SOLUTN(MSMW,R,TW,MASSS,MS,RHOSOL,MASSD,RHOW,VA)
!  Last modification 7/5/06.
!  THIS SUBROUTINE COMPUTES THE DENSITY, RHOSOL, AND MASS, MASSD, OF
!  AN AQUEOUS SOLUTION DROPLET FOR WHICH THE SALT TO PURE WATER MASS
!  RATIO, MASSS/MASSW, IS MSMW.  THE DROPLET HAS RADIUS R AND
!  TEMPERATURE TW.  MS IS THE MOLECULAR WEIGHT OF SALT.  BY-PRODUCTS ARE
!  THE APPARENT MOLAL VOLUME OF THE SALT, VA, AND THE DENSITY OF PURE
!  WATER AT TW, RHOW.  THE EQUATION FOR RHOSOL IS FROM PRUPPACHER AND
!  KLETT (1978, P. 87).
!
!
!      REAL MSMW, MASSS, MS, MASSD
!
!      DATA PI/3.141592/

IMPLICIT NONE
REAL, INTENT(IN) :: MSMW,R,TW, MASSS, MS
REAL, INTENT(OUT) :: RHOSOL,MASSD,RHOW,VA
REAL :: VOL,CONC,VA0,SVSTAR
!
!         FIND RHOW IN KG/M**3.  EQUATONS FROM P AND K, P. 86.
      IF(TW .LT. 0.0) GO TO 20
!         OTHERWISE, TW IS 0 DEGREES C OR GREATER.
      RHOW = (999.8396 + TW*(18.224944 - 7.922210E-3*TW))/(1 + 1.8159725E-2*TW)
      GO TO 30
!         CALCULATIONS FOR TW LESS THAN 0 DEGREES C.
20    RHOW = 999.84 + TW*(8.60E-2 - 1.08E-2*TW)
!
!
30    VOL = 1.333333*PI*(R**3)        !VOLUME OF DROPLET IN M**3
      CONC = (MASSS/MS)/VOL
!         MOLAR CONCENTRATION (MOLARITY) OF SALT IN MOLE/M**3.
!
!
!         FIND APPARENT MOLAL VOLUME IN M**3/MOLE.
!         I DERIVED THE EQUATIONS FROM DATA FROM MILLERO (1972).
      VA0 = 12.97 + TW*(0.2340 + TW*(-4.210E-3 + 2.857E-5*TW))
!         APPARENT MOLAL VOLUME AT INFINITE DILUTION IN CM**3/MOLE.
      SVSTAR = 2.982 + TW*(-4.970E-2 + 6.032E-4*TW)
!         SLOPE PARAMETER IN (CM**3)(L**1/2)/(MOLE**3/2).
      VA = (VA0 + SVSTAR*SQRT(1.0E-3*CONC))*1.0E-6
!         APPARENT MOLAL VOLUME IN M**3/MOLE.
!
!         FIND SOLUTION DENSITY IN KG/M**3.
      RHOSOL = RHOW*(1 + MSMW)/(1 + (VA*RHOW*MSMW)/MS)
!
!         FIND DROPLET MASS IN KG.
      MASSD = RHOSOL*VOL
!
RETURN
END SUBROUTINE SOLUTN
!
!
!
SUBROUTINE FALSPD(R,TA,S,P,US,USF)
!  Last Modification 7/6/06.
!  THIS SUBROUTINE COMPUTES THE TERMINAL FALL SPEED USF OF A SPHERICAL SOLUTION
!  DROPLET OF RADIUS R (IN METERS), TEMPERATURE TA (IN DEG C), AND
!  SALINITY S (AS A FRACTION) AS THE STOKES FALL SPEED
!  US MODIFIED FOR REYNOLDS NUMBER (RE) EFFECTS,
!
!              USF   =   US/(1 + 0.158*(RE**(2/3)))
!
!  WHERE       RE   =   2*USF*R/GNU
!
!  AND         US   =   (2/9)*(R**2)*(G/GNU)*(RHOSOL/RHOA - 1)
!
!
!      REAL NU, MSMW, MASSS, MS, MASSD
!
!      COMMON /CONSTANTS/ DUMK, G
!         G IS THE ACCELERATION OF GRAVITY IN M/S**2.

IMPLICIT NONE
REAL, INTENT(IN) :: R,TA,S,P
REAL, INTENT(OUT) :: US,USF
INTEGER :: KI
REAL :: CONILM
REAL :: AIRDEN,STOKES,GNU,RHOA,MSMW,MASSD,RHOW,VA,NU,USF2,FAC1,USF1,F,FPRIME 
!
!
!      DATA MS/58.443E-3/, PI/3.141592/, TK/273.15/, CONILM/0.0001/
MS=58.443E-3
CONILM=0.0001
!		MS IS THE MOLECULAR WEIGHT OF NaCl IN kg/mol.
!         CONLIM IS THE CONVERGENCE LIMIT IN NEWTON'S METHOD.
!
!      AIRDEN(T,P) = 1.2922*(TK/(TK + T))*(P/1013.25)
!         AIR DENSITY IN KG/M**3.
!      STOKES(R,GNU,RHOA,RHOSOL) = 0.222222*(G/GNU)*(R**2)* ((RHOSOL/RHOA) - 1.0)
!         STOKES TERMINAL FALL VELOCITY IN M/S.
!
      MSMW = S/(1 - S)                  !MASSW/MASSS
      MASSS = 1000.0*1.333333*PI*(R**3)*MSMW
!         APPROXIMATE MASS OF SALT.
!		THE 1000 IS THE APPROXIMATE DENSITY OF PURE WATER.
      CALL SOLUTN(MSMW,R,TA,MASSS,MS,RHOSOL,MASSD,RHOW,VA)
!         THIS COMPUTES THE DENSITY OF THE DROPLET, RHOSOL.
!
      CALL KV(TA,GNU) !modified by zhaobiao original is NU(TA)           !VISCOSITY OF AIR AT TA
!      RHOA = AIRDEN(TA,P)             !DENSITY OF AIR
      AIRDEN = 1.2922*(TK/(TK + TA))*(P/1013.25)
      RHOA = AIRDEN             !DENSITY OF AIR
!      US = STOKES(R,GNU,RHOA,RHOSOL)  !STOKES FALL SPEED
      STOKES = 0.222222*(G/GNU)*(R**2)* ((RHOSOL/RHOA) - 1.0)
      US = STOKES  !STOKES FALL SPEED
!
!         FIND CORRECTED TERMINAL VELOCITY BY NEWTON'S METHOD.
      USF2 = US
      FAC1 = 0.158*((2.0*R/GNU)**0.6666667)
          DO 100 KI=1,20
          USF1 = USF2
          F = (USF1 - US) + FAC1*(USF1**1.666667)
          FPRIME = 1.0 + 1.666667*FAC1*(USF1**0.6666667)
          USF2 = USF1 - F/FPRIME        !NEW ESTIMATE OF USF
          IF(ABS((USF2 - USF1)/USF1) .LE. CONILM) GO TO 150
100       CONTINUE
!
150   USF = USF2                      !MODIFIED FALL VELOCITY
      RETURN
      END SUBROUTINE FALSPD
!
!
!
SUBROUTINE SIG_WAVE(U10,D,h13)
!  Last modification 5/11/06.
!  This routine estimates the significant wave height based on the
!  formulation of Andreas and Wang (2006), which is
!
!		H13 = C(D)					for U10 .LE. 4 m/s
!		H13 = a(D)(U10**2) + b(D)	for 4 m/s .LE. U10
!
!  U10 must be the 10-meter wind speed in m/s.
!  D is the water depth in meters.
!
!
IMPLICIT NONE
REAL, INTENT(IN) :: U10
REAL, INTENT(out) :: h13
REAL :: D
REAL :: FIX_DEPTH
REAL :: FUNC_A,FUNC_C,C,A,B
!      DATA FIX_DEPTH/3000.0/
!	  FIX_DEPTH is the assumed depth if D is .LE. 0.
FIX_DEPTH=3000.
!
!      FUNC_A(D) = 0.0134*TANH((ALOG(D/9.0))/1.3)
!      FUNC_A = 0.0134*TANH((ALOG(D/9.0))/1.3)
!	  The slope function in (s**2)/m.
!
!      FUNC_C(D) = 1.36*TANH((ALOG(D/6.0))/1.9)
!      FUNC_C = 1.36*TANH((ALOG(D/6.0))/1.9)
!	  The constant wave-heigth range in m.
!
!
      IF(D .LE. 0.0) D = FIX_DEPTH                 !In case depth is unknown
!
      C = 1.36*TANH((ALOG(D/6.0))/1.9)             !In constant wave-height region
        IF(U10 .GT. 4.0) GO TO 100
!	  Otherwise, have all we need.
      h13 = C                                      !Wave height in meters
        RETURN
!
!	  Region where wind speed matters.
100   A = 0.0134*TANH((ALOG(D/9.0))/1.3)           !Slope parameter
      B = C - 16.0*A                               !Offset
      h13 = B + A*(U10**2)                       !Wave height in meters
!
      RETURN
END SUBROUTINE SIG_WAVE
!
!
!
SUBROUTINE R_EQUILB(R0,TEQ50,REQ,TAU_R,TA,RH,S,P)
!  Last modification 7/27/06.
!  This subroutine computes equilibrium parameters relating to radius
!  evolution for an evaporating spray droplet.  Relevant quantities
!  are as follows:
!  R0 is the droplet radius in meters.  (Really 50 micrometers.)
!  TEQ50 is the equilibrium temperature of 50 micrometer droplets
!  in degrees C.
!  REQ is the equilibrium radius in meters
!  TAU_R is the radius e-folding time.
!
!
!      REAL MA, MS, MW, MASSW, MASSS, KA, KAP, LV, LATENT
!		MA is the molecular weight of air.
!		MS is the molecular weight of salt.
!		MW is the molecular weight of salt.
!		MASSW is the mass of water in the droplet.
!		MASSS is the mass of salt in the droplet.
!		KA is the thermal conductivity of air.
!		KAP is the modified thermal conductivity of air.
!		LV is the latent heat of vaporization.
!		LATENT is a function.
!
!      COMMON /M_CONSTANTS/ MS, MW, RHOSOL, MASSS, RGAS, DWP, KAP,
!     A LV, Y, YTERM1, YTERM2
!		RHOSOL is the droplet solution density.
!		RGAS is the universal gas constant.
!		DWP is the modified molecular diffusivity of water vapor.
!		Y quantities how curvature and salinity depress a droplet's
!		vapor pressure.
!		YTERM1 is related to the curvature effect.
!		YTERM2 is related to the salinity effect.
!
!      COMMON /CONDITIONS/DUM1, DUM2, TAIR, DUM3, RH, DUM4, DUM5,
!     A DUM6, P, S, DUM
!		TAIR is the air temperature in degr C.
!		RH is the  relative humidity in %.
!		P is the barometric pressure in millibars.
!		S is the relative humidity in psu.
!
!
!      DATA TK/273.15/, CP/1.006E3/, CPW/4.0E3/, MA/28.9644E-3/,
!     A  A/17.502/, B/240.97/, IFLAG/1/,  PI/3.141593/
!		TK is 0 degrees C in kelvins.
!		CP is the specific heat of air in J/kg-K.
!		CPW is the specific heat of seawater in J/kg-K.
!		MA is the molecular weight of air in kg/mole
!		A and B are coefficients in Buck's (1981) relation for the
!		saturation vapor pressure over water.
!		IFLAG = 1 means to compute saturation vapor pressure with
!		respect to water.
!
!

IMPLICIT NONE
REAL, EXTERNAL :: SPEC_HT_AIR
REAL, INTENT(IN) :: R0,TA,RH,S,P
REAL, INTENT(OUT) :: TEQ50,REQ,TAU_R
INTEGER :: IFLAG
REAL :: CP, CPW, MA, A, B
REAL :: TINF, F, S_FRAC, RHOA, KA, DW, MASSW, & 
        TERM1, TERM2, TERM3, TERM4, TERM5, TERM6, DR_DT, DY_DR, & 
        DENOM1, DENOM2A, DENOM2B, DENOM2C, DENOM3, DENOM,D2R_DT2, & 
        RDIF, COR_FAC, XINSQRT, TS_NUM1, TS_DENOM, ESATTA

CPW=4.0E3
MA=28.9644E-3
A=17.502
B=240.97
IFLAG=1

      TINF = TA  + TK                           ! In kelvins
        F = RH/100.0                            ! Fractional RH
        IF(F .GT. 1.)THEN
          F = 1.
        ENDIF                                   ! Limit relative humidity
        IF(F .LT. 0.75)THEN
          F = 0.75
        ENDIF                                   ! Spray model can't handle lower RH
        S_FRAC = S/1000.0                       ! Fractional salinity
!
!
!		Find TEQ.
      CALL SUB_MOD_KEPERT(R0,S_FRAC,TINF,F,P,TEQ50)
!		This returns TEQ50 and populates COMMON block M_CONSTANTS
!		with values appropriate at R0.
!
!
!		Calculate saturation vapor pressure at TAIR, ESATTA.
      CALL SATVPR(P,TA,ESATTA,IFLAG)
!		Calculate air density in kg/m**3.
      RHOA = 1.2923*(TK/(TK+TEQ50))*(P/1013.25)
!		Calculate thermal conductivity of air in W/kg-K.
      KA = 0.02411*(1.0 + TEQ50*(3.309E-3 - 1.441E-6*TEQ50))
!		Calculate vapor diffusivity in m**2/s.
      DW = 2.11E-5*(((TK+TEQ50)/TK)**1.94)*(1013.25/P)
!		Calculate latent heat of vaporization approriate for
!		50 micrometer droplets.
      CALL LATENT(TEQ50,LV)
!
!
!		Setup for estimating TAU_R.
      MASSW = (4.0/3.0)*PI*(R0**3)*RHOSOL - MASSS
      TERM1 = (RHOSOL*RGAS*TINF)/(MW*100.0*ESATTA)
      TERM2 = (LV*RHOSOL/TINF)*((LV*MW)/(RGAS*TINF) - 1.0)
      TERM3 = YTERM1
      TERM4 = YTERM2
      TERM5 = SQRT((2*PI*MW)/(RGAS*TINF))/0.036
      TERM6 = SQRT((2*PI*MA)/(RGAS*TINF))/(0.7*RHOA*SPEC_HT_AIR(TA))
!
!		Find derivative of r with respect to time.
      DR_DT = ((F - 1.0) - Y)/(R0*(TERM1/DWP + TERM2/KAP))
!
!		Find derivative of y with respect to r.
      DY_DR = -TERM3/(R0**2) + (TERM4/(MASSW**2))*(4.0*PI*RHOSOL*(R0**2))
!
!		Additional computations for finding second derivative.
      DENOM1 = 1/R0 + DY_DR/((F - 1.0) - Y)
      DENOM2A = ((TERM1*TERM5) + (TERM2*TERM6))/(R0**2)
      DENOM2B = -(TERM1/DW)*(8E-8/((R0 + 8E-8)**2))
      DENOM2C = -(TERM2/KA)*(2.16E-7/((R0 + 2.16E-7)**2))
      DENOM3 = -(TERM1/DWP + TERM2/KAP)
      DENOM = DENOM1 + (DENOM2A + DENOM2B + DENOM2C)/DENOM3
!		Compute 2nd derivative.
        D2R_DT2 = -(DR_DT**2)*DENOM
!
!		To compute TAU_R, need a good estimate of REQ.
        CALL EST_REQ(R0,TINF,TEQ50,F,P,REQ)
!		This returns REQ.
!
!
      RDIF = R0 - REQ
!		Estimate TAU_R.  Method depends on growth rate.
      IF(DR_DT) 30, 10, 30
!		DT_DR = 0; no growth.  TAU_R is zero.
10    TAU_R = 0.0
      RETURN
!
!		DR_DT .GT. zero, so 2nd-order model does not work.
!		Linear growth model.
20      TAU_R = -RDIF/DR_DT                         !In seconds
!		Correct for inaccuracy at high relative humidity.
        COR_FAC = -1.3709486E3 + F*(2.8142127E3 - 1.4430667E3*F)
        TAU_R = TAU_R/COR_FAC
      RETURN
!
!		Estimate of TAU_R based on a 2nd-order Taylor series.
30    XINSQRT = 3.0*(DR_DT**2) - 2.0*RDIF*D2R_DT2
      IF(XINSQRT .LT. 0.0) GO TO 20
!		Would be an error if try SQRT of a negative.
!		Negative only at high humidity, above about 98%.
!		2nd-order estimate also has unpredictable behavior for
!		high RH.  Avoid this.
      IF(RH .GT. 97.5) GO TO 20
        TS_NUM1 = SQRT(XINSQRT)
        TS_DENOM = D2R_DT2 - ((DR_DT**2)/RDIF)
        TAU_R = (-DR_DT - TS_NUM1)/TS_DENOM
!									!In seconds
!		Test for reasonable value.
!		TAU_R can be computed to be negative at high RH.
      IF(TAU_R .LE. 0.0) GO TO 20
!
      RETURN
        END SUBROUTINE R_EQUILB
!
!
!
SUBROUTINE EST_REQ(R0,TINF,TEQ,F,P,REQ)
!  Last modification 12/5/06.
!  This subroutine uses Newton's method to estimate the equilibrium
!  radius (REQ) of a droplet that has initial radius R0 (in meters).
!  TINF is the air temperature in kelvins.
!  TEQ is the droplet (equilibrium) temperature in degrees C.
!  F is the fractional relative humidity.
!  P is the barometric pressure in millibars.
!
!
!      REAL MS, MW, MASSS, MASSS0, MASSW, MSMW, KAP, LV
!		MS is the molecular weight of salt.
!		MW is the molecular weight of water.
!		MASSS is the mass of salt in a droplet In kg.
!		MASSS0 is the initial mass of salt in a droplet.
!		MASSW is the mass of pure water in a droplet.
!		MSMW = MASSS0/MASSW.
!		KAP is the modified thermal conductivity of air.
!		LV is the latent heat of vaporization.
!
!      COMMON /M_CONSTANTS/ MS, MW, RHOSOL, MASSS, RGAS, DWP, KAP, LV,
!     A Y, YTERM1, YTERM2
!		RHOSOL is the solution density of a droplet.
!		RGAS is the universal gas constant.
!		DWP is the modified molecular diffusivity of water vapor.
!		Y quantifies the effects of curvature and salinity on a
!		droplet's vapor pressure.
!		YTERM1 is related to the curvature effect.
!		YTERM2 is related to the salinity effect.
!
!
!      DATA PI/3.141593/, TK/273.15/, RADCON/0.001/
!		TK is 0 degrees C in kelvins.
!		RADCON is the criterion for convergence of the radius estimate.

IMPLICIT NONE
REAL, INTENT(IN) :: R0,TINF,TEQ,F,P
REAL, INTENT(OUT) :: REQ
INTEGER :: KOUNT
REAL :: RADCON
REAL :: TEQK, MASSS0, FAC1, FAC2, MASSW, MSMW, G0, G1, G2, DG_DR, REQ_OLD, DUM1, DUM3, RHOW 
!
RADCON=0.001
!
      TEQK = TEQ + TK                                   !Droplet temperature in kelvins
      MASSS0 = MASSS                                    !Save mass of salt
      FAC1 = (4.0/3.0)*PI
      FAC2 = 4.0*PI
      KOUNT = 0                                         !Iteration count
      REQ = R0                                          !First estimate of REQ
!
!		Find RHOSOL appropriate for R0, which is first guess.
      MASSW = FAC1*RHOSOL*(REQ**3) - MASSS0
        MSMW = MASSS0/MASSW                     !Ratio mass salt to mass water
        CALL SOLUTN(MSMW,REQ,TEQ,MASSS0,MS,RHOSOL,DUM1,RHOW,DUM3)
!		This returns RHOSOL and RHOW.
!
10    KOUNT = KOUNT + 1                         !Iteration count
      REQ_OLD = REQ                             !Save previous value
!
!		Estimate mass of pure water in the droplet.
      MASSW = FAC1*RHOSOL*(REQ**3) - MASSS0
        IF(MASSW .LE. 0.0) PRINT *, 'Mass of water .LE. 0!'
!
!		Recompute Y.
      Y = (YTERM1/REQ) - (YTERM2/MASSW)
      G0 = (F - 1.0) - Y                        !Function we need to zero
!
      G1 = YTERM1/(REQ**2)
        G2 = -(YTERM2*(FAC2*RHOSOL*(REQ**2))/(MASSW**2))
        DG_DR = G1 + G2                         !Derivative of G function
!
!		Make new estimate of equilibrium radius using Newton's method.
      REQ = REQ - (G0/DG_DR)
!		Prevent unusually small computed radii.
        IF(REQ .LT. 0.5*REQ_OLD) REQ = 0.5*REQ_OLD
        IF(ABS((REQ - REQ_OLD)/REQ_OLD) .LE. RADCON) RETURN
!		Convergence if true, otherwise continue.
      IF(KOUNT .LE. 20) GO TO 50
!		Otherwise, end.
        PRINT *, 'In finding REQ, KOUNT .GT. 20!'
        RETURN
!
50      MSMW = MASSS0/MASSW                     !Ratio of masses
        CALL SOLUTN(MSMW,REQ,TEQ,MASSS0,MS,RHOSOL,DUM1,RHOW,DUM3)
!		This returns a new estimate of the solution density, RHOSOL.
!
        CALL SPRAY_CONSTANTS_2(REQ,TINF,TEQK,P,MASSS0,MSMW,RHOW)
!		This computes new Y, YTERM1, YTERM2 values.
      GO TO 10
!
      END SUBROUTINE EST_REQ
!
!
!
SUBROUTINE SPRAY_CONSTANTS_2(R,TINF,TW,P,MASSS,MSMW,RHOW)
!  Last modification 5/10/06.
!  This program computes constants that are used in various
!  microphysical computations involving spray.
!  R is the droplet radius in meters.
!  TINF is the air temperature in kelvins.
!  TW is the water temperature in kelvins.
!  P is the barometric pressure in mb.
!  MASSS is the (constant) mass of salt in the droplet.
!  MSMW is the ratio of salt to water mass.
!
!      REAL MW, MS, KAP, LATENT, LV, MASSS, MASSW, MSMW
!
!	  Microphysical constants.
!      COMMON /M_CONSTANTS/ MS, MW, RHOSOL, DUM, RGAS, DWP, KAP, LV,
!     A Y, YTERM1, YTERM2
!
!         MW AND MS ARE THE MOLECULAR WEIGHTS OF WATER AND NACL IN KG/MOLE.
!		RHOSOL IS THE DROPLET SOLUTION DENSITY.
!		MASSS IS THE MASS OF SALT IN THE DROPLET.
!         LV IS THE LATENT HEAT OF VAPORIZATION OF WATER IN J/KG.
!         DWP AND KAP ARE THE MOLECULAR DIFFUSIVITY OF WATER VAPOR IN M**2/S
!         AND THE THERMAL CONDUCTIVITY OF AIR IN W/M-K, ADJUSTED FOR
!         NON-CONTINUUM EFFECTS.
!         MASSS AND MASSW ARE THE MASSES (IN KG) OF THE SALT AND PURE WATER IN
!         THE SOLUTION DROPLET.
!         MASSD IS THE MASS OF THE DROPLET.
!         MSMW IS MASSS/MASSW.
!         RGAS is the universal gas constant.
!         Y IS A MESSY, NONDIMENSIONAL FUNCTION.
!		YTERM1 AND YTERM2 ARE TWO TERMS IN IT
!
!
!      DATA TK/273.15/, PI/3.141593/, NU/2/
!         NU IS THE NUMBER OF IONS IN NACL.

IMPLICIT NONE
REAL, INTENT(IN) :: R,TINF,TW,P,MASSS,RHOW
INTEGER :: NU
REAL :: TWC,MASSW,MSMW,SIGMA,CURLYM,PHIS,YFAC1,YFAC2
!
NU=2
!
!
!
      TWC = TW - TK                   !TW IN CELSIUS
!
15    MASSW = MASSS/MSMW              !INITIAL MASS OF PURE WATER
!
!
20    CALL LATENT(TWC,LV)                !LATENT HEAT OF VAPORIZATION
      CALL KAPDWP(R,TW,P)             !COMPUTES KAP AND DWP
      CALL TENSN(TWC,MSMW,SIGMA)         !COMPUTES SURFACE TENSION
!
!         COMPUTE PRACTICAL OSMOTIC COEFFICIENT, PHIS, WHICH IS DIMENSIONLESS.
      CURLYM = MSMW/MS                     !THE MOLALITY
      PHIS = 0.92701 + CURLYM*(-2.1637E-2 + CURLYM*(3.4855E-2 + CURLYM*(-5.9560E-3 + 3.9112E-4*CURLYM)))
!
!
60    YTERM1 = (2*MW*SIGMA)/(RGAS*TINF*RHOW)
      YTERM2 = NU*PHIS*MASSS*(MW/MS)
      YFAC1 = YTERM1/R
      YFAC2 = YTERM2/MASSW
      Y = YFAC1 - YFAC2
!
      RETURN
      END SUBROUTINE SPRAY_CONSTANTS_2
!
!
SUBROUTINE LATENT(T,lat)
!  Last modification 6/21/00.
!  This function computes the latent heat of vaporization or
!  sublimation in J/kg.
!  T is the temperature in degrees Celsius.
!  
IMPLICIT NONE
REAL, INTENT(IN) :: T
REAL, INTENT(out) :: lat

      IF(T) 10, 20, 20                  ! Sublimation or vaporization?
!               
!         Sublimation if T is below 0 deg C.
10    lat = (28.34 - 0.00149*T)*1.0E+5
      RETURN
!     
!         Vaporization if T is at or above 0 deg C.
20    lat = (25.00 - 0.02274*T)*1.0E+5

      RETURN
END SUBROUTINE LATENT
!
!
SUBROUTINE TENSN(T,MSMW,sigma)
!  Last modification 5/10/06.
!  This function computes the surface tension of a liquid-water/air interface
!  for temperatures between -45 and +40 deg C.  The water mAy be saline.
!  The approach is based on equations given in Andreas (2005, CRREL Monograph M-05-1).
!  T is the water temperature in degrees Celsius.
!  MSMW is the ratio of the mass of salt to the mass of pure water in the sample.
!  Units of TENSN are J/m**2.
!
!
IMPLICIT NONE
REAL, INTENT(IN) :: T,MSMW
REAL, INTENT(out) :: sigma
REAL :: TENSION_WATER
character*256 :: message   !added by zhao biao
!
!
!         Find surface tension of pure water at temperature T.
      IF (T .LT. -45.0) THEN
        write(message,*) 'seaspray subroutine warning:Temperature too low for calculating surface tension'
        CALL wrf_message(message)  ! added by zhaobiao
        PRINT *, 'Temperature too low for calculating surface tension.'
!        PAUSE
!
      ELSE IF (T .LT. 0.0) THEN
!         From Pruppacher and Klett (1997, p. 130).
        TENSION_WATER = 7.593E-2 + T*(1.15E-4 + T*(6.818E-5 + T* &
         (6.511E-6 + T*(2.933E-7 + T*(6.283E-9 + 5.285E-11*T)))))
!
      ELSE IF (T .LE. 40.0) THEN
!         From Vargaftik et al. (1983).
      TENSION_WATER = 0.2358*(((374.0 - T)/647.15)**1.256)*(1 - 0.625*((374.0 - T)/647.15))
!
      ELSE
        write(message,*) 'seaspray subroutine warning:Temperature too high for calculating surface tension'
        CALL wrf_message(message)  ! added by zhaobiao
        PRINT *, 'Temperature too high for calculating surface tension.'
!        PAUSE
!
      END IF
!
!         Add salinity effects.
      sigma = TENSION_WATER + 2.77E-2*MSMW
!
      RETURN
END SUBROUTINE TENSN
!
!
SUBROUTINE PSI_M(ZU,L,PSI)
!  Last modification 11/7/01.
!  This function computes the stability correction for the wind speed
!  profile.
!  ZU is the height of the wind speed observation.
!  L is the Obukhov length.
!
IMPLICIT NONE
REAL, INTENT(IN) :: ZU,L
REAL, INTENT(out) :: PSI
REAL :: ZETA,X,TERM1
!
      ZETA = ZU/L                       !Stability parameter
      IF(ZETA) 10, 30, 20               !Check stability
!
!         Unstable stratification (Paulson, 1970).
10    X = (1 - 16.0*ZETA)**0.25
      PSI = 2.0*ALOG(0.5*(1.0 + X)) + ALOG(0.5*(1 + X*X)) - 2.0*ATAN(X) + 1.570796
      RETURN
!
!         Stable stratification (Dutch formulation).
20    TERM1 = 0.0
      IF(ZETA .LE. 250.0) TERM1 = EXP(-0.35*ZETA)
!         This test prevents underfloe errors.
      PSI = -(0.7*ZETA + 0.75*(ZETA - 14.28)*TERM1 + 10.71)
      RETURN
!
!         Neutral stratification.
30    PSI = 0.0
      RETURN
!
END SUBROUTINE PSI_M
!
SUBROUTINE KV(T,GNU)
IMPLICIT NONE
!  Last modification 6/21/00.
!  This function computes the kinematic viscosity of air in m**2/s.
!  T is the air temperature in degrees Celsius.
REAL, INTENT(IN) :: T
REAL, INTENT(out) :: GNU

      GNU = 1.326E-5*(1.0 + T*(6.542E-3 + T*(8.301E-6 - 4.840E-9*T)))

END SUBROUTINE KV
!


END MODULE module_sf_seaspray

FUNCTION SPEC_HT_AIR(T)
IMPLICIT NONE
REAL,INTENT(IN) :: T
REAL :: SPEC_HT_AIR
!  Last modification 8/23/00.
!  This function computes the specific heat of dry air in J/kg/K
!  for air temperature T in degrees C between -40 and +40 C.

      SPEC_HT_AIR = 1005.60 + T*(0.017211 + 0.000392*T)
RETURN
END 
