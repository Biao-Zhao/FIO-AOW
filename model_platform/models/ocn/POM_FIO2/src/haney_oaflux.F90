 SUBROUTINE HANEY_OAFLUX(IM,JM,PSA,TSA,SW,UVA,QAR,SST,NETHEAT,MASK)
! ======================
!     COMPUTE QQQ AND DDD IN TERMS OF HANEYS FORMULA
!
!     HEATFLUX = DDD(TA-SST)+QQQ
!
!     INDEX=1 FOR ICE ; 0 FOR WATER
!
!------------------------------------------------------------------------
! INPUT
!     PSA  : Sea surface air pressure                   (Pa)
!     TSA  : Sea surface air temperature                (Celsius)
!     SW  : Total net downward solar radiation         (W/M**2)
!     UVA  : Wind speed                                 (M/S)
!     QAR  : Surface air mixing ratio                   ( - )
!     CLD  : Cloud fraction                             ( - )
!------------------------------------------------------------------------
! OUTPUT
!     DDD  : COEFFICIENT FOR COMPUTING THE HEAT FLUX    (W/M**2/DEG)
!     QQQ  : COEFFICIENT FOR COMPUTING THE HEAT FLUX    (W/M**2)
!------------------------------------------------------------------------
!     SWV  : Total net downward solar radiation         (W/M**2)
!     ESTA : SURFACE AIR WATER VAPOR PRESSURE           (MB)
!     QAS  : SURFACE SATURATED AIR MIXING RATIO         ( - )
!     RH   : SURFACE AIR RELATIVE HUMIDITY              ( - )
!     CPA  : SPECIFIC HEAT CAPACITY AT CONSTAN PRESSURE (M*M/DEG/SEC)
!     ALAT : LATENT HEAT OF EVAPORATION                 (M*M/SEC)
!     RGAS : GAS CONSTANT                               (M*M/DEG/SEC/SEC)
!     SIGMA: STEFEAN-BOLTZMANN CONSTANT                 (W/M*M/DEG**4)
!------------------------------------------------------------------------
!
 IMPLICIT NONE
 INTEGER, INTENT(IN) :: IM, JM
 REAL, INTENT(IN) :: PSA(IM, JM), TSA(IM, JM), SW(IM, JM), QAR(IM, JM)
 REAL, INTENT(IN) :: SST(IM, JM), MASK(IM, JM)
 REAL, INTENT(OUT) :: NETHEAT(IM, JM)
 REAL :: SWV(IM, JM), DDD(IM, JM), QQQ(IM, JM)
 INTEGER, PARAMETER :: INDEX = 0
 REAL, PARAMETER :: CPA = 1004.64, RGAS = 287., SIGMA = 0.56735E-7
 REAL, PARAMETER :: ALATW = 2.5E+6, ALATI = 2.834E+6
 REAL, PARAMETER :: OALBEDO = 0.055
 REAL :: CLD(IM, JM)
!
 REAL :: CBI,PSAIJ,ALAT,SWVIJ,TA,A01,B01,ESTA,QAS,RH, &
         EA,CL,VABS,CD,RHOA,QSTR,QLW1,QLW2,QE0,QE1,QE2,QH2
 REAL :: QLW, TO
 REAL :: UVA(IM, JM)
 INTEGER :: I, J
!
! 
 SWV = SW
 CBI=(1.0-0.068)*(1.0-0.5)/0.9
!
!
 DO J=1, JM
 DO I=1, IM
!
 IF(mask(I,J).GT.0) THEN
!
 PSAIJ=PSA(I,J)*0.01
!
!     DETERMINE THE LATENT HEAT OF EVAPORATION (ALAT) AND SOLAR
!     RADIATION ACCORDING TO GROUND PROPERTY(WATER OR ICE).
!
 ALAT     = (1-INDEX)*ALATW    + INDEX*ALATI
 SWVIJ    = (1-INDEX)*SWV(I,J) + INDEX*CBI*SWV(I,J)
!
!
!     COMPUTE SURFACE AIR WATER VAPOR PRESSURE
!
!      TA   = TSA(I,J)+273.15
 TA = TSA(I,J)
 !!!TO = sst(I,J)  by song
 TO = sst(I,J)+273.15
 A01  = (1-INDEX)*7.5   + INDEX*9.5
 B01  = (1-INDEX)*35.86 + INDEX*7.66
 ESTA = 611.*10.**(A01*(TA-273.15)/(TA-B01))*0.01
!
!
!     COMPUTE SURFACE SATURATED MIXING RATIO
!
 QAS = ESTA*0.622/(PSAIJ-ESTA)
!
!
!     COMPUTE SURFACE AIR RELATIVE HUMIDITY
!
 RH   = AMIN1(1.0,QAR(I,J)/QAS)
!
!
!     COMPUTE Q & D
!
!     QQQ  = SOLAR-QSTR*SIGMA*TA**4-QE1
!     DDD  = 4*QSTR*SIGMA*TA**3+QE2+QH2
!     if there is no cld, then
!     QQQ = SOLAR-SIGMA*TO**4-QE1
!     DDD  = QE2+QH2
!
 EA   = RH*ESTA
 CL   = CLD(I,J)
 VABS = AMAX1(UVA(I,J),2.0)
 CD   = AMIN1(0.001+7.E-5*VABS,0.0025)
 RHOA = PSA(I,J)/(RGAS*TA)
!
!     LONGWAVE RADIATION
!      QSTR = 0.985*(0.39-0.05*SQRT(EA))*(1.0-0.6*CL*CL)
!      QLW1 = QSTR*SIGMA*TA*TA*TA*TA
!      QLW2 = 4.*QSTR*SIGMA*TA*TA*TA
       QLW = SIGMA*TO*TO*TO*TO
!
!     LATENT HEAT
 QE0  = CD*VABS*RHOA*ALAT*QAS
 QE1  = QE0*(1.-RH)
 QE2  = QE0*2353.*ALOG(10.)/(TA*TA)
!
!     SENSIBLE HEAT
 QH2  = CD*VABS*RHOA*CPA
!
!      QQQ(I,J) = SWVIJ-QLW1-QE1
 QQQ(I,J) = SWVIJ-QLW-QE1
!
!      DDD(I,J) = QLW2+QE2+QH2
 DDD(I,J) = QE2+QH2

!
 ENDIF
!
 END DO
 END DO
!
 DO J=1, JM
 DO I=1, IM
 IF(mask(I,J).EQ.0) THEN
 QQQ(I,J) = 1.0
 DDD(I,J) = 30.0
 ENDIF
 ENDDO
 ENDDO
!
 NETHEAT = (DDD*(TSA-273.15-SST)+QQQ)*MASK
 !!NETHEAT = (DDD*(TSA-273.15-SST)+QQQ)*MASK
 RETURN
 END
