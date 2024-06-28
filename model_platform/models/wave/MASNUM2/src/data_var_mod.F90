!===============================================================================
! CVS $Id: data_var_mod.F90,v 1.6 2013/07/09 07:39:05 wgs Exp $
! CVS $Source: /soa04/users/wgs/.mycvsroot/intercomm/wave/data_var_mod.F90,v $
! CVS $Name:  $
!===============================================================================

!==============================================================================|
!   GLOBAL LIMITS AND ARRAY SIZING PARAMETERS                                  !
!==============================================================================|
MODULE LIMS_WAVE
   USE data_kind_mod_wave
   IMPLICIT NONE
   SAVE
   INTEGER(kind_in) KB                 !!NUMBER OF Z LEVELS FOR BV
   INTEGER(kind_in) MYID               !!UNIQUE PROCESSOR ID (1 => NPROCS)
   INTEGER(kind_in) NPROCS             !!NUMBER OF PROCESSORS

   INTEGER(kind_in) NUMXCPU,NUMYCPU    !!NUMBER OF CPU IN X AND Y AXIES
   INTEGER(kind_in) IM,JM              !!NUMBER OF GRIDS IN X AND Y AXIES
   INTEGER(kind_in) IGRID

   REAL(kind_r4) :: GRID
   REAL(kind_r4),DIMENSION(2) :: LON_Dom,LAT_Dom

END MODULE LIMS_WAVE

!==============================================================================|
!   CONTROL VARIABLES                                                          |
!==============================================================================|
MODULE CONTROL_WAVE

   USE data_kind_mod_wave 
   IMPLICIT NONE
   SAVE

   LOGICAL SERIAL                  !!TRUE IF SINGLE PROCESSOR
   LOGICAL MSR                     !!TRUE IF MASTER PROCESSOR (MYID==1)
   LOGICAL PAR                     !!TRUE IF MULTIPROCESSOR RUN
   LOGICAL DomainFig               !!TRUE IF REGIONAL MODEL

   CHARACTER(LEN=80) input,output
   CHARACTER(LEN=80) ModelName
   CHARACTER(LEN=10) FORCE_NAME(3)
   CHARACTER(LEN=10) CFSM,CDEPTH
   CHARACTER(LEN=80)  RunType
   CHARACTER(LEN=80) Initial_DataFile
   LOGICAL            Has_Initial_Data
   LOGICAL,ALLOCATABLE :: OUT_DATA_FIG(:)
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!
!* DEFINE NEW COMMUNICATOR FOR COMMUNICATE BETWEEN WAVE AND OTHER MODEL
!* CPL_WAV_WORLD  defined for wave model and couple
!* WAV_OCN_WORLD  defined for wave model and ocean 
!*                                  add by guanso   20130704 15:29:22
!
   INTEGER(kind_in) CPL_WAV_WORLD,WAV_OCN_WORLD
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   INTEGER(kind_in) ISLAT,IELAT,ISLON,IELON,WAV_COMM_WORLD
   INTEGER(kind_in) IUP,IDOWN,NUM_CORNER_SEND !IRIGHT,ILEFT
   INTEGER(kind_in) NEIGHBOR(4),CORNER_REC(2,4),NEIGHBOR_LON(4),NEIGHBOR_LAT(4) 

   INTEGER(kind_in) YEAR,MONTH,DAY,HOUR,SECOND
   INTEGER(kind_in) Y1,M1,DD1,H1,MM1,S1
   INTEGER(kind_in) RUN_TIME,INTERVAL_OUT,INTERVAL_RES,INTERVAL_FORCING
   DOUBLE PRECISION T1,T2,TT
   
   INTEGER,ALLOCATABLE,DIMENSION(:) :: LHALO,RHALO
   INTEGER,ALLOCATABLE,DIMENSION(:,:) :: CORNER_SEND
 
   REAL(kind_r4) DELTTM
   REAL(kind_r4),ALLOCATABLE,DIMENSION(:,:) :: DX,DY
   LOGICAL       ISBREAK, ISSWELL, ISCURRENT
   INTEGER(kind_in) OBCTYPE
END MODULE CONTROL_WAVE

!==============================================================================|


!==============================================================================|
!   CONTROL VARIABLES                                                          |
!==============================================================================|
MODULE CONST_WAVE 
  use data_kind_mod_wave
  implicit none
  !----------------------------------------------------------------------------
  ! physical constants (all data public)
  !----------------------------------------------------------------------------
   INTEGER(kind_I4),PARAMETER :: LE = 1
   INTEGER(kind_I4),PARAMETER :: KL = 25
   INTEGER(kind_I4),PARAMETER :: KLP1 = 26
   INTEGER(kind_I4),PARAMETER :: KLD = 30
   INTEGER(kind_I4),PARAMETER :: KLDP1 = 31
   INTEGER(kind_I4),PARAMETER :: JL = 36    !zhaobiao new=36  old=12
   INTEGER(kind_I4),PARAMETER :: JLP1 = 37  !zhaobiao new=37  old=13

   REAL(kind_r4),PARAMETER :: PWK = 1.21
   REAL(kind_r4),PARAMETER :: ALOG10PWK = ALOG10(PWK)
   REAL(kind_r4),PARAMETER :: WKMAX = 0.6894
   REAL(kind_r4),PARAMETER :: WKMIN = 0.0047 !new=0.0047 old=0.0071 zhaobiao
   REAL(kind_r4),PARAMETER :: PWF = 1.1
   REAL(kind_r4),PARAMETER :: WFMAX = 0.413  
   REAL(kind_r4),PARAMETER :: WFMIN = 0.035  !new=0.035 old=0.042 zhaobiao

   REAL(kind_r4),PARAMETER :: RS = 6367451.637  ! the global radius

   REAL(kind_r4),PARAMETER :: beta0 = 1.12      ! wind input coefficient
   REAL(kind_r4),PARAMETER :: beta1 = 0.        ! coefficient
   REAL(kind_r4),PARAMETER :: acu = 0.          ! coefficient
   REAL(kind_r4),PARAMETER :: beta10 = beta0*0.25*1.25*0.001

!   REAL(kind_r4),PARAMETER :: pi = acos(-1.0)     ! pi
   REAL(kind_r4),PARAMETER :: pi = 3.141593     ! pi
   REAL(kind_r4),PARAMETER :: zpi = 2.0*pi        ! 2*pi
   REAL(kind_r4),PARAMETER :: pi2 = pi/2.0
   REAL(kind_r4),PARAMETER :: g = 9.81
   REAL(kind_r4),PARAMETER :: gc2 = 0.877**2*g
   REAL(kind_r4),PARAMETER :: gg = g**2
   REAL(kind_r4),PARAMETER :: tztp = 1.2
   REAL(kind_r4),PARAMETER :: tztz = 1.099314
   REAL(kind_r4),PARAMETER :: d1 = 0.000132
   REAL(kind_r4),PARAMETER :: d2 = 2.61

   REAL(kind_r4),PARAMETER :: p = 0.025
   REAL(kind_r4),PARAMETER :: sbo = 0.038*2./g

! difine in set wave
   REAL(kind_r4),PARAMETER :: DELTTH = ZPI/FLOAT(JL) 

   REAL(kind_r4),PARAMETER :: SMvalue = 1.E-6
   REAL(kind_r4),PARAMETER :: Missvalue = 1.E10

!   REAL(kind_r4),PARAMETER :: Zero = 1.E-34
   REAL(kind_r4),PARAMETER :: Zero = 0. 

   REAL(kind_r4),PARAMETER :: CKSP = 14.0

   REAL(kind_r4),PARAMETER :: ADS = 1.0
   REAL(kind_r4),PARAMETER :: ABO = 1.0
   REAL(kind_r4),PARAMETER :: CKSA = 4.5
   REAL(kind_r4),PARAMETER :: RHOW = 1025     !zhaobiao,density of sea water,units:kgm-3
!
!!   REAL(kind_r4),PARAMETER :: scalefac = 1.0  ! MODIFICATION FACTOR OF WIND SPEED
!!   INTEGER(kind_i4),PARAMETER :: logscurr = 1    ! THE SWITCH FOR WAVE-CURRENT INTERACTION  1-YES,0-NO
!!   INTEGER(kind_i4),PARAMETER :: logsidiss = 1   ! THE SWITCH FOR Sds,1-Yuan's,0-Komen's
END MODULE CONST_WAVE
!==============================================================================|

MODULE ALL_VAR_WAVE 
     use data_kind_mod_wave
     use CONST_WAVE
     implicit none

     SAVE
    
     INTEGER(kind_i4),DIMENSION(KL) :: IKP,IKP1,IKM,IKM1 
     INTEGER(kind_i4),DIMENSION(2,JL) :: JP1,JP2,JM1,JM2 

     REAL(kind_r4) :: CONG,AL11,AL21,AL31,AL13,AL23
     REAL(kind_r4),DIMENSION(KL) :: GROLIM,WKS17 ! dkdf never used
     REAL(kind_r4),DIMENSION(KLDP1) :: WK,FR 
     REAL(kind_r4),DIMENSION(KLDP1) :: WKH,DWK     !FRH never used
     REAL(kind_r4),DIMENSION(JLP1) :: THET 
     REAL(kind_r4),DIMENSION(KLP1,JL) :: SE,DSE,SEIN,SEDISS    ! added by zhaobiao for caculate Janssen's wave-induced stress           
     REAL(kind_r4),DIMENSION(KL,2,2) :: WP,WM 
     REAL(kind_r4),DIMENSION(3001)  :: zz,gau  !added by zhaobiao for caculated gauss distribution
!--------------------------Temporary Array------------------------------------------!
     REAL(kind_r4), ALLOCATABLE :: ZBV(:)               !!Standard depth levels
     REAL(kind_r4), ALLOCATABLE :: XGRID(:,:),YGRID(:,:),RS2DLAT(:,:),D(:,:),NSP(:,:)
     REAL(KIND_r4), ALLOCATABLE,DIMENSION(:,:,:) :: WF,CCG,DWF           !! ALLOCATE IN init_wave.F90
     REAL(kind_r4),ALLOCATABLE,DIMENSION(:,:) :: WINDX,WINDY,WIND0X,WIND0Y
     REAL(kind_r4),ALLOCATABLE,DIMENSION(:,:) :: WX,WY,W0,W1,W,WINC
     REAL(kind_r4),ALLOCATABLE,DIMENSION(:,:) :: UXX,UXY,UYX,UYY,UX,UY 
     REAL(kind_r4),ALLOCATABLE,DIMENSION(:,:) :: H1_3,TPF,APE,AET
     REAL(kind_r4),ALLOCATABLE,DIMENSION(:,:) :: UORBITAL,VORBITAL,USTOKES,VSTOKES,RA,HEN,PT,KT,ET,BBR,MT,UST,TAUINX,TAUINY,TAUDSX,TAUDSY  !added by zhaobiao
     REAL(kind_r4),ALLOCATABLE,DIMENSION(:,:) :: swell_hs,swell_th,swell_tp,swell_tz  !! for swell by wgs 20190926
     REAL(kind_r4),ALLOCATABLE,DIMENSION(:,:) :: windy_hs,windy_th,windy_tp,windy_tz  !! for windy by wgs 20190926
     REAL(kind_r4),ALLOCATABLE,DIMENSION(:,:,:) :: TAUBB11,TAUBB12,TAUBB22,TAUBB33 
     REAL(kind_r4),ALLOCATABLE,DIMENSION(:,:,:) :: BV 
     REAL(kind_r4),ALLOCATABLE,DIMENSION(:,:,:,:) :: EE,E,EA
     REAL(kind_r4),ALLOCATABLE,DIMENSION(:) :: EHS,ETP,ETH,WHS,WTP,WTH,NHS,NTP,NTH,SHS,STP,STH  !! FOR REAL BDY DATA FROM OTHER MODEL DATA
     INTEGER(kind_in) :: RFE,RFW,RFN,RFS

!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


END MODULE ALL_VAR_WAVE
