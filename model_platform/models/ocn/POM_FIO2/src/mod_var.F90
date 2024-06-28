!===============================================================================
! CVS $Id: mod_var.F90,v 1.3 2013/06/21 07:22:01 wgs Exp $
! CVS $Source: /soa04/users/wgs/.mycvsroot/intercomm/ocean/mod_var.F90,v $
! CVS $Name:  $
!===============================================================================

!==============================================================================|
!   GLOBAL LIMITS AND ARRAY SIZING PARAMETERS                                  !
!==============================================================================|
MODULE LIMS
   USE data_kind_mod 
   IMPLICIT NONE
   SAVE
   INTEGER(kind_in) KB,KBM1,KBM2       !!NUMBER OF Z LEVELS FOR BV
   INTEGER(kind_in) MYID               !!UNIQUE PROCESSOR ID (1 => NPROCS)
   INTEGER(kind_in) NPROCS             !!NUMBER OF PROCESSORS

   INTEGER(kind_in) NUMXCPU,NUMYCPU    !!NUMBER OF CPU IN X AND Y AXIES
   INTEGER(kind_in) IM,JM              !!NUMBER OF GRIDS IN X AND Y AXIES
   INTEGER(kind_in) IGRID

   REAL(kind_r4) :: GRID
   REAL(kind_r4),DIMENSION(2) :: LON_Dom,LAT_Dom

END MODULE LIMS

!==============================================================================|
!   CONTROL VARIABLES                                                          |
!==============================================================================|
MODULE CONTROL

   USE data_kind_mod 
   IMPLICIT NONE
   SAVE

   LOGICAL SERIAL                  !!TRUE IF SINGLE PROCESSOR
   LOGICAL MSR                     !!TRUE IF MASTER PROCESSOR (MYID==1)
   LOGICAL PAR                     !!TRUE IF MULTIPROCESSOR RUN
   LOGICAL TSTYPE 
   LOGICAL WINDTYPE
   LOGICAL HEATTYPE
   LOGICAL BVTYPE
   LOGICAL BCTYPE(4)
! for tide swich
   LOGICAL TIDETYPE

   CHARACTER(LEN=80) input,output
   CHARACTER(LEN=80) ModelName
   CHARACTER(LEN=10) FORCE_NAME(3)
   CHARACTER(LEN=80)  RunType
   CHARACTER(LEN=80) Initial_DataFile
   LOGICAL            Has_Initial_Data
   LOGICAL,ALLOCATABLE :: OUT_DATA_OCN_FIG(:)
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
!
!* DEFINE NEW COMMUNICATOR FOR COMMUNICATE BETWEEN WAVE AND OTHER MODEL
!* CPL_OCN_WORLD  defined for wave model and couple
!* WAV_OCN_WORLD  defined for wave model and ocean 
!*                                  add by guanso  20130708 09:55:49 
!
   INTEGER(kind_in) CPL_OCN_WORLD,WAV_OCN_WORLD
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


   INTEGER(kind_in) I,J,K
   INTEGER(kind_in) ISLAT,IELAT,ISLON,IELON,   &
                    JSLON,JELON,JSLAT,JELAT,   &
                    KSLON,KELON,KSLAT,KELAT,   &
                    LSLON,LELON,LSLAT,LELAT,   &
                    MSLON,MELON,MSLAT,MELAT,   &
                    NSLON,NELON,NSLAT,NELAT,   &
                    L1SLON,L1ELON,L1SLAT,L1ELAT,&
                    OCN_COMM_WORLD

   INTEGER(kind_in) IUP,IDOWN,IRIGHT,ILEFT
   INTEGER(kind_in) NEIGHBOR(4)
   INTEGER(kind_in) NEIGHBOR_ID(2,4)
   INTEGER(kind_in),ALLOCATABLE,DIMENSION(:,:):: IPART

   INTEGER(kind_in) YEAR,MONTH,DAY,HOUR,MINS,SEC
   INTEGER(kind_in) Y1,M1,DD1,H1,MM1,S1
   INTEGER(kind_in) RUN_TIME,INTERVAL_OUT,INTERVAL_RES,INTERVAL_FORCING
   INTEGER(kind_in) ISPLIT,MODE,ISPADV
!  MONTH TIME
   INTEGER(kind_in) MON(12)

!   DOUBLE PRECISION T1,T2,TT
   REAL(kind_r8) T1,T2,TT,ModelTT
 
   REAL(kind_r4) DELTTM,FACT
   REAL(kind_r4) DTE,DTI,DTE2,DTI2
   REAL(kind_r4) ISPI,ISP2I 

END MODULE CONTROL

!==============================================================================|


!==============================================================================|
!   CONTROL VARIABLES                                                          |
!==============================================================================|
MODULE CONST 
  use data_kind_mod
  implicit none
  SAVE
  !----------------------------------------------------------------------------
  ! physical constants (all data public)
  !----------------------------------------------------------------------------
  INTEGER(kind_in),PARAMETER :: LE = 3

  REAL(kind_r4),PARAMETER :: OUT_LAND = 1.E10 
  REAL(kind_r4),PARAMETER :: PI = 3.141592654 
  REAL(kind_r4),PARAMETER :: SMALL = 1.E-8
  REAL(kind_r4),PARAMETER :: KAPPA = 0.4
  REAL(kind_r4),PARAMETER :: GRAV = 9.806
  REAL(kind_r4),PARAMETER :: TBIAS = 0.
  REAL(kind_r4),PARAMETER :: SBIAS = 0.
  REAL(kind_r4),PARAMETER :: RHOREF = 1025.
  REAL(kind_r4),PARAMETER :: LAND = 0
  !REAL(kind_r4),PARAMETER :: ERAD = 6378136.6
  REAL(kind_r4),PARAMETER :: ERAD = 6.371E6
  REAL(kind_r4),PARAMETER :: VEL = 0.2
  REAL(kind_r4),PARAMETER :: ZERO = 0.
  REAL(kind_r4),PARAMETER :: HORCON = 0.2
  REAL(kind_r4),PARAMETER :: SMOTH = 0.1
  REAL(kind_r4),PARAMETER :: UMOL = 1.0E-5
  REAL(kind_r4),PARAMETER :: TPRNI= 1.0
  REAL(kind_r4),PARAMETER :: ONE= 1.0
  
  REAL(kind_r4),PARAMETER,DIMENSION(12) ::     &
  QW = (/11008,11903,16825,25254,33345,40342,  &
        52183,44065,39315,32952,21817,13413/)
! FOR TIDE
  INTEGER(kind_in),PARAMETER :: NUMTIDE = 1
!  REAL(kind_r4),PARAMETER,DIMENSION(8) ::      &
!  OMG = (/15.04106864,13.94303559,14.95893136, &
!          13.3986609,28.98410424,30.0,         &
!          28.43972954,30.08213728/)
END MODULE CONST
!==============================================================================|

MODULE ALL_VAR 
     use data_kind_mod
     use CONST 
     implicit none
     SAVE
!    ONCE set never reset variables
     REAL(kind_r4),ALLOCATABLE,DIMENSION(:) :: X,Y,Z,ZZ,DZ,DZZ,ZSS
     REAL(kind_r4),ALLOCATABLE,DIMENSION(:,:) :: H,COR,DY,DX,ART,FSM 
     REAL(kind_r4),ALLOCATABLE,DIMENSION(:,:) :: ARU,ARV,DUM,DVM 
!    FOR TEMPERIATURE AND SALITY
     REAL(kind_r4),ALLOCATABLE,DIMENSION(:,:,:) :: TB,SB,T,S
!    FOR OCEAN DENSITY
     REAL(kind_r4),ALLOCATABLE,DIMENSION(:,:,:) :: RHO,RMEAN 
!    lateral boundary      
     INTEGER(kind_in) :: RFE,RFW,RFN,RFS
     REAL(kind_r4),ALLOCATABLE,DIMENSION(:) :: UABW,UABE,ELE,ELW,ELN,ELS,VABN,VABS
     REAL(kind_r4),ALLOCATABLE,DIMENSION(:,:) :: TBN,TBS,SBN,SBS,TBE,TBW,SBE,SBW 
     REAL(kind_r4),ALLOCATABLE,DIMENSION(:,:,:) :: SSOUTH,TSOUTH,SNORTH,TNORTH 
     REAL(kind_r4),ALLOCATABLE,DIMENSION(:,:,:) :: SEAST,TEAST,SWEST,TWEST 
     REAL(kind_r4),ALLOCATABLE,DIMENSION(:,:) :: ETSOUTH,VASOUTH,ETNORTH,VANORTH 
     REAL(kind_r4),ALLOCATABLE,DIMENSION(:,:) :: ETEAST,UAEAST,ETWEST,UAWEST 
!    add by wgs 
     REAL(kind_r4),ALLOCATABLE,DIMENSION(:,:) :: UBN,UBS,VBN,VBS,UBE,UBW,VBE,VBW 
     REAL(kind_r4),ALLOCATABLE,DIMENSION(:,:,:) :: USOUTH,VSOUTH,UNORTH,VNORTH 
     REAL(kind_r4),ALLOCATABLE,DIMENSION(:,:,:) :: UEAST,VEAST,UWEST,VWEST 
!    surface boundary
     REAL(kind_r4),ALLOCATABLE,DIMENSION(:,:,:) :: DQDT,HEAT,TEMP,STRX,STRY
     REAL(kind_r4),ALLOCATABLE,DIMENSION(:,:) :: WUSURF,WVSURF
!    FOR TIDE
     REAL(kind_r4),ALLOCATABLE,DIMENSION(:) :: OMG
     REAL(kind_r4),ALLOCATABLE,DIMENSION(:,:) :: AMPE,AMPW,AMPS,AMPN,PHAE,PHAW,&
                                                 PHAS,PHAN,M2UVE,M2UVW,M2UVS,M2UVN

     REAL(kind_r4),ALLOCATABLE,DIMENSION(:,:) :: UTB,VTB,UTF,VTF,ADVUA,ADVVA,  &
                   TSURF,SSURF,DRX2D,DRY2D,ADX2D,ADY2D,SWRAD,D,DT,CBC,         &
                   WUBOT,WVBOT,WTSURF,WSSURF,TPS,AAM2D,UAF,UA,UAB,VAF,         &
                   VA,VAB,ELF,EL,ELB,PSI,ETF,ET,ETB,FLUXUA,FLUXVA,EGF,EGB
     REAL(kind_r4),ALLOCATABLE,DIMENSION(:,:,:) :: ADVX,ADVY,DRHOX,DRHOY,TCLIM,&
                   SCLIM,A,C,EE,GG,UF,VF,ZDE,KM,KH,KQ,L,Q2,Q2B,AAM,Q2L,Q2LB,WZ,&
                   U,UB,W,W2,V,VB,DTEF,BV
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    
END MODULE ALL_VAR
