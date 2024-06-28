!==============================================================================|
!   Input Parameters Which Control the Model Run                               |
!==============================================================================|

   SUBROUTINE DATA_RUN            

!------------------------------------------------------------------------------|

   USE NCFILEMOD
   USE data_kind_mod 
   USE LIMS
   USE CONTROL
   USE ALL_VAR
   USE MOD_UTILS
   USE MOD_INP
   USE parallel_mod
   USE time_mod
   USE CCPL_interface_mod, only : CCPL_report_error   !zhaobiao, C-Coupler2
   USE coupling_pom_mod                               !zhaobiao, C-Coupler2
   IMPLICIT NONE
   REAL(kind_r4) REALVEC(150)
   INTEGER  INTVEC(150),ISCAN,KTEMP
   CHARACTER(LEN=120) :: FNAME,FILENAME
   CHARACTER(LEN=80) :: STRTIME
   CHARACTER(LEN=80) :: CC(150) 
   CHARACTER(LEN=19) :: STRTT 
   INTEGER TTVEC(6)
   INTEGER refyear


!==============================================================================|
!   READ IN VARIABLES AND SET VALUES                                           |
!==============================================================================|

   FNAME = "runsetocean.dat"
!------------------------------------------------------------------------------|
!  Model name 'ModelVersion' 
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"ModelVersion",CVAL = ModelName)
     WRITE(6,*) "ModelVersion :",ModelName
   IF(ISCAN /= 0) CALL CCPL_report_error(pom_state_variables(pom_grid_id)%comp_id,.false., "ERROR in READING  ModelVersion")  !zhaobiao, C-Coupler2

!------------------------------------------------------------------------------|
!  Model run type, must be one of "initial" and "restart" 
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"RunType",CVAL = RunType)
     WRITE(6,*) "RunType is :",RunType
   IF(ISCAN /= 0) CALL CCPL_report_error(pom_state_variables(pom_grid_id)%comp_id,.false., "ERROR in READING RunType")  !zhaobiao, C-Coupler2
   IF (RunType /= "initial" .and. RunType /= "restart") THEN
       CALL CCPL_report_error(pom_state_variables(pom_grid_id)%comp_id,.false., "RunType must be one of initial and restart")  !zhaobiao, C-Coupler2
   ENDIF
   IF (RunType == "initial") THEN
       ISCAN = SCAN_FILE(FNAME,"Initial_DataFile",CVAL = Initial_DataFile)
       IF (ISCAN == 0) THEN
           Has_Initial_Data = .true.
            WRITE(6,*) "Initial_DataFile is :",Initial_DataFile,Has_Initial_Data
       ELSE
           Has_Initial_Data = .false.
            WRITE(6,*) "Initial_DataFile is :",Has_Initial_Data
       ENDIF
   END IF

!------------------------------------------------------------------------------|
!  set input and output data path 
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"INPUT",CVAL = input)
     WRITE(6,*) "INPUT is :",input
   IF(ISCAN /= 0) CALL CCPL_report_error(pom_state_variables(pom_grid_id)%comp_id,.false., "ERROR in READING INPUT")  !zhaobiao, C-Coupler2

   ISCAN = SCAN_FILE(FNAME,"OUTPUT",CVAL = output)
     WRITE(6,*) "OUTPUT is :",output
   IF(ISCAN /= 0) CALL CCPL_report_error(pom_state_variables(pom_grid_id)%comp_id,.false., "ERROR in READING OUTPUT")  !zhaobiao, C-Coupler2

!------------------------------------------------------------------------------|
!     "GRID" !!
!------------------------------------------------------------------------------|
   !!!ISCAN = SCAN_FILE(FNAME,"GRID",FSCAL = GRID)
   ISCAN = SCAN_FILE(FNAME,"IGRID",ISCAL = IGRID )
     WRITE(6,*) "IGRID is :",IGRID
   GRID=1./IGRID
   IF(ISCAN /= 0) CALL CCPL_report_error(pom_state_variables(pom_grid_id)%comp_id,.false., "ERROR in READING IGRID")  !zhaobiao, C-Coupler2

!------------------------------------------------------------------------------|
!     "LON" !! DOMAIN RANGE IN LONGITUDE 
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"LON",FVEC=REALVEC,NSZE=KTEMP)
   IF(ISCAN /= 0) CALL CCPL_report_error(pom_state_variables(pom_grid_id)%comp_id,.false., "ERROR in READING LON")  !zhaobiao, C-Coupler2
   LON_DOM(1:2)=REALVEC(1:2)

!------------------------------------------------------------------------------|
!     "LAT" !! DOMAIN RANGE IN LATITUDE 
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"LAT",FVEC=REALVEC,NSZE=KTEMP)
   IF(ISCAN /= 0) CALL CCPL_report_error(pom_state_variables(pom_grid_id)%comp_id,.false., "ERROR in READING LAT")  !zhaobiao, C-Coupler2
   LAT_DOM(1:2)=REALVEC(1:2)

!------------------------------------------------------------------------------|
!     "SIGMA_LAYER" 
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"SIGMA_LAYER",ISCAL = KB)
     WRITE(6,*) "SIGMA_LAYER is :",KB
   IF(ISCAN /= 0) CALL CCPL_report_error(pom_state_variables(pom_grid_id)%comp_id,.false., "ERROR in READING SIGMA_LAYER")  !zhaobiao, C-Coupler2
   KBM1=KB-1
   KBM2=KB-2
!------------------------------------------------------------------------------|
!     "NUMX" !! 
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"NUMX",ISCAL = NumXCpu )
     WRITE(6,*) "NUMX is :",NumXCpu
   IF(ISCAN /= 0) CALL CCPL_report_error(pom_state_variables(pom_grid_id)%comp_id,.false., "ERROR in READING NUMX")  !zhaobiao, C-Coupler2
!------------------------------------------------------------------------------|
!     "NUMY" !! 
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"NUMY",ISCAL = NumYCpu )
     WRITE(6,*) "NUMY is :",NumYCpu
   IF(ISCAN /= 0) CALL CCPL_report_error(pom_state_variables(pom_grid_id)%comp_id,.false., "ERROR in READING NUMY")  !zhaobiao, C-Coupler2
!------------------------------------------------------------------------------|
!  set the grads number in longitude (IM) and the JM ( the number of the lat)
!------------------------------------------------------------------------------|
   IM = INT((LON_DOM(2)-LON_DOM(1))*IGRID)+1
   JM = INT((LAT_DOM(2)-LAT_DOM(1))*IGRID)+1
   !IM = INT((LON_DOM(2)-LON_DOM(1))/GRID)+2
   !JM = INT((LAT_DOM(2)-LAT_DOM(1))/GRID)+2
   write(6,*),"IM=",im,"JM=",JM,LON_DOM,LAT_DOM,GRID
!------------------------------------------------------------------------------|
!     "ISPLIT" !! 
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"ISPLIT",ISCAL = ISPLIT )
     WRITE(6,*) "ISPLIT is :",ISPLIT
   IF(ISCAN /= 0) CALL CCPL_report_error(pom_state_variables(pom_grid_id)%comp_id,.false., "ERROR in READING ISPLIT")  !zhaobiao, C-Coupler2
   ISPI = 1.E0/FLOAT(ISPLIT)
   ISP2I = 1.E0/(2.E0*FLOAT(ISPLIT))
!------------------------------------------------------------------------------|
!  read time step 
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"TIME_STEP",FSCAL = DTE)
     WRITE(6,*) "TIME_STEP is :",DTE
   IF(ISCAN /= 0) CALL CCPL_report_error(pom_state_variables(pom_grid_id)%comp_id,.false., "ERROR in READING TIME_STEP")  !zhaobiao, C-Coupler2
   DTI = DTE*FLOAT(ISPLIT)
   DTE2 = DTE*2
   DTI2 = DTI*2
!------------------------------------------------------------------------------|
!  START_TIME set the time of the of start run point 
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"START_TIME",CVAL = STRTIME)
     WRITE(6,*) "START_TIME is :",STRTIME
     READ(STRTIME(1:4),*) refyear
     WRITE(6,*) "START year  is :",refyear,STRTIME(1:4)
   IF(ISCAN /= 0) CALL CCPL_report_error(pom_state_variables(pom_grid_id)%comp_id,.false., "ERROR in READING START_TIME")  !zhaobiao, C-Coupler2
!!!   CALL INIT_TIME_MOD([2003,1,13,4,00,50])
   CALL INIT_TIME_MOD([0000,1,1,00,0,0])
   TTVEC=DATEVEC(TRIM(STRTIME),31)
   Y1=TTVEC(1);M1=TTVEC(2);DD1=TTVEC(3)
   H1=TTVEC(4);MM1=TTVEC(5);S1=TTVEC(6)
   T1=DATENUM(TTVEC,1)
!------------------------------------------------------------------------------|
!  RUN TIME 
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"RUN_TIME",ISCAL = RUN_TIME)
     WRITE(6,*) "RUN_TIME is :",RUN_TIME
   IF(ISCAN /= 0) CALL CCPL_report_error(pom_state_variables(pom_grid_id)%comp_id,.false., "ERROR in READING RUN_TIME")  !zhaobiao, C-Coupler2
   IF(RUN_TIME<0)THEN
   T2=T1-RUN_TIME/24.
   ELSE
   T2=T1+RUN_TIME
   END IF
!------------------------------------------------------------------------------|
!  INTERVAL_OUT
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"INTERVAL_OUT",ISCAL = INTERVAL_OUT)
     WRITE(6,*) "INTERVAL_OUT is :",INTERVAL_OUT
   IF(ISCAN /= 0) CALL CCPL_report_error(pom_state_variables(pom_grid_id)%comp_id,.false., "ERROR in READING INTERVAL_OUT")  !zhaobiao, C-Coupler2
!------------------------------------------------------------------------------|
!  OUT_DATA_FIG
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"OUT_DATA_OCN_FIG",CVEC=CC,NSZE = KTEMP)
   IF(ISCAN /= 0) CALL CCPL_report_error(pom_state_variables(pom_grid_id)%comp_id,.false., "ERROR in READING OUT_DATA_FIG")  !zhaobiao, C-Coupler2
   ALLOCATE(OUT_DATA_OCN_FIG(KTEMP))
   DO I=1,KTEMP
   IF(CC(I)=="T".OR.CC(I)=="t")THEN
   OUT_DATA_OCN_FIG(I)=.TRUE.
   ELSE
   OUT_DATA_OCN_FIG(I)=.FALSE.
   END IF
   END DO
!-----------------------------------------------------------------------------!
!  open boundary conditions flag
!-----------------------------------------------------------------------------!
   ISCAN = SCAN_FILE(FNAME,"OBC_FLG",CVEC=CC,NSZE = KTEMP)
   IF(ISCAN /= 0) CALL CCPL_report_error(pom_state_variables(pom_grid_id)%comp_id,.false., "ERROR in READING OBC_FLG")  !zhaobiao, C-Coupler2
        WRITE(6,*) "OBC_FLG is :",CC(1),CC(2),CC(3),CC(4)
   DO I=1,KTEMP
   IF(CC(I)=="T".OR.CC(I)=="t") THEN
     BCTYPE(I)=.TRUE.
   ELSE IF (CC(I)=="F".OR.CC(I)=="f") THEN
     BCTYPE(I)=.FALSE.
   ELSE
     WRITE(6,*) "error input OBC_FLG,it shuold be F or T"
     STOP  
   END IF
   END DO
!------------------------------------------------------------------------------|
!  INTERVAL_RESTART 
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"INTERVAL_RESTART",ISCAL = INTERVAL_RES)
     WRITE(6,*) "INTERVAL_RESTART is :",INTERVAL_RES
   IF(ISCAN /= 0) CALL CCPL_report_error(pom_state_variables(pom_grid_id)%comp_id,.false., "ERROR in READING INTERVAL_RESTART")  !zhaobiao, C-Coupler2
!------------------------------------------------------------------------------|
!  MODE 
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"MODE",ISCAL = MODE)
     WRITE(6,*) "MODE is :",MODE
   IF(ISCAN /= 0) CALL CCPL_report_error(pom_state_variables(pom_grid_id)%comp_id,.false., "ERROR in READING MODE")  !zhaobiao, C-Coupler2
!------------------------------------------------------------------------------|
!  INTERVAL_FORCING 
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"INTERVAL_FORCING",ISCAL = INTERVAL_FORCING)
     WRITE(6,*) "INTERVAL_FORCING is :",INTERVAL_FORCING
   IF(ISCAN /= 0) CALL CCPL_report_error(pom_state_variables(pom_grid_id)%comp_id,.false., "ERROR in READING INTERVAL_FORCING")  !zhaobiao, C-Coupler2
!------------------------------------------------------------------------------|
!  ISPADV 
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"ISPADV",ISCAL = ISPADV)
     WRITE(6,*) "ISPADV is :",ISPADV
   IF(ISCAN /= 0) CALL CCPL_report_error(pom_state_variables(pom_grid_id)%comp_id,.false., "ERROR in READING ISPADV")  !zhaobiao, C-Coupler2
!------------------------------------------------------------------------------|
!  TIDETYPE 
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"TIDETYPE",LVAL = TIDETYPE)
     WRITE(6,*) "TIDETYPE is :",TIDETYPE
    IF(ISCAN /= 0) CALL CCPL_report_error(pom_state_variables(pom_grid_id)%comp_id,.false., "ERROR in READING TIDETYPE")  !zhaobiao, C-Coupler2
!------------------------------------------------------------------------------|
!  WINDTYPE 
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"WINDTYPE",LVAL = WINDTYPE)
     WRITE(6,*) "WINDTYPE is :",WINDTYPE
   IF(ISCAN /= 0) CALL CCPL_report_error(pom_state_variables(pom_grid_id)%comp_id,.false., "ERROR in READING WINDTYPE")  !zhaobiao, C-Coupler2
!------------------------------------------------------------------------------|
!  HEATTYPE 
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"HEATTYPE",LVAL = HEATTYPE)
     WRITE(6,*) "HEATTYPE is :",HEATTYPE
   IF(ISCAN /= 0) CALL CCPL_report_error(pom_state_variables(pom_grid_id)%comp_id,.false., "ERROR in READING HEATTPYE")  !zhaobiao, C-Coupler2
!------------------------------------------------------------------------------|
!  INITIAL DATA OF T AND S 
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"INITIAL_TS",LVAL = TSTYPE)
     WRITE(6,*) "INITIAL_TS is :",TSTYPE
   IF(ISCAN /= 0) CALL CCPL_report_error(pom_state_variables(pom_grid_id)%comp_id,.false., "ERROR in READING INITIAL_TS")  !zhaobiao, C-Coupler2
!------------------------------------------------------------------------------!
! whether coupled with wave model using BV   ----edited by zhaobiao
!------------------------------------------------------------------------------!
   ISCAN = SCAN_FILE(FNAME,"BVTYPE",LVAL = BVTYPE)
     WRITE(6,*) "BVTYPE is :",BVTYPE
   IF(ISCAN /= 0) CALL CCPL_report_error(pom_state_variables(pom_grid_id)%comp_id,.false., "ERROR in READING BVTYPE")  !zhaobiao, C-Coupler2
!------------------------------------------------------------------------------|
!  THE VERTICAL SIGMA GRID 
!------------------------------------------------------------------------------|
!!   ISCAN = SCAN_FILE(FNAME,"ZSS",FVEC=REALVEC,NSZE=KTEMP)
!!  IF(ISCAN /= 0)  CALL CCPL_report_error(pom_state_variables(pom_grid_id)%comp_id,.false., "ERROR in READING ZSS")  !zhaobiao, C-Coupler2
!!
!!  IF(MSR)THEN
!!   IF(KTEMP /= KB)THEN
!!       WRITE(*,*)'NUMBER OF SPECIFIED DEPTHS IN DEPTHSL IS NOT EQUAL TO KSL'
!!       WRITE(*,*)'KSL: ',KB
!!       WRITE(*,*)'DPTHSL: ',REALVEC       !DPTHSL
!!   END IF
!!   END IF
   ALLOCATE(ZSS(KB))
!!   ZSS(1:KB)= REALVEC(1:KB)
   FileName=trim(input)//'initial/'//trim(ModelName)//'_topo.nc'
   CALL NC_READ(FileName,'depth',zss)
   ZSS=ZSS*(-1.)
!!   IF(MSR)WRITE(6,*)KB
!!   IF(MSR)WRITE(6,*)ZSS


   RETURN
   END SUBROUTINE DATA_RUN    
!------------------------------------------------------------------------------|
