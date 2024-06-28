!==============================================================================|
!   Input Parameters Which Control the Model Run                               |
!==============================================================================|

   SUBROUTINE DATA_RUN_WAVE            

!------------------------------------------------------------------------------|

   USE data_kind_mod_wave 
   USE LIMS_WAVE
   USE CONTROL_WAVE
   USE ALL_VAR_WAVE
   USE MOD_UTILS_WAVE
   USE MOD_INP_WAVE
   USE parallel_mod_wave
   USE time_mod
   USE CCPL_interface_mod, only : CCPL_report_error                       !zhaobiao, c-coupler2
   USE coupling_wave_mod
   IMPLICIT NONE
   REAL(kind_r4) REALVEC(150)
   INTEGER  INTVEC(150),ISCAN,KTEMP
   CHARACTER(LEN=120) :: FNAME
   CHARACTER(LEN=80) :: STRTIME
   CHARACTER(LEN=80) :: CC(150) 
   CHARACTER(LEN=19) :: STRTT 
   INTEGER I
   INTEGER TTVEC(6)


!==============================================================================|
!   READ IN VARIABLES AND SET VALUES                                           |
!==============================================================================|

   FNAME = "runset.dat"

!------------------------------------------------------------------------------|
!  Model name 'ModelVersion'
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"ModelVersion",CVAL = ModelName)
   IF(ISCAN /= 0) CALL CCPL_report_error(masnum_state_variables(masnum_grid_id)%comp_id,.false., "ERROR READING Modelversion")  !zhaobiao, C-Coupler2
!------------------------------------------------------------------------------|
!  Model run type, must be one of "initial" and "restart" 
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"RunType",CVAL = RunType)
   IF(ISCAN /= 0) CALL CCPL_report_error(masnum_state_variables(masnum_grid_id)%comp_id,.false., "ERROR in READING RunType")  !zhaobiao, C-Coupler2
   IF (RunType /= "initial" .and. RunType /= "restart") THEN
       CALL CCPL_report_error(masnum_state_variables(masnum_grid_id)%comp_id,.false., "RunType must be initial or restart")  !zhaobiao, C-Coupler2
   ENDIF
   IF (RunType == "initial") THEN
       ISCAN = SCAN_FILE(FNAME,"Initial_DataFile",CVAL = Initial_DataFile)
       IF (ISCAN == 0) THEN
           Has_Initial_Data = .true.
       ELSE
           Has_Initial_Data = .false.
       ENDIF
   END IF

!------------------------------------------------------------------------------|
!  set input and output data path
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"INPUT",CVAL = input)
   IF(ISCAN /= 0) CALL CCPL_report_error(masnum_state_variables(masnum_grid_id)%comp_id,.false., "ERROR READING INPUT: ")  !zhaobiao, C-Coupler2
   ISCAN = SCAN_FILE(FNAME,"OUTPUT",CVAL = output)
   IF(ISCAN /= 0) CALL CCPL_report_error(masnum_state_variables(masnum_grid_id)%comp_id,.false., "ERROR READING OUTPUT: ")  !zhaobiao, C-Coupler2
   ISCAN = SCAN_FILE(FNAME,"DOMAIN",LVAL = DomainFig)
     WRITE(6,*) "DomainFig is :", DomainFig
   IF(ISCAN /= 0) CALL CCPL_report_error(masnum_state_variables(masnum_grid_id)%comp_id,.false., "ERROR READING DomainFig: ")
   IF(DomainFig)THEN
   WRITE(6,*) "Domain is Region"
   ELSE
   WRITE(6,*) "Domain is Global"
   ENDIF

!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"IM",ISCAL = IM )
     WRITE(6,*) "IM is :",IM
   IF(ISCAN /= 0) CALL CCPL_report_error(masnum_state_variables(masnum_grid_id)%comp_id,.false.,"ERROR READING IM: ")  !zhaobiao, C-Coupler2
   ISCAN = SCAN_FILE(FNAME,"JM",ISCAL = JM )
      WRITE(6,*) "JM is :",JM
   IF(ISCAN /= 0) CALL CCPL_report_error(masnum_state_variables(masnum_grid_id)%comp_id,.false.,"ERROR READING JM: ")  !zhaobiao, C-Coupler2
!------------------------------------------------------------------------------|
!     "NUMX" !! 
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"NUMX",ISCAL = NumXCpu )
   IF(ISCAN /= 0) CALL CCPL_report_error(masnum_state_variables(masnum_grid_id)%comp_id,.false., "ERROR READING NUMX: ")  !zhaobiao, C-Coupler2
!------------------------------------------------------------------------------|
!     "NUMY" !! 
!------------------------------------------------------------------------------|
    ISCAN = SCAN_FILE(FNAME,"NUMY",ISCAL = NumYCpu )
   IF(ISCAN /= 0) CALL CCPL_report_error(masnum_state_variables(masnum_grid_id)%comp_id,.false., "ERROR READING NUMY: ")  !zhaobiao, C-Coupler2
!------------------------------------------------------------------------------|
!    "KSL" !! 
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"KSL",ISCAL = KB)
   IF(ISCAN /= 0) CALL CCPL_report_error(masnum_state_variables(masnum_grid_id)%comp_id,.false., "ERROR READING KSL: ")  !zhaobiao, C-Coupler2
!------------------------------------------------------------------------------|
!     read model z 
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"DEPTHSL",FVEC=REALVEC,NSZE=KTEMP)
   IF(ISCAN /= 0) CALL CCPL_report_error(masnum_state_variables(masnum_grid_id)%comp_id,.false., "ERROR READING DEPTHSL: ")  !zhaobiao, C-Coupler2
   IF(KTEMP /= KB)THEN
     WRITE(6,*) 'NUMBER OF SPECIFIED DEPTHS IN DEPTHSL IS NOT EQUAL TO KSL' 
     WRITE(6,*) 'KSL: ',KB
     WRITE(6,*) 'DPTHSL: ',REALVEC       !DPTHSL
   END IF
   ALLOCATE(ZBV(KB))
   ZBV(1:KB)= REALVEC(1:KB)
!------------------------------------------------------------------------------|
!  read time step 
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"TIME_STEP",FSCAL = DELTTM)
   IF(ISCAN /= 0) CALL CCPL_report_error(masnum_state_variables(masnum_grid_id)%comp_id,.false., "ERROR READING TIME_STEP: ")  !zhaobiao, C-Coupler2
!------------------------------------------------------------------------------|
!  START_TIME set the time of the of start run point 
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"START_TIME",CVAL = STRTIME)
   IF(ISCAN /= 0) CALL CCPL_report_error(masnum_state_variables(masnum_grid_id)%comp_id,.false., "ERROR READING START_TIME: ")  !zhaobiao, C-Coupler2
   TTVEC=DATEVEC(TRIM(STRTIME),31)
   Y1=TTVEC(1);M1=TTVEC(2);DD1=TTVEC(3)
   H1=TTVEC(4);MM1=TTVEC(5);S1=TTVEC(6)
   T1=DATENUM(TTVEC,1)
!------------------------------------------------------------------------------|
!  RUN TIME 
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"RUN_TIME",ISCAL = RUN_TIME)
   IF(ISCAN /= 0) CALL CCPL_report_error(masnum_state_variables(masnum_grid_id)%comp_id,.false., "ERROR READING RUN_TIME: ")  !zhaobiao, C-Coupler2
   IF(RUN_TIME<0)THEN
   T2=T1-RUN_TIME/24.
   ELSE
   T2=T1+RUN_TIME
   END IF
!------------------------------------------------------------------------------|
!  INTERVAL_OUT
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"INTERVAL_OUT",ISCAL = INTERVAL_OUT)
   IF(ISCAN /= 0) CALL CCPL_report_error(masnum_state_variables(masnum_grid_id)%comp_id,.false., "ERROR READING INTERVAL_OUT: ")  !zhaobiao, C-Coupler2
!------------------------------------------------------------------------------|

!------------------------------------------------------------------------------|
!  OUT_DATA_FIG
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"OUT_DATA_FIG",CVEC=CC,NSZE = KTEMP)
   IF(ISCAN /= 0) CALL CCPL_report_error(masnum_state_variables(masnum_grid_id)%comp_id,.false., "ERROR OUT_DATA_FIG: ")  !zhaobiao, C-Coupler2
   ALLOCATE(OUT_DATA_FIG(KTEMP))
   DO I=1,KTEMP
   IF(CC(I)=="T".OR.CC(I)=="t")THEN
   OUT_DATA_FIG(I)=.TRUE.
   ELSE
   OUT_DATA_FIG(I)=.FALSE.
   END IF
   END DO
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"ISBREAK",LVAL = ISBREAK)
     WRITE(6,*) "ISBREAK is :",ISBREAK
   IF(ISCAN /= 0) CALL CCPL_report_error(masnum_state_variables(masnum_grid_id)%comp_id,.false., "ERROR READING ISBREAK: ")  !zhaobiao, C-Coupler2
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"ISSWELL",LVAL = ISSWELL)
     WRITE(6,*) "ISSWELL is :",ISSWELL
   IF(ISCAN /= 0) CALL CCPL_report_error(masnum_state_variables(masnum_grid_id)%comp_id,.false., "ERROR READING ISSWELL: ")  !zhaobiao, C-Coupler2
!------------------------------------------------------------------------------!
   ISCAN = SCAN_FILE(FNAME,"ISCURRENT",LVAL = ISCURRENT)
     WRITE(6,*) "ISCURRENT is :",ISCURRENT
   IF(ISCAN /= 0) CALL CCPL_report_error(masnum_state_variables(masnum_grid_id)%comp_id,.false., "ERROR READING ISCURRENT: ")  !zhaobiao, C-Coupler2
!------------------------------------------------------------------------------!
!  open boudary condition type
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"OBCTYPE",ISCAL = OBCTYPE)
      WRITE(6,*) "OBCTYPE is :",OBCTYPE
   IF(ISCAN /= 0) CALL CCPL_report_error(masnum_state_variables(masnum_grid_id)%comp_id,.false., "ERRORREADING OBCTYPE: ")  !zhaobiao, C-Coupler2
!------------------------------------------------------------------------------|
!  INTERVAL_RESTART 
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"INTERVAL_RESTART",ISCAL = INTERVAL_RES)
   IF(ISCAN /= 0) CALL CCPL_report_error(masnum_state_variables(masnum_grid_id)%comp_id,.false., "ERROR READING INTERVAL_RESTART: ")  !zhaobiao, C-Coupler2
!------------------------------------------------------------------------------|
!  INTERVAL_FORCING 
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"INTERVAL_FORCING",ISCAL = INTERVAL_FORCING)
   IF(ISCAN /= 0) CALL CCPL_report_error(masnum_state_variables(masnum_grid_id)%comp_id,.false., "ERROR READING INTERVAL_FORCING: ")  !zhaobiao, C-Coupler2
!------------------------------------------------------------------------------|
!  WIND NAME AND STRING TIME NAME 
!------------------------------------------------------------------------------|
   ISCAN = SCAN_FILE(FNAME,"FORCING_NAME",CVEC = CC,NSZE = KTEMP )
   IF(ISCAN /= 0) CALL CCPL_report_error(masnum_state_variables(masnum_grid_id)%comp_id,.false., "ERROR READING RESTART: ")  !zhaobiao, C-Coupler2
   FORCE_NAME(1) = TRIM(CC(1))
   FORCE_NAME(2) = TRIM(CC(2))
   FORCE_NAME(3) = TRIM(CC(3))

!------------------------------------------------------------------------------|
!  caculated gauss ditribution for wave breaking subroutine
!------------------------------------------------------------------------------|
   CALL GAUSS_DISTRIBUTION

   RETURN
   END SUBROUTINE DATA_RUN_WAVE 
!------------------------------------------------------------------------------|
