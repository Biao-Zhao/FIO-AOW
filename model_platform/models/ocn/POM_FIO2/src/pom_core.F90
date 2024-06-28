 SUBROUTINE POM_CORE
 USE DATA_KIND_MOD
 USE MOD_IO
 USE CONTROL
 USE LIMS
 USE CONST
 USE ALL_VAR
 USE TIME_MOD
 USE PARALLEL_MOD
 USE CCPL_interface_mod   !zhaobiao, C-Coupler2 
 USE coupling_pom_mod
 !USE CHECK_SUM_MOD      !zhaobiao, C-Coupler2

 IMPLICIT NONE
 INCLUDE 'netcdf.inc'  
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 INTEGER(kind_in) :: IINT,IEND
 INTEGER(kind_in) :: FSTEP,OUTSTEP,RESSTEP,STEP1,STEP2,STEP3
 REAL(kind_r4) :: MAXV,MINV,MAXU,MINU,MAXT,MINT,MAXWINDU, &
      MAXWINDV,MINWINDU,MINWINDV,MAXSA,MINSA,MAXBV,MINBV
 REAL(kind_r4),DIMENSION(ISLON:IELON,ISLAT:IELAT) :: AVEL 
 REAL(kind_r4),DIMENSION(ISLON:IELON,ISLAT:IELAT,KB) :: AVT,AVS,AVU,AVV
 INTEGER TTVEC(6),FLREC,NUMOUT,OUTLREC
 CHARACTER*200 FileName,OutFile,OUTFILE_RES
 CHARACTER*10 DATE 
 LOGICAL LEXIST
 REAL TIME1,TIME2,RUNTIME,MAXTIME
 INTEGER seconds
 CHARACTER*15 FLG_RES
 FLG_RES='_XXXX_XXXX_XXXX'

!----------------------------------------------------------------------
!     ESTABLISH PROBLEM CHARACTERISTICS
!     ****** ALL UNITS IN M.K.S. SYSTEM ******
! F,BLANK AND B REFERS TO FORWARD,CENTRAL AND BACKWARD TIME LEVELS.
!----------------------------------------------------------------------
! LOCATATE FOR MEMORY
  CALL ALLOCATE_VARIABLE
  WRITE(6,*)'Step-1: LOCATE MEMORY OVER'
!----------------------------------------------------------------------
! READ TOPOGRAPHY DATA AND SET THE INITIAL MODEL GRID
! SET FSM ARU,ARV,DUM,DVM,COR,DX,DY,ART X Y
!----------------------------------------------------------------------
  CALL SET_TOPO
  WRITE(6,*)'Step-2: SET CONST VARIABLE OVER' 
!----------------------------------------------------------------------
! SET INITIAL CONDITIONS
!----------------------------------------------------------------------
  CALL INITIAL
  WRITE(6,*)'Step-3: Set cool start initial condition over' 
!----------------------------------------------------------------------
! SET THE BOTTOM ROUGHNESS --CBC 
!----------------------------------------------------------------------
  CALL SET_CBC
  WRITE(6,*)'Step-4: Set bottom roughness over' 
!----------------------------------------------------------------------
! Evaluate external CFL time step
!----------------------------------------------------------------------
  CALL CFL
  WRITE(6,*)'Step-5: Set CFL time over' 
!----------------------------------------------------------------------
! THE FOLLOWING DATA ARE NEEDED FOR A SEAMLESS RESTART
! IF RESTART=T DATA HAD BEEN CREATED BY A PREVIOUS RUN 
! at END of this PROGRAM. RESTART=F DENOTES A FIRST TIME RUN.
!----------------------------------------------------------------------

  IF (RunType == "initial" .and. (.not. Has_Initial_Data)) THEN
      WRITE(6,*)'Initial run without initial data '
  ELSE
      IF (RunType == "initial") THEN
         FileName= trim(input)//'/start/'//trim(Initial_DataFile)//' '
         WRITE(6,*)'Read Initial Data File in: ',Trim(FileName)
      ELSE
         DATE=DATESTR(DATEVEC(T1,1),-1)
         FileName= trim(output)//'restart/'//TRIM(ModelNAME)//'.res.'//DATE//'.nc'
         WRITE(6,*)'Read Restart File in: ',Trim(FileName)
      ENDIF
      CALL READ_RES(FileName)
      CALL BARRIER

      CALL COMM_2D(CBC,1,1)
      CALL COMM_2D(EL,1,1)
      CALL COMM_2D(ELB,1,1)
      DO J=ISLAT,IELAT
      DO I=ISLON,IELON
         D(I,J)=H(I,J)+EL(I,J)
         DT(I,J)=H(I,J)+ET(I,J) 
      END DO
      END DO
      WRITE(6,*)'Step-6: Read HOT START/RESTART DATA over'
      MAXT=EXTREMUM(T(ISLON:IELON,ISLAT:IELAT,:),'MAX') 
      MINT=EXTREMUM(T(ISLON:IELON,ISLAT:IELAT,:),'MIN') 
      WRITE(6,*)"START MAX MIN TEMP",MAXT,MINT
      MAXT=EXTREMUM(TB(ISLON:IELON,ISLAT:IELAT,:),'MAX') 
      MINT=EXTREMUM(TB(ISLON:IELON,ISLAT:IELAT,:),'MIN') 
      WRITE(6,*)"START MAX MIN TEMP",MAXT,MINT
  END IF
!----------------------------------------------------------------------
! SET INITIAL AND LATERAL BOUNDARY CONDITIONS
!----------------------------------------------------------------------
  CALL INITIAL_BC
  WRITE(6,*)'Step-7: Set lateral boundary condition over'
!----------------------------------------------------------------------
! READ TIDE CONDITION
!----------------------------------------------------------------------
  IF(TIDETYPE)THEN
    CALL INITIAL_TIDE
    WRITE(6,*)'Step-8: Set tide boundary condition over'
  END IF
!----------------------------------------------------------------------
! READ CLIMATIC WIND STRESS AND HEAT FLUX AND SEA SURFACE TEMPERATURE
! IN HEAT STRX STRY TEMP DQDT  
!CALL INITIAL_SURF_BNDRY
  CALL BARRIER
  WRITE(6,*)'Step-9: Set surface boundary condition over'
!**********************************************************************
!                                                                     *
!  BEGIN NUMERICAL INTEGRATION                                         *
!                                                                     *
!**********************************************************************
! SET SOME PARAMETER FOR READ WIND, WRITE RESTART, 
! WRITE RESULT AND SO ON
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  FSTEP=0;OUTSTEP=0;RESSTEP=0
  STEP1=(INTERVAL_FORCING*3600)/DTI
  STEP2=(INTERVAL_OUT*3600)/DTI
  STEP3=(INTERVAL_RES*3600)/DTI
  IEND=NINT(((T2-T1)*24*3600)/DTI)+1
  AVT=0.;AVS=0;AVEL=0;AVU=0.;AVV=0.;NUMOUT=0
  CALL register_component_coupling_configuration    !zhaobiao,c-coupler2
  CALL run_coupling               !zhaobiao, c-coupler2
 !!DO IINT=1,IEND
  IINT=0
 
DO WHILE(.NOT.CCPL_is_model_run_ended(pom_state_variables(pom_grid_id)%comp_id,annotation="check last step") )      !zhaobiao, c-coupler2
  IINT=IINT+1
! call CHECK_SUM_VARIABLES(IINT, 'at beginning of timeloop')                                                          ! zhaobiao,c-coupler2 ????????????????????
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
! SET MODEL TIME
  CALL CPU_TIME(TIME1)

  CALL CCPL_get_current_time(pom_state_variables(pom_grid_id)%comp_id, YEAR, MONTH, DAY, seconds, annotation="get the start year,month,day,seccond") !zhaobiao, c-coupler2
  HOUR=seconds/3600
  MINS=(seconds-HOUR*3600)/60
  SEC=seconds-HOUR*3600-MINS*60
  TT=DATENUM([YEAR,MONTH,DAY,HOUR,MINS,SEC],1)

!  TT=T1+((IINT)*DTI)/24./3600.
!  TTVEC=DATEVEC(TT,1) 
!  YEAR=TTVEC(1);MONTH=TTVEC(2);DAY=TTVEC(3)
!  HOUR=TTVEC(4);MINS=TTVEC(5);SEC=TTVEC(6)
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  CALL LEAP(YEAR,MON)
  FACT=1.
 
  WRITE(6,*) CCPL_get_number_of_current_step(pom_state_variables(pom_grid_id)%comp_id,annotation="get current number of time step")," NOW CALCULAT TIME:",DATESTR(TT,1)

  MAXT=EXTREMUM(T(ISLON:IELON,ISLAT:IELAT,:),'MAX',FIJK='t')
  MINT=EXTREMUM(T(ISLON:IELON,ISLAT:IELAT,:),'MIN',FIJK='t')
  MAXSA=EXTREMUM(S(ISLON:IELON,ISLAT:IELAT,:),'MAX')
  MINSA=EXTREMUM(S(ISLON:IELON,ISLAT:IELAT,:),'MIN')
  MAXBV=EXTREMUM(BV(ISLON:IELON,ISLAT:IELAT,:),'MAX')
  MINBV=EXTREMUM(BV(ISLON:IELON,ISLAT:IELAT,:),'MIN')
  MAXU=EXTREMUM(U(ISLON:IELON,ISLAT:IELAT,:),'MAX')
  MINU=EXTREMUM(U(ISLON:IELON,ISLAT:IELAT,:),'MIN')
  MAXV=EXTREMUM(V(ISLON:IELON,ISLAT:IELAT,:),'MAX')
  MINV=EXTREMUM(V(ISLON:IELON,ISLAT:IELAT,:),'MIN')
  MAXWINDU=EXTREMUM(WUSURF(ISLON:IELON,ISLAT:IELAT),'MAX')
  MINWINDU=EXTREMUM(WUSURF(ISLON:IELON,ISLAT:IELAT),'MIN')
  MAXWINDV=EXTREMUM(WVSURF(ISLON:IELON,ISLAT:IELAT),'MAX')
  MINWINDV=EXTREMUM(WVSURF(ISLON:IELON,ISLAT:IELAT),'MIN')

  IF(MOD(IINT-1,5)==0)THEN
!    WRITE(6,'(A6,I7)') 'IINT:',IINT
!    WRITE(6,*),'MAX-MIN TEMP: ',MAXT,MINT
!    WRITE(6,*),'MAX-MIN SALT: ',MAXSA,MINSA
!    WRITE(6,*),'MAX-MIN BV: ',MAXBV,MINBV
!    WRITE(6,*),'MAX-MIN U: ',MAXU,MINU
!    WRITE(6,*),'MAX-MIN V: ',MAXV,MINV
!    WRITE(6,*),'MAX-MIN WINDU: ',MAXWINDU,MINWINDU
!    WRITE(6,*),'MAX-MIN WINDV: ',MAXWINDV,MINWINDV
  END IF

! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
! WRITE RESTART SECTION
! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  IF(RESSTEP==STEP3)RESSTEP=0
  IF(RESSTEP==0.AND.IINT/=1)THEN
  WRITE(6,*)"NOW WRITE RESTART TIME: ",DATESTR(TT,1)
   TTVEC=DateVec(TT,1)
   YEAR=TTVEC(1);MONTH=TTVEC(2);DAY=TTVEC(3);HOUR=TTVEC(4)
   DATE=DATESTR(DATEVEC(TT,1),-1)
   WRITE(DATE,'(i4.4,3i2.2)')YEAR,MONTH,DAY,HOUR
   OutFile_RES=trim(output)//'restart/'//TRIM(ModelNAME)//'.OCN.res.'//DATE//'.nc'
   CALL WRITE_RES(OutFile_RES)
  END IF
  RESSTEP=RESSTEP+1
! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
! BEGIN PRINT SECTION
! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  ! FILTER TIDE
  NUMOUT=NUMOUT+1
  AVEL(ISLON:IELON,ISLAT:IELAT)=AVEL(ISLON:IELON,ISLAT:IELAT)+ET(ISLON:IELON,ISLAT:IELAT)
  AVT(ISLON:IELON,ISLAT:IELAT,:)=AVT(ISLON:IELON,ISLAT:IELAT,:)+T(ISLON:IELON,ISLAT:IELAT,:)
  AVS(ISLON:IELON,ISLAT:IELAT,:)=AVS(ISLON:IELON,ISLAT:IELAT,:)+S(ISLON:IELON,ISLAT:IELAT,:)
  AVU(ISLON:IELON,ISLAT:IELAT,:)=AVU(ISLON:IELON,ISLAT:IELAT,:)+U(ISLON:IELON,ISLAT:IELAT,:)
  AVV(ISLON:IELON,ISLAT:IELAT,:)=AVV(ISLON:IELON,ISLAT:IELAT,:)+V(ISLON:IELON,ISLAT:IELAT,:)

  IF(OUTSTEP==STEP2)OUTSTEP=0
!  IF(OUTSTEP==0.and.IINT/=1)THEN
   IF(OUTSTEP==0)THEN
    !TTVEC=DateVec(T1,1)  ! output to single file
    TTVEC=DateVec(TT,1) ! output to a multiple files
    YEAR=TTVEC(1);MONTH=TTVEC(2);DAY=TTVEC(3);HOUR=TTVEC(4)
    WRITE(DATE,'(i4.4,3i2.2)')YEAR,MONTH,DAY,HOUR
    OutFile=trim(output)//TRIM(ModelNAME)//'.OCN.output.'//DATE//'.nc'
    WRITE(6,*)"NOW WRITE RESULT TIME: ",DATESTR(TT,1)
    WRITE(6,*)"NOW WRITE RESULT IN: ",TRIM(OutFile)
    AVEL=AVEL/NUMOUT;AVT=AVT/NUMOUT
    AVS=AVS/NUMOUT;AVU=AVU/NUMOUT
    AVV=AVV/NUMOUT
    CALL OutNcFile(OutFile,AVT,AVS,AVU,AVV,AVEL)
    AVT=0.;AVS=0;AVEL=0;AVU=0.;AVV=0.;NUMOUT=0
  END IF
  OUTSTEP=OUTSTEP+1
! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
! END WRITE DATA SECTION
! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

! SET LATERAL BOUNDARY CONDITIONS
! OUTPUT TBS TBN TBE TBW SBS SBN SBE SBW
! OUTPUT ELS ELN ELE ELW UABE UABW VABS VABN
  TTVEC=DATEVEC(TT,1)
  YEAR=TTVEC(1);MONTH=TTVEC(2);DAY=TTVEC(3)
  HOUR=TTVEC(4);MINS=TTVEC(5);SEC=TTVEC(6)
  IF(HOUR==0.AND.MINS==0.AND.SEC==0)THEN
    WRITE(6,*) "UPDATED BOUNDARY DATA FIELDS!"
    CALL READ_BC(TT)
  !!CALL OUTNCBC
  END IF
  CALL SETBC
!-----------------------------------------
! SET SURFACE BOUNDARY CONDITIONS
! IF(WINDTYPE)THEN
!   CALL SET_SURF_BNDRY_CLI
! ELSE
!!   IF(FSTEP==STEP1)FSTEP=0
!!   IF(FSTEP==0)THEN
!!     !WRITE(FileName,'(a<len_trim(input)>,a22)')trim(input),'wind/ncep_wind_k_cu.nc'
!!     FileName=trim(input)//'wind/ncep_wind_k_cu.nc'
!!     !FLREC = INT(((TT-T1)*24)/INTERVAL_FORCING)+1
!!     FLREC = INT(((DTI*IINT)/3600.)/INTERVAL_FORCING)+1
!!     WRITE(6,*),((DTI*IINT)/3600.)/INTERVAL_FORCING,(DTI*IINT)/3600
!!     WRITE(6,*)IINT,"NOW Read Wind Time: ",DATESTR(TT,1),FLREC
!!     CALL SET_SURF_BNDRY(FileName,FLREC)
!!   END IF
!!   !CALL SET_SURF_BNDRY(FileName,FLREC)
!!   FSTEP=FSTEP+1
!!   IF(IINT==1)FSTEP=FSTEP+1
! END IF
  CALL SET_SURF_BNDRY
!-----------------------------------------
! LOOP 529,SET THE BOTTOM SALT AND TEMP 
! EQUAL ThAT OF THE LAST BUT ONE BOTTOM
  S(:,:,KB)=S(:,:,KBM1)
  SB(:,:,KB)=SB(:,:,KBM1)
  T(:,:,KB)=T(:,:,KBM1)
  TB(:,:,KB)=TB(:,:,KBM1)
!-----------------------------------------
! LOOP 299 ADD FOR PARALLEL VERSION BY Guanso.
! FOR CALCULATION PRECISON, ALL OF TB,SB AND RHO
! SUBTRACT THE AVERAGE TEMP,SALT AND DENSITY
  TB=TB-TCLIM
  SB=SB-SCLIM
  RHO=RHO-RMEAN

!----------------------------------------
! ADD BY GUANSUO FOR PARALLEL VERSION
! UPDATE ALL THE VARIABLES FOR KEEP CONSISTENT
! WITH THE SERIAL CODE
! OUTPUT
  CALL COMM_INTERNAL_MODE
!----------------------------------------
! MODE = 2; 
! 2-D CALCULATION (BOTTOM STRESS CALCULATED IN ADVAVE)
  IF(MODE/=2)CALL SECTION_8001
!----------------------------------------
! LOOP 399
  EGF = EL * ISPI
! LOOP 400
! DO 400 J=2,JM
! DO 400 I=2,IM
  DO J=ISLAT,IELAT
  DO I=JSLON,IELON
     UTF(I,J)=UA(I,J)*(D(I,J)+D(I-1,J))*ISP2I
   ! VTF(I,J)=VA(I,J)*(D(I,J)+D(I,J-1))*ISP2I
  END DO
  END DO
! REPLACE  VTF(I,J)=VA(I,J)*(D(I,J)+D(I,J-1))*ISP2I
  DO J=JSLAT,IELAT
  DO I=ISLON,IELON
     VTF(I,J)=VA(I,J)*(D(I,J)+D(I,J-1))*ISP2I
  END DO
  END DO
! test ok 20130108 16:03:18
!----------------------------------------
!----------------------------------------
! EXTERNAL MODE 
!----------------------------------------
  CALL EXTERNAL_MODE
!----------------------------------------
!Continue with internal (3-D) mode calculation
!----------------------------------------
  IF(IINT==1.AND.TT==0.)GO TO 8200
  IF(MODE==2)GO TO 8200


  CALL INTERNAL_MODE(IINT)
  
  8200 CONTINUE
!----------------------------------------
! LOOP 8210
! DO 8210 J=1,JM
! DO 8210 I=1,IM
  DO J=ISLAT,IELAT
  DO I=ISLON,IELON
     EGB(I,J)=EGF(I,J)
     ETB(I,J)=ET(I,J)
     ET(I,J)=ETF(I,J)
     DT(I,J)=H(I,J)+ET(I,J)
     UTB(I,J)=UTF(I,J)
     VTB(I,J)=VTF(I,J)
  END DO
  END DO
CALL run_coupling               !zhaobiao, c-coupler2
CALL CPU_TIME(TIME2)
RUNTIME=TIME2-TIME1
MAXTIME=EXTREMUM(RUNTIME,'MAX')
!IF(MSR)WRITE(6,*)"CALCULATED TIME IS ",MAXTIME,TIME2-TIME1
! END PRINT SECTION
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
END DO

!**********************************************************************
!                                                                     *
!  END NUMERICAL INTEGRATION                                          *
!                                                                     *
!**********************************************************************
! remove memory
!
  CALL DEALLOCATE_VARIABLE

 RETURN
 END
