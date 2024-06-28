SUBROUTINE READWI
  USE CONTROL_WAVE
  USE ALL_VAR_WAVE,ONLY : WINDX,WINDY,WIND0X,WIND0Y,WX,WY,W0,W1,W,   &
                    WINC,EE,E,EA,UXX,UXY,UYY,UYX,UX,UY,H1_3,BV,      &
                    D
  USE MOD_IO_WAVE
  USE CONST_WAVE,ONLY : KL,JL,LE,ZERO
  USE parallel_mod_wave
  USE time_mod
  USE LIMS_WAVE
  USE CCPL_interface_mod                       !zhaobiao, c-coupler2
  USE coupling_wave_mod

  IMPLICIT NONE
  INTEGER(kind_in) :: COMMSTEP,STEP4
  INTEGER(kind_in) :: NUMOUT
  INTEGER(kind_in) :: OUTLREC,STEP2,STEP3
  CHARACTER*10 STRTIME,DATE
  CHARACTER*200 OUTFILE,InFile,OUTFILE_RES
  INTEGER I,J,K,L
  LOGICAL LEXIST
  REAL    MAX_BV,MAX_EE,MAX_EA
  INTEGER IINT,IEND
  LOGICAL timer_status
!============================ initialization ===========================================

  CALL run_coupling          !zhaobiao, c-coupler2
  W1=(WX**2+WY**2)**0.5

!=============================== Cold start =============================================
  IF (RunType == "initial" .and. (.not. Has_Initial_Data)) THEN
     WINC=0.
     CALL SETSPEC(1)
     DO I=1,86400/(DELTTM*60)
        CALL CCPL_get_current_time(masnum_state_variables(masnum_grid_id)%comp_id, year, month, day, second, annotation="get the start year,month,day,seccond") !zhaobiao, c-coupler2
        HOUR=SECOND/3600
        MM1=(SECOND-HOUR*3600)/60
        S1=SECOND-HOUR*3600-MM1*60
        TT=DATENUM([YEAR,MONTH,DAY,HOUR,MM1,S1],1)
        WRITE(6,*) CCPL_get_number_of_current_step(masnum_state_variables(masnum_grid_id)%comp_id,annotation="get current number of time step"), "NOW CALCULAT TIME:",DATESTR(TT,1)
!================================== read BDY data ===== ============================================       ! Guansuo 2021/02/18 
  IF(mod(CCPL_get_number_of_current_step(masnum_state_variables(masnum_grid_id)%comp_id),STEP2)==0) THEN 
     IF(OBCTYPE==2)THEN
     WRITE(6,*) CCPL_get_number_of_current_step(masnum_state_variables(masnum_grid_id)%comp_id,annotation="get current number of time step"),"NOW READ BDY DATA TIME: ",DATESTR(TT,1)
       CALL READ_BDY(TT)
     ENDIF
  ENDIF
!================================== output restart file ============================================
        CALL WAVE_RUN
        CALL CCPL_advance_time(masnum_state_variables(masnum_grid_id)%comp_id,annotation="advance the time")
     END DO
     CALL CCPL_reset_current_time_to_start_time(masnum_state_variables(masnum_grid_id)%comp_id, annotation='reset current time to start time')
!----------------------------------------------------------------------------------------
     CALL CCPL_get_current_time(masnum_state_variables(masnum_grid_id)%comp_id, year, month, day, second, annotation="get the start year,month,day,seccond") !zhaobiao, c-coupler2
!     CALL CCPL_get_start_time(masnum_state_variables(masnum_grid_id)%comp_id,year, month, day, second,annotation="get the start year,month,day,seccond")
     HOUR = SECOND/3600
     MM1=(SECOND-HOUR*3600)/60
     S1=SECOND-HOUR*3600-MM1*60
     TT=DATENUM([YEAR,MONTH,DAY,HOUR,MM1,S1],1)
     WRITE(DATE,'(i4.4,3i2.2)')YEAR,MONTH,DAY,HOUR
     OUTFILE_RES= trim(output)//TRIM(ModelNAME)//'.res.'//DATE//'.wave.nc'
     WRITE(6,*) CCPL_get_number_of_current_step(masnum_state_variables(masnum_grid_id)%comp_id,annotation="get current number of time step"),"NOW WRITE RESTART TIME:",DATESTR(TT,1) !zhaobiao, c-coupler2
     CALL WRITE_RES(OUTFILE_RES,EE(:,:,ISLON:IELON,ISLAT:IELAT))
     CALL BARRIER
!=================================== Hot start ==========================================
  ELSE  
     IF (RunType == "initial") THEN
        OUTFILE_RES= trim(input)//'/start/'//trim(Initial_DataFile)//' '
        WRITE(6,*) 'Read Initial Data File in: ',Trim(OUTFILE_RES)
     ENDIF
     CALL BARRIER
     CALL READ_RES(OUTFILE_RES,EE(:,:,ISLON:IELON,ISLAT:IELAT))
  END IF
!==================================== main program =======================================
  
  COMMSTEP=0;
  STEP4=INT(60/DELTTM)
  OUTLREC=0;EA=ZERO;NUMOUT=0
  STEP2=(INTERVAL_OUT*60)/DELTTM
  STEP3=(INTERVAL_RES*60)/DELTTM
  IINT=0
  IEND=CCPL_get_number_of_total_steps(masnum_state_variables(masnum_grid_id)%comp_id,annotation="get total numbers of total time step")
  WRITE(6,*) "ALL CALCULATE STEPS: ",CCPL_get_number_of_total_steps(masnum_state_variables(masnum_grid_id)%comp_id,annotation="get total numbers of total time step") !zhaobiao, c-coupler2
    DO WHILE (.not.CCPL_is_model_run_ended(masnum_state_variables(masnum_grid_id)%comp_id,annotation="check last step"))   !zhaobiao, c-coupler2

     ! cumulation of EA 
     NUMOUT=NUMOUT+1
     IINT=IINT+1
     !EA(:,:,ISLON:IELON,ISLAT:IELAT)=EE(:,:,ISLON:IELON,ISLAT:IELAT) &
     !                                +EA(:,:,ISLON:IELON,ISLAT:IELAT)
      DO J=ISLAT,IELAT
      DO I=ISLON,IELON
      IF(NSP(I,J)==0)CYCLE
       EA(:,:,I,J)=EE(:,:,I,J)+EA(:,:,I,J)
      END DO
      END DO
  
     CALL CCPL_get_current_time(masnum_state_variables(masnum_grid_id)%comp_id, year, month, day, second, annotation="get the start year,month,day,seccond") !zhaobiao, c-coupler2
     HOUR=SECOND/3600
     MM1=(SECOND-HOUR*3600)/60
     S1=SECOND-HOUR*3600-MM1*60
     TT=DATENUM([YEAR,MONTH,DAY,HOUR,MM1,S1],1)
     WRITE(DATE,'(i4.4,3i2.2)')YEAR,MONTH,DAY,HOUR
     MAX_BV=EXTREMUM(BV,'MAX') 
     MAX_EE=EXTREMUM(EE,'MAX') 
!     WRITE(6,*)  '1MAX BV at', CCPL_get_number_of_current_step(masnum_state_variables(masnum_grid_id)%comp_id,annotation="get current number of time step"), 'is', MAX_BV            !zhaobiao, c-couple2
!     WRITE(6,*)  '1MAX EE at', CCPL_get_number_of_current_step(masnum_state_variables(masnum_grid_id)%comp_id,annotation="get current number of time step"), 'is', MAX_EE            !zhaobiao, c-couple2
     WRITE(6,*)  CCPL_get_number_of_current_step(masnum_state_variables(masnum_grid_id)%comp_id,annotation="get current number of time step"),"NOW CALCULAT TIME: ",DATESTR(TT,1)    !zhaobiao, c-coupler2
!================================== read BDY data ===== ============================================       ! Guansuo 2021/02/18 
  IF(mod(CCPL_get_number_of_current_step(masnum_state_variables(masnum_grid_id)%comp_id),STEP2)==0) THEN 
     IF(OBCTYPE==2)THEN
        WRITE(6,*) CCPL_get_number_of_current_step(masnum_state_variables(masnum_grid_id)%comp_id,annotation="get current number of time step"),"NOW READ BDY DATA TIME: ",DATESTR(TT,1)
        CALL READ_BDY(TT)
     ENDIF
  ENDIF
!===================================================================================================       
      CALL WAVE_RUN                                   ! MASNUM-Wave main program
      timer_status=CCPL_is_timer_on(masnum_state_variables(masnum_grid_id)%timer_id,annotation="check the timer whether is on")
      if (timer_status)then
        CALL MIXTURE_COUPLE                             ! zhaobiao calculate instantaneous BV based on EE
      end if

      IF(ISSWELL)THEN
        CALL MEAN_SWELL                               ! zhaobiao seperate swell from waves
      END IF
      IF(ISBREAK)THEN   
        CALL BREAKING                                 ! zhaobiao calculate Whitecape
      END IF
!============================= output meanvalue of Bv ==============================================
  IF(mod(CCPL_get_number_of_current_step(masnum_state_variables(masnum_grid_id)%comp_id),STEP2)==0.or.iint==IEND) THEN   !zhaobiao, c-coupler2
     WRITE(6,*) CCPL_get_number_of_current_step(masnum_state_variables(masnum_grid_id)%comp_id,annotation="get current number of time step"),"NOW WRITE RESULT TIME: ",DATESTR(TT,1)   !zhaobiao. c-coupler2
     EA=EA/NUMOUT
     !CALL MIXTURE ! zhaobiao calculate averaged BV based on EA at interval of interval_out
     OUTLREC=1
     IF(iint==IEND)THEN
        HOUR=(SECOND+DELTTM*60)/3600
        MM1=(SECOND-HOUR*3600)/60
        S1=SECOND-HOUR*3600-MM1*60
        TT=DATENUM([YEAR,MONTH,DAY,HOUR,MM1,S1],1)
        WRITE(DATE,'(i4.4,3i2.2)')YEAR,MONTH,DAY,HOUR 
        OUTFILE= trim(output)//TRIM(ModelNAME)//'.out.'//DATE//'.wave.nc'
        CALL OutNcFile(OUTFILE,Lrec=OUTLREC)
     ELSE
        OUTFILE= trim(output)//TRIM(ModelNAME)//'.out.'//DATE//'.wave.nc'
        CALL OutNcFile(OUTFILE,Lrec=OUTLREC)
     END IF 
     EA=0.
     NUMOUT=0
     CALL BARRIER
  END IF
!================================== output restart file ============================================
IF(mod(CCPL_get_number_of_current_step(masnum_state_variables(masnum_grid_id)%comp_id),STEP3)==0 .and. IINT/=1) THEN    !zhaobiao, c-coupler2
     STEP3=100000
     WRITE(6,*) CCPL_get_number_of_current_step(masnum_state_variables(masnum_grid_id)%comp_id,annotation="get current number of time step"),"NOW WRITE RESTART TIME: ",DATESTR(TT,1)
     WRITE(DATE,'(i4.4,3i2.2)')YEAR,MONTH,DAY,HOUR
     OUTFILE_RES= trim(output)//'restart/'//TRIM(ModelNAME)//'.res.'//DATE//'.wave.nc'
     CALL WRITE_RES(OUTFILE_RES,EE(:,:,ISLON:IELON,ISLAT:IELAT))
     CALL BARRIER
END IF
!================================== Exchange data with other model =================================     
  CALL run_coupling                    !zhaobiao, c-coupler2  
    W1=(WX**2+WY**2)**.5
     WINC=W1-W0
!IF(CCPL_is_last_step_of_model_run(masnum_state_variables(masnum_grid_id)%comp_id, annotation="check last step masnum")) EXIT
!===================================================================================================

  END DO
  CALL DEALLOCATE_VARIABLE_WAVE
RETURN
END SUBROUTINE READWI 


SUBROUTINE WAVE_RUN
  USE LIMS_WAVE, ONLY : MYID,IM,JM
  USE CONST_WAVE, ONLY : ZERO,SMVALUE,JL,KL
  USE CONTROL_WAVE, ONLY : ISLAT,IELAT,ISLON,IELON
  USE ALL_VAR_WAVE, ONLY : EE,E,NSP,h1_3
  USE parallel_mod_wave, ONLY : COMM_DATA,COMPTEST,BARRIER,EXTREMUM
  USE data_kind_mod_wave
  IMPLICIT NONE
  INTEGER,PARAMETER :: A = 24
  INTEGER N,JJ,IA,IC
  LOGICAL LEXIST
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  WHERE(EE<=ZERO)
  EE=SMVALUE
  END WHERE
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!!    start exchange data
  CALL COMM_DATA(EE)
!!    end exchange
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  CALL PROPAGAT
!
  WHERE(E<=ZERO)
  E=SMVALUE
  END WHERE
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  CALL IMPLSCH
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!  FILTER
  CALL COMM_DATA(EE)
  E=ZERO
  DO IC=ISLAT,IELAT
  DO IA=ISLON,IELON
    IF(NSP(IA,IC)==2)E(:,:,IA,IC)=EE(:,:,IA,IC)
    IF(NSP(IA,IC)==1.and.IA-1/=0.AND.IA+1/=IM+1.AND.IC-1/=0.AND.IC+1/=JM+1)THEN
    N=A
    IF(NSP(IA-1,IC).GT.0)N=N+1
    IF(NSP(IA+1,IC).GT.0)N=N+1
    IF(NSP(IA,IC-1).GT.0)N=N+1
    IF(NSP(IA,IC+1).GT.0)N=N+1
    E(:,:,IA,IC)=(A*EE(:,:,IA,IC)+EE(:,:,IA-1,IC)+       &
                  EE(:,:,IA+1,IC)+EE(:,:,IA,IC-1)+       &
                  EE(:,:,IA,IC+1))/N
    END IF
  END DO
  END DO

  DO IC=ISLAT,IELAT
  DO IA=ISLON,IELON
    IF(NSP(IA,IC)/=0)THEN
    EE(:,:,IA,IC)=E(:,:,IA,IC)
    END IF
  END DO
  END DO

  WHERE(EE<ZERO)
  EE=SMVALUE
  END WHERE
  CALL MEAN1


  RETURN
  END
!=======================================================================      
  SUBROUTINE ALLOCATE_VARIABLE_WAVE
  USE LIMS_WAVE, ONLY : KB,IM,JM
  USE CONTROL_WAVE, ONLY : ISLON,IELON,ISLAT,IELAT
  USE CONST_WAVE, ONLY : KL,JL,LE
  USE ALL_VAR_WAVE,ONLY : WINDX,WINDY,WIND0X,WIND0Y,WX,WY,W0,W1,W,           &
                    WINC,EE,E,EA,UXX,UXY,UYY,UYX,UX,UY,H1_3,BV,              &
                    TPF,APE,AET,TAUBB11,TAUBB12,TAUBB22,TAUBB33,             &
                    UORBITAL,VORBITAL,USTOKES,VSTOKES,RA,HEN,PT,KT,          &
                    ET,BBR,MT,UST,TAUINX,TAUINY,TAUDSX,TAUDSY,               &
                    swell_hs,swell_tp,swell_th,swell_tz,                     &
                    windy_hs,windy_tp,windy_th,windy_tz,                     &
                    EHS,ETP,ETH,WHS,WTP,WTH,SHS,STP,STH,NHS,NTP,NTH                                           
                                                                             !ADDED BY ZHAOBIAO
  USE MOD_IO_WAVE
  IMPLICIT NONE
!=======================================================================
! ALLOCATE  MEMORY FOR THE ALLOCATABLE VARIABLES
!=======================================================================
! FOR FORECING DATA
  ALLOCATE(WINDX(ISLON:IELON,ISLAT:IELAT),WINDY(ISLON:IELON,ISLAT:IELAT))
  ALLOCATE(WIND0X(ISLON:IELON,ISLAT:IELAT),WIND0Y(ISLON:IELON,ISLAT:IELAT))
  ALLOCATE(WX(ISLON:IELON,ISLAT:IELAT),WY(ISLON:IELON,ISLAT:IELAT))
  ALLOCATE(W0(ISLON:IELON,ISLAT:IELAT),W1(ISLON:IELON,ISLAT:IELAT))
  ALLOCATE(W(ISLON:IELON,ISLAT:IELAT),WINC(ISLON:IELON,ISLAT:IELAT))
  ALLOCATE(EE(KL,JL,ISLON-LE:IELON+LE,ISLAT-LE:IELAT+LE))
  ALLOCATE(E(KL,JL,ISLON:IELON,ISLAT:IELAT),EA(KL,JL,ISLON:IELON,ISLAT:IELAT))

  ALLOCATE(UXX(ISLON:IELON,ISLAT:IELAT),UXY(ISLON:IELON,ISLAT:IELAT))
  ALLOCATE(UYY(ISLON:IELON,ISLAT:IELAT),UYX(ISLON:IELON,ISLAT:IELAT))
  ALLOCATE(UY(ISLON-LE:IELON+LE,ISLAT-LE:IELAT+LE),UX(ISLON-LE:IELON+LE,ISLAT-LE:IELAT+LE))

  ALLOCATE(H1_3(ISLON:IELON,ISLAT:IELAT))
  ALLOCATE(TPF(ISLON:IELON,ISLAT:IELAT))
  ALLOCATE(APE(ISLON:IELON,ISLAT:IELAT))
  ALLOCATE(AET(ISLON:IELON,ISLAT:IELAT))
  ALLOCATE(BV(ISLON:IELON,ISLAT:IELAT,KB))
  ALLOCATE(TAUBB11(KB,ISLON:IELON,ISLAT:IELAT))
  ALLOCATE(TAUBB12(KB,ISLON:IELON,ISLAT:IELAT))
  ALLOCATE(TAUBB22(KB,ISLON:IELON,ISLAT:IELAT))
  ALLOCATE(TAUBB33(KB,ISLON:IELON,ISLAT:IELAT))
  ALLOCATE(UORBITAL(ISLON:IELON,ISLAT:IELAT))
  ALLOCATE(VORBITAL(ISLON:IELON,ISLAT:IELAT))
  ALLOCATE(USTOKES(ISLON:IELON,ISLAT:IELAT))
  ALLOCATE(VSTOKES(ISLON:IELON,ISLAT:IELAT)) 
  ALLOCATE(RA(ISLON:IELON,ISLAT:IELAT))
  ALLOCATE(HEN(ISLON:IELON,ISLAT:IELAT))
  ALLOCATE(PT(ISLON:IELON,ISLAT:IELAT))
  ALLOCATE(KT(ISLON:IELON,ISLAT:IELAT))
  ALLOCATE(ET(ISLON:IELON,ISLAT:IELAT))
  ALLOCATE(BBR(ISLON:IELON,ISLAT:IELAT))
  ALLOCATE(MT(ISLON:IELON,ISLAT:IELAT))
  ALLOCATE(UST(ISLON:IELON,ISLAT:IELAT))
  ALLOCATE(TAUINX(ISLON:IELON,ISLAT:IELAT))
  ALLOCATE(TAUINY(ISLON:IELON,ISLAT:IELAT))
  ALLOCATE(TAUDSX(ISLON:IELON,ISLAT:IELAT))
  ALLOCATE(TAUDSY(ISLON:IELON,ISLAT:IELAT))
!! add for swell by wgs 20190926
  ALLOCATE(swell_hs(ISLON:IELON,ISLAT:IELAT))
  ALLOCATE(swell_tp(ISLON:IELON,ISLAT:IELAT))
  ALLOCATE(swell_tz(ISLON:IELON,ISLAT:IELAT))
  ALLOCATE(swell_th(ISLON:IELON,ISLAT:IELAT))
  ALLOCATE(windy_hs(ISLON:IELON,ISLAT:IELAT))
  ALLOCATE(windy_tp(ISLON:IELON,ISLAT:IELAT))
  ALLOCATE(windy_tz(ISLON:IELON,ISLAT:IELAT))
  ALLOCATE(windy_th(ISLON:IELON,ISLAT:IELAT))
!! end for swell

!! add for BDY
!! WEST BDY
  IF(ISLON==1)THEN
    ALLOCATE(WHS(ISLAT:IELAT),WTH(ISLAT:IELAT),WTP(ISLAT:IELAT))
    WHS=0;WTH=0;WTP=0
  ENDIF
!! EAST BDY
  IF(IELON==IM)THEN
    ALLOCATE(EHS(ISLAT:IELAT),ETH(ISLAT:IELAT),ETP(ISLAT:IELAT))
    EHS=0;ETH=0;ETP=0
  ENDIF
!! NORTH  BDY
  IF(IELAT==JM)THEN
    ALLOCATE(NHS(ISLON:IELON),NTH(ISLON:IELON),NTP(ISLON:IELON))
    NHS=0;NTH=0;NTP=0
  ENDIF
!! SOUTH BDY
  IF(ISLAT==1)THEN
    ALLOCATE(SHS(ISLON:IELON),STH(ISLON:IELON),STP(ISLON:IELON))
    SHS=0;STH=0;STP=0
  ENDIF

  WINDX   = 0
  WINDY   = 0
  WIND0X  = 0
  WIND0Y  = 0
  WX      = 0
  WY      = 0
  W0      = 0
  W1      = 0
  W       = 0
  WINC    = 0
  EE      = 0
  E       = 0
  EA      = 0
  UXX     = 0
  UXY     = 0
  UYY     = 0
  UYX     = 0
  UY      = 0
  UX      = 0
  H1_3    = 0
  TPF     = 0
  APE     = 0
  AET     = 0
  BV      = 0
  TAUBB11 = 0
  TAUBB12 = 0
  TAUBB22 = 0
  TAUBB33 = 0
  UST     = 0
  TAUINX  = 0
  TAUINY  = 0
  TAUDSX  = 0
  TAUDSY  = 0
  swell_hs= 0
  swell_tp= 0
  swell_th= 0
  swell_tz= 0
  windy_hs= 0
  windy_tp= 0
  windy_th= 0
  windy_tz= 0
  RETURN
  END
!=======================================================================      
  SUBROUTINE DEALLOCATE_VARIABLE_WAVE
  USE coupling_wave_mod                                                         !zhaobiao, c-coupler2
  USE ALL_VAR_WAVE,ONLY : WINDX,WINDY,WIND0X,WIND0Y,WX,WY,W0,W1,W,         &
                    WINC,EE,E,EA,UXX,UXY,UYY,UYX,UX,UY,H1_3,BV,WF,CCG,DWF, &
                    TPF,APE,AET,TAUBB11,TAUBB12,TAUBB22,TAUBB33,           &
                    UORBITAL,VORBITAL,USTOKES,VSTOKES,RA,HEN,PT,KT,ET,     &
                    BBR,MT,UST,TAUINX,TAUINY,TAUDSX,TAUDSY,                &
                    swell_hs,swell_tp,swell_th,swell_tz,                   &
                    windy_hs,windy_tp,windy_th,windy_tz,                   &
                    XGRID,YGRID,D,NSP,RS2DLAT,                             &
                    EHS,ETP,ETH,WHS,WTP,WTH,SHS,STP,STH,NHS,NTP,NTH                                           
  USE CONTROL_WAVE, ONLY : DX,DY
  USE CONTROL_WAVE, ONLY : ISLON,IELON,ISLAT,IELAT
  USE LIMS_WAVE, ONLY : IM,JM
  DEALLOCATE(ZBV)
  DEALLOCATE(XGRID,YGRID,D,NSP,RS2DLAT)
  DEALLOCATE(DX,DY)   
  DEALLOCATE(WF,CCG,DWF)
  DEALLOCATE(WINDX,WINDY,WIND0X,WIND0Y)
  DEALLOCATE(WX,WY,W0,W1,W,WINC)
  DEALLOCATE(UXX,UXY,UYY,UYX,UY,UX)
  DEALLOCATE(H1_3,TPF,APE,AET)
  DEALLOCATE(UORBITAL,VORBITAL,USTOKES,VSTOKES,RA,HEN,PT,KT,ET,BBR,MT,UST,TAUINX,TAUINY,TAUDSX,TAUDSY)
  DEALLOCATE(swell_hs,swell_tp,swell_th,swell_tz)
  DEALLOCATE(windy_hs,windy_tp,windy_th,windy_tz)
  DEALLOCATE(TAUBB11,TAUBB12,TAUBB22,TAUBB33)
  DEALLOCATE(BV)
  DEALLOCATE(EE,E,EA)
!! add for BDY
!! WEST BDY
  IF(ISLON==1)THEN
    DEALLOCATE(WHS,WTH,WTP)
  ENDIF
!! EAST BDY
  IF(IELON==IM)THEN
    DEALLOCATE(EHS,ETH,ETP)
  ENDIF
!! NORTH  BDY
  IF(IELAT==JM)THEN
    DEALLOCATE(NHS,NTH,NTP)
  ENDIF
!! SOUTH BDY
  IF(ISLAT==1)THEN
    DEALLOCATE(SHS,STH,STP)
  ENDIF
  CALL release_masnum_buf                                                       !zhaobiao, c-coupler2
  END


