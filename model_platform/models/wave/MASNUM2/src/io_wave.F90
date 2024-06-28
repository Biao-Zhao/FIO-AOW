  MODULE MOD_IO_WAVE
     USE CONTROL_WAVE, ONLY : FORCE_NAME,MSR
     USE data_kind_mod_wave
     USE NcFileMod
     USE parallel_mod_wave, ONLY: masnum_mpi_comm
!     USE LIMS, ONLY : MYID
     CONTAINS

     !======================================================================
     ! READ FORCING DATA
     !======================================================================
     SUBROUTINE GETWIND(FileName,UU,VV,STRTIME,LREC)
     USE CONTROL_WAVE, ONLY : ISLON,IELON,ISLAT,IELAT
     USE data_kind_mod_wave
     USE NcFileMod
  
     IMPLICIT NONE
     CHARACTER(LEN=*),INTENT(IN) :: FileName
     INTEGER,INTENT(IN) :: LREC
     REAL(kind_r4),DIMENSION(:,:),INTENT(OUT) :: UU,VV 
     CHARACTER*10,INTENT(OUT) :: STRTIME
! 
     UU=0.
     VV=0.
     CALL NC_READ(FileName,FORCE_NAME(1),UU,LREC=LREC,I1=ISLON,J1=ISLAT)
     CALL NC_READ(FileName,FORCE_NAME(2),VV,LREC=LREC,I1=ISLON,J1=ISLAT)
     CALL NC_READ(FileName,FORCE_NAME(3),STRTIME,LREC=LREC) 
     WRITE(6,*) "READ WIND FIELD TIME: ",STRTIME
!    set maxvalue and minvalue in wind
     WHERE(ABS(UU)<0.1) 
     UU=0.1
     END WHERE

     WHERE(ABS(UU)>=50.0) 
     UU=50.0
     END WHERE

     WHERE(ABS(VV)<0.1) 
     VV=0.1
     END WHERE

     WHERE(ABS(VV)>=50.0) 
     VV=50.0
     END WHERE
 
     RETURN
     END SUBROUTINE GETWIND
     !======================================================================
     SUBROUTINE READ_BDY(TTT)
     USE LIMS_WAVE, ONLY : IM,JM
     USE CONTROL_WAVE, ONLY : ISLON,IELON,ISLAT,IELAT,input
     USE data_kind_mod_wave
     USE NcFileMod
     USE ALL_VAR_WAVE,ONLY : EHS,ETH,ETP,WHS,WTH,WTP,NHS,NTH,NTP,SHS,STH,STP
     USE time_mod
     USE CCPL_interface_mod, only : CCPL_report_error !zhaobiao, c-coupler2
     USE coupling_wave_mod 
     IMPLICIT NONE
     DOUBLE PRECISION,INTENT(IN) :: TTT
     DOUBLE PRECISION :: TTT1
     CHARACTER(LEN=200) :: FileName
     CHARACTER*4 :: Ystr
     INTEGER :: LREC,TTVEC(6)
     CHARACTER*10 STRTIME
     REAL(kind_r4),DIMENSION(IM) :: TMP2DIM
     REAL(kind_r4),DIMENSION(JM) :: TMP2DJM
     LOGICAL                     :: ISEXIST

     TTVEC=DATEVEC(TTT,1)
     TTT1=datenum([TTVEC(1),1,1,0,0,0],1)
     LREC=INT((TTT-TTT1)*8)+1

     WRITE(YSTR,'(i4.4)')TTVEC(1)
! 
!    WEST BDY
     IF(ISLON==1)THEN
       FileName=TRIM(input)//"BDY/"//TRIM(YSTR)//"_bc_wave_w.nc"
       INQUIRE(FILE=FileName,EXIST=ISEXIST)
       IF(ISEXIST)THEN  
         WRITE(6,*)TRIM(FileName)
         CALL NC_READ(FileName,'hs',TMP2DJM,LREC=LREC,land=0.)
         WHS(ISLAT:IELAT)=TMP2DJM(ISLAT:IELAT) 
         CALL NC_READ(FileName,'tp',TMP2DJM,LREC=LREC,land=0.)
         WTP(ISLAT:IELAT)=TMP2DJM(ISLAT:IELAT) 
         CALL NC_READ(FileName,'th',TMP2DJM,LREC=LREC,land=0.)
         WTH(ISLAT:IELAT)=TMP2DJM(ISLAT:IELAT) 
       ELSE
         WRITE(6,*) TRIM(FileName)," is not exist"
         CALL CCPL_report_error(masnum_state_variables(masnum_grid_id)%comp_id,.false., "ERROR READING open baoudary condition")
       END IF
     END IF
!    EAST BDY
     IF(IELON==IM)THEN
       FileName=TRIM(input)//"BDY/"//TRIM(YSTR)//"_bc_wave_e.nc"
       INQUIRE(FILE=FileName,EXIST=ISEXIST)
       IF(ISEXIST)THEN
         WRITE(6,*)TRIM(FileName)
         CALL NC_READ(FileName,'hs',TMP2DJM,LREC=LREC,land=0.)
         EHS(ISLAT:IELAT)=TMP2DJM(ISLAT:IELAT) 
         CALL NC_READ(FileName,'tp',TMP2DJM,LREC=LREC,land=0.)
         ETP(ISLAT:IELAT)=TMP2DJM(ISLAT:IELAT) 
         CALL NC_READ(FileName,'th',TMP2DJM,LREC=LREC,land=0.)
         ETH(ISLAT:IELAT)=TMP2DJM(ISLAT:IELAT) 
       ELSE
         WRITE(6,*) TRIM(FileName)," is not exist"
         CALL CCPL_report_error(masnum_state_variables(masnum_grid_id)%comp_id,.false., "ERROR READING open baoudary condition")
       END IF
     END IF
!    NORTH BDY
     IF(IELAT==JM)THEN
        FileName=TRIM(input)//"BDY/"//TRIM(YSTR)//"_bc_wave_n.nc"
       INQUIRE(FILE=FileName,EXIST=ISEXIST)
       IF(ISEXIST)THEN
         WRITE(6,*)TRIM(FileName)
         CALL NC_READ(FileName,'hs',TMP2DIM,LREC=LREC,land=0.)
         NHS(ISLON:IELON)=TMP2DIM(ISLON:IELON) 
         CALL NC_READ(FileName,'tp',TMP2DIM,LREC=LREC,land=0.)
         NTP(ISLON:IELON)=TMP2DIM(ISLON:IELON) 
         CALL NC_READ(FileName,'th',TMP2DIM,LREC=LREC,land=0.)
         NTH(ISLON:IELON)=TMP2DIM(ISLON:IELON) 
       ELSE
         WRITE(6,*) TRIM(FileName)," is not exist"
         CALL CCPL_report_error(masnum_state_variables(masnum_grid_id)%comp_id,.false., "ERROR READING open baoudary condition")
       END IF
     END IF
!    SOUTH BDY
     IF(ISLAT==1)THEN
       FileName=TRIM(input)//"BDY/"//TRIM(YSTR)//"_bc_wave_s.nc"
       INQUIRE(FILE=FileName,EXIST=ISEXIST)
       IF(ISEXIST)THEN
          WRITE(6,*)TRIM(FileName)
          CALL NC_READ(FileName,'hs',TMP2DIM,LREC=LREC,land=0.)
          SHS(ISLON:IELON)=TMP2DIM(ISLON:IELON) 
          CALL NC_READ(FileName,'tp',TMP2DIM,LREC=LREC,land=0.)
          STP(ISLON:IELON)=TMP2DIM(ISLON:IELON) 
          CALL NC_READ(FileName,'th',TMP2DIM,LREC=LREC,land=0.)
          STH(ISLON:IELON)=TMP2DIM(ISLON:IELON) 
          CALL NC_READ(FileName,'ctime',STRTIME,LREC=LREC) 
          WRITE(6,*) "READ BDY FIELD TIME: ",STRTIME,LREC
       ELSE
          WRITE(6,*) TRIM(FileName)," is not exist"
         CALL CCPL_report_error(masnum_state_variables(masnum_grid_id)%comp_id,.false., "ERROR READING open baoudary condition")
       END IF
     END IF
     
     END SUBROUTINE READ_BDY
     !======================================================================
     SUBROUTINE SetNcFile(FileName)
     USE NcFileMod
     USE ALL_VAR_WAVE,ONLY:XGRID,YGRID,ZBV
     USE parallel_mod_wave
     USE LIMS_WAVE,ONLY:IM,JM,KB
     USE CONTROL_WAVE,ONLY:MSR,OUT_DATA_FIG,ISBREAK, ISSWELL, ISCURRENT 
     USE data_kind_mod_wave
     USE pnetcdf
     include "mpif.h"

     CHARACTER(LEN=*),INTENT(IN) :: FileName
     INTEGER(kind_i8) :: IM_p,JM_p,KB_p,ulimlen,clen,irec
     INTEGER :: vid,vidlon,vidlat,viddep,vidXGRID,vidYGRID,dimids(4),dimids_2dt(3),dimidscl(2)
     INTEGER :: ncid,stat
     REAL,PARAMETER ::OUT_LAND=1.E20

     IM_p=IM;JM_p=JM;KB_p=KB;ulimlen=NF90_UNLIMITED;clen=20


      stat=nf90mpi_create(masnum_mpi_comm,FileName,         &
                          IOR(NF90_CLOBBER,NF90_64BIT_OFFSET), &
                          MPI_INFO_NULL,ncid)

      stat=nf90mpi_def_dim(ncid,"lon",IM_p,dimids(1))
      stat=nf90mpi_def_var(ncid,"lon",nf90_real,dimids(1),vidlon)
      stat=nf90mpi_put_att(ncid,vidlon,'units','degrees_east')

      stat=nf90mpi_def_dim(ncid,"lat",JM_p,dimids(2))
      stat=nf90mpi_def_var(ncid,"lat",nf90_real,dimids(2),vidlat)
      stat=nf90mpi_put_att(ncid,vidlat,'units','degrees_north')

      stat=nf90mpi_def_dim(ncid,"dep",KB_p,dimids(3))
      stat=nf90mpi_def_var(ncid,"dep",nf90_real,dimids(3),viddep)
      stat=nf90mpi_put_att(ncid,viddep,'positive','down')
      stat=nf90mpi_put_att(ncid,viddep,'units','meter')

      stat=nf90mpi_def_dim(ncid,"char",clen,dimidscl(1))

      stat=nf90mpi_def_dim(ncid,"time",ulimlen,dimids(4))
      stat=nf90mpi_def_var(ncid,"time",nf90_double,dimids(4),vid)
      stat=nf90mpi_put_att(ncid,vid,'units','hours since 0000-01-01 00:00:00')

      dimids_2dt(1)=dimids(1)
      dimids_2dt(2)=dimids(2)
      dimids_2dt(3)=dimids(4)
      dimidscl(2)=dimids(4)

      stat=nf90mpi_def_var(ncid,'xgrid',nf90_real,dimids_2dt(1:2),vidXGRID)
      stat=nf90mpi_put_att(ncid,vid,'units','degrees_east')
      stat=nf90mpi_put_att(ncid,vid,'description','2 dimensional latitude')
      
      stat=nf90mpi_def_var(ncid,'ygrid',nf90_real,dimids_2dt(1:2),vidYGRID)
      stat=nf90mpi_put_att(ncid,vid,'units','degree_north')
      stat=nf90mpi_put_att(ncid,vid,'description','2 dimensional longitude')

      IF(OUT_DATA_FIG(1))THEN
      stat=nf90mpi_def_var(ncid,'bv',nf90_real,dimids,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
      stat=nf90mpi_put_att(ncid,vid,'units','unitless')
      stat=nf90mpi_put_att(ncid,vid,'description','Non-breaking wave induced vertical mixing coefficient')
      stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
      END IF
      
      IF(OUT_DATA_FIG(2))THEN
      stat=nf90mpi_def_var(ncid,'windu',nf90_int2,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',NF90_FILL_int2)
      stat=nf90mpi_put_att(ncid,vid,'scale_factor',0.01)
      stat=nf90mpi_put_att(ncid,vid,'units','m/s')
      stat=nf90mpi_put_att(ncid,vid,'description','Zonal wind velocity')
      stat=nf90mpi_def_var_fill(ncid,vid,0,NF90_FILL_int2)

      stat=nf90mpi_def_var(ncid,'windv',nf90_int2,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',NF90_FILL_int2)
      stat=nf90mpi_put_att(ncid,vid,'scale_factor',0.01)
      stat=nf90mpi_put_att(ncid,vid,'units','m/s')
      stat=nf90mpi_put_att(ncid,vid,'description','Meridional wind velocity')
      stat=nf90mpi_def_var_fill(ncid,vid,0,NF90_FILL_int2)

      stat=nf90mpi_def_var(ncid,'ustar',nf90_real,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
      stat=nf90mpi_put_att(ncid,vid,'units','m/s')
      stat=nf90mpi_put_att(ncid,vid,'description','Friction velocity')
      stat=nf90mpi_def_var_fill(ncid,vid,0, OUT_LAND)
      END IF

      IF(OUT_DATA_FIG(3))THEN
      stat=nf90mpi_def_var(ncid,'hs',nf90_int2,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',NF90_FILL_int2)
      stat=nf90mpi_put_att(ncid,vid,'scale_factor',0.01)
      stat=nf90mpi_put_att(ncid,vid,'units','meter')
      stat=nf90mpi_put_att(ncid,vid,'description','Significant wave height of of the largest 1/3 of waves')
      stat=nf90mpi_def_var_fill(ncid,vid,0,NF90_FILL_int2)

      stat=nf90mpi_def_var(ncid,'tp',nf90_int2,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',NF90_FILL_int2)
      stat=nf90mpi_put_att(ncid,vid,'scale_factor',0.001)
      stat=nf90mpi_put_att(ncid,vid,'units','second')
      stat=nf90mpi_put_att(ncid,vid,'description','Spectrum peak wave period')
      stat=nf90mpi_def_var_fill(ncid,vid,0,NF90_FILL_int2)

      stat=nf90mpi_def_var(ncid,'tz',nf90_int2,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',NF90_FILL_int2)
      stat=nf90mpi_put_att(ncid,vid,'scale_factor',0.001)
      stat=nf90mpi_put_att(ncid,vid,'units','second')
      stat=nf90mpi_put_att(ncid,vid,'description','Zero-crossing wave period')
      stat=nf90mpi_def_var_fill(ncid,vid,0,NF90_FILL_int2)

      stat=nf90mpi_def_var(ncid,'th',nf90_int2,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',NF90_FILL_int2)
      stat=nf90mpi_put_att(ncid,vid,'scale_factor',0.1)
      stat=nf90mpi_put_att(ncid,vid,'units','degree')
      stat=nf90mpi_put_att(ncid,vid,'description','Mean wave direction')
      stat=nf90mpi_def_var_fill(ncid,vid,0,NF90_FILL_int2)
      END IF

      IF(OUT_DATA_FIG(4))THEN
      stat=nf90mpi_def_var(ncid,'uorb',nf90_real,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
      stat=nf90mpi_put_att(ncid,vid,'units','m/s')
      stat=nf90mpi_put_att(ncid,vid,'description','Zonal wave orbital velocity')
      stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)

      stat=nf90mpi_def_var(ncid,'vorb',nf90_real,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
      stat=nf90mpi_put_att(ncid,vid,'units','m/s')
      stat=nf90mpi_put_att(ncid,vid,'description','Meridional wave orbital velocity')
      stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)

      stat=nf90mpi_def_var(ncid,'ustok',nf90_real,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
      stat=nf90mpi_put_att(ncid,vid,'units','m/s')
      stat=nf90mpi_put_att(ncid,vid,'description','Zonal Stokes velocity')
      stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)

      stat=nf90mpi_def_var(ncid,'vstok',nf90_real,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
      stat=nf90mpi_put_att(ncid,vid,'units','degree')
      stat=nf90mpi_put_att(ncid,vid,'description','Meridional Stokes velocity')
      stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
      END IF

      IF(OUT_DATA_FIG(5))THEN
      stat=nf90mpi_def_var(ncid,'tauinx',nf90_real,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
      stat=nf90mpi_put_att(ncid,vid,'units','meter')
      stat=nf90mpi_put_att(ncid,vid,'description','Zonal wave-induced stress based on Janssen 2012')
      stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)

      stat=nf90mpi_def_var(ncid,'tauiny',nf90_real,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
      stat=nf90mpi_put_att(ncid,vid,'units','second')
      stat=nf90mpi_put_att(ncid,vid,'description','Meridional wave-induced stress based on Janssen 2012')
      stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)

      stat=nf90mpi_def_var(ncid,'taudsx',nf90_real,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
      stat=nf90mpi_put_att(ncid,vid,'units','second')
      stat=nf90mpi_put_att(ncid,vid,'description','Zonal dissipation stress based on Janssen 2012')
      stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)

      stat=nf90mpi_def_var(ncid,'taudsy',nf90_real,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
      stat=nf90mpi_put_att(ncid,vid,'units','degree')
      stat=nf90mpi_put_att(ncid,vid,'description','Meridional dissipation stress based on Janssen 2012')
      stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
      END IF

      IF(ISSWELL)THEN
      stat=nf90mpi_def_var(ncid,'s_hs',nf90_int2,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',NF90_FILL_int2)
      stat=nf90mpi_put_att(ncid,vid,'scale_factor',0.01)
      stat=nf90mpi_put_att(ncid,vid,'units','meter')
      stat=nf90mpi_put_att(ncid,vid,'description','Significant wave height of of the largest 1/3 of swell')
      stat=nf90mpi_def_var_fill(ncid,vid,0,NF90_FILL_int2)

      stat=nf90mpi_def_var(ncid,'s_tp',nf90_int2,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',NF90_FILL_int2)
      stat=nf90mpi_put_att(ncid,vid,'scale_factor',0.001)
      stat=nf90mpi_put_att(ncid,vid,'units','second')
      stat=nf90mpi_put_att(ncid,vid,'description','Spectrum peak wave period of swell')
      stat=nf90mpi_def_var_fill(ncid,vid,0,NF90_FILL_int2)

      stat=nf90mpi_def_var(ncid,'s_tz',nf90_int2,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',NF90_FILL_int2)
      stat=nf90mpi_put_att(ncid,vid,'scale_factor',0.001)
      stat=nf90mpi_put_att(ncid,vid,'units','second')
      stat=nf90mpi_put_att(ncid,vid,'description','Zero-crossing wave period of swell')
      stat=nf90mpi_def_var_fill(ncid,vid,0,NF90_FILL_int2)

      stat=nf90mpi_def_var(ncid,'s_th',nf90_int2,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',NF90_FILL_int2)
      stat=nf90mpi_put_att(ncid,vid,'scale_factor',0.1)
      stat=nf90mpi_put_att(ncid,vid,'units','degree')
      stat=nf90mpi_put_att(ncid,vid,'description','Mean wave direction of swell')
      stat=nf90mpi_def_var_fill(ncid,vid,0,NF90_FILL_int2)

      stat=nf90mpi_def_var(ncid,'w_hs',nf90_int2,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',NF90_FILL_int2)
      stat=nf90mpi_put_att(ncid,vid,'scale_factor',0.01)
      stat=nf90mpi_put_att(ncid,vid,'units','meter')
      stat=nf90mpi_put_att(ncid,vid,'description','Significant wave height of of the largest 1/3 of wind waves')
      stat=nf90mpi_def_var_fill(ncid,vid,0,NF90_FILL_int2)

      stat=nf90mpi_def_var(ncid,'w_tp',nf90_int2,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',NF90_FILL_int2)
      stat=nf90mpi_put_att(ncid,vid,'scale_factor',0.001)
      stat=nf90mpi_put_att(ncid,vid,'units','second')
      stat=nf90mpi_put_att(ncid,vid,'description','Spectrum peak wave period of wind waves')
      stat=nf90mpi_def_var_fill(ncid,vid,0,NF90_FILL_int2)

      stat=nf90mpi_def_var(ncid,'w_tz',nf90_int2,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',NF90_FILL_int2)
      stat=nf90mpi_put_att(ncid,vid,'scale_factor',0.001)
      stat=nf90mpi_put_att(ncid,vid,'units','second')
      stat=nf90mpi_put_att(ncid,vid,'description','Zero-crossing wave period of wind waves')
      stat=nf90mpi_def_var_fill(ncid,vid,0,NF90_FILL_int2)

      stat=nf90mpi_def_var(ncid,'w_th',nf90_int2,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',NF90_FILL_int2)
      stat=nf90mpi_put_att(ncid,vid,'scale_factor',0.1)
      stat=nf90mpi_put_att(ncid,vid,'units','degree')
      stat=nf90mpi_put_att(ncid,vid,'description','Mean wave direction of wind waves')
      stat=nf90mpi_def_var_fill(ncid,vid,0,NF90_FILL_int2)
      END IF

      IF(ISBREAK)THEN
      stat=nf90mpi_def_var(ncid,'ra',nf90_real,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
      stat=nf90mpi_put_att(ncid,vid,'units','unitless')
      stat=nf90mpi_put_att(ncid,vid,'description','Whitecap coverage based on Yuan Yeli 2009')
      stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)

      stat=nf90mpi_def_var(ncid,'bbr',nf90_real,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
      stat=nf90mpi_put_att(ncid,vid,'units','second')
      stat=nf90mpi_put_att(ncid,vid,'description','Spectrum peak wave period')
      stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
      END IF
      
      IF(ISCURRENT)THEN
      stat=nf90mpi_def_var(ncid,'UX',nf90_real,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
      stat=nf90mpi_put_att(ncid,vid,'units','m/s')
      stat=nf90mpi_put_att(ncid,vid,'description','Sea surface zonal current velocity')
      stat=nf90mpi_def_var_fill(ncid,vid,0, OUT_LAND)

      stat=nf90mpi_def_var(ncid,'UY',nf90_real,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
      stat=nf90mpi_put_att(ncid,vid,'units','m/s')
      stat=nf90mpi_put_att(ncid,vid,'description','Sea surface meridional current velocity')
      stat=nf90mpi_def_var_fill(ncid,vid,0, OUT_LAND)

      stat=nf90mpi_def_var(ncid,'UXX',nf90_real,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
      stat=nf90mpi_put_att(ncid,vid,'units','m/s')
      stat=nf90mpi_put_att(ncid,vid,'description','dU/dx')
      stat=nf90mpi_def_var_fill(ncid,vid,0, OUT_LAND)

      stat=nf90mpi_def_var(ncid,'UXY',nf90_real,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
      stat=nf90mpi_put_att(ncid,vid,'units','m/s')
      stat=nf90mpi_put_att(ncid,vid,'description','dU/dy')
      stat=nf90mpi_def_var_fill(ncid,vid,0, OUT_LAND)

      stat=nf90mpi_def_var(ncid,'UYX',nf90_real,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
      stat=nf90mpi_put_att(ncid,vid,'units','m/s')
      stat=nf90mpi_put_att(ncid,vid,'description','dV/dx')
      stat=nf90mpi_def_var_fill(ncid,vid,0, OUT_LAND)

      stat=nf90mpi_def_var(ncid,'UYY',nf90_real,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
      stat=nf90mpi_put_att(ncid,vid,'units','m/s')
      stat=nf90mpi_put_att(ncid,vid,'description','dV/dy')
      stat=nf90mpi_def_var_fill(ncid,vid,0, OUT_LAND)
      END IF

      stat=nf90mpi_def_var(ncid,'ctime',nf90_char,dimidscl,vid)

      stat=nf90mpi_enddef(ncid)

      stat=nf90mpi_begin_indep_data(ncid)
      IF(MSR)THEN
         stat=nf90mpi_put_var(ncid,vidlon,XGRID(1:IM_p,1))
         stat=nf90mpi_put_var(ncid,vidlat,YGRID(1,:))
         stat=nf90mpi_put_var(ncid,viddep,ZBV)
         stat=nf90mpi_put_var(ncid,vidXGRID,XGRID(1:IM_p,:))
         stat=nf90mpi_put_var(ncid,vidYGRID,YGRID)
      ENDIF

       stat=nf90mpi_end_indep_data(ncid)

       stat=nf90mpi_close(ncid)

     CALL BARRIER

     END SUBROUTINE SetNcFile
!======================================================================
     SUBROUTINE OutNcFile(FileName,Lrec)
     USE data_kind_mod_wave
     USE NcFileMod
     USE time_mod
     USE LIMS_WAVE,ONLY:MYID,NPROCS,KB
     USE CONTROL_WAVE,ONLY:ISLON,ISLAT,OUT_DATA_FIG,TT,IELAT,IELON, ISBREAK, ISSWELL, ISCURRENT
     USE ALL_VAR_WAVE,ONLY:H1_3,WX,WY,UXX,UXY,UYX,UYY,UX,UY,BV,TPF,APE,AET,NSP,windy_hs,windy_tp,windy_th,windy_tz, &
                           swell_hs,swell_tp,swell_th,swell_tz
     USE ALL_VAR_WAVE,ONLY:UORBITAL,VORBITAL,USTOKES,VSTOKES,RA,TAUINX,TAUINY,TAUDSX,TAUDSY,BBR,UST  !zhaobiao
     USE pnetcdf
     IMPLICIT NONE
     INCLUDE 'mpif.h'

     CHARACTER(LEN=*),INTENT(IN) :: FileName
     INTEGER,INTENT(IN) :: LREC
     INTEGER I,J,NcID,vID,stat
     real,parameter :: eps=1.0e-10 

     INTEGER(kind_i8) :: start1dt(1),count1dt(1)
     INTEGER(kind_i8) :: start2dt(3),start3dt(4),startct(2)
     INTEGER(kind_i8) :: count2dt(3),count3dt(4),countct(2)
     INTEGER(kind_i8) :: irec
!    LOCAL
     INTEGER MES,IERR,STATUS(MPI_STATUS_SIZE)
     CHARACTER*14 CTIME
     integer(KIND_IN),allocatable :: iv2(:,:),iv3(:,:,:)
     real(kind_r8) TTT(1)

     IF(LREC==1)CALL SetNcFile(FileName)
     start2dt(1)=islon;start2dt(2)=islat;start2dt(3)=LREC
     start3dt(1)=islon;start3dt(2)=islat;start3dt(4)=LREC
     start3dt(3)=1
     start1dt(1)=LREC
     startct(1)=1;startct(2)=LREC;irec=LREC

     count2dt(1)=ielon-islon+1;count2dt(2)=ielat-islat+1;count2dt(3)=1
     count3dt(1)=ielon-islon+1;count3dt(2)=ielat-islat+1;count3dt(4)=1;count3dt(3)=KB
     countct(1)=20;countct(2)=1
     count1dt(1)=1

     allocate(iv2(count2dt(1),count2dt(2)),iv3(count2dt(1),count2dt(2),KB))

!    open file
     stat=nf90mpi_open(masnum_mpi_comm,&
                       FileName      ,&
                       NF_WRITE      ,&
                       MPI_INFO_NULL ,&
                       ncid           )
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!    WRITE DATA
     IF(OUT_DATA_FIG(1))THEN
        stat=nf90mpi_inq_varid(ncid,'bv',vid)
        stat=nf90mpi_fill_var_rec(ncid,vid,irec)
        stat=nf90mpi_put_var_all(ncid,vid,bv,start3dt,count3dt)
     ENDIF

!    WINDU
     IF(OUT_DATA_FIG(2))THEN
       iv2=wx/0.01
       DO J=ISLAT,IELAT
       DO I=ISLON,IELON
          IF(NSP(I,J)==0)THEN
            IV2(I-ISLON+1,J-ISLAT+1)=-32767
          ENDIF
       ENDDO
       ENDDO
       stat=nf90mpi_inq_varid(ncid,'windu',vid)
       stat=nf90mpi_fill_var_rec(ncid,vid,irec)
       stat=nf90mpi_put_var_all(ncid,vid,iv2,start2dt,count2dt)
!   WINDV
       iv2=wy/0.01
       DO J=ISLAT,IELAT
       DO I=ISLON,IELON
          IF(NSP(I,J)==0)THEN
            IV2(I-ISLON+1,J-ISLAT+1)=-32767
          ENDIF
       ENDDO
       ENDDO
       stat=nf90mpi_inq_varid(ncid,'windv',vid)
       stat=nf90mpi_fill_var_rec(ncid,vid,irec)
       stat=nf90mpi_put_var_all(ncid,vid,iv2,start2dt,count2dt)

       stat=nf90mpi_inq_varid(ncid,'ustar',vid)
       stat=nf90mpi_fill_var_rec(ncid,vid,irec)
       stat=nf90mpi_put_var_all(ncid,vid,UST,start2dt,count2dt)
     ENDIF

!    SWELL +WINDY
!    HS
     IF(OUT_DATA_FIG(3))THEN
       iv2=H1_3/0.01
       DO J=ISLAT,IELAT
       DO I=ISLON,IELON
          IF(NSP(I,J)==0)THEN
            IV2(I-ISLON+1,J-ISLAT+1)=-32767
          ENDIF
       ENDDO
       ENDDO
       stat=nf90mpi_inq_varid(ncid,'hs',vid)
       stat=nf90mpi_fill_var_rec(ncid,vid,irec)
       stat=nf90mpi_put_var_all(ncid,vid,iv2,start2dt,count2dt)
!    TP
       iv2=TPF/0.001
       DO J=ISLAT,IELAT
       DO I=ISLON,IELON
          IF(NSP(I,J)==0)THEN
            IV2(I-ISLON+1,J-ISLAT+1)=-32767
          ENDIF
       ENDDO
       ENDDO
       stat=nf90mpi_inq_varid(ncid,'tp',vid)
       stat=nf90mpi_fill_var_rec(ncid,vid,irec)
       stat=nf90mpi_put_var_all(ncid,vid,iv2,start2dt,count2dt)
!    TZ
       iv2=APE/0.001
       DO J=ISLAT,IELAT
       DO I=ISLON,IELON
          IF(NSP(I,J)==0)THEN
            IV2(I-ISLON+1,J-ISLAT+1)=-32767
          ENDIF
       ENDDO
       ENDDO
       stat=nf90mpi_inq_varid(ncid,'tz',vid)
       stat=nf90mpi_fill_var_rec(ncid,vid,irec)
       stat=nf90mpi_put_var_all(ncid,vid,iv2,start2dt,count2dt)
!    TH
       iv2=AET/0.1
       DO J=ISLAT,IELAT
       DO I=ISLON,IELON
          IF(NSP(I,J)==0)THEN
            IV2(I-ISLON+1,J-ISLAT+1)=-32767
          ENDIF
       ENDDO
       ENDDO
       stat=nf90mpi_inq_varid(ncid,'th',vid)
       stat=nf90mpi_fill_var_rec(ncid,vid,irec)
       stat=nf90mpi_put_var_all(ncid,vid,iv2,start2dt,count2dt)
     END IF

     IF(OUT_DATA_FIG(4))THEN
        stat=nf90mpi_inq_varid(ncid,'uorb',vid)
        stat=nf90mpi_fill_var_rec(ncid,vid,irec)
        stat=nf90mpi_put_var_all(ncid,vid,UORBITAL,start2dt,count2dt)

        stat=nf90mpi_inq_varid(ncid,'vorb',vid)
        stat=nf90mpi_fill_var_rec(ncid,vid,irec)
        stat=nf90mpi_put_var_all(ncid,vid,VORBITAL,start2dt,count2dt)

        stat=nf90mpi_inq_varid(ncid,'ustok',vid)
        stat=nf90mpi_fill_var_rec(ncid,vid,irec)
        stat=nf90mpi_put_var_all(ncid,vid,USTOKES,start2dt,count2dt)

        stat=nf90mpi_inq_varid(ncid,'vstok',vid)
        stat=nf90mpi_fill_var_rec(ncid,vid,irec)
        stat=nf90mpi_put_var_all(ncid,vid,USTOKES,start2dt,count2dt)
     ENDIF

     IF(OUT_DATA_FIG(5))THEN
        stat=nf90mpi_inq_varid(ncid,'tauinx',vid)
        stat=nf90mpi_fill_var_rec(ncid,vid,irec)
        stat=nf90mpi_put_var_all(ncid,vid,TAUINX,start2dt,count2dt)

        stat=nf90mpi_inq_varid(ncid,'tauiny',vid)
        stat=nf90mpi_fill_var_rec(ncid,vid,irec)
        stat=nf90mpi_put_var_all(ncid,vid,TAUINY,start2dt,count2dt)

        stat=nf90mpi_inq_varid(ncid,'taudsx',vid)
        stat=nf90mpi_fill_var_rec(ncid,vid,irec)
        stat=nf90mpi_put_var_all(ncid,vid,TAUDSX,start2dt,count2dt)

        stat=nf90mpi_inq_varid(ncid,'taudsy',vid)
        stat=nf90mpi_fill_var_rec(ncid,vid,irec)
        stat=nf90mpi_put_var_all(ncid,vid,TAUDSY,start2dt,count2dt)
     ENDIF

!    SWELL 
     IF(ISSWELL)THEN
       iv2=swell_hs/0.01
       DO J=ISLAT,IELAT
       DO I=ISLON,IELON
          IF(NSP(I,J)==0)THEN
            IV2(I-ISLON+1,J-ISLAT+1)=-32767
          ENDIF
       ENDDO
       ENDDO
       stat=nf90mpi_inq_varid(ncid,'s_hs',vid)
       stat=nf90mpi_fill_var_rec(ncid,vid,irec)
       stat=nf90mpi_put_var_all(ncid,vid,iv2,start2dt,count2dt)
!    TP
       iv2=SWELL_TP/0.001
       DO J=ISLAT,IELAT
       DO I=ISLON,IELON
          IF(NSP(I,J)==0)THEN
            IV2(I-ISLON+1,J-ISLAT+1)=-32767
          ENDIF
       ENDDO
       ENDDO
       stat=nf90mpi_inq_varid(ncid,'s_tp',vid)
       stat=nf90mpi_fill_var_rec(ncid,vid,irec)
       stat=nf90mpi_put_var_all(ncid,vid,iv2,start2dt,count2dt)
!    TZ
       iv2=swell_tz/0.001
       DO J=ISLAT,IELAT
       DO I=ISLON,IELON
          IF(NSP(I,J)==0)THEN
            IV2(I-ISLON+1,J-ISLAT+1)=-32767
          ENDIF
       ENDDO
       ENDDO
       stat=nf90mpi_inq_varid(ncid,'s_tz',vid)
       stat=nf90mpi_fill_var_rec(ncid,vid,irec)
       stat=nf90mpi_put_var_all(ncid,vid,iv2,start2dt,count2dt)
!    TH
       iv2=swell_th/0.1
       DO J=ISLAT,IELAT
       DO I=ISLON,IELON
          IF(NSP(I,J)==0)THEN
            IV2(I-ISLON+1,J-ISLAT+1)=-32767
          ENDIF
       ENDDO
       ENDDO
       stat=nf90mpi_inq_varid(ncid,'s_th',vid)
       stat=nf90mpi_fill_var_rec(ncid,vid,irec)
       stat=nf90mpi_put_var_all(ncid,vid,iv2,start2dt,count2dt)
!    WIND WAVE
       iv2=windy_hs/0.01
       DO J=ISLAT,IELAT
       DO I=ISLON,IELON
          IF(NSP(I,J)==0)THEN
            IV2(I-ISLON+1,J-ISLAT+1)=-32767
          ENDIF
       ENDDO
       ENDDO
       stat=nf90mpi_inq_varid(ncid,'w_hs',vid)
       stat=nf90mpi_fill_var_rec(ncid,vid,irec)
       stat=nf90mpi_put_var_all(ncid,vid,iv2,start2dt,count2dt)
!    TP
       iv2=windy_tp/0.001
       DO J=ISLAT,IELAT
       DO I=ISLON,IELON
          IF(NSP(I,J)==0)THEN
            IV2(I-ISLON+1,J-ISLAT+1)=-32767
          ENDIF
       ENDDO
       ENDDO
       stat=nf90mpi_inq_varid(ncid,'w_tp',vid)
       stat=nf90mpi_fill_var_rec(ncid,vid,irec)
       stat=nf90mpi_put_var_all(ncid,vid,iv2,start2dt,count2dt)
!    TZ
       iv2=windy_tz/0.001
       DO J=ISLAT,IELAT
       DO I=ISLON,IELON
          IF(NSP(I,J)==0)THEN
            IV2(I-ISLON+1,J-ISLAT+1)=-32767
          ENDIF
       ENDDO
       ENDDO
       stat=nf90mpi_inq_varid(ncid,'w_tz',vid)
       stat=nf90mpi_fill_var_rec(ncid,vid,irec)
       stat=nf90mpi_put_var_all(ncid,vid,iv2,start2dt,count2dt)
!    TH
       iv2=windy_th/0.1
       DO J=ISLAT,IELAT
       DO I=ISLON,IELON
          IF(NSP(I,J)==0)THEN
            IV2(I-ISLON+1,J-ISLAT+1)=-32767
          ENDIF
       ENDDO
       ENDDO
       stat=nf90mpi_inq_varid(ncid,'w_th',vid)
       stat=nf90mpi_fill_var_rec(ncid,vid,irec)
       stat=nf90mpi_put_var_all(ncid,vid,iv2,start2dt,count2dt)
     END IF

     IF(ISBREAK)THEN
        stat=nf90mpi_inq_varid(ncid,'ra',vid)
        stat=nf90mpi_fill_var_rec(ncid,vid,irec)
        stat=nf90mpi_put_var_all(ncid,vid,RA,start2dt,count2dt)

        stat=nf90mpi_inq_varid(ncid,'bbr',vid)
        stat=nf90mpi_fill_var_rec(ncid,vid,irec)
        stat=nf90mpi_put_var_all(ncid,vid,BBR,start2dt,count2dt)
     ENDIF
    
     IF(ISCURRENT)THEN
       stat=nf90mpi_inq_varid(ncid,'UX',vid)
       stat=nf90mpi_fill_var_rec(ncid,vid,irec)
       stat=nf90mpi_put_var_all(ncid,vid,UX(ISLON:IELON,ISLAT:IELAT),start2dt,count2dt)

       stat=nf90mpi_inq_varid(ncid,'UY',vid)
       stat=nf90mpi_fill_var_rec(ncid,vid,irec)
       stat=nf90mpi_put_var_all(ncid,vid,UY(ISLON:IELON,ISLAT:IELAT),start2dt,count2dt)

       stat=nf90mpi_inq_varid(ncid,'UXX',vid)
       stat=nf90mpi_fill_var_rec(ncid,vid,irec)
       stat=nf90mpi_put_var_all(ncid,vid,UXX,start2dt,count2dt)

       stat=nf90mpi_inq_varid(ncid,'UXY',vid)
       stat=nf90mpi_fill_var_rec(ncid,vid,irec)
       stat=nf90mpi_put_var_all(ncid,vid,UXY,start2dt,count2dt)

       stat=nf90mpi_inq_varid(ncid,'UYX',vid)
       stat=nf90mpi_fill_var_rec(ncid,vid,irec)
       stat=nf90mpi_put_var_all(ncid,vid,UYX,start2dt,count2dt)

       stat=nf90mpi_inq_varid(ncid,'UYY',vid)
       stat=nf90mpi_fill_var_rec(ncid,vid,irec)
       stat=nf90mpi_put_var_all(ncid,vid,UYY,start2dt,count2dt)
     ENDIF

     stat=nf90mpi_begin_indep_data(ncid)

     IF(MSR)THEN
       CTIME=DATESTR(DATEVEC(TT,1),-1)
       stat=nf90mpi_inq_varid(ncid,'ctime',vid)
       stat=nf90mpi_put_var(ncid,vid,ctime,startct,countct)

       stat=nf90mpi_inq_varid(ncid,'time',vid)
       TTT(1)=TT
       stat=nf90mpi_put_var(ncid,vid,TTT,start1dt,count1dt)
     ENDIF
     stat=nf90mpi_end_indep_data(ncid)
     stat=nf90mpi_close(ncid)

     deallocate(iv2,iv3)
      IF(MYID==NPROCS-1)PRINT*,"ALL DATA WRITE OVER!"
     
     RETURN
     END SUBROUTINE OutNcFile
!======================================================================

     SUBROUTINE WRITE_RES(FileName,ARR)
     USE data_kind_mod_wave 
     USE CONTROL_WAVE,ONLY : ISLON,IELON,ISLAT,IELAT,TT,MSR
     USE CONST_WAVE, ONLY : KL,JL
     USE ALL_VAR_WAVE,ONLY : XGRID,YGRID,ZBV,WKS17,THET
     USE LIMS_WAVE,ONLY : MYID,NPROCS,IM,JM
     USE NcFileMod
     USE time_mod
     USE pnetcdf
     IMPLICIT NONE
     INCLUDE 'mpif.h'

     REAL(kind_r4),INTENT(IN),DIMENSION(KL,JL,ISLON:IELON,ISLAT:IELAT) :: ARR
     CHARACTER(LEN=*),INTENT(IN) :: FileName
!    Pnetcdf
     INTEGER(kind_i8) :: IM_p,JM_p,KB_p,KZ_p,clen,irec,ulimlen
     INTEGER :: vid,vidlon,vidlat,viddep,vidz,dimids(5),dimids_4dt(4),dimidscl(2),dimst
     INTEGER :: ncid,stat
     REAL,PARAMETER ::OUT_LAND=1.E20
     INTEGER(kind_i8) :: start4dt(5),count4dt(5)
     real(kind_r8) :: ttt(1)
     integer(kind_i8) :: start1dt(1),count1dt(1),startct(2),countct(2)
!    LOCAL
     INTEGER I,J,K
     INTEGER MES,IERR,STATUS(MPI_STATUS_SIZE)
     REAL(kind_r4),DIMENSION(KL,JL,ISLON:IELON,ISLAT:IELAT) :: TMPARR 
     CHARACTER*14 CTIME

     IM_p=IM;JM_p=JM;KB_p=KL;KZ_p=JL;clen=20;ulimlen=NF90_UNLIMITED

     stat=nf90mpi_create(masnum_mpi_comm,FileName,         &
                         IOR(NF90_CLOBBER,NF90_64BIT_OFFSET), &
                         MPI_INFO_NULL,ncid)

     stat=nf90mpi_def_dim(ncid,"lon",IM_p,dimids(3))
     stat=nf90mpi_def_var(ncid,"lon",nf90_real,dimids(3),vidlon)
     stat=nf90mpi_put_att(ncid,vidlon,'units','degrees_east')

     stat=nf90mpi_def_dim(ncid,"lat",JM_p,dimids(4))
     stat=nf90mpi_def_var(ncid,"lat",nf90_real,dimids(4),vidlat)
     stat=nf90mpi_put_att(ncid,vidlat,'units','degrees_north')

     stat=nf90mpi_def_dim(ncid,"fre",KB_p,dimids(1))
     stat=nf90mpi_def_var(ncid,"fre",nf90_real,dimids(1),viddep)
     stat=nf90mpi_put_att(ncid,viddep,'units','second')

     stat=nf90mpi_def_dim(ncid,"dir",KZ_p,dimids(2))
     stat=nf90mpi_def_var(ncid,"dir",nf90_real,dimids(2),vidz)
     stat=nf90mpi_put_att(ncid,vidz,'units','degree')

     stat=nf90mpi_def_dim(ncid,"time",ulimlen,dimst)
     stat=nf90mpi_def_var(ncid,"time",nf90_double,dimst,vid)
     stat=nf90mpi_put_att(ncid,vid,'units','hours since 0000-01-01 00:00:00')
      dimids(5)=dimst

     stat=nf90mpi_def_dim(ncid,"char",clen,dimidscl(1))

     dimidscl(2)=dimst
     stat=nf90mpi_def_var(ncid,'ctime',nf90_char,dimidscl,vid)

     stat=nf90mpi_def_var(ncid,'ee',nf90_real,dimids,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)

     stat=nf90mpi_enddef(ncid)

     stat=nf90mpi_begin_indep_data(ncid)

     IF(MSR)THEN
        stat=nf90mpi_put_var(ncid,vidlon,XGRID(1:IM_p,1))
        stat=nf90mpi_put_var(ncid,vidlat,YGRID(1,:))
        stat=nf90mpi_put_var(ncid,viddep,WKS17(1:KL))
        stat=nf90mpi_put_var(ncid,vidz,THET(1:JL))
      ENDIF

      stat=nf90mpi_end_indep_data(ncid)

      stat=nf90mpi_close(ncid)

!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!    WRITE DATA
!>>>>>>>>>>>>>>>>>>o>>>>>>>>>>>>>>>>>>

    stat=nf90mpi_open(masnum_mpi_comm,&
                      FileName      ,&
                      NF_WRITE      ,&
                      MPI_INFO_NULL ,&
                      ncid           )


     start4dt(3)=islon;start4dt(4)=islat;start4dt(1)=1;start4dt(2)=1;start4dt(5)=1
     count4dt(3)=ielon-islon+1;count4dt(4)=ielat-islat+1;count4dt(1)=KL;count4dt(2)=JL;count4dt(5)=1

     startct(1)=1;startct(2)=1
     countct(1)=20;countct(2)=1
     irec=1
     TMPARR=ARR
     stat=nf90mpi_inq_varid(ncid,'ee',vid)
     stat=nf90mpi_fill_var_rec(ncid,vid,irec)
     stat=nf90mpi_put_var_all(ncid,vid,tmparr,start4dt,count4dt)

     stat=nf90mpi_begin_indep_data(ncid)
     IF(MSR)THEN
     CTIME=DATESTR(DATEVEC(TT,1),-1)
     stat=nf90mpi_inq_varid(ncid,'ctime',vid)
     stat=nf90mpi_put_var(ncid,vid,ctime,startct,countct)

     stat=nf90mpi_inq_varid(ncid,'time',vid)
     TTT(1)=TT;start1dt=1;count1dt=1
     stat=nf90mpi_put_var(ncid,vid,TTT,start1dt,count1dt)
     ENDIF
     stat=nf90mpi_end_indep_data(ncid)
     stat=nf90mpi_close(ncid)
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
     IF(MYID==NPROCS-1)PRINT*,"RESTART DATA WRITE OVER!"
     RETURN
     END SUBROUTINE
!======================================================================
     SUBROUTINE READ_RES(FileName,ARR)
     USE data_kind_mod_wave 
     USE CONTROL_WAVE,ONLY : ISLON,IELON,ISLAT,IELAT,TT,T1
     USE CONST_WAVE, ONLY : KL,JL,SMVALUE
     USE ALL_VAR_WAVE,ONLY : ZBV
     USE LIMS_WAVE,ONLY : MYID,NPROCS
     USE NcFileMod
     USE time_mod
     USE parallel_mod_wave
     IMPLICIT NONE
     INCLUDE 'netcdf.inc'
     REAL(kind_r4),INTENT(OUT),DIMENSION(KL,JL,ISLON:IELON,ISLAT:IELAT) :: ARR
     CHARACTER(LEN=*),INTENT(IN) :: FileName
!    LOCAL
     INTEGER I,J,K
!!     REAL(kind_r4),DIMENSION(ISLON:IELON,ISLAT:IELAT,KL) :: TMPARR 
     REAL T
     CHARACTER*14 CTIME
      INTEGER ncID,vID,STATUS
     INTEGER COUNT(5),START(5)
     DATA START/1,1,1,1,1/
     DATA COUNT/1,1,1,1,1/

     START(3)=ISLON;START(4)=ISLAT;start(5)=1
     COUNT(3)=IELON-ISLON+1;COUNT(4)=IELAT-ISLAT+1
     COUNT(1)=KL;COUNT(2)=JL;count(5)=1

     CALL NC_READ(FileName,'ctime',CTIME,LREC=1)
     CALL NC_READ(FileName,'time',T,VAR_DOU=TT,LREC=1)

     IF(ABS(TT-T1)>SMVALUE)THEN
     WRITE(*,*)"the resart time and the set time are not consistent"
     WRITE(*,*)MYID,T1,CTIME,TT
     STOP
     END IF

     Status=NF_OPEN(trim(FILENAME),NF_NOWRITE,ncID)
     IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

     Status=NF_INQ_VARID(ncID,'ee',vID)
     IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

     Status=NF_GET_VARA_REAL(ncID,vID,start,count,ARR)
     IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

     Status=NF_CLOSE(ncID)
     IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

     RETURN
     END SUBROUTINE READ_RES
!======================================================================
END MODULE MOD_IO_WAVE
