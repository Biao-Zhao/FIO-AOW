  MODULE MOD_IO
     USE CONTROL, ONLY : MSR
     USE data_kind_mod
     USE time_mod
     USE NcFileMod
     USE parallel_mod
!     USE LIMS, ONLY : MYID
     CONTAINS
!======================================================================
     SUBROUTINE READ_RES(FileName)
     USE data_kind_mod 
     USE CONTROL
     USE ALL_VAR,ONLY : WUBOT,WVBOT,AAM2D,UA,UAB,VA,VAB,EL,    &
         ELB,ET,ETB,EGB,UTB,VTB,U,UB,W,V,VB,T,TB,S,SB,RHO,     &
         ADX2D,ADY2D,ADVUA,ADVVA,KM,KH,KQ,L,Q2,Q2B,AAM,Q2L,Q2LB
     IMPLICIT NONE
     CHARACTER(LEN=*),INTENT(IN) :: FileName
     REAL,PARAMETER::MISLAND=1.E8
!----------------------------------------------------------------------
!     2D
!     WUBOT
     CALL Nc_Read(FILENAME,'WUBOT',WUBOT(ISLON:IELON,ISLAT:IELAT),I1=ISLON,J1=ISLAT,LAND=misland)
!     WVBOT
     CALL Nc_Read(FILENAME,'WVBOT',WVBOT(ISLON:IELON,ISLAT:IELAT),I1=ISLON,J1=ISLAT,LAND=misland)
!     AAM2D
     CALL Nc_Read(FILENAME,'AAM2D',AAM2D(ISLON:IELON,ISLAT:IELAT),I1=ISLON,J1=ISLAT,LAND=misland)
!     UA
     CALL Nc_Read(FILENAME,'UA',UA(ISLON:IELON,ISLAT:IELAT),I1=ISLON,J1=ISLAT,LAND=misland)
!     UAB
     CALL Nc_Read(FILENAME,'UAB',UAB(ISLON:IELON,ISLAT:IELAT),I1=ISLON,J1=ISLAT,LAND=misland)
!     VA
     CALL Nc_Read(FILENAME,'VA',VA(ISLON:IELON,ISLAT:IELAT),I1=ISLON,J1=ISLAT,LAND=misland)
!     VAB
     CALL Nc_Read(FILENAME,'VAB',VAB(ISLON:IELON,ISLAT:IELAT),I1=ISLON,J1=ISLAT,LAND=misland)
!     EL
     CALL Nc_Read(FILENAME,'EL',EL(ISLON:IELON,ISLAT:IELAT),I1=ISLON,J1=ISLAT,LAND=misland)
!     ELB
     CALL Nc_Read(FILENAME,'ELB',ELB(ISLON:IELON,ISLAT:IELAT),I1=ISLON,J1=ISLAT,LAND=misland)
!     ET
     CALL Nc_Read(FILENAME,'ET',ET(ISLON:IELON,ISLAT:IELAT),I1=ISLON,J1=ISLAT,LAND=misland)
!     ETB
     CALL Nc_Read(FILENAME,'ETB',ETB(ISLON:IELON,ISLAT:IELAT),I1=ISLON,J1=ISLAT,LAND=misland)
!     EGB
     CALL Nc_Read(FILENAME,'EGB',EGB(ISLON:IELON,ISLAT:IELAT),I1=ISLON,J1=ISLAT,LAND=misland)
!     UTB
     CALL Nc_Read(FILENAME,'UTB',UTB(ISLON:IELON,ISLAT:IELAT),I1=ISLON,J1=ISLAT,LAND=misland)
!     VTB
     CALL Nc_Read(FILENAME,'VTB',VTB(ISLON:IELON,ISLAT:IELAT),I1=ISLON,J1=ISLAT,LAND=misland)
!     ADX2D
     CALL Nc_Read(FILENAME,'ADX2D',ADX2D(ISLON:IELON,ISLAT:IELAT),I1=ISLON,J1=ISLAT,LAND=misland)
!     ADY2D
     CALL Nc_Read(FILENAME,'ADY2D',ADY2D(ISLON:IELON,ISLAT:IELAT),I1=ISLON,J1=ISLAT,LAND=misland)
!     ADVUA
     CALL Nc_Read(FILENAME,'ADVUA',ADVUA(ISLON:IELON,ISLAT:IELAT),I1=ISLON,J1=ISLAT,LAND=misland)
!     ADVVA
     CALL Nc_Read(FILENAME,'ADVVA',ADVVA(ISLON:IELON,ISLAT:IELAT),I1=ISLON,J1=ISLAT,LAND=misland)
! ----------------------------------------------------------------------
!     3D
!     U
     CALL Nc_Read(FILENAME,'U',U(ISLON:IELON,ISLAT:IELAT,:),I1=ISLON,J1=ISLAT,LAND=misland)
!     UB
     CALL Nc_Read(FILENAME,'UB',UB(ISLON:IELON,ISLAT:IELAT,:),I1=ISLON,J1=ISLAT,LAND=misland)
!     V
     CALL Nc_Read(FILENAME,'V',V(ISLON:IELON,ISLAT:IELAT,:),I1=ISLON,J1=ISLAT,LAND=misland)
!     VB
     CALL Nc_Read(FILENAME,'VB',VB(ISLON:IELON,ISLAT:IELAT,:),I1=ISLON,J1=ISLAT,LAND=misland)
!     W
     CALL Nc_Read(FILENAME,'W',W(ISLON:IELON,ISLAT:IELAT,:),I1=ISLON,J1=ISLAT,LAND=misland)
!     T
     !CALL Nc_Read(FILENAME,'T',T(ISLON:IELON,ISLAT:IELAT,:),I1=ISLON,J1=ISLAT,LAND=misland)
     CALL Nc_Read(FILENAME,'TT',T(ISLON:IELON,ISLAT:IELAT,:),I1=ISLON,J1=ISLAT,LAND=misland)
!     TB
     CALL Nc_Read(FILENAME,'TB',TB(ISLON:IELON,ISLAT:IELAT,:),I1=ISLON,J1=ISLAT,LAND=misland)
!     S
     CALL Nc_Read(FILENAME,'S',S(ISLON:IELON,ISLAT:IELAT,:),I1=ISLON,J1=ISLAT,LAND=misland)
!     SB
     CALL Nc_Read(FILENAME,'SB',SB(ISLON:IELON,ISLAT:IELAT,:),I1=ISLON,J1=ISLAT,LAND=misland)
!     RHO
     CALL Nc_Read(FILENAME,'RHO',RHO(ISLON:IELON,ISLAT:IELAT,:),I1=ISLON,J1=ISLAT,LAND=misland)
!     KM
     CALL Nc_Read(FILENAME,'KM',KM(ISLON:IELON,ISLAT:IELAT,:),I1=ISLON,J1=ISLAT,LAND=misland)
!     KH
     CALL Nc_Read(FILENAME,'KH',KH(ISLON:IELON,ISLAT:IELAT,:),I1=ISLON,J1=ISLAT,LAND=misland)
!     KQ
     CALL Nc_Read(FILENAME,'KQ',KQ(ISLON:IELON,ISLAT:IELAT,:),I1=ISLON,J1=ISLAT,LAND=misland)
!     L
     !CALL Nc_Read(FILENAME,'L',L(ISLON:IELON,ISLAT:IELAT,:),I1=ISLON,J1=ISLAT,LAND=misland)
     CALL Nc_Read(FILENAME,'LL',L(ISLON:IELON,ISLAT:IELAT,:),I1=ISLON,J1=ISLAT,LAND=misland)
!     Q2
     CALL Nc_Read(FILENAME,'Q2',Q2(ISLON:IELON,ISLAT:IELAT,:),I1=ISLON,J1=ISLAT,LAND=misland)
!     Q2B
     CALL Nc_Read(FILENAME,'Q2B',Q2B(ISLON:IELON,ISLAT:IELAT,:),I1=ISLON,J1=ISLAT,LAND=misland)
!     AAM
     CALL Nc_Read(FILENAME,'AAM',AAM(ISLON:IELON,ISLAT:IELAT,:),I1=ISLON,J1=ISLAT,LAND=misland)
!     Q2L
     CALL Nc_Read(FILENAME,'Q2L',Q2L(ISLON:IELON,ISLAT:IELAT,:),I1=ISLON,J1=ISLAT,LAND=misland)
!     Q2LB
     CALL Nc_Read(FILENAME,'Q2LB',Q2LB(ISLON:IELON,ISLAT:IELAT,:),I1=ISLON,J1=ISLAT,LAND=misland)
     RETURN
     END SUBROUTINE READ_RES
!======================================================================
     SUBROUTINE READ_RES_HYCOM(FileName)
     USE data_kind_mod
     USE CONTROL
     USE ALL_VAR,ONLY : WUBOT,WVBOT,AAM2D,UA,UAB,VA,VAB,EL,    &
         ELB,ET,ETB,EGB,UTB,VTB,U,UB,W,V,VB,T,TB,S,SB,RHO,     &
         ADX2D,ADY2D,ADVUA,ADVVA,KM,KH,KQ,L,Q2,Q2B,AAM,Q2L,Q2LB
     IMPLICIT NONE
     CHARACTER(LEN=*),INTENT(IN) :: FileName
     REAL,PARAMETER::MISLAND=1.E8
!----------------------------------------------------------------------
     CALL Nc_Read(FILENAME,'CURU',UB(ISLON:IELON,ISLAT:IELAT,:),I1=ISLON,J1=ISLAT,LAND=misland)
     CALL Nc_Read(FILENAME,'CURV',VB(ISLON:IELON,ISLAT:IELAT,:),I1=ISLON,J1=ISLAT,LAND=misland)
     CALL Nc_Read(FILENAME,'TEMP',TB(ISLON:IELON,ISLAT:IELAT,:),I1=ISLON,J1=ISLAT,LAND=misland)
     CALL Nc_Read(FILENAME,'SALT',SB(ISLON:IELON,ISLAT:IELAT,:),I1=ISLON,J1=ISLAT,LAND=misland)
     CALL Nc_Read(FILENAME,'EL',ELB(ISLON:IELON,ISLAT:IELAT),I1=ISLON,J1=ISLAT,LAND=misland)
     CALL Nc_Read(FILENAME,'UA',UAB(ISLON:IELON,ISLAT:IELAT),I1=ISLON,J1=ISLAT,LAND=misland)
     CALL Nc_Read(FILENAME,'VA',VAB(ISLON:IELON,ISLAT:IELAT),I1=ISLON,J1=ISLAT,LAND=misland) 
     RETURN
     END SUBROUTINE READ_RES_HYCOM

!======================================================================
     SUBROUTINE WRITE_RES(FileName)
     USE data_kind_mod 
     USE CONTROL
     USE LIMS
     USE CONST
     USE ALL_VAR,ONLY : WUBOT,WVBOT,AAM2D,UA,UAB,VA,VAB,EL,    &
         ELB,ET,ETB,EGB,UTB,VTB,U,UB,W,V,VB,T,TB,S,SB,RHO,     &
         ADX2D,ADY2D,ADVUA,ADVVA,KM,KH,KQ,L,Q2,Q2B,AAM,Q2L,    &
         Q2LB,X,Y,ZZ
     USE pnetcdf
     IMPLICIT NONE
     include 'mpif.h'
     
     CHARACTER(LEN=*),INTENT(IN) :: FileName
     CHARACTER*14 CTIME
     INTEGER MES,IERR,STATUS(MPI_STATUS_SIZE)
     
     INTEGER(kind_i8) :: IM_p,JM_p,KB_p,ulimlen,clen,irec
     INTEGER :: vid,vidlon,vidlat,viddep,dimids(4),dimids_2dt(3),dimidscl(2)
     INTEGER(kind_i8) :: start2dt(3),start3dt(4),startct(2)
     INTEGER(kind_i8) :: count2dt(3),count3dt(4),countct(2)
     INTEGER(kind_i8) :: start1dt(1),count1dt(1)
     real(kind_r8) TTT(1)
     INTEGER :: ncid,stat

     IM_p=IM;JM_p=JM;KB_p=KB;ulimlen=NF90_UNLIMITED;clen=20

     stat=nf90mpi_create(pom_mpi_comm,filename,           &
                         IOR(NF_CLOBBER,NF_64BIT_OFFSET), &
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

     stat=nf90mpi_def_dim(ncid,"time",ulimlen,dimids(4))
     stat=nf90mpi_def_var(ncid,"time",nf90_double,dimids(4),vid)
     stat=nf90mpi_put_att(ncid,vid,'units','hours since 0000-01-01 00:00:00')

     stat=nf90mpi_def_dim(ncid,"char",clen,dimidscl(1))

     dimids_2dt(1)=dimids(1)
     dimids_2dt(2)=dimids(2)
     dimids_2dt(3)=dimids(4)
     dimidscl(2)=dimids(4)


!------------------------------------------------------------------------------------------------------------------------
!!  define
!!  2D
!   WUBOT
     stat=nf90mpi_def_var(ncid,'WUBOT',nf90_real,dimids_2dt,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
!   WVBOT
     stat=nf90mpi_def_var(ncid,'WVBOT',nf90_real,dimids_2dt,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
!   AAM2D
     stat=nf90mpi_def_var(ncid,'AAM2D',nf90_real,dimids_2dt,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND) 
!   UA
     stat=nf90mpi_def_var(ncid,'UA',nf90_real,dimids_2dt,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
!   UAB
     stat=nf90mpi_def_var(ncid,'UAB',nf90_real,dimids_2dt,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
!   VA
     stat=nf90mpi_def_var(ncid,'VA',nf90_real,dimids_2dt,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
!   VAB
     stat=nf90mpi_def_var(ncid,'VAB',nf90_real,dimids_2dt,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
!   EL
     stat=nf90mpi_def_var(ncid,'EL',nf90_real,dimids_2dt,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
!   ELB
     stat=nf90mpi_def_var(ncid,'ELB',nf90_real,dimids_2dt,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
!   ET
     stat=nf90mpi_def_var(ncid,'ET',nf90_real,dimids_2dt,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
!   ETB
     stat=nf90mpi_def_var(ncid,'ETB',nf90_real,dimids_2dt,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
!   EGB
     stat=nf90mpi_def_var(ncid,'EGB',nf90_real,dimids_2dt,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
!   UTB
     stat=nf90mpi_def_var(ncid,'UTB',nf90_real,dimids_2dt,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
!   VTB
     stat=nf90mpi_def_var(ncid,'VTB',nf90_real,dimids_2dt,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
!   ADX2D
     stat=nf90mpi_def_var(ncid,'ADX2D',nf90_real,dimids_2dt,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
!   ADY2D
     stat=nf90mpi_def_var(ncid,'ADY2D',nf90_real,dimids_2dt,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
!   ADVUA
     stat=nf90mpi_def_var(ncid,'ADVUA',nf90_real,dimids_2dt,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
!   ADVVA
     stat=nf90mpi_def_var(ncid,'ADVVA',nf90_real,dimids_2dt,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)

!------------------------------------------------------------------------------------------------------------------------
!!  3D
!!  U
     stat=nf90mpi_def_var(ncid,'U',nf90_real,dimids,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
!!  UB
     stat=nf90mpi_def_var(ncid,'UB',nf90_real,dimids,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
!!  V
     stat=nf90mpi_def_var(ncid,'V',nf90_real,dimids,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
!!  VB
     stat=nf90mpi_def_var(ncid,'VB',nf90_real,dimids,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
!!  W
     stat=nf90mpi_def_var(ncid,'W',nf90_real,dimids,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
!!  TT
     stat=nf90mpi_def_var(ncid,'TT',nf90_real,dimids,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
!!  TB
     stat=nf90mpi_def_var(ncid,'TB',nf90_real,dimids,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
!!  S
     stat=nf90mpi_def_var(ncid,'S',nf90_real,dimids,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
!!  SB
     stat=nf90mpi_def_var(ncid,'SB',nf90_real,dimids,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
!!  RHO
     stat=nf90mpi_def_var(ncid,'RHO',nf90_real,dimids,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
!!  KM
     stat=nf90mpi_def_var(ncid,'KM',nf90_real,dimids,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
!!  KH
     stat=nf90mpi_def_var(ncid,'KH',nf90_real,dimids,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
!!  KQ
     stat=nf90mpi_def_var(ncid,'KQ',nf90_real,dimids,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
!!  LL
     stat=nf90mpi_def_var(ncid,'LL',nf90_real,dimids,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
!!  Q2
     stat=nf90mpi_def_var(ncid,'Q2',nf90_real,dimids,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
!!  Q2B
     stat=nf90mpi_def_var(ncid,'Q2B',nf90_real,dimids,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
!!  AAM
     stat=nf90mpi_def_var(ncid,'AAM',nf90_real,dimids,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
!!  Q2L
     stat=nf90mpi_def_var(ncid,'Q2L',nf90_real,dimids,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
!!  Q2LB
     stat=nf90mpi_def_var(ncid,'Q2LB',nf90_real,dimids,vid)
     stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
     stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)
 !!  time
     stat=nf90mpi_def_var(ncid,'ctime',nf90_char,dimidscl,vid)
 !!  end define
     stat=nf90mpi_enddef(ncid)
 !!  write x y z
     stat=nf90mpi_begin_indep_data(ncid)
     IF(MSR)THEN
       stat=nf90mpi_put_var(ncid,vidlon,x)
       stat=nf90mpi_put_var(ncid,vidlat,y)
       stat=nf90mpi_put_var(ncid,viddep,zz)
     ENDIF
     stat=nf90mpi_end_indep_data(ncid)
 !!  close
     stat=nf90mpi_close(ncid)
!------------------------------------------------------------------------------------------------------------------------
     start2dt(1)=islon;start2dt(2)=islat;start2dt(3)=1
     start3dt(1)=islon;start3dt(2)=islat;start3dt(4)=1
     start3dt(3)=1
     startct(1)=1;startct(2)=1;irec=1
     start1dt(1)=1

     count2dt(1)=ielon-islon+1;count2dt(2)=ielat-islat+1;count2dt(3)=1
     count3dt(1)=ielon-islon+1;count3dt(2)=ielat-islat+1;count3dt(4)=1;count3dt(3)=kb
     countct(1)=20;countct(2)=1
     count1dt(1)=1
!------------------------------------------------------------------------------------------------------------------------
!!  write
     stat=nf90mpi_open(pom_mpi_comm  ,&
                       filename      ,&
                       NF_WRITE      ,&
                       MPI_INFO_NULL ,&
                       ncid           )
!------------------------------------------------------------------------------------------------------------------------
!!  2D
!   WUBOT
    stat=nf90mpi_inq_varid(ncid,'WUBOT',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,WUBOT(ISLON:IELON,ISLAT:IELAT),start2dt,count2dt)
!   WVBOT
    stat=nf90mpi_inq_varid(ncid,'WVBOT',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,WVBOT(ISLON:IELON,ISLAT:IELAT),start2dt,count2dt)
!   AAM2D
    stat=nf90mpi_inq_varid(ncid,'AAM2D',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,AAM2D(ISLON:IELON,ISLAT:IELAT),start2dt,count2dt)
!   UA
    stat=nf90mpi_inq_varid(ncid,'UA',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,UA(ISLON:IELON,ISLAT:IELAT),start2dt,count2dt)
!   UAB
    stat=nf90mpi_inq_varid(ncid,'UAB',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,UAB(ISLON:IELON,ISLAT:IELAT),start2dt,count2dt)
!   VA
    stat=nf90mpi_inq_varid(ncid,'VA',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,VA(ISLON:IELON,ISLAT:IELAT),start2dt,count2dt)
!   VAB
    stat=nf90mpi_inq_varid(ncid,'VAB',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,VAB(ISLON:IELON,ISLAT:IELAT),start2dt,count2dt)
!   EL
    stat=nf90mpi_inq_varid(ncid,'EL',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,EL(ISLON:IELON,ISLAT:IELAT),start2dt,count2dt)
!   ELB
    stat=nf90mpi_inq_varid(ncid,'ELB',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,ELB(ISLON:IELON,ISLAT:IELAT),start2dt,count2dt)
!   ET
    stat=nf90mpi_inq_varid(ncid,'ET',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,ET(ISLON:IELON,ISLAT:IELAT),start2dt,count2dt)
!   ETB
    stat=nf90mpi_inq_varid(ncid,'ETB',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,ETB(ISLON:IELON,ISLAT:IELAT),start2dt,count2dt)
!   EGB
    stat=nf90mpi_inq_varid(ncid,'EGB',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,ETB(ISLON:IELON,ISLAT:IELAT),start2dt,count2dt)
!   UTB
    stat=nf90mpi_inq_varid(ncid,'UTB',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,UTB(ISLON:IELON,ISLAT:IELAT),start2dt,count2dt)
!   VTB
    stat=nf90mpi_inq_varid(ncid,'VTB',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,VTB(ISLON:IELON,ISLAT:IELAT),start2dt,count2dt)
!   ADX2D
    stat=nf90mpi_inq_varid(ncid,'ADX2D',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,ADX2D(ISLON:IELON,ISLAT:IELAT),start2dt,count2dt)
!   ADY2D
    stat=nf90mpi_inq_varid(ncid,'ADY2D',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,ADY2D(ISLON:IELON,ISLAT:IELAT),start2dt,count2dt)
!   ADVUA
    stat=nf90mpi_inq_varid(ncid,'ADVUA',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,ADVUA(ISLON:IELON,ISLAT:IELAT),start2dt,count2dt)
!   ADVVA
    stat=nf90mpi_inq_varid(ncid,'ADVVA',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,ADVVA(ISLON:IELON,ISLAT:IELAT),start2dt,count2dt)
!------------------------------------------------------------------------------------------------------------------------
!!  write
!!  3D
!   U
    stat=nf90mpi_inq_varid(ncid,'U',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,U(ISLON:IELON,ISLAT:IELAT,:),start3dt,count3dt)
!   UB
    stat=nf90mpi_inq_varid(ncid,'UB',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,UB(ISLON:IELON,ISLAT:IELAT,:),start3dt,count3dt)
!   V
    stat=nf90mpi_inq_varid(ncid,'V',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,V(ISLON:IELON,ISLAT:IELAT,:),start3dt,count3dt)
!   VB
    stat=nf90mpi_inq_varid(ncid,'VB',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,VB(ISLON:IELON,ISLAT:IELAT,:),start3dt,count3dt)
!   W
    stat=nf90mpi_inq_varid(ncid,'W',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,W(ISLON:IELON,ISLAT:IELAT,:),start3dt,count3dt)
!   TT
    stat=nf90mpi_inq_varid(ncid,'TT',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,T(ISLON:IELON,ISLAT:IELAT,:),start3dt,count3dt)
!   TB
    stat=nf90mpi_inq_varid(ncid,'TB',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,TB(ISLON:IELON,ISLAT:IELAT,:),start3dt,count3dt)
!   S
    stat=nf90mpi_inq_varid(ncid,'S',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,S(ISLON:IELON,ISLAT:IELAT,:),start3dt,count3dt)
!   SB
    stat=nf90mpi_inq_varid(ncid,'SB',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,SB(ISLON:IELON,ISLAT:IELAT,:),start3dt,count3dt)
!   RHO
    stat=nf90mpi_inq_varid(ncid,'RHO',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,RHO(ISLON:IELON,ISLAT:IELAT,:),start3dt,count3dt)
!   KM
    stat=nf90mpi_inq_varid(ncid,'KM',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,KM(ISLON:IELON,ISLAT:IELAT,:),start3dt,count3dt)
!   KH
    stat=nf90mpi_inq_varid(ncid,'KH',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,KH(ISLON:IELON,ISLAT:IELAT,:),start3dt,count3dt)
!   KQ
    stat=nf90mpi_inq_varid(ncid,'KQ',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,KQ(ISLON:IELON,ISLAT:IELAT,:),start3dt,count3dt)
!   LL
    stat=nf90mpi_inq_varid(ncid,'LL',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,L(ISLON:IELON,ISLAT:IELAT,:),start3dt,count3dt)
!   Q2
    stat=nf90mpi_inq_varid(ncid,'Q2',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,Q2(ISLON:IELON,ISLAT:IELAT,:),start3dt,count3dt)
!   Q2B
    stat=nf90mpi_inq_varid(ncid,'Q2B',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,Q2B(ISLON:IELON,ISLAT:IELAT,:),start3dt,count3dt)
!   AAM
    stat=nf90mpi_inq_varid(ncid,'AAM',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,AAM(ISLON:IELON,ISLAT:IELAT,:),start3dt,count3dt)
!   Q2L
    stat=nf90mpi_inq_varid(ncid,'Q2L',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,Q2L(ISLON:IELON,ISLAT:IELAT,:),start3dt,count3dt)
!   Q2LB
    stat=nf90mpi_inq_varid(ncid,'Q2LB',vid)
    stat=nf90mpi_fill_var_rec(ncid,vid,irec)
    stat=nf90mpi_put_var_all(ncid,vid,Q2LB(ISLON:IELON,ISLAT:IELAT,:),start3dt,count3dt)
!------------------------------------------------------------------------------------------------------------------------
!   time
    stat=nf90mpi_begin_indep_data(ncid)
    IF(MSR)THEN
      CTIME=DATESTR(DATEVEC(TT,1),-1)
      stat=nf90mpi_inq_varid(ncid,'ctime',vid)
      stat=nf90mpi_put_var(ncid,vid,ctime(1:10),startct,countct)

      stat=nf90mpi_inq_varid(ncid,'time',vid)
      TTT(1)=TT
      stat=nf90mpi_put_var(ncid,vid,TTT,start1dt,count1dt)
    PRINT*,"RESTART DATA WRITE OVER!"
    ENDIF
    stat=nf90mpi_end_indep_data(ncid)
    stat=nf90mpi_close(ncid)

    RETURN
    END SUBROUTINE WRITE_RES
!======================================================================
     SUBROUTINE SetNcFile(FileName)
     USE NcFileMod
     USE ALL_VAR,ONLY:X,Y,ZZ
     USE CONTROL,ONLY:MSR, OUT_DATA_OCN_FIG
     USE LIMS,ONLY : IM,JM,KB
     USE data_kind_mod
     USE CONST,ONLY : OUT_LAND
     USE pnetcdf
     include 'mpif.h'

     CHARACTER(LEN=*),INTENT(IN) :: FileName
     INTEGER(kind_i8) :: IM_p,JM_p,KB_p,ulimlen,clen,irec
     integer :: vid,vidlon,vidlat,viddep,dimids(4),dimids_2dt(3),dimidscl(2)
     integer :: ncid,stat

     IM_p=IM;JM_p=JM;KB_p=KB;ulimlen=NF90_UNLIMITED;clen=20

     stat=nf90mpi_create(pom_mpi_comm,filename,         &
                         IOR(NF_CLOBBER,NF_64BIT_OFFSET), &
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
   
      IF(OUT_DATA_OCN_FIG(1))THEN
      stat=nf90mpi_def_var(ncid,'temp',nf90_int2,dimids,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',NF90_FILL_int2)
      stat=nf90mpi_put_att(ncid,vid,'scale_factor',0.01)
      stat=nf90mpi_put_att(ncid,vid,'units','degree')
      stat=nf90mpi_def_var_fill(ncid,vid,0,NF90_FILL_int2)
      END IF

      IF(OUT_DATA_OCN_FIG(2))THEN
      stat=nf90mpi_def_var(ncid,'salt',nf90_int2,dimids,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',NF90_FILL_int2)
      stat=nf90mpi_put_att(ncid,vid,'scale_factor',0.01)
      stat=nf90mpi_put_att(ncid,vid,'units','psu')
      stat=nf90mpi_def_var_fill(ncid,vid,0,NF90_FILL_int2)
      END IF

      IF(OUT_DATA_OCN_FIG(3))THEN
      stat=nf90mpi_def_var(ncid,'curu',nf90_int2,dimids,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',NF90_FILL_int2)
      stat=nf90mpi_put_att(ncid,vid,'scale_factor',0.001)
      stat=nf90mpi_put_att(ncid,vid,'units','m/s')
      stat=nf90mpi_def_var_fill(ncid,vid,0,NF90_FILL_int2)
      END IF

      IF(OUT_DATA_OCN_FIG(4))THEN
      stat=nf90mpi_def_var(ncid,'curv',nf90_int2,dimids,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',NF90_FILL_int2)
      stat=nf90mpi_put_att(ncid,vid,'scale_factor',0.001)
      stat=nf90mpi_put_att(ncid,vid,'units','m/s')
      stat=nf90mpi_def_var_fill(ncid,vid,0,NF90_FILL_int2)
      END IF

      IF(OUT_DATA_OCN_FIG(5))THEN
      stat=nf90mpi_def_var(ncid,'el',nf90_int2,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',NF90_FILL_INT2)
      stat=nf90mpi_put_att(ncid,vid,'scale_factor',0.01)
      stat=nf90mpi_put_att(ncid,vid,'units','m')
      stat=nf90mpi_def_var_fill(ncid,vid,0,NF90_FILL_int2)
      END IF

      IF(OUT_DATA_OCN_FIG(6))THEN
      stat=nf90mpi_def_var(ncid,'bv',nf90_real,dimids,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
      stat=nf90mpi_put_att(ncid,vid,'units','unitless')
      stat=nf90mpi_put_att(ncid,vid,'description','Non-breaking wave induced vertical mixing coefficient')
      stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)

      stat=nf90mpi_def_var(ncid,'tauwx',nf90_real,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
      stat=nf90mpi_put_att(ncid,vid,'units','m/s')
      stat=nf90mpi_put_att(ncid,vid,'description','tau_in_x + tau_diss_x,based on Janssen 2012')
      stat=nf90mpi_def_var_fill(ncid,vid,0, OUT_LAND)

      stat=nf90mpi_def_var(ncid,'tauwy',nf90_real,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
      stat=nf90mpi_put_att(ncid,vid,'units','m/s')
      stat=nf90mpi_put_att(ncid,vid,'description','tau_in_y + tau_diss_y,based on Janssen 2012')
      stat=nf90mpi_def_var_fill(ncid,vid,0, OUT_LAND)

      END IF

      IF(OUT_DATA_OCN_FIG(7))THEN
      stat=nf90mpi_def_var(ncid,'windu',nf90_real,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
      stat=nf90mpi_put_att(ncid,vid,'units','m/s')
      stat=nf90mpi_put_att(ncid,vid,'description','Zonal wind velocity')
      stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)

      stat=nf90mpi_def_var(ncid,'windv',nf90_real,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
      stat=nf90mpi_put_att(ncid,vid,'units','m/s')
      stat=nf90mpi_put_att(ncid,vid,'description','Meridional wind velocity')
      stat=nf90mpi_def_var_fill(ncid,vid,0,OUT_LAND)

      stat=nf90mpi_def_var(ncid,'ustar',nf90_real,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
      stat=nf90mpi_put_att(ncid,vid,'units','m/s')
      stat=nf90mpi_put_att(ncid,vid,'description','Friction velocity')
      stat=nf90mpi_def_var_fill(ncid,vid,0, OUT_LAND)

      stat=nf90mpi_def_var(ncid,'st',nf90_real,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
      stat=nf90mpi_put_att(ncid,vid,'units','W m-2')
      stat=nf90mpi_put_att(ncid,vid,'description','Sensible heat flux')
      stat=nf90mpi_def_var_fill(ncid,vid,0, OUT_LAND)

      stat=nf90mpi_def_var(ncid,'lt',nf90_real,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
      stat=nf90mpi_put_att(ncid,vid,'units','W m-2')
      stat=nf90mpi_put_att(ncid,vid,'description','Latent heat flux')
      stat=nf90mpi_def_var_fill(ncid,vid,0, OUT_LAND)

      stat=nf90mpi_def_var(ncid,'slp',nf90_real,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
      stat=nf90mpi_put_att(ncid,vid,'units','Pasca')
      stat=nf90mpi_put_att(ncid,vid,'description','Mean sea level pressure')
      stat=nf90mpi_def_var_fill(ncid,vid,0, OUT_LAND)

      stat=nf90mpi_def_var(ncid,'rain',nf90_real,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
      stat=nf90mpi_put_att(ncid,vid,'units','mm s-1')
      stat=nf90mpi_put_att(ncid,vid,'description','Precipitation')
      stat=nf90mpi_def_var_fill(ncid,vid,0, OUT_LAND)

      stat=nf90mpi_def_var(ncid,'dsw',nf90_real,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
      stat=nf90mpi_put_att(ncid,vid,'units','W m-2')
      stat=nf90mpi_put_att(ncid,vid,'description','Downward shortwave radiation flux')
      stat=nf90mpi_def_var_fill(ncid,vid,0, OUT_LAND)

      stat=nf90mpi_def_var(ncid,'lw',nf90_real,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
      stat=nf90mpi_put_att(ncid,vid,'units','W m-2')
      stat=nf90mpi_put_att(ncid,vid,'description','Downward longwave radiation flux')
      stat=nf90mpi_def_var_fill(ncid,vid,0, OUT_LAND)

      stat=nf90mpi_def_var(ncid,'rhoa',nf90_real,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
      stat=nf90mpi_put_att(ncid,vid,'units','kg m-3')
      stat=nf90mpi_put_att(ncid,vid,'description','Surface air density')
      stat=nf90mpi_def_var_fill(ncid,vid,0, OUT_LAND)

      stat=nf90mpi_def_var(ncid,'qfx',nf90_real,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
      stat=nf90mpi_put_att(ncid,vid,'units','kg m-2 s-1')
      stat=nf90mpi_put_att(ncid,vid,'description','Surface moisture flux')
      stat=nf90mpi_def_var_fill(ncid,vid,0, OUT_LAND)
      END IF
    
      IF(OUT_DATA_OCN_FIG(8))THEN
      stat=nf90mpi_def_var(ncid,'sst',nf90_real,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
      stat=nf90mpi_put_att(ncid,vid,'units','Celsius')
      stat=nf90mpi_put_att(ncid,vid,'description','Sea surfacce temperature')
      stat=nf90mpi_def_var_fill(ncid,vid,0, OUT_LAND)

      stat=nf90mpi_def_var(ncid,'sss',nf90_real,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
      stat=nf90mpi_put_att(ncid,vid,'units','PSU')
      stat=nf90mpi_put_att(ncid,vid,'description','Sea surface salinity')
      stat=nf90mpi_def_var_fill(ncid,vid,0, OUT_LAND)

      stat=nf90mpi_def_var(ncid,'ssu',nf90_real,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
      stat=nf90mpi_put_att(ncid,vid,'units','m/s')
      stat=nf90mpi_put_att(ncid,vid,'description','Sea surface zonal current velocity')
      stat=nf90mpi_def_var_fill(ncid,vid,0, OUT_LAND)

      stat=nf90mpi_def_var(ncid,'ssv',nf90_real,dimids_2dt,vid)
      stat=nf90mpi_put_att(ncid,vid,'missing_value',OUT_LAND)
      stat=nf90mpi_put_att(ncid,vid,'units','m/s')
      stat=nf90mpi_put_att(ncid,vid,'description','Sea surface meridional current velocity')
      stat=nf90mpi_def_var_fill(ncid,vid,0, OUT_LAND)
      END IF
      

      stat=nf90mpi_def_var(ncid,'ctime',nf90_char,dimidscl,vid)

      stat=nf90mpi_enddef(ncid)

      stat=nf90mpi_begin_indep_data(ncid)

      IF(MSR)THEN
         stat=nf90mpi_put_var(ncid,vidlon,x)
         stat=nf90mpi_put_var(ncid,vidlat,y)
         stat=nf90mpi_put_var(ncid,viddep,zz)
       ENDIF

      stat=nf90mpi_end_indep_data(ncid)
      stat=nf90mpi_close(ncid)
     END SUBROUTINE SetNcFile
!======================================================================
     SUBROUTINE OutNcFile(FileName,TEMPE,SALT,CU,CV,EL)
     USE data_kind_mod
     USE LIMS,ONLY : MYID,NPROCS,KB
     USE CONTROL,ONLY : ISLON,ISLAT,OUT_DATA_OCN_FIG,TT,IELAT,IELON
     USE ALL_VAR,ONLY : FSM,DUM,DVM
     USE coupling_pom_mod
     USE pnetcdf
     INCLUDE 'mpif.h'

     CHARACTER(LEN=*),INTENT(IN) :: FileName
     INTEGER(kind_i8) :: irec
     INTEGER I,J,NcID,vID,stat
     REAL(kind_r4),DIMENSION(ISLON:IELON,ISLAT:IELAT) :: EL
     REAL(kind_r4),DIMENSION(ISLON:IELON,ISLAT:IELAT,KB) :: TEMPE,SALT,CU,CV
     INTEGER(kind_i8) :: start2dt(3),start3dt(4),startct(2)
     INTEGER(kind_i8) :: count2dt(3),count3dt(4),countct(2)
     INTEGER(kind_i8) :: start1dt(1),count1dt(1)
     INTEGER :: LREC
!    LOCAL
     INTEGER MES,IERR,STATUS(MPI_STATUS_SIZE)
     integer(KIND_IN),allocatable :: iv2(:,:),iv3(:,:,:)

     CHARACTER*14 CTIME
     LOGICAL IEXISTS
     real(kind_r8) TTT(1)
     
     CALL SetNcFile(FileName)
     LREC=1

     start2dt(1)=islon;start2dt(2)=islat;start2dt(3)=LREC
     start3dt(1)=islon;start3dt(2)=islat;start3dt(4)=LREC
     start3dt(3)=1
     start1dt(1)=LREC
     startct(1)=1;startct(2)=LREC;irec=LREC


     count2dt(1)=ielon-islon+1;count2dt(2)=ielat-islat+1;count2dt(3)=1
     count3dt(1)=ielon-islon+1;count3dt(2)=ielat-islat+1;count3dt(4)=1;count3dt(3)=kb
     countct(1)=20;countct(2)=1
     count1dt(1)=1

     allocate(iv2(count2dt(1),count2dt(2)),iv3(count2dt(1),count2dt(2),kb))

!    open file
     stat=nf90mpi_open(pom_mpi_comm , &
                       filename      , &
                       NF_WRITE      , &
                       MPI_INFO_NULL , &
                       ncid           )

!    temp
     IF(OUT_DATA_OCN_FIG(1))THEN
       iv3=tempe/0.01
       DO J=ISLAT,IELAT
       DO I=ISLON,IELON
          IF(fsm(I,J)==0)THEN
            IV3(I-ISLON+1,J-ISLAT+1,:)=-32767
          ENDIF
       ENDDO
       ENDDO
       stat=nf90mpi_inq_varid(ncid,'temp',vid)
       stat=nf90mpi_fill_var_rec(ncid,vid,irec)
       stat=nf90mpi_put_var_all(ncid,vid,iv3,start3dt,count3dt)
     END IF
!    salt
     IF(OUT_DATA_OCN_FIG(2))THEN
       iv3=salt/0.01
       DO J=ISLAT,IELAT
       DO I=ISLON,IELON
          IF(fsm(I,J)==0)THEN
            IV3(I-ISLON+1,J-ISLAT+1,:)=-32767
          ENDIF
       ENDDO
       ENDDO
       stat=nf90mpi_inq_varid(ncid,'salt',vid)
       stat=nf90mpi_fill_var_rec(ncid,vid,irec)
       stat=nf90mpi_put_var_all(ncid,vid,iv3,start3dt,count3dt)
     END IF  
!    curu
     IF(OUT_DATA_OCN_FIG(3))THEN
       iv3=cu/0.001
       DO J=ISLAT,IELAT
       DO I=ISLON,IELON
          IF(DUM(I,J)==0)THEN
            IV3(I-ISLON+1,J-ISLAT+1,:)=-32767
          ENDIF
       ENDDO
       ENDDO
       stat=nf90mpi_inq_varid(ncid,'curu',vid)
       stat=nf90mpi_fill_var_rec(ncid,vid,irec)
       stat=nf90mpi_put_var_all(ncid,vid,iv3,start3dt,count3dt)
     END IF  
!    curv
     IF(OUT_DATA_OCN_FIG(4))THEN
       iv3=cv/0.001
       DO J=ISLAT,IELAT
       DO I=ISLON,IELON
          IF(DVM(I,J)==0)THEN
            IV3(I-ISLON+1,J-ISLAT+1,:)=-32767
          ENDIF
       ENDDO
       ENDDO
       stat=nf90mpi_inq_varid(ncid,'curv',vid)
       stat=nf90mpi_fill_var_rec(ncid,vid,irec)
       stat=nf90mpi_put_var_all(ncid,vid,iv3,start3dt,count3dt)
     END IF
!    el
     IF(OUT_DATA_OCN_FIG(5))THEN
       iv2=el/0.01
       DO J=ISLAT,IELAT
       DO I=ISLON,IELON
          IF(fsm(I,J)==0)THEN
            IV2(I-ISLON+1,J-ISLAT+1)=-32767
          ENDIF
       ENDDO
       ENDDO
       stat=nf90mpi_inq_varid(ncid,'el',vid)
       stat=nf90mpi_fill_var_rec(ncid,vid,irec)
       stat=nf90mpi_put_var_all(ncid,vid,iv2,start2dt,count2dt)
     END IF     

     IF(OUT_DATA_OCN_FIG(6))THEN
       DO J=ISLAT,IELAT
       DO I=ISLON,IELON
          IF(fsm(I,J)==0)THEN
            BV(I,J,:)=0.0
          ENDIF
       ENDDO
       ENDDO
        stat=nf90mpi_inq_varid(ncid,'bv',vid)
        stat=nf90mpi_fill_var_rec(ncid,vid,irec)
        stat=nf90mpi_put_var_all(ncid,vid,BV(ISLON:IELON,ISLAT:IELAT,:),start3dt,count3dt)
        !stat=nf90mpi_put_var_all(ncid,vid,pom_state_variables(pom_grid_id)%bv_from_wave,start3dt,count3dt)

        stat=nf90mpi_inq_varid(ncid,'tauwx',vid)
        stat=nf90mpi_fill_var_rec(ncid,vid,irec)
        stat=nf90mpi_put_var_all(ncid,vid,pom_state_variables(pom_grid_id)%tauwx_from_wave,start2dt,count2dt)

        stat=nf90mpi_inq_varid(ncid,'tauwy',vid)
        stat=nf90mpi_fill_var_rec(ncid,vid,irec)
        stat=nf90mpi_put_var_all(ncid,vid,pom_state_variables(pom_grid_id)%tauwy_from_wave,start2dt,count2dt)
     ENDIF

     IF(OUT_DATA_OCN_FIG(7))THEN
        stat=nf90mpi_inq_varid(ncid,'windu',vid)
        stat=nf90mpi_fill_var_rec(ncid,vid,irec)
        stat=nf90mpi_put_var_all(ncid,vid,pom_state_variables(pom_grid_id)%u10_from_atm,start2dt,count2dt)

        stat=nf90mpi_inq_varid(ncid,'windv',vid)
        stat=nf90mpi_fill_var_rec(ncid,vid,irec)
        stat=nf90mpi_put_var_all(ncid,vid,pom_state_variables(pom_grid_id)%v10_from_atm,start2dt,count2dt)

        stat=nf90mpi_inq_varid(ncid,'ustar',vid)
        stat=nf90mpi_fill_var_rec(ncid,vid,irec)
        stat=nf90mpi_put_var_all(ncid,vid,pom_state_variables(pom_grid_id)%ust_from_atm,start2dt,count2dt)

        stat=nf90mpi_inq_varid(ncid,'st',vid)
        stat=nf90mpi_fill_var_rec(ncid,vid,irec)
        stat=nf90mpi_put_var_all(ncid,vid,pom_state_variables(pom_grid_id)%st_from_atm,start2dt,count2dt)

        stat=nf90mpi_inq_varid(ncid,'lt',vid)
        stat=nf90mpi_fill_var_rec(ncid,vid,irec)
        stat=nf90mpi_put_var_all(ncid,vid,pom_state_variables(pom_grid_id)%lt_from_atm,start2dt,count2dt)

        stat=nf90mpi_inq_varid(ncid,'slp',vid)
        stat=nf90mpi_fill_var_rec(ncid,vid,irec)
        stat=nf90mpi_put_var_all(ncid,vid,pom_state_variables(pom_grid_id)%slp_from_atm,start2dt,count2dt)

        stat=nf90mpi_inq_varid(ncid,'rain',vid)
        stat=nf90mpi_fill_var_rec(ncid,vid,irec)
        stat=nf90mpi_put_var_all(ncid,vid,pom_state_variables(pom_grid_id)%rain_from_atm,start2dt,count2dt)

        stat=nf90mpi_inq_varid(ncid,'dsw',vid)
        stat=nf90mpi_fill_var_rec(ncid,vid,irec)
        stat=nf90mpi_put_var_all(ncid,vid,pom_state_variables(pom_grid_id)%dsw_from_atm,start2dt,count2dt)

        stat=nf90mpi_inq_varid(ncid,'lw',vid)
        stat=nf90mpi_fill_var_rec(ncid,vid,irec)
        stat=nf90mpi_put_var_all(ncid,vid,pom_state_variables(pom_grid_id)%lw_from_atm,start2dt,count2dt)

        stat=nf90mpi_inq_varid(ncid,'rhoa',vid)
        stat=nf90mpi_fill_var_rec(ncid,vid,irec)
        stat=nf90mpi_put_var_all(ncid,vid,pom_state_variables(pom_grid_id)%rhoa_from_atm,start2dt,count2dt)

        stat=nf90mpi_inq_varid(ncid,'qfx',vid)
        stat=nf90mpi_fill_var_rec(ncid,vid,irec)
        stat=nf90mpi_put_var_all(ncid,vid,pom_state_variables(pom_grid_id)%qfx_from_atm,start2dt,count2dt)
     ENDIF

     IF(OUT_DATA_OCN_FIG(8))THEN
        stat=nf90mpi_inq_varid(ncid,'sst',vid)
        stat=nf90mpi_fill_var_rec(ncid,vid,irec)
        stat=nf90mpi_put_var_all(ncid,vid,pom_state_variables(pom_grid_id)%SST,start2dt,count2dt)

        stat=nf90mpi_inq_varid(ncid,'sss',vid)
        stat=nf90mpi_fill_var_rec(ncid,vid,irec)
        stat=nf90mpi_put_var_all(ncid,vid,pom_state_variables(pom_grid_id)%SSS,start2dt,count2dt)

        stat=nf90mpi_inq_varid(ncid,'ssu',vid)
        stat=nf90mpi_fill_var_rec(ncid,vid,irec)
        stat=nf90mpi_put_var_all(ncid,vid,pom_state_variables(pom_grid_id)%SSU,start2dt,count2dt)

        stat=nf90mpi_inq_varid(ncid,'ssv',vid)
        stat=nf90mpi_fill_var_rec(ncid,vid,irec)
        stat=nf90mpi_put_var_all(ncid,vid,pom_state_variables(pom_grid_id)%SSV,start2dt,count2dt)
     END IF

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
     RETURN
    END SUBROUTINE OutNcFile
!======================================================================
     SUBROUTINE OutNcFile_COUPLE(FileName)
     USE data_kind_mod
     USE LIMS,ONLY : MYID,NPROCS,KB
     USE CONTROL,ONLY : ISLON,ISLAT,OUT_DATA_OCN_FIG,TT,IELAT,IELON
     USE ALL_VAR,ONLY : FSM,DUM,DVM
     USE coupling_pom_mod
     IMPLICIT NONE
     INCLUDE 'mpif.h'
     INCLUDe 'netcdf.inc'
     CHARACTER(LEN=*),INTENT(IN) :: FileName
     INTEGER I,J
     REAL(kind_r4),PARAMETER :: OUTLAND=1.E20
!    LOCAL
     INTEGER :: LREC
     INTEGER MES,IERR,STATUS(MPI_STATUS_SIZE)
     CHARACTER*14 CTIME
     LOGICAL IEXISTS

     INQUIRE(FILE=FileName,Exist=IEXISTS)
     !!IF(MSR)THEN
     !!  IF(.NOT.IEXISTS)THEN
        CALL SetNcFile(FileName)
        LREC=1
     !!  ELSE
     !!   CALL NCLEN(FILENAME,'time',LREC)
     !!   LREC=LREC+1
     !!  END IF
     !!END IF

     !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
     !    WRITE DATA
     !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
     WRITE(6,*)"WRITE RESULT DATA",DATESTR(DATEVEC(TT,1),-1),LREC
     DO J=ISLAT,IELAT
     DO I=ISLON,IELON
     IF(FSM(I,J)==0)THEN
     END IF
     END DO
     END DO
     
     CALL OutNc(FileName,'bv',BV(ISLON:IELON,ISLAT:IELAT,:),vtype=nf_real,Lrec=Lrec,LAND=OUTLAND)
     CALL OutNc(FileName,'bbv',pom_state_variables(pom_grid_id)%bbv_from_wave,vtype=nf_real,Lrec=Lrec,LAND=OUTLAND)
     CALL OutNc(FileName,'windu',pom_state_variables(pom_grid_id)%u10_from_atm,FACT=0.01,units='m/s',Lrec=Lrec,LAND=OUTLAND)
     CALL OutNc(FileName,'windv',pom_state_variables(pom_grid_id)%v10_from_atm,FACT=0.01,units='m/s',Lrec=Lrec,LAND=OUTLAND)
     CALL OutNc(FileName,'tauwx',pom_state_variables(pom_grid_id)%tauwx_from_wave,vtype=nf_real,Lrec=Lrec,LAND=OUTLAND)
     CALL OutNc(FileName,'tauwy',pom_state_variables(pom_grid_id)%tauwy_from_wave,vtype=nf_real,Lrec=Lrec,LAND=OUTLAND)
     CALL OutNc(FileName,'LT',pom_state_variables(pom_grid_id)%lt_from_atm,vtype=nf_real,Lrec=Lrec,LAND=OUTLAND)
     CALL OutNc(FileName,'ST',pom_state_variables(pom_grid_id)%st_from_atm,vtype=nf_real,Lrec=Lrec,LAND=OUTLAND)
     CALL OutNc(FileName,'slp',pom_state_variables(pom_grid_id)%slp_from_atm,vtype=nf_real,Lrec=Lrec,LAND=OUTLAND)
     CALL OutNc(FileName,'rain',pom_state_variables(pom_grid_id)%rain_from_atm,vtype=nf_real,Lrec=Lrec,LAND=OUTLAND)
     CALL OutNc(FileName,'dsw',pom_state_variables(pom_grid_id)%dsw_from_atm,vtype=nf_real,Lrec=Lrec,LAND=OUTLAND)
     CALL OutNc(FileName,'lw',pom_state_variables(pom_grid_id)%lw_from_atm,vtype=nf_real,Lrec=Lrec,LAND=OUTLAND)
     CALL OutNc(FileName,'SST',pom_state_variables(pom_grid_id)%SST,vtype=nf_real,Lrec=Lrec,LAND=OUTLAND)
     CALL OutNc(FileName,'SSS',pom_state_variables(pom_grid_id)%SSS,vtype=nf_real,Lrec=Lrec,LAND=OUTLAND)
     CALL OutNc(FileName,'SSU',pom_state_variables(pom_grid_id)%SSU,vtype=nf_real,Lrec=Lrec,LAND=OUTLAND)
     CALL OutNc(FileName,'SSV',pom_state_variables(pom_grid_id)%SSV,vtype=nf_real,Lrec=Lrec,LAND=OUTLAND)
     CALL OutNc(FileName,'UST',pom_state_variables(pom_grid_id)%ust_from_atm,vtype=nf_real,Lrec=Lrec,LAND=OUTLAND)
     CALL OutNc(FileName,'RHOA',pom_state_variables(pom_grid_id)%rhoa_from_atm,vtype=nf_real,Lrec=Lrec,LAND=OUTLAND)
     CALL OutNc(FileName,'QFX',pom_state_variables(pom_grid_id)%qfx_from_atm,vtype=nf_real,Lrec=Lrec,LAND=OUTLAND)
     CTIME=DATESTR(DATEVEC(TT,1),-1)
     CALL OutNc(FileName,'ctime',CTIME(1:10),LREC=LREC)
     WRITE(6,*) "ALL DATA WRITE OVER!"
     RETURN
     END SUBROUTINE OutNcFile_COUPLE
END MODULE MOD_IO
