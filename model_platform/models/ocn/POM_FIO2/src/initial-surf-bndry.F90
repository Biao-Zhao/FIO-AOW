 SUBROUTINE INITIAL_SURF_BNDRY
 USE NcFileMod
 USE ALL_VAR, ONLY : DQDT,HEAT,TEMP,STRX,STRY
 USE CONTROL
 USE CONST, ONLY : LAND
 USE LIMS
 IMPLICIT NONE
 CHARACTER(LEN=200) FileName
 INTEGER IERR

 IF(HEATTYPE)THEN
 ALLOCATE(HEAT(ISLON:IELON,ISLAT:IELAT,12),STAT=IERR)
 HEAT = 0
 !WRITE(FILENAME,"(a<len_trim(input)>,a12,a<len_trim(ModelName)>,a15)")  &
 !trim(input),'csurf-bndry/',trim(ModelName),'_csurf_bndry.nc'
 FileName=trim(input)//'csurf-bndry/'//trim(ModelName)//'_csurf_bndry.nc'
 !!CALL Nc_Read(FileName,'NETHEAT24',HEAT,I1=ISLON,J1=ISLAT,LAND=LAND)
 CALL Nc_Read(FileName,'netheat',HEAT,I1=ISLON,J1=ISLAT,LAND=LAND)
 END IF

 IF(WINDTYPE)THEN
 ALLOCATE(HEAT(ISLON:IELON,ISLAT:IELAT,12),STAT=IERR)
 HEAT = 0
 !WRITE(FILENAME,"(a<len_trim(input)>,a12,a<len_trim(ModelName)>,a15)")  &
 !trim(input),'csurf-bndry/',trim(ModelName),'_csurf_bndry.nc'
 FileName=trim(input)//'csurf-bndry/'//trim(ModelName)//'_csurf_bndry.nc'
 ALLOCATE(STRX(ISLON:IELON,ISLAT:IELAT,12),STAT=IERR)
 ALLOCATE(STRY(ISLON:IELON,ISLAT:IELAT,12),STAT=IERR)
 STRX = 0
 STRY = 0
 !!CALL Nc_Read(FileName,'TAUX24',STRX,I1=ISLON,J1=ISLAT,LAND=LAND)
 !!CALL Nc_Read(FileName,'TAUY24',STRY,I1=ISLON,J1=ISLAT,LAND=LAND)
 CALL Nc_Read(FileName,'taux3',STRX,I1=ISLON,J1=ISLAT,LAND=LAND)
 CALL Nc_Read(FileName,'tauy3',STRY,I1=ISLON,J1=ISLAT,LAND=LAND)
 END IF

 ALLOCATE(DQDT(ISLON:IELON,ISLAT:IELAT,12),STAT=IERR)
 DQDT = 0
 !WRITE(FILENAME,"(a<len_trim(input)>,a12,a<len_trim(ModelName)>,a15)")  &
 !trim(input),'csurf-bndry/',trim(ModelName),'_csurf_bndry.nc'
 FileName=trim(input)//'csurf-bndry/'//trim(ModelName)//'_csurf_bndry.nc'
 !!CALL Nc_Read(FileName,'DQDSST24',DQDT,I1=ISLON,J1=ISLAT,LAND=LAND)
 CALL Nc_Read(FileName,'dqdsst',DQDT,I1=ISLON,J1=ISLAT,LAND=LAND)


 ALLOCATE(TEMP(ISLON:IELON,ISLAT:IELAT,12),STAT=IERR)
 TEMP = 0
 !WRITE(FILENAME,"(a<len_trim(input)>,a12,a<len_trim(ModelName)>,a13)")  &
 !trim(input),'csurf-bndry/',trim(ModelName),'_csurf_sst.nc'
 !!FileName=trim(input)//'csurf-bndry/'//trim(ModelName)//'_csurf_sst.nc'
 FileName=trim(input)//'csurf-bndry/'//trim(ModelName)//'_csurf_bndry.nc'
 CALL Nc_Read(FileName,'sst',TEMP,I1=ISLON,J1=ISLAT,LAND=LAND)

 IF(ISLON==1)THEN
   DO J=ISLAT,IELAT
      HEAT(2,J,:)=HEAT(1,J,:)
      STRX(2,J,:)=STRX(1,J,:)
      STRY(2,J,:)=STRY(1,J,:)
      DQDT(2,J,:)=DQDT(1,J,:)
      TEMP(2,J,:)=TEMP(1,J,:)
   ENDDO
 END IF

 IF(IELON==IM)THEN
   DO J=ISLAT,IELAT
      HEAT(IM-1,J,:)=HEAT(IM,J,:)
      STRX(IM-1,J,:)=STRX(IM,J,:)
      STRY(IM-1,J,:)=STRY(IM,J,:)
      DQDT(IM-1,J,:)=DQDT(IM,J,:)
      TEMP(IM-1,J,:)=TEMP(IM,J,:)
   END DO
 END IF

 IF(ISLAT==1)THEN
    DO I=ISLON,IELON
      HEAT(I,2,:)=HEAT(I,1,:)
      STRX(I,2,:)=STRX(I,1,:)
      STRY(I,2,:)=STRY(I,1,:)
      DQDT(I,2,:)=DQDT(I,1,:)
      TEMP(I,2,:)=TEMP(I,1,:)
    END DO
 END IF

 IF(IELAT==JM)THEN
   DO I=ISLON,IELON
      HEAT(I,JM-1,:)=HEAT(I,JM,:)
      STRX(I,JM-1,:)=STRX(I,JM,:)
      STRY(I,JM-1,:)=STRY(I,JM,:)
      DQDT(I,JM-1,:)=DQDT(I,JM,:)
      TEMP(I,JM-1,:)=TEMP(I,JM,:)
    ENDDO
 END IF

 RETURN
 END
