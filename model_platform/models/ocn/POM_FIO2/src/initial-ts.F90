 SUBROUTINE INITIAL_TS
 USE ALL_VAR, ONLY : TB,SB,RHO,RMEAN,DT,H,FSM,ZZ
 USE NcFileMod
 USE CONST, ONLY : LAND,TBIAS,SBIAS
 USE LIMS
 USE CONTROL
 IMPLICIT NONE
 CHARACTER(LEN=200) FileName
 
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!  SET INITIAL CONDITIONS FOR SEAMOUNT PROBLEM.
!  THAT RMEAN HAS BEEN AREA AVERAGED BEFORE TRANSFER TO SIGMA CO.
! READ CLIMATE AVERAGE OCEAN TEMPERATURE
 TSTYPE=.TRUE. !IF SET TSTYPE T, MEAN MODEL RUN USING HOT RESTART FILE
               !IF SET TSTYPE F, MODEL COLD START 
 IF(.NOT.TSTYPE)THEN
 WRITE(*,*)'MODEL COLD START'
 DO K=1,KBM1
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
 TB(I,J,K)=5.+15.*EXP(ZZ(K)*H(I,J)/1000.)-TBIAS
 SB(I,J,K)=35.0-SBIAS
 END DO
 END DO
 END DO
 CALL DENS(SB,TB,RMEAN,H)
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
    RMEAN(I,J,:)=RMEAN(I,J,:)*FSM(I,J)    
 END DO
 END DO
 RETURN
 END IF
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!! !WRITE(FileName,'(a<len_trim(input)>,a8,a<len_trim(ModelName)>,a10)')trim(input),'initial/',trim(ModelName),'_temave.nc'
!! FileName=trim(input)//'initial/'//trim(ModelName)//'_temave.nc'
!! WRITE(*,*)TRIM(FileName)
!! CALL Nc_Read(FileName,'temp',TB(ISLON:IELON,ISLAT:IELAT,:),I1=ISLON,J1=ISLAT,LAND=LAND)
! READ CLIMATE AVERAGE OCEAN SALINITY
 !WRITE(FileName,'(a<len_trim(input)>,a8,a<len_trim(ModelName)>,a10)')trim(input),'initial/',trim(ModelName),'_salave.nc'
!! FileName=trim(input)//'initial/'//trim(ModelName)//'_salave.nc'
!! WRITE(*,*)TRIM(FileName)
!! CALL Nc_Read(FileName,'sal',SB(ISLON:IELON,ISLAT:IELAT,:),I1=ISLON,J1=ISLAT,LAND=LAND)
 
!! IF(ISLON==1)THEN
!!   DO J=ISLAT,IELAT
!!      TB(1,J,:)=TB(2,J,:)
!!      SB(1,J,:)=SB(2,J,:)
!!   ENDDO
!! END IF

!! IF(IELON==IM)THEN
!!   DO J=ISLAT,IELAT
!!      TB(IM,J,:)=TB(IM-1,J,:)
!!      SB(IM,J,:)=SB(IM-1,J,:)
!!   END DO
!! END IF
    
!! IF(ISLAT==1)THEN
!!    DO I=ISLON,IELON
!!      TB(I,1,:)=TB(I,2,:)
!!      SB(I,1,:)=SB(I,2,:)
!!    END DO
!! END IF

!! IF(IELAT==JM)THEN  
!!   DO I=ISLON,IELON
!!      TB(I,JM,:)=TB(I,JM-1,:)
!!      SB(I,JM,:)=SB(I,JM-1,:)
!!    ENDDO
!! END IF
    
!! DO J=ISLAT,IELAT
!! DO I=ISLON,IELON
!!    DT(I,J)=H(I,J)
!!    SB(I,J,:)=SB(I,J,:)*FSM(I,J)                                 
!!    TB(I,J,:)=TB(I,J,:)*FSM(I,J)  
!! ENDDO
!! ENDDO

!! CALL DENS(SB,TB,RHO,DT)
!! DO K=1,KBM1
!! DO J=ISLAT,IELAT
!! DO I=ISLON,IELON
!! RMEAN(I,J,K)=RHO(I,J,K)
!! ENDDO
!! ENDDO
!! ENDDO
 FileName=trim(input)//'initial/'//trim(ModelName)//'_initRMEAN.nc'
 WRITE(6,*)TRIM(FileName)
 CALL Nc_Read(FileName,'rmean_ave',RMEAN(ISLON:IELON,ISLAT:IELAT,:),I1=ISLON,J1=ISLAT,LAND=LAND)
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
! READ INITIAL OCEAN TEMPERATURE
 !WRITE(FileName,'(a<len_trim(input)>,a8,a<len_trim(ModelName)>,a11)')trim(input),'initial/',trim(ModelName),'_inittem.nc'
 FileName=trim(input)//'initial/'//trim(ModelName)//'_inittem.nc'
 WRITE(6,*)TRIM(FileName)
 CALL Nc_Read(FileName,'temp',TB(ISLON:IELON,ISLAT:IELAT,:),I1=ISLON,J1=ISLAT,LAND=LAND)
! READ INITIAL OCEAN SALINITY
 !WRITE(FileName,'(a<len_trim(input)>,a8,a<len_trim(ModelName)>,a11)')trim(input),'initial/',trim(ModelName),'_initsal.nc'
 FileName=trim(input)//'initial/'//trim(ModelName)//'_initsal.nc'
 WRITE(6,*)TRIM(FileName)
 CALL Nc_Read(FileName,'sal',SB(ISLON:IELON,ISLAT:IELAT,:),I1=ISLON,J1=ISLAT,LAND=LAND)
     
 IF(ISLON==1)THEN
   DO J=ISLAT,IELAT
      TB(1,J,:)=TB(2,J,:)
      SB(1,J,:)=SB(2,J,:)
      RMEAN(1,J,:)=RMEAN(2,J,:)
   ENDDO
 END IF

 IF(IELON==IM)THEN
   DO J=ISLAT,IELAT
      TB(IM,J,:)=TB(IM-1,J,:)
      SB(IM,J,:)=SB(IM-1,J,:)
      RMEAN(IM,J,:)=RMEAN(IM-1,J,:)
   END DO
 END IF

 IF(ISLAT==1)THEN
    DO I=ISLON,IELON
      TB(I,1,:)=TB(I,2,:)
      SB(I,1,:)=SB(I,2,:)
      RMEAN(I,1,:)=RMEAN(I,2,:)
    END DO
 END IF

 IF(IELAT==JM)THEN  
   DO I=ISLON,IELON
      TB(I,JM,:)=TB(I,JM-1,:)
      SB(I,JM,:)=SB(I,JM-1,:)
      RMEAN(I,JM,:)=RMEAN(I,JM-1,:)
    ENDDO
 END IF
    
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
    SB(I,J,:)=SB(I,J,:)*FSM(I,J)                                 
    TB(I,J,:)=TB(I,J,:)*FSM(I,J)
    RMEAN(I,J,:)=RMEAN(I,J,:)*FSM(I,J)    
 END DO
 END DO
 RETURN
 END
