!!
  SUBROUTINE SETTOPOG
  USE LIMS_WAVE
  USE CONTROL_WAVE
  USE CONST_WAVE
  USE ALL_VAR_WAVE
  USE NcFileMod
  IMPLICIT NONE
  INTEGER I,J
  CHARACTER(LEN=100) FileName

  ALLOCATE(XGRID(0:IM+1,JM),YGRID(IM,JM),RS2DLAT(IM,JM))
  ALLOCATE(D(0:IM+1,JM),NSP(IM,JM))
  ALLOCATE(DX(IM,JM),DY(IM,JM))

  FileName=trim(input)//trim(ModelName)//'_topo.nc'
  CALL Nc_Read(FileName,'xgrid',XGRID(1:IM,:))
  CALL Nc_Read(FileName,'ygrid',YGRID)

  DO I=2,IM
     DX(I,:)=XGRID(I,:)-XGRID(I-1,:)
  ENDDO
     DX(1,:)=DX(2,:)

  DO J=2,JM
     DY(:,J)=YGRID(:,J)-YGRID(:,J-1)
  END DO
     DY(:,1)=DY(:,2)

  XGRID(0,:) = XGRID(1,:)-DX(1,:)
  XGRID(IM+1,:) = XGRID(IM,:)+DX(IM,:)

  DO J = 1,JM
  DO I = 1,IM
    IF(ABS(YGRID(I,1)+90.)<SMVALUE.AND.J==1)THEN
     RS2DLAT(I,J)=0.05
    ELSEIF(ABS(YGRID(I,JM)-90.)<SMVALUE.AND.J==JM)THEN
     RS2DLAT(I,J)=0.05
    ELSE
     RS2DLAT(I,J)=RS*COSD(YGRID(I,J))
    END IF
  END DO
  END DO
!
  CALL Nc_Read(FileName,'nsp',D(1:IM,:),LAND=0.)
  NSP=INT(D(1:IM,:))
  CALL Nc_Read(FileName,'depth',D(1:IM,:),LAND=0.)

  D(0,:)=D(IM-1,:)
  D(IM+1,:)=D(2,:)
  WHERE(D<0)
  D=0.
  END WHERE
  WHERE(D>0.AND.D<=5)
  D=5
  END WHERE

  DO I=1,IM
  DO J=1,JM
  IF(NSP(I,J)/=0)THEN
    IF(D(I,J)<5)D(I,J)=5
  END IF
  END DO
  END DO

  RETURN
  END
