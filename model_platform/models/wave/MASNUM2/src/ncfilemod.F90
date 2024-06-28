!----------------------------------------------------------------------
!* NcFile Module
!* 关于Nc格式文件的Module
!----------------------------------------------------------------------

 Module NcFileMod
 use time_mod
 implicit None
 include 'netcdf.inc'
!----------------------------------------------------------------------
 Public Create_File,DefVar,Set_Att,Nc_Read,Nc_Write,Handle_Err,       &
        ncfileset,outnc,NcLen
 Private
!----------------------------------------------------------------------
 INTERFACE SET_ATT
 Module Procedure Set_Att_Text1,Set_Att_Text2 ,                       &
                  Set_Att_Int2,Set_Att_Real
 END INTERFACE SET_ATT
!----------------------------------------------------------------------
 INTERFACE Nc_Read
 Module Procedure Nc_Read_0Dim,Nc_Read_1Dim,Nc_Read_2Dim,             &
                  Nc_Read_3Dim,Nc_Read_Text,Nc_Read_Text1D,           &
                  Nc_Read_TEXT2D
 END INTERFACE Nc_Read
!----------------------------------------------------------------------
 INTERFACE Nc_Write
 Module Procedure Nc_Write_0Dim,Nc_Write_1Dim,Nc_Write_2Dim,          &
                  Nc_Write_3Dim,Nc_Write_Text,Nc_Write_0Dim_DOU
 END INTERFACE Nc_Write
!----------------------------------------------------------------------
 INTERFACE OUTNC
 Module Procedure OUTNC_1D,OUTNC_2D ,OUTNC_3D,OUTNC_0D,               &
                  OUTNC_0D_DOU,OUTNC_TEXT,OUTNC_TEXT1
 END INTERFACE OUTNC
!----------------------------------------------------------------------
 Integer :: Status,VarID
 Integer :: I,J,K,IERR
!----------------------------------------------------------------------
Contains
!----------------------------------------------------------------------
!处理NC格式文件读写的信息
!To Handle Return Info for NC File input or output 
 SUBROUTINE HANDLE_ERR(Stat)
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: Stat
 IF(Stat.NE.NF_NOERR) THEN
  WRITE(6,*) "MASNUM TO STOP"
  flush(6)
  WRITE(6,*)  NF_STRERROR(Stat)
  STOP 'MASNUM STOPPED'
 ENDIF
 RETURN
 END SubRoutine Handle_Err
!----------------------------------------------------------------------
!To define dimension for a NcFile.
 SUBROUTINE DefDim(FileName,DimName,DimLen,DimVarName,DimVarType)
 IMPLICIT NONE

 CHARACTER(len=*), intent(in) :: DimName, DimVarName,FileName
 INTEGER, intent(in) :: DimLen, DimVarType

 INTEGER :: NcID

 Status=NF_OPEN(TRIM(FILENAME),NF_WRITE,ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_REDEF(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 status = NF_DEF_DIM(NcID, DimName, DimLen, VarID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_ENDDEF(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 
 Status=NF_CLOSE(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 return
 END SUBROUTINE DefDim
!----------------------------------------------------------------------
!定义变量
!To define variable for a NcFile.
 SUBROUTINE DefVar(FileName,VarName,VarType,VarDimsName,VarID)
 IMPLICIT NONE
 CHARACTER(Len=*),INTENT(IN) :: FileName,VarName
 CHARACTER(Len=*),INTENT(IN) :: VarDimsName(:)
 INTEGER,INTENT(IN)::VarType
 INTEGER,OPTIONAL,INTENT(OUT) :: VarID

 INTEGER VarRank,VarID1,I,NcID
 INTEGER VarDims(4)

 Status=NF_OPEN(TRIM(FILENAME),NF_WRITE,ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_REDEF(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 VarRank = size(VarDimsName)
 DO I = 1, VarRank
 status = NF_INQ_DIMID(NcID, trim(VarDimsName(i)), VarDims(i))
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 ENDDO
 Status = NF_DEF_VAR(NcID,trim(VarName),VarType,VarRank,VarDims(1:VarRank),VarID1)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_ENDDEF(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_CLOSE(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 if(present(VarID))VarID = VarID1
 END SUBROUTINE DefVar
!----------------------------------------------------------------------
!设置坐标粥或者变量的相关属性      
!To define attribute 
 SUBROUTINE Set_Att_Text1(NcID,AttName,Attribute,VarName)
 IMPLICIT NONE
 INTEGER,INTENT(IN) :: NcID
 CHARACTER(len=*),INTENT(IN) :: AttName,Attribute
 CHARACTER(len=*),Optional,INTENT(IN) :: VarName
 VarID=0
 IF(Present(VarName))THEN
 status = NF_INQ_VarID(NcID, trim(VarName), VarID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 END IF
 status = NF_PUT_ATT_TEXT(NcID, VarID, AttName,len_trim(attribute),trim(attribute))
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 END SUBROUTINE Set_Att_Text1
!----------------------------------------------------------------------
!设置坐标粥或者变量的相关属性      
!To define attribute 
 SUBROUTINE Set_Att_Text2(FileName,AttName,Attribute,VarName)
 IMPLICIT NONE
 CHARACTER(len=*),INTENT(IN) :: AttName,Attribute,FileName
 CHARACTER(len=*),Optional,INTENT(IN) :: VarName

 INTEGER NCID
 VarID=0
 
 Status=NF_OPEN(TRIM(FILENAME),NF_WRITE,ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_REDEF(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 
 IF(Present(VarName))THEN
 status = NF_INQ_VarID(NcID, trim(VarName), VarID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 END IF

 status = NF_PUT_ATT_TEXT(NcID,VarID,TRIM(AttName),len_trim(attribute),trim(attribute))
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_ENDDEF(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_CLOSE(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 END SUBROUTINE Set_Att_Text2
!----------------------------------------------------------------------
!设置坐标粥或者变量的相关属性      
!To define attribute 
 SUBROUTINE Set_Att_Int2(FileName,AttName,Attribute,VarName)
 IMPLICIT NONE
 CHARACTER(len=*),INTENT(IN) :: AttName,FileName
 INTEGER*2,INTENT(IN) :: Attribute
 CHARACTER(len=*),Optional,INTENT(IN) :: VarName

 INTEGER NCID
 VarID=0
 
 Status=NF_OPEN(TRIM(FILENAME),NF_WRITE,ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_REDEF(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 
 IF(Present(VarName))THEN
 status = NF_INQ_VarID(NcID, trim(VarName), VarID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 END IF

 status = NF_PUT_ATT_INT2(NcID,VarID,TRIM(AttName),NF_INT2,1,attribute)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_ENDDEF(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_CLOSE(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 END SUBROUTINE Set_Att_Int2
!----------------------------------------------------------------------
!设置坐标粥或者变量的相关属性      
!To define attribute 
 SUBROUTINE Set_Att_Real(FileName,AttName,Attribute,VarName)
 IMPLICIT NONE
 CHARACTER(len=*),INTENT(IN) :: AttName,FileName
 REAL,INTENT(IN) :: Attribute
 CHARACTER(len=*),Optional,INTENT(IN) :: VarName

 INTEGER NCID
 VarID=0
 
 Status=NF_OPEN(TRIM(FILENAME),NF_WRITE,ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_REDEF(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 
 IF(Present(VarName))THEN
 status = NF_INQ_VarID(NcID, trim(VarName), VarID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 END IF

 status = NF_PUT_ATT_REAL(NcID,VarID,TRIM(AttName),NF_REAL,1,attribute)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_ENDDEF(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_CLOSE(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 END SUBROUTINE Set_Att_Real
!----------------------------------------------------------------------
!创建新的nc文件
!Create New Nc File      
!IF TIMEFIG/=0, The Time dimension defined!
 SUBROUTINE CREATE_FILE(FILENAME,TIMEFIG,IM,VName1,JM,VName2,KB,VName3)
 IMPLICIT NONE
 INTEGER, INTENT(IN) :: IM,TIMEFIG
 INTEGER, OPTIONAL, INTENT(IN) :: JM, KB 
 CHARACTER(LEN=*), INTENT(IN):: FILENAME
 CHARACTER(LEN=*), INTENT(IN):: VName1 
 CHARACTER(LEN=*), OPTIONAL, INTENT(IN):: VName2, VName3 
!DEFINE VARIABLES FOR LOCAL FUNCTION
 INTEGER ncID

 WRITE(6,*) "CREATING NEW FILE"
 WRITE(6,*) TRIM(FILENAME)

! Status=NF_CREATE(TRIM(FILENAME),IOR(NF_NOCLOBBER,NF_64BIT_OFFSET),ncID)
!Status=NF_CREATE(TRIM(FILENAME),IOR(NF_CLOBBER,NF_64BIT_OFFSET),ncID)
Status=NF_CREATE(TRIM(FILENAME),IOR(NF_CLOBBER,NF_NETCDF4),ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_CLOSE(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!DEFINE VName1
 IF(IM>0)THEN
 CALL DefDim(FileName,VName1,IM,VName1,NF_REAL)
 CALL DefVar(FileName,VName1,NF_REAL,[trim(Vname1)])

 IF(INDEX(VName1,'lon'))THEN
 CALL Set_Att(FileName,'units','degrees_east',VName1)
 ELSEIF(INDEX(VName1,'lat'))THEN
 CALL Set_Att(FileName,'units','degrees_north',VName1)
 ELSEIF(INDEX(VName1,'dep'))THEN
 CALL Set_Att(FileName,'positive','down',VName1)
 CALL Set_Att(FileName,'units','meter',VName1)
 END IF
 END IF
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!DEFINE VName2
 IF(JM>0)THEN
 IF(PRESENT(VName2))THEN
 CALL DefDim(FileName,VName2,JM,VName2,NF_REAL)
 CALL DefVar(FileName,VName2,NF_REAL,[trim(Vname2)])

 IF(INDEX(VName2,'lon'))THEN
 CALL Set_Att(FileName,'units','degrees_east',VName2)
 ELSEIF(INDEX(VName2,'lat'))THEN
 CALL Set_Att(FileName,'units','degrees_north',VName2)
 ELSEIF(INDEX(VName2,'dep'))THEN
 CALL Set_Att(FileName,'positive','down',VName2)
 CALL Set_Att(FileName,'units','meter',VName2)
 END IF

 END IF
 END IF
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!DEFINE VName3
 IF(KB>0)THEN
 IF(PRESENT(VName3))THEN
 CALL DefDim(FileName,VName3,KB,VName3,NF_REAL)
 CALL DefVar(FileName,VName3,NF_REAL,[trim(Vname3)])

 IF(INDEX(VName3,'lon'))THEN
 CALL Set_Att(FileName,'units','degrees_east',VName3)
 ELSEIF(INDEX(VName3,'lat'))THEN
 CALL Set_Att(FileName,'units','degrees_north',VName3)
 ELSEIF(INDEX(VName3,'dep'))THEN
 CALL Set_Att(FileName,'positive','down',VName3)
 CALL Set_Att(FileName,'units','meter',VName3)
 END IF

 END IF
 END IF
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!DEFINE TIME DIMENSION

 IF(TIMEFIG)THEN
 CALL DefDim(FileName,'time',nf_unlimited,'time',nf_double)
! CALL DefDim(FileName,'time',2,'time',nf_real)
 CALL DefVar(FileName,'time',NF_DOUBLE,['time'])
 CALL Set_Att(FileName,'units',                     &
              'hours since 0000-01-01 00:00:00','time')
 CALL DefDim(FileName,'char',20,'time',nf_real)
 END IF
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 CALL Set_Att(FileName,'descript','Demo By Guanso')

 END SUBROUTINE Create_File
!----------------------------------------------------------------------
 SUBROUTINE NcLen(FileName,VN,Len)
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FileName,VN
 INTEGER,INTENT(OUT) :: LEN

 INTEGER ncID,VarID,DimID
 STATUS=NF_OPEN(trim(FILENAME),NF_NOWRITE,ncID)
 IF(STATUS.NE.NF_NOERR) CALL HANDLE_ERR(STATUS)

 STATUS = NF_INQ_DIMID(NcID, VN, DIMID)
 IF(STATUS.NE.NF_NOERR) CALL HANDLE_ERR(STATUS)

! STATUS = NF_INQ_VARDIMID(NcID,VarID,DimID)
! IF(STATUS.NE.NF_NOERR) CALL HANDLE_ERR(STATUS)
 STATUS = NF_INQ_DIMLEN(NcID, DimID, Len)
 IF(STATUS.NE.NF_NOERR) CALL HANDLE_ERR(STATUS)
 STATUS=NF_CLOSE(ncID)
 IF(STATUS.NE.NF_NOERR) CALL HANDLE_ERR(STATUS)
 RETURN 
 END Subroutine NcLen
!----------------------------------------------------------------------
 SUBROUTINE Nc_Read_Text(FileName,Var_Name,Var,Lrec)
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FileName,Var_Name
 CHARACTER(LEN=*),INTENT(INOUT) :: VAR
 INTEGER,OPTIONAL,INTENT(IN) :: Lrec

 INTEGER ncID
 INTEGER COUNT2(2),START2(2)

 DATA START2/1,1/ COUNT2/10,1/
 COUNT2(1)=LEN_TRIM(VAR)

! WRITE(6,*) TRIM(FILENAME)
 STATUS=NF_OPEN(trim(FILENAME),NF_NOWRITE,ncID)
 IF(STATUS.NE.NF_NOERR) CALL HANDLE_ERR(STATUS) 

 STATUS=NF_INQ_VARID(ncID,trim(VAR_NAME),varID)
 IF(STATUS.NE.NF_NOERR) CALL HANDLE_ERR(STATUS) 
 IF(PRESENT(LREC))THEN 
 START2(2)=LREC
 STATUS=NF_GET_VARA_TEXT(ncID,varID,START2,COUNT2,VAR)
 IF(STATUS.NE.NF_NOERR) CALL HANDLE_ERR(STATUS)
 ELSE
 STATUS=NF_GET_VAR_TEXT(ncID,varID,VAR)
 IF(STATUS.NE.NF_NOERR) CALL HANDLE_ERR(STATUS)
 END IF

 STATUS=NF_CLOSE(ncID)
 IF(STATUS.NE.NF_NOERR) CALL HANDLE_ERR(STATUS)
 END SUBROUTINE Nc_Read_Text
!----------------------------------------------------------------------
 SUBROUTINE Nc_Read_Text1D(FileName,Var_Name,Var,Lrec)
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FileName,Var_Name
 CHARACTER(LEN=*),INTENT(INOUT) :: VAR(:)
 INTEGER,OPTIONAL,INTENT(IN) :: Lrec

 INTEGER,PARAMETER :: NUMDIM=1
 INTEGER ncID
 INTEGER COUNT2(NUMDIM+2),START2(NUMDIM+2)
! DATA START2/1,1,1/ COUNT2/10,1,1/
 START2=1;COUNT2=1
 COUNT2(1)=LEN_TRIM(VAR(1))
 COUNT2(2:NUMDIM+1)=SHAPE(VAR)

! WRITE(6,*) TRIM(FILENAME)
 STATUS=NF_OPEN(trim(FILENAME),NF_NOWRITE,ncID)
 IF(STATUS.NE.NF_NOERR) CALL HANDLE_ERR(STATUS) 

 STATUS=NF_INQ_VARID(ncID,trim(VAR_NAME),varID)
 IF(STATUS.NE.NF_NOERR) CALL HANDLE_ERR(STATUS) 
 IF(PRESENT(LREC))THEN 
 START2(NUMDIM+2)=LREC
 STATUS=NF_GET_VARA_TEXT(ncID,varID,START2,COUNT2,VAR)
 IF(STATUS.NE.NF_NOERR) CALL HANDLE_ERR(STATUS)
 ELSE
 STATUS=NF_GET_VAR_TEXT(ncID,varID,VAR)
 IF(STATUS.NE.NF_NOERR) CALL HANDLE_ERR(STATUS)
 END IF

 STATUS=NF_CLOSE(ncID)
 IF(STATUS.NE.NF_NOERR) CALL HANDLE_ERR(STATUS)
 END SUBROUTINE Nc_Read_Text1D
!----------------------------------------------------------------------
 SUBROUTINE Nc_Read_Text2D(FileName,Var_Name,Var,Lrec)
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FileName,Var_Name
 CHARACTER(LEN=*),INTENT(INOUT) :: VAR(:,:)
 INTEGER,OPTIONAL,INTENT(IN) :: Lrec

 INTEGER,PARAMETER :: NUMDIM=2
 INTEGER ncID
 INTEGER COUNT2(NUMDIM+2),START2(NUMDIM+2)
! DATA START2/1,1,1/ COUNT2/10,1,1/
 START2=1;COUNT2=1
 COUNT2(1)=LEN_TRIM(VAR(1,1))
 COUNT2(2:NUMDIM+1)=SHAPE(VAR)

! WRITE(6,*) TRIM(FILENAME)
 STATUS=NF_OPEN(trim(FILENAME),NF_NOWRITE,ncID)
 IF(STATUS.NE.NF_NOERR) CALL HANDLE_ERR(STATUS) 

 STATUS=NF_INQ_VARID(ncID,trim(VAR_NAME),varID)
 IF(STATUS.NE.NF_NOERR) CALL HANDLE_ERR(STATUS) 
 IF(PRESENT(LREC))THEN 
 START2(NUMDIM+2)=LREC
 STATUS=NF_GET_VARA_TEXT(ncID,varID,START2,COUNT2,VAR)
 IF(STATUS.NE.NF_NOERR) CALL HANDLE_ERR(STATUS)
 ELSE
 STATUS=NF_GET_VAR_TEXT(ncID,varID,VAR)
 IF(STATUS.NE.NF_NOERR) CALL HANDLE_ERR(STATUS)
 END IF

 STATUS=NF_CLOSE(ncID)
 IF(STATUS.NE.NF_NOERR) CALL HANDLE_ERR(STATUS)
 END SUBROUTINE Nc_Read_Text2D
!----------------------------------------------------------------------
!读取单个数据
!Read single data
 SUBROUTINE Nc_Read_0DIM(FILENAME,VAR_NAME,VAR,VAR_DOU,LREC,LAND)
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN)::VAR_NAME,FILENAME
 REAL,INTENT(OUT) :: VAR
 DOUBLE PRECISION,INTENT(OUT),OPTIONAL :: VAR_DOU
 INTEGER,INTENT(IN) :: LREC
 REAL,OPTIONAL,INTENT(IN) :: LAND 

 INTEGER*2 V_INT2
 DOUBLE PRECISION V_DOU
 CHARACTER*100 ATT_NAME,FACT_NAME

 INTEGER ncID,vType,AttID
 INTEGER I,J,K
 INTEGER*2 INT_LAND
 DOUBLE PRECISION DOU_LAND
 REAL FACT,REAL_LAND
 INTEGER FIG

 ATT_NAME='missing_value'
 FACT_NAME='scale_factor'
 FIG=0

 Status=NF_OPEN(trim(FILENAME),NF_NOWRITE,ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status) 

 Status=NF_INQ_VARID(ncID,trim(VAR_NAME),varID)
 IF(Status==-49)THEN
 WRITE(6,*) "NO Such Data"
 IF(PRESENT(LAND))THEN
 VAR=LAND
 END IF
 RETURN
 END IF
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status) 

 Status=NF_INQ_VARTYPE(ncID,varID,vtype)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status) 
!GET INTEGER TYPE DATA
 IF(vTYPE==NF_SHORT)THEN
 
 Status=NF_INQ_ATTID(NcID,varID,Trim(ATT_NAME),AttID)
 IF(Status==-43)THEN
! WRITE(6,*) TRIM(Var_NAME),TRIM(ATT_NAME),' not exist'
 FIG=1
 ELSE
 Status=NF_GET_ATT_INT2(ncID,varID,trim(ATT_NAME),INT_LAND)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 END IF
 
 Status=NF_INQ_ATTID(NcID,varID,Trim(Fact_NAME),AttID)
 IF(Status==-43)THEN
! WRITE(6,*) TRIM(Var_NAME),TRIM(Fact_NAME),' not exist'
 FACT=1.
 ELSE
 Status=NF_GET_ATT_REAL(ncID,varID,trim(Fact_NAME),FACT)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 END IF

 Status=NF_GET_VAR1_INT2(ncID,varID,Lrec,V_INT2)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_CLOSE(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 IF(FIG==1)THEN
 VAR=V_INT2*FACT
 ELSE
 IF(V_INT2==INT_LAND)THEN
 IF(PRESENT(LAND))THEN
 VAR=LAND
 END IF
 ELSE
 VAR=V_INT2*FACT
 END IF
 END IF
 RETURN
 END IF
!GET FLOAT TYPE DATA
 IF(vTYPE==NF_FLOAT)THEN

 Status=NF_INQ_ATTID(NcID,varID,Trim(ATT_NAME),AttID)
 IF(Status==-43)THEN
! WRITE(6,*) TRIM(VAR_NAME),TRIM(ATT_NAME),' not exist'
 FIG=1
 ELSE
 Status=NF_GET_ATT_REAL(ncID,varID,trim(ATT_NAME),REAL_LAND)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 END IF

 Status=NF_GET_VAR1_REAL(ncID,varID,Lrec,VAR)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_CLOSE(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 IF(FIG==0)THEN
 IF(VAR==REAL_LAND)THEN
 IF(PRESENT(LAND))THEN
 VAR=LAND
 END IF
 END IF
 END IF
 RETURN
 END IF
!GET DOUBLE PRECISION TYPE DATA
 IF(vTYPE==NF_DOUBLE)THEN
 Status=NF_INQ_ATTID(NcID,varID,Trim(ATT_NAME),AttID)
 DOU_LAND = -1
 IF(Status==-43)THEN
! WRITE(6,*) TRIM(VAR_NAME),TRIM(ATT_NAME),' not exist'
 FIG=1
 ELSE
 Status=NF_GET_ATT_DOUBLE(ncID,varID,trim(ATT_NAME),DOU_LAND)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 END IF

 Status=NF_GET_VAR1_DOUBLE(ncID,varID,Lrec,V_DOU)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_CLOSE(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 IF(PRESENT(VAR_DOU))VAR_DOU=V_DOU
 IF(FIG==0)THEN
 VAR=REAL(V_DOU)
 ELSE
 IF(V_DOU==DOU_LAND)THEN
 IF(PRESENT(LAND))THEN
 VAR=LAND
 END IF
 END IF
 END IF
 RETURN
 END IF
!GET OTHER TYPE DATA
 WRITE(6,*) "OTHER TYEP DATA"
 Status=NF_CLOSE(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 RETURN
 END Subroutine Nc_Read_0Dim
!----------------------------------------------------------------------
!读取1维数据
!Read one dimension data      
 SUBROUTINE Nc_Read_1DIM(FILENAME,VAR_NAME,VAR,VAR_DOU,LAND,LREC)
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FileName,VAR_NAME
 REAL,OPTIONAL,INTENT(IN) :: LAND
 REAL,INTENT(OUT) :: VAR(:)
 DOUBLE PRECISION,OPTIONAL,INTENT(OUT) :: VAR_DOU(:)
 INTEGER,Optional,INTENT(IN) :: Lrec 

 INTEGER*2,ALLOCATABLE :: V_INT2(:)
 DOUBLE PRECISION,ALLOCATABLE :: V_DOU(:)
 CHARACTER*100 ATT_NAME,FACT_NAME

 INTEGER ncID,vType,AttID
 INTEGER*2 INT_LAND
 INTEGER IM
 INTEGER,PARAMETER :: DIMNUM=1
 INTEGER COUNT(DIMNUM+1),START(DIMNUM+1)
 REAL FACT,REAL_LAND
 DOUBLE PRECISION :: DOUBLE_LAND
 INTEGER FIG
 
 DATA START/1,1/

 COUNT(1:DIMNUM)=SHAPE(VAR)
 COUNT(DIMNUM+1)=1
 IM=COUNT(1)

 FIG=0
 ATT_NAME='missing_value'
 FACT_NAME='scale_factor'

 Status=NF_OPEN(trim(FILENAME),NF_NOWRITE,ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status) 

 Status=NF_INQ_VARID(ncID,trim(VAR_NAME),varID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status) 

 Status=NF_INQ_VARTYPE(ncID,varID,vtype)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status) 

!GET INTEGER TYPE DATA
 IF(vTYPE==NF_SHORT)THEN

 ALLOCATE(V_INT2(IM),STAT=IERR)
 CALL ISLOCATE(IERR) 

 Status=NF_INQ_ATTID(NcID,varID,Trim(ATT_NAME),AttID)
 IF(Status==-43)THEN
! WRITE(6,*) TRIM(VAR_NAME),TRIM(ATT_NAME),' not exist'
 FIG=1
 ELSE
 Status=NF_GET_ATT_INT2(ncID,varID,trim(ATT_NAME),INT_LAND)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 END IF
 
 Status=NF_INQ_ATTID(NcID,varID,Trim(Fact_NAME),AttID)
 IF(Status==-43)THEN
! WRITE(6,*) TRIM(FACT_NAME),TRIM(Fact_NAME),' not exist'
 FACT=1.
 ELSE
 Status=NF_GET_ATT_REAL(ncID,varID,trim(Fact_NAME),FACT)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 END IF

 IF(present(Lrec))THEN
 START(DIMNUM+1)=LREC
 Status=NF_GET_VARA_INT2(ncID,varID,start,count,V_INT2)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 ELSE
 Status=NF_GET_VAR_INT2(ncID,varID,V_INT2)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 END IF

 Status=NF_CLOSE(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 IF(FIG==1)THEN
 VAR=V_INT2*FACT
 ElSE
 DO I=1,IM
 IF(V_INT2(I)==INT_LAND)THEN
 IF(PRESENT(LAND))THEN
 VAR(I)=LAND
 ELSE
 VAR(I)=INT_LAND*1.
 END IF
 ELSE
 VAR(I)=V_INT2(I)*FACT
 END IF
 END DO
 END IF

 DEALLOCATE(V_INT2)
 RETURN
 END IF
!GET FLOAT TYPE DATA
 IF(vTYPE==NF_FLOAT)THEN

 Status=NF_INQ_ATTID(NcID,varID,Trim(ATT_NAME),AttID)
 IF(Status==-43)THEN
! WRITE(6,*) TRIM(VAR_NAME),TRIM(ATT_NAME),' not exist'
 FIG=1
 ELSE
 Status=NF_GET_ATT_REAL(ncID,varID,trim(ATT_NAME),REAL_LAND)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 END IF

 IF(present(Lrec))THEN
 START(DIMNUM+1)=LREC
 Status=NF_GET_VARA_REAL(ncID,varID,start,count,VAR)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 ELSE
 Status=NF_GET_VAR_REAL(ncID,varID,VAR)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 END IF

 Status=NF_CLOSE(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 IF(FIG==0)THEN
 DO I=1,IM
 IF(ABS(VAR(I)-REAL_LAND)<0.0001)THEN
 IF(PRESENT(LAND))THEN
 VAR(I)=LAND
 END IF
 END IF
 END DO
 END IF
 RETURN
 END IF
! GET DOUBLE PRECISION DATA
 IF(VTYPE==NF_DOUBLE)THEN

 ALLOCATE(V_DOU(IM),STAT=IERR)
 CALL ISLOCATE(IERR) 

 
 Status=NF_INQ_ATTID(NcID,varID,Trim(ATT_NAME),AttID)
 IF(Status==-43)THEN
! WRITE(6,*) TRIM(VAR_NAME),TRIM(ATT_NAME),' not exist'
 FIG=1
 ELSE
 Status=NF_GET_ATT_DOUBLE(ncID,varID,trim(ATT_NAME),DOUBLE_LAND)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 END IF

 IF(present(Lrec))THEN
 START(DIMNUM+1)=LREC
 Status=NF_GET_VARA_DOUBLE(ncID,varID,start,count,V_DOU)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 ELSE
 Status=NF_GET_VAR_DOUBLE(ncID,varID,V_DOU)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 END IF

 Status=NF_CLOSE(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 
 IF(FIG==1)THEN
 VAR=REAL(V_DOU)
 ELSE 
 DO I=1,IM
 IF(ABS(V_DOU(I)-DOUBLE_LAND)<0.0001)THEN
 IF(PRESENT(LAND))THEN
 V_DOU(I)=LAND
 END IF
 END IF
 END DO
 Var=REAL(V_DOU)
 END IF

 IF(PRESENT(VAR_DOU))VAR_DOU=V_DOU

 DEALLOCATE(V_DOU)
 RETURN
 END IF
!GET OTHER TYPE DATA
 WRITE(6,*) "OTHER TYEP DATA"
 Status=NF_CLOSE(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 RETURN

 END Subroutine Nc_Read_1Dim
!----------------------------------------------------------------------
!读取2维数据
!Read two dimension data      
 SUBROUTINE Nc_Read_2DIM(FILENAME,VAR_NAME,VAR,LAND,LREC,I1,J1)
! SUBROUTINE Nc_Read_2DIM(FILENAME,VAR_NAME,VAR,LAND,LREC)
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FileName,VAR_NAME
 REAL,OPTIONAL,INTENT(IN) :: LAND
 REAL,INTENT(OUT) :: VAR(:,:)
 INTEGER,Optional,INTENT(IN) :: Lrec,I1,J1 

 INTEGER*2,ALLOCATABLE :: V_INT2(:,:)
 DOUBLE PRECISION,ALLOCATABLE :: V_DOU(:,:)
 CHARACTER*100 ATT_NAME,FACT_NAME

 INTEGER ncID,vType,AttID
 INTEGER*2 INT_LAND
 INTEGER IM,JM
 INTEGER,PARAMETER :: DIMNUM=2
 INTEGER COUNT(DIMNUM+1),START(DIMNUM+1)
 REAL FACT,REAL_LAND
 DOUBLE PRECISION :: DOUBLE_LAND
 INTEGER FIG
 
 DATA START/1,1,1/
 DATA COUNT/1,1,1/
 IF(PRESENT(I1))START(1)=I1
 IF(PRESENT(J1))START(2)=J1
 IF(PRESENT(LREC))START(DIMNUM+1)=LREC

 COUNT(1:DIMNUM)=SHAPE(VAR)
 COUNT(DIMNUM+1)=1
 IM=COUNT(1)
 JM=COUNT(2)

 FIG=0
 ATT_NAME='missing_value'
 FACT_NAME='scale_factor'

 Status=NF_OPEN(trim(FILENAME),NF_NOWRITE,ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status) 

 Status=NF_INQ_VARID(ncID,trim(VAR_NAME),varID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status) 

 Status=NF_INQ_VARTYPE(ncID,varID,vtype)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status) 

!GET INTEGER TYPE DATA
 IF(vTYPE==NF_SHORT)THEN

 ALLOCATE(V_INT2(IM,JM),STAT=IERR)
 CALL ISLOCATE(IERR) 

 Status=NF_INQ_ATTID(NcID,varID,Trim(ATT_NAME),AttID)
 IF(Status==-43)THEN
! WRITE(6,*) TRIM(VAR_NAME),TRIM(ATT_NAME),' not exist'
 FIG=1
 ELSE
 Status=NF_GET_ATT_INT2(ncID,varID,trim(ATT_NAME),INT_LAND)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 END IF
 
 Status=NF_INQ_ATTID(NcID,varID,Trim(Fact_NAME),AttID)
 IF(Status==-43)THEN
! WRITE(6,*) TRIM(FACT_NAME),TRIM(Fact_NAME),' not exist'
 FACT=1.
 ELSE
 Status=NF_GET_ATT_REAL(ncID,varID,trim(Fact_NAME),FACT)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 END IF

 IF(present(Lrec).OR.PRESENT(I1))THEN
! IF(present(Lrec))THEN
 Status=NF_GET_VARA_INT2(ncID,varID,start,count,V_INT2)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 ELSE
 Status=NF_GET_VAR_INT2(ncID,varID,V_INT2)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 END IF

 Status=NF_CLOSE(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 IF(FIG==1)THEN
 VAR=V_INT2*FACT
 ElSE
 DO J=1,JM
 DO I=1,IM
 IF(V_INT2(I,J)==INT_LAND)THEN
 IF(PRESENT(LAND))THEN
 VAR(I,J)=LAND
 ELSE
 VAR(I,J)=INT_LAND*1.
 END IF
 ELSE
 VAR(I,J)=V_INT2(I,J)*FACT
 END IF
 END DO
 END DO
 END IF

 DEALLOCATE(V_INT2)
 RETURN
 END IF
!GET FLOAT TYPE DATA
 IF(vTYPE==NF_FLOAT)THEN

 Status=NF_INQ_ATTID(NcID,varID,Trim(ATT_NAME),AttID)
 IF(Status==-43)THEN
! WRITE(6,*) TRIM(VAR_NAME),TRIM(ATT_NAME),' not exist'
 FIG=1
 ELSE
 Status=NF_GET_ATT_REAL(ncID,varID,trim(ATT_NAME),REAL_LAND)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 END IF

 IF(present(Lrec).OR.PRESENT(I1))THEN
! IF(present(Lrec))THEN
 Status=NF_GET_VARA_REAL(ncID,varID,start,count,VAR)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 ELSE
 Status=NF_GET_VAR_REAL(ncID,varID,VAR)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 END IF

 Status=NF_CLOSE(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 IF(FIG==0)THEN
 DO J=1,JM
 DO I=1,IM
 IF(ABS(VAR(I,J)-REAL_LAND)<0.0001)THEN
 IF(PRESENT(LAND))THEN
 VAR(I,J)=LAND
 END IF
 END IF
 END DO
 END DO
 END IF
 RETURN
 END IF
! GET DOUBLE PRECISION DATA
 IF(VTYPE==NF_DOUBLE)THEN

 ALLOCATE(V_DOU(IM,JM),STAT=IERR)
 CALL ISLOCATE(IERR) 

 
 Status=NF_INQ_ATTID(NcID,varID,Trim(ATT_NAME),AttID)
 IF(Status==-43)THEN
! WRITE(6,*) TRIM(VAR_NAME),TRIM(ATT_NAME),' not exist'
 FIG=1
 ELSE
 Status=NF_GET_ATT_DOUBLE(ncID,varID,trim(ATT_NAME),DOUBLE_LAND)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 END IF

 IF(present(Lrec).OR.PRESENT(I1))THEN
! IF(present(Lrec))THEN
 Status=NF_GET_VARA_DOUBLE(ncID,varID,start,count,V_DOU)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 ELSE
 Status=NF_GET_VAR_DOUBLE(ncID,varID,V_DOU)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 END IF

 Status=NF_CLOSE(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 IF(FIG==1)THEN
 VAR=REAL(V_DOU)
 ELSE 
 DO J=1,JM
 DO I=1,IM
 IF(ABS(V_DOU(I,J)-DOUBLE_LAND)<0.0001)THEN
 IF(PRESENT(LAND))THEN
 V_DOU(I,J)=LAND
 END IF
 END IF
 END DO
 END DO
 Var=REAL(V_DOU)
 END IF

 DEALLOCATE(V_DOU)
 RETURN
 END IF
!GET OTHER TYPE DATA
 WRITE(6,*) "OTHER TYEP DATA"
 Status=NF_CLOSE(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 RETURN
 END Subroutine Nc_Read_2Dim
!----------------------------------------------------------------------
!读取3维数据
!Read three dimension data      
 SUBROUTINE Nc_Read_3DIM(FILENAME,VAR_NAME,VAR,LAND,LREC,I1,J1)
! SUBROUTINE Nc_Read_3DIM(FILENAME,VAR_NAME,VAR,LAND,LREC)
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FileName,VAR_NAME
 REAL,OPTIONAL,INTENT(IN) :: LAND
 REAL,INTENT(OUT) :: VAR(:,:,:)
 INTEGER,Optional,INTENT(IN) :: Lrec ,I1,J1 

 INTEGER*2,ALLOCATABLE :: V_INT2(:,:,:)
 DOUBLE PRECISION,ALLOCATABLE :: V_DOU(:,:,:)
 CHARACTER*100 ATT_NAME,FACT_NAME

 INTEGER ncID,vType,AttID
 INTEGER*2 INT_LAND
 INTEGER IM,JM,KB
 INTEGER,PARAMETER :: DIMNUM=3
 INTEGER COUNT(DIMNUM+1),START(DIMNUM+1)
 REAL FACT,REAL_LAND
 DOUBLE PRECISION :: DOUBLE_LAND
 INTEGER FIG
 
 DATA START/1,1,1,1/
 DATA COUNT/1,1,1,1/

 IF(PRESENT(I1))START(1)=I1
 IF(PRESENT(J1))START(2)=J1
 IF(PRESENT(LREC))START(DIMNUM+1)=LREC

 COUNT(1:DIMNUM)=SHAPE(VAR)
 COUNT(DIMNUM+1)=1
 IM=COUNT(1)
 JM=COUNT(2)
 KB=COUNT(3)

 FIG=0
 ATT_NAME='missing_value'
 FACT_NAME='scale_factor'

 Status=NF_OPEN(trim(FILENAME),NF_NOWRITE,ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status) 

 Status=NF_INQ_VARID(ncID,trim(VAR_NAME),varID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status) 

 Status=NF_INQ_VARTYPE(ncID,varID,vtype)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status) 

!GET INTEGER TYPE DATA
 IF(vTYPE==NF_SHORT)THEN

 ALLOCATE(V_INT2(IM,JM,KB),STAT=IERR)
 CALL ISLOCATE(IERR) 

 Status=NF_INQ_ATTID(NcID,varID,Trim(ATT_NAME),AttID)
 IF(Status==-43)THEN
! WRITE(6,*) TRIM(VAR_NAME),TRIM(ATT_NAME),' not exist'
 FIG=1
 ELSE
 Status=NF_GET_ATT_INT2(ncID,varID,trim(ATT_NAME),INT_LAND)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 END IF
 
 Status=NF_INQ_ATTID(NcID,varID,Trim(Fact_NAME),AttID)
 IF(Status==-43)THEN
! WRITE(6,*) TRIM(FACT_NAME),TRIM(Fact_NAME),' not exist'
 FACT=1.
 ELSE
 Status=NF_GET_ATT_REAL(ncID,varID,trim(Fact_NAME),FACT)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 END IF

 IF(present(Lrec).OR.PRESENT(I1))THEN
! IF(present(Lrec))THEN
 Status=NF_GET_VARA_INT2(ncID,varID,start,count,V_INT2)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 ELSE
 Status=NF_GET_VAR_INT2(ncID,varID,V_INT2)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 END IF

 Status=NF_CLOSE(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 IF(FIG==1)THEN
 VAR=V_INT2*FACT
 ElSE
 DO K=1,KB
 DO J=1,JM
 DO I=1,IM
 IF(V_INT2(I,J,K)==INT_LAND)THEN
 IF(PRESENT(LAND))THEN
 VAR(I,J,K)=LAND
 ELSE
 VAR(I,J,K)=INT_LAND*1.
 END IF
 ELSE
 VAR(I,J,K)=V_INT2(I,J,K)*FACT
 END IF
 END DO
 END DO
 END DO
 END IF

 DEALLOCATE(V_INT2)
 RETURN
 END IF
!GET FLOAT TYPE DATA
 IF(vTYPE==NF_FLOAT)THEN

 Status=NF_INQ_ATTID(NcID,varID,Trim(ATT_NAME),AttID)
 IF(Status==-43)THEN
! WRITE(6,*) TRIM(VAR_NAME),TRIM(ATT_NAME),' not exist'
 FIG=1
 ELSE
 Status=NF_GET_ATT_REAL(ncID,varID,trim(ATT_NAME),REAL_LAND)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 END IF

 IF(present(Lrec).OR.PRESENT(I1))THEN
! IF(present(Lrec))THEN
 Status=NF_GET_VARA_REAL(ncID,varID,start,count,VAR)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 ELSE
 Status=NF_GET_VAR_REAL(ncID,varID,VAR)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 END IF

 Status=NF_CLOSE(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 IF(FIG==0)THEN
 DO K=1,KB
 DO J=1,JM
 DO I=1,IM
 IF(ABS(VAR(I,J,K)-REAL_LAND)<0.01)THEN
 IF(PRESENT(LAND))THEN
 VAR(I,J,K)=LAND
 END IF
 END IF
 END DO
 END DO
 END DO
 END IF
 RETURN
 END IF
! GET DOUBLE PRECISION DATA
 IF(VTYPE==NF_DOUBLE)THEN

 ALLOCATE(V_DOU(IM,JM,KB),STAT=IERR)
 CALL ISLOCATE(IERR) 

 
 Status=NF_INQ_ATTID(NcID,varID,Trim(ATT_NAME),AttID)
 IF(Status==-43)THEN
! WRITE(6,*) TRIM(VAR_NAME),TRIM(ATT_NAME),' not exist'
 FIG=1
 ELSE
 Status=NF_GET_ATT_DOUBLE(ncID,varID,trim(ATT_NAME),DOUBLE_LAND)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 END IF

 IF(present(Lrec).OR.PRESENT(I1))THEN
! IF(present(Lrec))THEN
 Status=NF_GET_VARA_DOUBLE(ncID,varID,start,count,V_DOU)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 ELSE
 Status=NF_GET_VAR_DOUBLE(ncID,varID,V_DOU)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 END IF

 Status=NF_CLOSE(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 IF(FIG==1)THEN
 VAR=REAL(V_DOU)
 ELSE 
 DO K=1,KB
 DO J=1,JM
 DO I=1,IM
 IF(ABS(V_DOU(I,J,K)-DOUBLE_LAND)<0.0001)THEN
 IF(PRESENT(LAND))THEN
 V_DOU(I,J,K)=LAND
 END IF
 END IF
 END DO
 END DO
 END DO
 Var=REAL(V_DOU)
 END IF

 DEALLOCATE(V_DOU)
 RETURN
 END IF
!GET OTHER TYPE DATA
 WRITE(6,*) "OTHER TYEP DATA"
 Status=NF_CLOSE(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 RETURN
 END Subroutine Nc_Read_3Dim
!----------------------------------------------------------------------
!写3维的NC文件
!Write three dimension file
 SUBROUTINE Nc_Write_3Dim(FILENAME,Var_Name,Var,InLand,Fact,Lrec,I1,J1)
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: Var_Name,FileName
 REAL,INTENT(IN) :: Var(:,:,:)
 REAL,OPtional,INTENT(IN) :: InLand
 REAL,Optional,INTENT(IN) :: Fact 
 INTEGER,Optional,INTENT(IN) :: Lrec,I1,J1

 INTEGER :: Vtype
 INTEGER :: IM,JM,KB
 INTEGER*2,ALLOCATABLE :: VAR_INT2(:,:,:)
 INTEGER*2 :: Land=32767
!DEFINE VARIABLES FOR LOCAL FUNCTION
 INTEGER ncID,IERR
 INTEGER,PARAMETER :: DIMNUM = 3
 INTEGER,DIMENSION(DIMNUM+1) :: START,COUNT
 Data Start/1,1,1,1/
 IF(PRESENT(I1))START(1)=I1
 IF(PRESENT(J1))START(2)=J1
 COUNT(1:DIMNUM)=SHAPE(VAR)
 IM=COUNT(1)
 JM=COUNT(2)
 KB=COUNT(3)
 COUNT(DIMNUM+1)=1

 Status=NF_OPEN(TRIM(FILENAME),NF_WRITE,ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_INQ_VARID(ncID,TRIM(VAR_NAME),varID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_INQ_VARTYPE(ncID,varID,vtype)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 IF(Vtype==NF_INT2)THEN
 ALLOCATE(VAR_INT2(IM,JM,KB),STAT=IERR)
 CALL ISLOCATE(IERR)
 IF(PRESENT(INLAND))THEN
 DO K=1,KB
 DO J=1,JM
 DO I=1,IM
 IF(ABS(Var(I,J,K)-INLAND)<0.00001)THEN
 Var_Int2(I,J,K)=Land
 ELSE
 Var_Int2(I,J,K)=int(Var(I,J,K)/fact)
 END IF
 ENDDO 
 ENDDO
 ENDDO
 ELSE
 Var_Int2=int(Var/fact)
 END IF
 END IF

 IF(Present(Lrec))THEN
 Start(DIMNUM+1)=Lrec

 IF(Vtype==NF_INT2)THEN
 Status=NF_PUT_VARA_INT2(ncID,varID,start,count,Var_INT2)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 DEALLOCATE(VAR_INT2)
 ELSEIF(Vtype==NF_REAL)THEN
 Status=NF_PUT_VARA_REAL(ncID,varID,start,count,Var)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 ELSE
   WRITE(6,*) "ERROR DATA TYPE"
 END IF

 ELSE

 IF(Vtype==NF_INT2)THEN
 Status=NF_PUT_VAR_INT2(ncID,varID,Var_INT2)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 DEALLOCATE(VAR_INT2)
 ELSEIF(Vtype==NF_REAL)THEN
 Status=NF_PUT_VAR_REAL(ncID,varID,Var)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 ELSE
   WRITE(6,*) "ERROR DATA TYPE"
 END IF

 END IF

 Status=NF_CLOSE(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 END SubRoutine Nc_Write_3Dim
!----------------------------------------------------------------------
!写2维的NC文件
!Write two dimension file
 SUBROUTINE Nc_Write_2Dim(FILENAME,Var_Name,Var,InLand,Fact,Lrec,I1,J1)
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: Var_Name,FileName
 REAL,INTENT(IN) :: Var(:,:)
 REAL,Optional,INTENT(IN) :: InLand
 REAL,Optional,INTENT(IN) :: Fact 
 INTEGER,Optional,INTENT(IN) :: Lrec,I1,J1

 INTEGER :: Vtype
 INTEGER :: IM,JM
 INTEGER*2,ALLOCATABLE :: VAR_INT2(:,:)
 INTEGER*2 :: Land=32767
!DEFINE VARIABLES FOR LOCAL FUNCTION
 INTEGER ncID,IERR
 INTEGER,PARAMETER :: DIMNUM = 2
 INTEGER,DIMENSION(DIMNUM+1) :: START,COUNT
 Data Start/1,1,1/
 IF(PRESENT(I1))START(1)=I1
 IF(PRESENT(J1))START(2)=J1
 COUNT(1:DIMNUM)=SHAPE(VAR)
 IM=COUNT(1)
 JM=COUNT(2)
 COUNT(DIMNUM+1)=1

 Status=NF_OPEN(TRIM(FILENAME),NF_WRITE,ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_INQ_VARID(ncID,TRIM(VAR_NAME),varID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_INQ_VARTYPE(ncID,varID,vtype)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 IF(Vtype==NF_INT2)THEN
 ALLOCATE(VAR_INT2(IM,JM),STAT=IERR)
 CALL ISLOCATE(IERR)
 IF(PRESENT(INLAND))THEN
 DO J=1,JM
 DO I=1,IM
 IF(ABS(Var(I,J)-INLAND)<0.00001)THEN
 Var_Int2(I,J)=Land
 ELSE
 Var_Int2(I,J)=int(Var(I,J)/fact)
 END IF
 ENDDO 
 ENDDO
 ELSE
 Var_Int2=int(Var/fact)
 END IF
 END IF

 IF(Present(Lrec))THEN
 Start(DIMNUM+1)=Lrec

 IF(Vtype==NF_INT2)THEN
 Status=NF_PUT_VARA_INT2(ncID,varID,start,count,Var_INT2)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 DEALLOCATE(VAR_INT2)
 ELSEIF(Vtype==NF_REAL)THEN
 Status=NF_PUT_VARA_REAL(ncID,varID,start,count,Var)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 ELSE
   WRITE(6,*) "ERROR DATA TYPE"
 END IF

 ELSE

 IF(Vtype==NF_INT2)THEN
 Status=NF_PUT_VAR_INT2(ncID,varID,Var_INT2)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 DEALLOCATE(VAR_INT2)
 ELSEIF(Vtype==NF_REAL)THEN
 Status=NF_PUT_VAR_REAL(ncID,varID,Var)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 ELSE
   WRITE(6,*) "ERROR DATA TYPE"
 END IF

 END IF

 Status=NF_CLOSE(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 END SubRoutine Nc_Write_2Dim
!----------------------------------------------------------------------
!写1维的NC文件
!Write one dimension file
 SUBROUTINE Nc_Write_1Dim(FILENAME,Var_Name,Var,InLand,Fact,Lrec)
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: Var_Name,FileName
 REAL,INTENT(IN) :: Var(:)
 REAL,Optional,INTENT(IN) :: InLand
 REAL,Optional,INTENT(IN) :: Fact 
 INTEGER,Optional,INTENT(IN) :: Lrec

 INTEGER :: Vtype
 INTEGER :: IM
 INTEGER*2,ALLOCATABLE :: VAR_INT2(:)
 INTEGER*2 :: Land=32767
!DEFINE VARIABLES FOR LOCAL FUNCTION
 INTEGER ncID,IERR
 INTEGER,PARAMETER :: DIMNUM = 1
 INTEGER,DIMENSION(DIMNUM+1) :: START,COUNT
 Data Start/1,1/
 COUNT(1:DIMNUM)=SHAPE(VAR)
 IM=COUNT(1)
 COUNT(DIMNUM+1)=1

 Status=NF_OPEN(TRIM(FILENAME),NF_WRITE,ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_INQ_VARID(ncID,TRIM(VAR_NAME),varID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_INQ_VARTYPE(ncID,varID,vtype)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 IF(Vtype==NF_INT2)THEN
 ALLOCATE(VAR_INT2(IM),STAT=IERR)
 CALL ISLOCATE(IERR)
 IF(PRESENT(INLAND))THEN
 DO I=1,IM
 IF(ABS(Var(i)-INLAND)<0.00001)THEN
 Var_Int2(i)=Land
 ELSE
 Var_Int2(i)=int(Var(I)/fact)
 END IF
 ENDDO
 ELSE
 Var_Int2=int(Var/Fact)
 END IF
 END IF

 IF(Present(Lrec))THEN
 Start(DIMNUM+1)=Lrec

 IF(Vtype==NF_INT2)THEN
 Status=NF_PUT_VARA_INT2(ncID,varID,start,count,Var_INT2)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 DEALLOCATE(VAR_INT2)
 ELSEIF(Vtype==NF_REAL)THEN
 Status=NF_PUT_VARA_REAL(ncID,varID,start,count,Var)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 ELSE
    WRITE(6,*) "ERROR DATA TYPE"
 END IF

 ELSE

 IF(Vtype==NF_INT2)THEN
 Status=NF_PUT_VAR_INT2(ncID,varID,Var_INT2)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 DEALLOCATE(VAR_INT2)
 ELSEIF(Vtype==NF_REAL)THEN
 Status=NF_PUT_VAR_REAL(ncID,varID,Var)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 ELSE
   WRITE(6,*) "ERROR DATA TYPE"
 END IF

 END IF

 Status=NF_CLOSE(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 END SubRoutine Nc_Write_1Dim
!----------------------------------------------------------------------
!写0维(单独)的NC文件
!Write zero dimension file
 SUBROUTINE Nc_Write_0Dim(FILENAME,Var_Name,Var,InLand,Fact,Lrec)
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: Var_Name,FileName
 REAL,INTENT(IN) :: Var
 REAL,Optional,INTENT(IN) :: InLand
 REAL,Optional,INTENT(IN) :: Fact 
 INTEGER,Optional,INTENT(IN) :: Lrec

 INTEGER :: Vtype
 INTEGER*2 :: VAR_INT2
 INTEGER*2,PARAMETER:: Land=32767
!DEFINE VARIABLES FOR LOCAL FUNCTION
 INTEGER ncID

 Status=NF_OPEN(TRIM(FILENAME),NF_WRITE,ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_INQ_VARID(ncID,TRIM(VAR_NAME),varID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_INQ_VARTYPE(ncID,varID,vtype)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 IF(Vtype==NF_INT2)THEN
 IF(PRESENT(INLAND))THEN
 IF(ABS(Var-INLAND)<0.00001)THEN
 Var_Int2=Land
 ELSE
 Var_Int2=int(Var/fact)
 END IF
 ELSE
 Var_Int2=int(Var/fact)
 END IF
 END IF

 IF(Present(Lrec))THEN

 IF(Vtype==NF_INT2)THEN
 Status=NF_PUT_VAR1_INT2(ncID,varID,Lrec,Var_INT2)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 ELSEIF(Vtype==NF_REAL)THEN
 Status=NF_PUT_VAR1_REAL(ncID,varID,Lrec,Var)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 ELSE
    WRITE(6,*) "ERROR DATA TYPE"
 END IF

 ELSE

 IF(Vtype==NF_INT2)THEN
 Status=NF_PUT_VAR_INT2(ncID,varID,Var_INT2)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 ELSEIF(Vtype==NF_REAL)THEN
 Status=NF_PUT_VAR_REAL(ncID,varID,Var)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 ELSE
   WRITE(6,*) "ERROR DATA TYPE"
 END IF

 END IF

 Status=NF_CLOSE(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 END SubRoutine Nc_Write_0Dim
!----------------------------------------------------------------------
!写0维(单独)的NC文件
!Write zero dimension file
 SUBROUTINE Nc_Write_0Dim_DOU(FILENAME,Var_Name,Var,Lrec)
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: Var_Name,FileName
 DOUBLE PRECISION,INTENT(IN) :: Var
 INTEGER,Optional,INTENT(IN) :: Lrec

!DEFINE VARIABLES FOR LOCAL FUNCTION
 INTEGER ncID

 Status=NF_OPEN(TRIM(FILENAME),NF_WRITE,ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_INQ_VARID(ncID,TRIM(VAR_NAME),varID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 IF(Present(Lrec))THEN
 Status=NF_PUT_VAR1_DOUBLE(ncID,varID,Lrec,Var)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 ELSE
 Status=NF_PUT_VAR_DOUBLE(ncID,varID,Var)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 END IF

 Status=NF_CLOSE(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 END SubRoutine Nc_Write_0Dim_DOU
!----------------------------------------------------------------------
!写字符串数据
!Write text data 
 SUBROUTINE Nc_Write_Text(FILENAME,Var_NAME,VAR,LREC)
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN)::VAR_NAME,FILENAME
 CHARACTER(LEN=*),INTENT(IN) :: VAR
 INTEGER,OPTIONAL :: LREC
!DEFINE VARIABLES FOR LOCAL FUNCTION
 INTEGER ncID
 INTEGER START(2),COUNT(2)
 DATA START/1,1/

 COUNT(1)=len_trim(Var)
 COUNT(2)=1

 Status=NF_OPEN(TRIM(FILENAME),NF_WRITE,ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_INQ_VARID(ncID,TRIM(VAR_NAME),varID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 IF(PRESENT(LREC))THEN
 START(2)=LREC
 Status=NF_PUT_VARA_TEXT(ncID,varID,start,count,VAR)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 ELSE
 Status=NF_PUT_VAR_TEXT(ncID,varID,VAR)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 END IF

 Status=NF_CLOSE(ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 END SubRoutine Nc_Write_Text
!----------------------------------------------------------------------
 Subroutine ncfileset(FileName,X,Y,Z)
! use NcFileMod
 Implicit none
 CHARACTER(LEN=*) FileName
 REAL,OPTIONAL,INTENT(IN) :: X(:),Y(:),Z(:)

 INTEGER IM,JM,KB,TFIG
 INTEGER DIMS(1)
 IM=0;JM=0;KB=0;TFIG=1
 IF(PRESENT(X))THEN
 DIMS(1:1)=SHAPE(X)
 IM=DIMS(1)
 END IF
 IF(PRESENT(Y))THEN
 DIMS=SHAPE(Y)
 JM=DIMS(1)
 END IF
 IF(PRESENT(Z))THEN
 DIMS=SHAPE(Z)
 KB=DIMS(1)
 END IF
 CALL CREATE_FILE(FILENAME,TFIG,IM,'lon',JM,'lat',KB,'dep')
 IF(IM/=0)THEN
 CALL Nc_Write(FileName,'lon',X)
 END IF
 IF(JM/=0)THEN
 CALL Nc_Write(FileName,'lat',Y)
 END IF
 IF(KB/=0)THEN
 CALL Nc_Write(FileName,'dep',Z)
 END IF
! Create File
 END Subroutine ncfileset
!----------------------------------------------------------------------
 Subroutine OUTNC_0D(FN,VN,Var,LAND,Vtype,Fact,Units,Lrec)
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FN,VN
 REAL,INTENT(IN) :: Var
 REAL,Optional,INTENT(IN) :: LAND,Fact
 CHARACTER(Len=*),Optional,INTENT(IN) :: Units
 INTEGER,Optional,INTENT(IN) :: Vtype,LREC

 INTEGER IM,JM,LLREC
 INTEGER VVType
 INTEGER NcID
 CHARACTER(LEN=10) :: DimsName(1)=(/'time'/)
 INTEGER*2,PARAMETER :: LAND_INT2=32767
 DOUBLE PRECISION TT

 VVtype=NF_INT2
 IF(PRESENT(Vtype))THEN
 VVtype=Vtype
 END IF

 Status=NF_OPEN(TRIM(FN),NF_WRITE,ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_INQ_VARID(ncID,TRIM(VN),varID)
 IF(Status==-49)THEN
! difine 2d data
! WRITE(6,*) "DEFINE NEW VARIABLES ",TRIM(VN)
 CALL DefVar(FN,VN,VVType,[DimsName])
 IF(PRESENT(LAND))THEN
 IF(VVtype==NF_INT2)THEN
 CALL Set_Att(FN,'missing_value',Land_INT2,VN) 
 ELSE 
 CALL Set_Att(FN,'missing_value',Land,VN) 
 END IF
 END IF
 IF(PRESENT(FACT))THEN
 CALL Set_Att(FN,'scale_factor',Fact,VN)
 END IF
 IF(PRESENT(UNITS))THEN
 CAll Set_Att(FN,'units',Units,VN)
 END IF
 END IF
 
 STATUS=NF_CLOSE(ncID)
 IF(STATUS.NE.NF_NOERR) CALL HANDLE_ERR(STATUS)

 Status=NF_OPEN(TRIM(FN),NF_WRITE,ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_INQ_VARID(ncID,VN,varID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 STATUS=NF_CLOSE(ncID)
 IF(STATUS.NE.NF_NOERR) CALL HANDLE_ERR(STATUS)

 LLREC=1
 IF(PRESENT(LREC))THEN
 LLrec=Lrec
 END IF
 TT=LLREC*1.
 CALL NC_WRITE(FN,'time',TT,Lrec=LLrec)

 IF(PRESENT(LAND).AND.PRESENT(FACT))THEN
 CALL NC_WRITE(FN,VN,Var,InLand=Land,Fact=Fact,Lrec=LLrec)
 Return
 END IF

 IF(PRESENT(LAND))THEN
 CALL NC_WRITE(FN,VN,Var,InLand=Land,Lrec=LLrec)
 RETURN
 END IF

 IF(PRESENT(Fact))THEN
 CALL NC_WRITE(FN,VN,Var,Fact=Fact,Lrec=LLrec)
 RETURN
 END IF
 CALL NC_WRITE(FN,VN,Var,Lrec=LLrec)
 END Subroutine OUTNC_0D
!----------------------------------------------------------------------
 Subroutine OUTNC_TEXT(FN,VN,Var,Lrec)
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FN,VN
 ChARACTER(LEN=*),INTENT(INOUT) :: Var
 INTEGER,INTENT(IN) :: LREC

 INTEGER NcID
 INTEGER YEAR,MONTH,DAY,HOUR,COUNTT,KTT
 DOUBLE PRECISION TT

 CHARACTER(LEN=10) :: DimsName(2)=(/'char','time'/)

 Status=NF_OPEN(TRIM(FN),NF_WRITE,ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_INQ_VARID(ncID,TRIM(VN),varID)
 IF(Status==-49)THEN
! difine 2d data
! WRITE(6,*) "DEFINE NEW VARIABLES ",TRIM(VN)
 CALL DefVar(FN,VN,NF_CHAR,[DimsName])
 END IF

 STATUS=NF_CLOSE(ncID)
 IF(STATUS.NE.NF_NOERR) CALL HANDLE_ERR(STATUS)

 COUNTT=LEN_TRIM(VAR)
 YEAR=0
 MONTH=1
 DAY=1
 HOUR=0

 IF(COUNTT>=4)THEN
 READ(Var(1:4),'(i4.4)')YEAR
 END IF
 IF(COUNTT>=6)THEN
 READ(Var(5:6),'(i2.2)')MONTH
 END IF
 IF(COUNTT>=8)THEN
 READ(Var(7:8),'(i2.2)')DAY
 END IF
 IF(COUNTT==10)THEN
 READ(Var(9:10),'(i2.2)')HOUR
 END IF

 TT=datenum([YEAR,MONTH,DAY,HOUR,0,0], 2)-24
! CALL OUTNC_0D_DOU(FN,'time',TT,Lrec)
 CALL OUTNC(FN,'time',TT,Lrec)
 CALL NC_WRITE(FN,VN,Var,Lrec=Lrec)

 END Subroutine OUTNC_TEXT
!----------------------------------------------------------------------
 Subroutine OUTNC_TEXT1(FN,VN,Var,Lrec)
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FN,VN
 INTEGER,INTENT(IN) :: Var(:)
 INTEGER,INTENT(IN) :: LREC

 INTEGER NcID,COUNT(1)
 INTEGER YEAR,MONTH,DAY,HOUR,COUNTT,KTT
 DOUBLE PRECISION TT
 CHARACTER*10 DATE

 CHARACTER(LEN=10) :: DimsName(2)=(/'char','time'/)

 Status=NF_OPEN(TRIM(FN),NF_WRITE,ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_INQ_VARID(ncID,TRIM(VN),varID)
 IF(Status==-49)THEN
! difine 2d data
! WRITE(6,*) "DEFINE NEW VARIABLES ",TRIM(VN)
 CALL DefVar(FN,VN,NF_CHAR,[DimsName])
 END IF

 STATUS=NF_CLOSE(ncID)
 IF(STATUS.NE.NF_NOERR) CALL HANDLE_ERR(STATUS)

 COUNT=SHAPE(VAR)
 COUNTT=COUNT(1)
 YEAR=0
 MONTH=1
 DAY=1
 HOUR=0

 IF(COUNTT>=1)THEN
 YEAR=Var(1)
 END IF
 IF(COUNTT>=2)THEN
 MONTH=Var(2)
 END IF
 IF(COUNTT>=3)THEN
 DAY=Var(3)
 END IF
 IF(COUNTT==4)THEN
 HOUR=Var(4)
 END IF

 TT=datenum([YEAR,MONTH,DAY,HOUR,0,0], 2)-24
! CALL OUTNC_0D_DOU(FN,'time',TT,Lrec)
 CALL OUTNC(FN,'time',TT,Lrec)
 WRITE(DATE(1:4),'(i4.4)')YEAR
 WRITE(DATE(5:6),'(i2.2)')MONTH
 WRITE(DATE(7:8),'(i2.2)')DAY
 WRITE(DATE(9:10),'(i2.2)')HOUR
 CALL NC_WRITE(FN,VN,DATE,Lrec=Lrec)

 END Subroutine OUTNC_TEXT1
!----------------------------------------------------------------------
 Subroutine OUTNC_0D_DOU(FN,VN,Var,Lrec)
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FN,VN
 DOUBLE PRECISION,INTENT(IN) :: Var
 INTEGER,INTENT(IN) :: LREC
 CALL NC_WRITE(FN,VN,Var,Lrec=Lrec)
 END Subroutine OUTNC_0D_DOU
!----------------------------------------------------------------------
 Subroutine OUTNC_1D(FN,VN,Var,LAND,VarDimsName,Vtype,Fact,Units,Lrec)
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FN,VN
 REAL,INTENT(IN) :: Var(:)
 REAL,Optional,INTENT(IN) :: LAND,Fact
 CHARACTER(Len=*),Optional,INTENT(IN) :: VarDimsName
 CHARACTER(Len=*),Optional,INTENT(IN) :: Units
 INTEGER,Optional,INTENT(IN) :: Vtype,LREC

 INTEGER IM,JM,LLREC
 INTEGER VVType
 INTEGER NcID
 CHARACTER(LEN=10) :: DimsName(2)=(/'lon','time'/)
 INTEGER*2,PARAMETER :: LAND_INT2=32767
 DOUBLE PRECISION TT

 IF(PRESENT(VarDimsName))THEN
 DimsName(1)=Trim(VarDimsName)
 END IF

 VVtype=NF_INT2
 IF(PRESENT(Vtype))THEN
 VVtype=Vtype
 END IF

 Status=NF_OPEN(TRIM(FN),NF_WRITE,ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_INQ_VARID(ncID,TRIM(VN),varID)
 IF(Status==-49)THEN
! difine 2d data
! WRITE(6,*) "DEFINE NEW VARIABLES ",TRIM(VN)
 CALL DefVar(FN,VN,VVType,[DimsName])
 IF(PRESENT(LAND))THEN
 IF(VVtype==NF_INT2)THEN
 CALL Set_Att(FN,'missing_value',Land_INT2,VN) 
 ELSE 
 CALL Set_Att(FN,'missing_value',Land,VN) 
 END IF
 END IF
 IF(PRESENT(FACT))THEN
 CALL Set_Att(FN,'scale_factor',Fact,VN)
 END IF
 IF(PRESENT(UNITS))THEN
 CAll Set_Att(FN,'units',Units,VN)
 END IF
 END IF
 STATUS=NF_CLOSE(ncID)
 IF(STATUS.NE.NF_NOERR) CALL HANDLE_ERR(STATUS)


 Status=NF_OPEN(TRIM(FN),NF_WRITE,ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_INQ_VARID(ncID,VN,varID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 STATUS=NF_CLOSE(ncID)
 IF(STATUS.NE.NF_NOERR) CALL HANDLE_ERR(STATUS)

 LLREC=1
 IF(PRESENT(LREC))THEN
 LLrec=Lrec
 END IF
 TT=LLREC*1.
 CALL NC_WRITE(FN,'time',TT,Lrec=LLrec)

 IF(PRESENT(LAND).AND.PRESENT(FACT))THEN
 CALL NC_WRITE(FN,VN,Var,InLand=Land,Fact=Fact,Lrec=LLrec)
 Return
 END IF

 IF(PRESENT(LAND))THEN
 CALL NC_WRITE(FN,VN,Var,InLand=Land,Lrec=LLrec)
 RETURN
 END IF

 IF(PRESENT(Fact))THEN
 CALL NC_WRITE(FN,VN,Var,Fact=Fact,Lrec=LLrec)
 RETURN
 END IF
 CALL NC_WRITE(FN,VN,Var,Lrec=LLrec)
 END Subroutine OUTNC_1D
!----------------------------------------------------------------------
 Subroutine OUTNC_2D(FN,VN,Var,LAND,VarDimsName,Vtype,Fact,Units,Lrec,I1,J1)
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FN,VN
 REAL,INTENT(IN) :: Var(:,:)
 REAL,Optional,INTENT(IN) :: LAND,Fact
 CHARACTER(Len=*),Optional,INTENT(IN) :: VarDimsName(2),Units
 INTEGER,Optional,INTENT(IN) :: Vtype,LREC,I1,J1

 INTEGER IM,JM,LLREC
 INTEGER VVType
 INTEGER NcID
 CHARACTER(LEN=10) :: DimsName(3)=(/'lon','lat','time'/)
 INTEGER*2,PARAMETER :: LAND_INT2=32767
 DOUBLE PRECISION TT


 IF(PRESENT(VarDimsName))THEN
 DimsName(1)=Trim(VarDimsName(1))
 DimsName(2)=Trim(VarDimsName(2))
 END IF

 VVtype=NF_INT2
 IF(PRESENT(Vtype))THEN
 VVtype=Vtype
 END IF

 Status=NF_OPEN(TRIM(FN),NF_WRITE,ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)
 Status=NF_INQ_VARID(ncID,TRIM(VN),varID)
 IF(Status==-49)THEN
! difine 2d data
! WRITE(6,*) "DEFINE NEW VARIABLES ",TRIM(VN)
 CALL DefVar(FN,VN,VVType,DimsName)
 IF(PRESENT(LAND))THEN
 IF(VVtype==NF_INT2)THEN
 CALL Set_Att(FN,'missing_value',Land_INT2,VN) 
 ELSE 
 CALL Set_Att(FN,'missing_value',Land,VN) 
 END IF
 END IF
 IF(PRESENT(FACT))THEN
 CALL Set_Att(FN,'scale_factor',Fact,VN)
 END IF
 IF(PRESENT(UNITS))THEN
 CAll Set_Att(FN,'units',Units,VN)
 END IF
 END IF

 STATUS=NF_CLOSE(ncID)
 IF(STATUS.NE.NF_NOERR) CALL HANDLE_ERR(STATUS)

 Status=NF_OPEN(TRIM(FN),NF_WRITE,ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_INQ_VARID(ncID,VN,varID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 STATUS=NF_CLOSE(ncID)
 IF(STATUS.NE.NF_NOERR) CALL HANDLE_ERR(STATUS)

 LLREC=1
 IF(PRESENT(LREC))THEN
 LLrec=Lrec
 END IF
 TT=LLREC*1.
 CALL NC_WRITE(FN,'time',TT,Lrec=LLrec)

 IF(PRESENT(LAND).AND.PRESENT(FACT))THEN
 IF(PRESENT(I1).AND.PRESENT(J1))THEN
 CALL NC_WRITE(FN,VN,Var,InLand=Land,Fact=Fact,Lrec=LLrec,I1=I1,J1=J1)
 ELSE
 CALL NC_WRITE(FN,VN,Var,InLand=Land,Fact=Fact,Lrec=LLrec)
 END IF
 Return
 END IF

 IF(PRESENT(LAND))THEN
 IF(PRESENT(I1).AND.PRESENT(J1))THEN
 CALL NC_WRITE(FN,VN,Var,InLand=Land,Lrec=LLrec,I1=I1,J1=J1)
 ELSE
 CALL NC_WRITE(FN,VN,Var,InLand=Land,Lrec=LLrec)
 END IF
 RETURN
 END IF

 IF(PRESENT(Fact))THEN
 IF(PRESENT(I1).AND.PRESENT(J1))THEN
 CALL NC_WRITE(FN,VN,Var,Fact=Fact,Lrec=LLrec,I1=I1,J1=J1)
 ELSE
 CALL NC_WRITE(FN,VN,Var,Fact=Fact,Lrec=LLrec)
 END IF
 RETURN
 END IF
 IF(PRESENT(I1).AND.PRESENT(J1))THEN
 CALL NC_WRITE(FN,VN,Var,Lrec=LLrec,I1=I1,J1=J1)
 ELSE
 CALL NC_WRITE(FN,VN,Var,Lrec=LLrec)
 END IF
 END Subroutine OUTNC_2D
!----------------------------------------------------------------------
 Subroutine OUTNC_3D(FN,VN,Var,LAND,VarDimsName,Vtype,Fact,Units,Lrec,I1,J1)
 IMPLICIT NONE
 CHARACTER(LEN=*),INTENT(IN) :: FN,VN
 REAL,INTENT(IN) :: Var(:,:,:)
 REAL,Optional,INTENT(IN) :: LAND,Fact
 CHARACTER(Len=*),Optional,INTENT(IN) :: VarDimsName(3),Units
 INTEGER,Optional,INTENT(IN) :: Vtype,LREC,I1,J1

 INTEGER IM,JM,LLREC
 INTEGER VVType
 INTEGER NcID
 CHARACTER(LEN=10) :: DimsName(4)=(/'lon','lat','dep','time'/)
 INTEGER*2,PARAMETER :: LAND_INT2=32767
 DOUBLE PRECISION TT


 IF(PRESENT(VarDimsName))THEN
 DimsName(1)=Trim(VarDimsName(1))
 DimsName(2)=Trim(VarDimsName(2))
 DimsName(3)=Trim(VarDimsName(3))
 END IF

 VVtype=NF_INT2
 IF(PRESENT(Vtype))THEN
 VVtype=Vtype
 END IF

 Status=NF_OPEN(TRIM(FN),NF_WRITE,ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_INQ_VARID(ncID,TRIM(VN),varID)
 IF(Status==-49)THEN
! difine 3d data
! WRITE(6,*) "DEFINE NEW VARIABLES ",TRIM(VN)
 CALL DefVar(FN,VN,VVType,DimsName)
 IF(PRESENT(LAND))THEN
 IF(VVtype==NF_INT2)THEN
 CALL Set_Att(FN,'missing_value',Land_INT2,VN) 
 ELSE 
 CALL Set_Att(FN,'missing_value',Land,VN) 
 END IF
 END IF
 IF(PRESENT(FACT))THEN
 CALL Set_Att(FN,'scale_factor',Fact,VN)
 END IF
 IF(PRESENT(UNITS))THEN
 CAll Set_Att(FN,'units',Units,VN)
 END IF
 END IF

 STATUS=NF_CLOSE(ncID)
 IF(STATUS.NE.NF_NOERR) CALL HANDLE_ERR(STATUS)

 Status=NF_OPEN(TRIM(FN),NF_WRITE,ncID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 Status=NF_INQ_VARID(ncID,VN,varID)
 IF(Status.NE.NF_NOERR) CALL HANDLE_ERR(Status)

 STATUS=NF_CLOSE(ncID)
 IF(STATUS.NE.NF_NOERR) CALL HANDLE_ERR(STATUS)

 LLREC=1
 IF(PRESENT(LREC))THEN
 LLrec=Lrec
 END IF

 TT=LLREC*1.
 CALL NC_WRITE(FN,'time',TT,Lrec=LLrec)

 IF(PRESENT(LAND).AND.PRESENT(FACT))THEN
 IF(PRESENT(I1).AND.PRESENT(J1))THEN
 CALL NC_WRITE(FN,VN,Var,InLand=Land,Fact=Fact,Lrec=LLrec,I1=I1,J1=J1)
 ELSE
 CALL NC_WRITE(FN,VN,Var,InLand=Land,Fact=Fact,Lrec=LLrec)
 END IF
 Return
 END IF

 IF(PRESENT(LAND))THEN
 IF(PRESENT(I1).AND.PRESENT(J1))THEN
 CALL NC_WRITE(FN,VN,Var,InLand=Land,Lrec=LLrec,I1=I1,J1=J1)
 ELSE
 CALL NC_WRITE(FN,VN,Var,InLand=Land,Lrec=LLrec)
 END IF
 RETURN
 END IF

 IF(PRESENT(Fact))THEN
 IF(PRESENT(I1).AND.PRESENT(J1))THEN
 CALL NC_WRITE(FN,VN,Var,Fact=Fact,Lrec=LLrec,I1=I1,J1=J1)
 ELSE
 CALL NC_WRITE(FN,VN,Var,Fact=Fact,Lrec=LLrec)
 END IF
 RETURN
 END IF
 IF(PRESENT(I1).AND.PRESENT(J1))THEN
 CALL NC_WRITE(FN,VN,Var,Lrec=LLrec,I1=I1,J1=J1)
 ELSE
 CALL NC_WRITE(FN,VN,Var,Lrec=LLrec)
 END IF
 END Subroutine OUTNC_3D
!----------------------------------------------------------------------
 SUBROUTINE ISLOCATE(IERR)
 INTEGER IERR
 IF(IERR>0)THEN
   WRITE(6,*) "Allocation error!"
   stop
 END IF
 END SUBROUTINE ISLOCATE
!--------------------------------------------------------------------
 END Module NcFileMod
!----------------------------------------------------------------------
