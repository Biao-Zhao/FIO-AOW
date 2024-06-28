MODULE MOD_INP_WAVE
  USE MOD_UTILS_WAVE
  CONTAINS

!==============================================================================!
!  DECOMPOSE INPUT LINE INTO VARIABLE NAME AND VARIABLE VALUE(S)               !
!==============================================================================!

SUBROUTINE GET_VAL(LNUM,NUMCHAR,TEXT_LINE,VARNAME,VARTYPE,LOGVAL,STRINGVAL,&
                    REALVAL,INTVAL,NVAL)

!==============================================================================!
  USE data_kind_mod_wave 
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: LNUM,NUMCHAR
  CHARACTER(LEN=NUMCHAR) :: TEXT_LINE
  CHARACTER(LEN=20), INTENT(OUT) :: VARNAME
  CHARACTER(LEN=7), INTENT(OUT) :: VARTYPE
  LOGICAL, INTENT(OUT) :: LOGVAL
  CHARACTER(LEN=80), INTENT(OUT) :: STRINGVAL(150)
  REAL(kind_r4), INTENT(INOUT) :: REALVAL(150)
  INTEGER, INTENT(INOUT) :: INTVAL(150)
  INTEGER, INTENT(OUT) :: NVAL
!------------------------------------------------------------------------------!
  CHARACTER(LEN=NUMCHAR) :: VARVAL,TEMP,FRAG(200)
  CHARACTER(LEN=80) :: TTSTRING
  CHARACTER(LEN=NUMCHAR) :: TSTRING
  CHARACTER(LEN=6) :: ERRSTRING
  CHARACTER(LEN=16) :: NUMCHARS 
  CHARACTER(LEN=500) :: ERRSTR
  INTEGER LENGTH,EQLOC,LVARVAL,DOTLOC
  INTEGER I,J,LOCEX,NP
  LOGICAL ONFRAG

!==============================================================================!
  FRAG = " "
  NUMCHARS = "0123456789+-Ee. " 
  VARTYPE = "error"
  LOGVAL = .FALSE.
  LENGTH = LEN_TRIM(TEXT_LINE) 
  WRITE(ERRSTRING,"(I6)") LNUM
  LOCEX = INDEX(TEXT_LINE,"!")

!
!-----------------------CHECK FOR BLANK LINE OR COMMENT------------------------!
!
  IF(LENGTH == 0 .OR. LOCEX==1)THEN
    VARTYPE = "no data"
    VARNAME = "no data"
    RETURN
  END IF

!
!-----------------------CHANGE COMMAS TO BLANKS--------------------------------!
!
  DO I=1,LENGTH
    IF(TEXT_LINE(I:I) == ",") TEXT_LINE(I:I) = " "
  END DO
!
!-----------------------REMOVING TRAILING COMMENTS-----------------------------!
!
  IF(LOCEX /= 0)THEN
    TEMP = TEXT_LINE(1:LOCEX-1)
    TEXT_LINE = TEMP
   END IF
!
!--------------------ENSURE "=" EXISTS AND DETERMINE LOCATION------------------!
!
   EQLOC = INDEX(TEXT_LINE,"=")
   IF(EQLOC == 0)THEN
   ERRSTR='DATA LINE '//ERRSTRING//' MUST CONTAIN "=" '
   CALL PERROR(6,TRIM(ERRSTR))
   END IF

!
!--------------------SPLIT OFF VARNAME AND VARVAL STRINGS----------------------!
!
   VARNAME = TEXT_LINE(1:EQLOC-1)
   VARVAL  = ADJUSTL(TEXT_LINE(EQLOC+1:LENGTH))
   LVARVAL = LEN_TRIM(VARVAL)

   IF(LVARVAL == 0)THEN
    ERRSTR='VARIABLE LINE'//ERRSTRING//' HAS NO ASSOCIATED VALUE'
    CALL PERROR(6,'IN DATA PARAMETER FILE',TRIM(ERRSTR)) 
   END IF
!
!-----------------DETERMINE TYPE OF VARVAL-------------------------------------!
!

!
!  CHECK FOR LOGICAL
!
   IF((VARVAL(1:1) == "T" .OR. VARVAL(1:1) == "F") .AND. LVARVAL == 1)THEN 
     VARTYPE = "logical"
     IF(VARVAL(1:1) == "T") LOGVAL = .TRUE.
     RETURN
   END IF

!
!  CHECK IF IT IS A STRING  (CONTAINS CHARACTERS OTHER THAN 0-9,+,-,e,E,.)
!
   DO I=1,LVARVAL
     IF(INDEX(NUMCHARS,VARVAL(I:I)) == 0) VARTYPE = "string" 
   END DO

!
!  PROCESS STRING (MAY BE MULTIPLE)

!   IF(VARTYPE == "string") THEN
!     TSTRING = VARVAL
!     STRINGVAL(1) = TSTRING 
!     NVAL = 1
!     ONFRAG = .TRUE.
!     DO I=1,LVARVAL
!       IF(VARVAL(I:I) /= " ")THEN
!         FRAG(NVAL) = TRIM(FRAG(NVAL))//VARVAL(I:I)
!         ONFRAG = .TRUE.
!       ELSE
!         IF(ONFRAG) NVAL = NVAL + 1
!         ONFRAG = .FALSE.
!       END IF
!     END DO
!     DO I=1,NVAL
!       STRINGVAL(I+1) = TRIM(FRAG(I))
!     END DO
!     RETURN
!   END IF
   IF(VARTYPE == "string")THEN

   TSTRING = VARVAL
   NVAL=1
   ONFRAG = .TRUE. 
   STRINGVAL(NVAL) = TSTRING 
   NVAL=2
   STRINGVAL(NVAL) = TSTRING 

   DO I=1,LVARVAL
      IF(VARVAL(I:I) /= " ")THEN
       TSTRING(I:I) = " " 
       ONFRAG = .TRUE.
      ELSE
       IF(ONFRAG)THEN
        NVAL=NVAL+1
        ONFRAG = .FALSE.
        STRINGVAL(NVAL)=ADJUSTL(TSTRING)
       END IF
      END IF
   END DO
   DO NP=2,NVAL
    LVARVAL=LEN_TRIM(STRINGVAL(NP))
    TTSTRING=STRINGVAL(NP) 
    DO I=1,LVARVAL
    IF(TTSTRING(I:I) == " ")THEN
    STRINGVAL(NP)=""
    STRINGVAL(NP)=TTSTRING(1:I-1)
    EXIT
    END IF
    END DO
   END DO
   RETURN
   END IF

!! Verified by Guanso. Wang
!! End Verified

!
!  CHECK IF IT IS A FLOAT
!

   DOTLOC = INDEX(VARVAL,".")
   IF(DOTLOC /= 0) THEN
     VARTYPE = "float"
   ELSE
     VARTYPE = "integer"
   END IF
!
!-----------------FRAGMENT INTO STRINGS FOR MULTIPLE VALUES---------------------!
!
!!   NP = 1
!!   ONFRAG = .TRUE.
!!   DO I=1,LVARVAL
!!     IF(VARVAL(I:I) /= " ")THEN 
!!      FRAG(NP) = TRIM(FRAG(NP))//VARVAL(I:I)
!!       ONFRAG = .TRUE.
!!     ELSE
!!       IF(ONFRAG) NP = NP + 1
!!       ONFRAG = .FALSE.
!!     END IF
!!   END DO
!!  VERIFY BY GUANSO. WANG
    NP = 1
    FRAG(NP)=VARVAL
    ONFRAG = .TRUE.
    DO I=1,LVARVAL
    IF(VARVAL(I:I) /= " ")THEN
    VARVAL(I:I)=" "
    ONFRAG = .TRUE.
    ELSE
     IF(ONFRAG)THEN
     NP=NP+1
     ONFRAG = .FALSE.
     FRAG(NP)=ADJUSTL(VARVAL)
     END IF
    END IF
    END DO
!!  END VERIFIED 
!    NP=NP-1
!
!-----------------EXTRACT NUMBER(S) FROM CHARACTER STRINGS----------------------!
!
   NVAL = NP
   DO I=1,NP
     TEMP = TRIM(FRAG(I))
     IF(VARTYPE == "float") THEN 
       READ(TEMP,*)REALVAL(I)
     ELSE
       READ(TEMP,*)INTVAL(I)
     END IF
   END DO

END SUBROUTINE GET_VAL 


!==============================================================================|

    FUNCTION SCAN_FILE(FNAME,VNAME,ISCAL,FSCAL,IVEC,FVEC,CVEC,NSZE,CVAL,LVAL)           

!==============================================================================|
!   Scan an Input File for a Variable                                          |
!   RETURN VALUE:                                                              |
!        0 = FILE FOUND, VARIABLE VALUE FOUND                                  |
!       -1 = FILE DOES NOT EXIST OR PERMISSIONS ARE INCORRECT                  |
!       -2 = VARIABLE NOT FOUND OR IMPROPERLY SET                              |
!       -3 = VARIABLE IS OF DIFFERENT TYPE, CHECK INPUT FILE                   |
!       -4 = VECTOR PROVIDED BUT DATA IS SCALAR TYPE                           |
!       -5 = NO DATATYPE DESIRED, EXITING                                      |
!							                       |
!   REQUIRED INPUT:		        				       |
!        FNAME = File Name					               |
!        FSIZE = Length of Filename					       |
!                                                                              | 
!   OPTIONAL (MUST PROVIDE ONE)        					       | 
!        ISCAL = INTEGER SCALAR					               |
!        FSCAL = FLOAT SCALAR  						       | 
!        CVAL = CHARACTER VARIABLE                                             |
!        LVAL = LOGICAL VARIABLE                                               |
!        IVEC = INTEGER VECTOR **                                              |
!        FVEC = FLOAT VECTOR **                                                |
!        CVEC = STRING VECTOR ** (STRINGS OF LENGTH 80)                        |
!      **NSZE = ARRAY SIZE (MUST BE PROVIDED WITH IVEC/FVEC)                   |
!                                                                              | 
!==============================================================================|

   USE data_kind_mod_wave 
   use parallel_mod_wave
   IMPLICIT NONE
   CHARACTER(LEN=*) :: FNAME,VNAME
   INTEGER, INTENT(INOUT), OPTIONAL :: ISCAL,IVEC(*)
   REAL(kind_r4),INTENT(INOUT), OPTIONAL :: FSCAL,FVEC(*)
   CHARACTER(LEN=80), OPTIONAL      :: CVAL,CVEC(*)
   LOGICAL, INTENT(INOUT), OPTIONAL :: LVAL
   INTEGER, INTENT(INOUT), OPTIONAL :: NSZE 
   
!------------------------------------------------------------------------------|

   INTEGER :: SCAN_FILE
   REAL(kind_r4) REALVAL(150)
   INTEGER  INTVAL(150)
   CHARACTER(LEN=20 ) :: VARNAME
   CHARACTER(LEN=80 ) :: STRINGVAL(150),TITLE
   CHARACTER(LEN=80 ) :: INPLINE
   CHARACTER(LEN=400) :: TLINE
   CHARACTER(LEN=7  ) :: VARTYPE
   CHARACTER(LEN=20 ), DIMENSION(200)  :: SET
   INTEGER I,NVAL,J,NSET,NLINE,NREP
   LOGICAL SETYES,ALLSET,CHECK,LOGVAL


   SCAN_FILE = 0
!==============================================================================|
!            OPEN THE INPUT FILE                                               |
!==============================================================================|
   INQUIRE(FILE=TRIM(FNAME),EXIST=CHECK)
   IF(.NOT.CHECK)THEN
     SCAN_FILE = -1
     RETURN
   END IF

   OPEN(10,FILE=TRIM(FNAME)) ; REWIND(10) 

!==============================================================================|
!            SCAN THE FILE FOR THE VARIABLE NAME                               |
!==============================================================================|

   NSET = 0
   NLINE = 0
   DO WHILE(.TRUE.)
     TLINE(1:LEN(TLINE)) = ' ' 
     NREP  = 0
     NLINE = NLINE + 1
     READ(10,'(a)',END=20) INPLINE
     TLINE(1:80) = INPLINE(1:80)

!----PROCESS LINE CONTINUATIONS------------------------------------------------!
 110 CONTINUE
     I = LEN_TRIM(INPLINE)
     IF(I /= 0)THEN
!#    if defined (COMPAQ) || defined (INTEL) || defined (IRIX)
     IF( INPLINE(I-1:I) == '\\')THEN
!#    else
!     IF( INPLINE(I-1:I) == '\\\\')THEN
!#    endif
       NREP = NREP + 1
       READ(10,'(a)',END=20) INPLINE
       NLINE = NLINE + 1
       TLINE( NREP*80 + 1 : NREP*80 +80) = INPLINE(1:80)
       GOTO 110
     END IF
     END IF
     IF(NREP > 4)CALL PERROR(6,"CANNOT HAVE > 4 LINE CONTINUATIONS")

!----REMOVE LINE CONTINUATION CHARACTER \\-------------------------------------!
     IF(NREP > 0)THEN
       DO I=2,LEN_TRIM(TLINE)
!#    if defined (COMPAQ) || defined (INTEL) || defined (IRIX)
         IF( TLINE(I-1:I) == '\\') TLINE(I-1:I) = '  '
!#        else
!         IF( TLINE(I-1:I) == '\\\\') TLINE(I-1:I) = '  '
!#        endif
       END DO
     END IF
       
!----PROCESS THE LINE----------------------------------------------------------!
     CALL GET_VAL(NLINE,LEN_TRIM(TLINE),ADJUSTL(TLINE),VARNAME,VARTYPE,LOGVAL,&
                 STRINGVAL,REALVAL,INTVAL,NVAL)
!----IF VARNAME MATCHES, PROCESS VARIABLE AND ERROR-CHECK----------------------!

     IF(TRIM(VARNAME) == TRIM(VNAME))THEN

       IF(PRESENT(ISCAL))THEN
         IF(VARTYPE == 'integer')THEN
           ISCAL = INTVAL(1)
           RETURN
         ELSE
           SCAN_FILE = -3
         END IF
       ELSE IF(PRESENT(FSCAL))THEN
         IF(VARTYPE == 'float')THEN
           FSCAL = REALVAL(1)
           RETURN
         ELSE
           SCAN_FILE = -3
         END IF
       ELSE IF(PRESENT(CVAL))THEN
         IF(VARTYPE == 'string')THEN
           CVAL = STRINGVAL(1) 
           RETURN
         ELSE
           SCAN_FILE = -3
         END IF
       ELSE IF(PRESENT(LVAL))THEN
         IF(VARTYPE == 'logical')THEN
           LVAL = LOGVAL 
           RETURN
         ELSE
           SCAN_FILE = -3
         END IF
       ELSE IF(PRESENT(IVEC))THEN
         IF(NVAL > 1)THEN
           IF(VARTYPE == 'integer')THEN
             IVEC(1:NVAL) = INTVAL(1:NVAL) 
             NSZE = NVAL         
             RETURN
           ELSE
             SCAN_FILE = -3
           END IF
           ELSE
           SCAN_FILE = -4 
         END IF
       ELSE IF(PRESENT(FVEC))THEN
         IF(NVAL > 1)THEN
           IF(VARTYPE == 'float')THEN
             FVEC(1:NVAL) = REALVAL(1:NVAL) 
             NSZE = NVAL           
             RETURN
           ELSE
             SCAN_FILE = -3
           END IF
         ELSE
           SCAN_FILE = -4 
         END IF
       ELSE IF(PRESENT(CVEC))THEN
         IF(NVAL > 0)THEN
           IF(VARTYPE == 'string')THEN
             CVEC(1:NVAL-1) = STRINGVAL(2:NVAL)
             NSZE = NVAL-1 
             RETURN
           ELSE
             SCAN_FILE = -3
           END IF
         ELSE
           SCAN_FILE = -4
         END IF
       ELSE
         SCAN_FILE = -5
       END IF
     END IF  !!VARIABLE IS CORRECT
            
   END DO !!LOOP OVER INPUT FILE
 20 CLOSE(10) 
   SCAN_FILE = -2

   RETURN 
   END FUNCTION SCAN_FILE
!==============================================================================|
END MODULE MOD_INP_WAVE
