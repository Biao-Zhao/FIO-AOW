!=======================================================================
!CVS $Id: parallel_mod_wave.F90,v 1.1 2013/06/20 08:49:59 wgs Exp $
!CVS $Source: /soa04/users/wgs/.mycvsroot/intercomm/wave/parallel_mod_wave.F90,v $
!CVS $Name:  $
!=======================================================================
MODULE parallel_mod_wave
   use data_kind_mod_wave
   USE LIMS_WAVE, ONLY : NUMXCPU,NUMYCPU,MYID,NPROCS
   USE CONTROL_WAVE, ONLY : ISLON,IELON,ISLAT,IELAT,  &
                       IUP,IDOWN,LHALO,RHALO,NEIGHBOR,&
                       CORNER_SEND,NUM_CORNER_SEND,   & 
                       CORNER_REC,NEIGHBOR_LON,       &
                       NEIGHBOR_LAT
   USE CONST_WAVE,ONLY : KL,JL,LE
   implicit none
   include 'mpif.h'
!-----------------------------------------------------------------------
!
!     module variables
!
!-----------------------------------------------------------------------
   PUBLIC PAR_INIT,PAR_END,BARRIER,MYPE,NPES,COMM_DATA,SET_MPI_ENV,    &
          GATHER_INT,BCAST_INT,COMPTEST,EXTREMUM, COMM_DATA_2D

   integer, public :: masnum_mpi_comm                ! the comm group of pom

   PRIVATE
!***********************************************************************   
   INTERFACE EXTREMUM 
   Module Procedure EXTREM_0D,EXTREM_1D,EXTREM_2D,EXTREM_3D,EXTREM_4D
   END INTERFACE EXTREMUM 
!***********************************************************************   
   INTEGER(kind_in),PRIVATE :: IERR
   INTEGER STATUS(MPI_STATUS_SIZE)
!***********************************************************************   
   CONTAINS
!***********************************************************************
!  Support routines for machines that support MPI
!  Initialise Parallel environment
   SUBROUTINE PAR_INIT
   IMPLICIT NONE
   if (masnum_mpi_comm .eq. -1) then
      CALL MPI_INIT(IERR)
      masnum_mpi_comm = MPI_COMM_WORLD 
   end if
   RETURN
   END SUBROUTINE

!************************************************************************
!  Shut down Parallel environment
   SUBROUTINE PAR_END
   use CCPL_interface_mod                       !zhaobiao, c-coupler2
   IMPLICIT NONE
   !CALL MPI_FINALIZE(IERR)
   !CALL c_coupler_finalize()
   CALL CCPL_finalize(.true.,annotation='CCPL_finalize')   !zhaobiao, c-coupler2  
   RETURN
   END SUBROUTINE
!************************************************************************
!
!  These two functions are much used in the code
!
   INTEGER(kind_in) FUNCTION MYPE()
   IMPLICIT NONE
   INTEGER(kind_in) TMP
   CALL MPI_COMM_RANK(masnum_mpi_comm,TMP,IERR)
   MYPE = TMP
   RETURN
   END FUNCTION
!************************************************************************
   INTEGER(kind_in) FUNCTION NPES()
   IMPLICIT NONE
   INTEGER(kind_in) TMP
   CALL MPI_COMM_SIZE(masnum_mpi_comm,TMP,IERR)
   NPES = TMP
   RETURN
   END FUNCTION
!************************************************************************
   SUBROUTINE BARRIER()
   IMPLICIT NONE
   CALL MPI_BARRIER(masnum_mpi_comm,IERR)
   RETURN
   END SUBROUTINE
!************************************************************************
   SUBROUTINE SET_MPI_ENV(MYID,NPROCS,SERIAL,PAR,MSR)
   IMPLICIT NONE
   INTEGER, INTENT(OUT) :: MYID,NPROCS
   LOGICAL, INTENT(OUT) :: SERIAL,PAR,MSR

   CALL PAR_INIT
   MYID=MYPE()
   NPROCS=NPES()

   IF(kind_r4==4)MPI_F=MPI_REAL4
   IF(kind_r4==8)MPI_F=MPI_REAL8

   IF(kind_i4==4)MPI_I=MPI_INTEGER4
   IF(kind_i4==8)MPI_I=MPI_INTEGER8

   IF(NPROCS > 1) SERIAL=.FALSE.
   IF(NPROCS > 1) PAR   =.TRUE.
   IF(MYID /=  0) MSR   =.FALSE.
   RETURN  
   END SUBROUTINE
!************************************************************************
   SUBROUTINE GATHER_INT(SBUFFER,RBUFFER)
   USE LIMS_WAVE, ONLY : MYID
   IMPLICIT NONE
   INTEGER,DIMENSION(:) :: SBUFFER,RBUFFER
   INTEGER ROOT,COUNT
   INTEGER DIM2(1)
   DIM2=SHAPE(SBUFFER)
   COUNT=DIM2(1)
   ROOT=0
   CALL MPI_GATHER(SBUFFER,COUNT,MPI_INTEGER,RBUFFER,COUNT,   &
                   MPI_INTEGER,ROOT,masnum_mpi_comm,IERR)
   END SUBROUTINE
!************************************************************************
   SUBROUTINE BCAST_INT(BUFF)
   IMPLICIT NONE
   INTEGER BUFF(:,:)
   INTEGER COUNT,ROOT,DIM2(2)

   DIM2=SHAPE(BUFF)
   COUNT = DIM2(1)*DIM2(2) 
   ROOT = 0 
   CALL MPI_BCAST(BUFF,COUNT,MPI_INTEGER,ROOT,masnum_mpi_comm,IERR)
   END SUBROUTINE
!************************************************************************
   SUBROUTINE COMM_DATA(ARR)  
   IMPLICIT NONE

!   REAL(kind_r4),DIMENSION(:,:,:,:) :: ARR
   REAL(kind_r4),DIMENSION(KL,JL,ISLON-LE:IELON+LE,ISLAT-LE:IELAT+LE) :: ARR
   LOGICAL     :: LEXIST 

   IF(NUMYCPU>1)CALL COMM_UP_BOTTOM(ARR)
   CALL COMM_LEFT_RIGHT(ARR)
   IF(NUMYCPU>1)CALL COMM_CORNER(ARR)   
!   IF(NUMXCPU>1.AND.NUMYCPU>1)CALL UPDATE_4D_BOTTOMLEFT_UPRIGHT_SGI(ARR,ISLAT-1)
!   IF(NUMXCPU>1.AND.NUMYCPU>1)CALL UPDATE_4D_BOTTOMLEFT_UPRIGHT_SGI(ARR,IELAT+1)
   RETURN
   END SUBROUTINE COMM_DATA
!************************************************************************
   SUBROUTINE COMM_UP_BOTTOM(ARR)
   IMPLICIT NONE
   REAL(kind_r4),DIMENSION(KL,JL,ISLON-LE:IELON+LE,ISLAT-LE:IELAT+LE) :: ARR
   INTEGER :: K,ISIZE
   integer :: srqest(4)

!  FROM BOTTOM TO UP EXCHANGE DATA
   ISIZE=KL*JL*(IELON-ISLON+1)

   CALL MPI_ISEND(arr(1,1,islon,ielat),isize,MPI_F,IUP,MYID,     &
                  masnum_mpi_comm,srqest(1),ierr)
   CALL MPI_ISEND(arr(1,1,islon,islat),isize,MPI_F,IDOWN,MYID,   &
                  masnum_mpi_comm,srqest(2),ierr)
   CALL MPI_IRECV(arr(1,1,islon,ielat+1),isize,MPI_F,IUP,IUP,   &
                  masnum_mpi_comm,srqest(3),ierr)
   CALL MPI_IRECV(arr(1,1,islon,islat-1),isize,MPI_F,IDOWN,IDOWN, &
                  masnum_mpi_comm,srqest(4),ierr)
   DO K = 1,4
     CALL MPI_WAIT(srqest(K), status, ierr)
   ENDDO
   if (.false.) then
   CALL MPI_SENDRECV(arr(1,1,islon,ielat),isize,MPI_F,IUP,1000,     &
                     arr(1,1,islon,islat-1),isize,MPI_F,IDOWN,1000, &
                      masnum_mpi_comm,status,ierr)
!  FROM UP TO BOTTOM EXCHANGE DATA
   CALL MPI_SENDRECV(arr(1,1,islon,islat),isize,MPI_F,IDOWN,1000,   &
                     arr(1,1,islon,ielat+1),isize,MPI_F,IUP,1000,   &
                     masnum_mpi_comm,status,ierr)
   endif
   END SUBROUTINE COMM_UP_BOTTOM
!************************************************************************
   SUBROUTINE COMM_LEFT_RIGHT(ARR)
   IMPLICIT NONE
   REAL(kind_r4),DIMENSION(KL,JL,ISLON-LE:IELON+LE,ISLAT-LE:IELAT+LE) :: ARR
   REAL(kind_r4),DIMENSION(KL,JL,ISLAT:IELAT)  :: SEND_BUFFER_LEFT, SEND_BUFFER_RIGHT
   REAL(kind_r4),DIMENSION(KL,JL,ISLAT:IELAT)  :: RECV_BUFFER_LEFT, RECV_BUFFER_RIGHT
   INTEGER K,I,J,ISIZE,NROW,NUM_REQUEST,REQUESTS((IELAT-ISLAT+1)*4)
   LOGICAL TRANSFER_DATA
   LOGICAL LEXIST

   ISIZE=KL*JL
   DO K=ISLAT,IELAT
     SEND_BUFFER_RIGHT(:,:,K) = ARR(:,:,IELON,K)
     SEND_BUFFER_LEFT(:,:,K)  = ARR(:,:,ISLON,K)
   ENDDO
  
   NUM_REQUEST = 0
   NROW = 0
   DO K=ISLAT,IELAT
     NROW = NROW+1
     TRANSFER_DATA = .false.
     IF (K.EQ.IELAT) TRANSFER_DATA = .true.
     IF (K < IELAT) THEN
       IF (RHALO(K).NE.RHALO(K+1)) TRANSFER_DATA = .true.
     ENDIF
     IF (TRANSFER_DATA) THEN
       NUM_REQUEST = NUM_REQUEST+1 
       CALL MPI_ISEND(SEND_BUFFER_RIGHT(:,:,K-NROW+1:K),ISIZE*NROW,MPI_F,RHALO(K),4000000+K, &
                      masnum_mpi_comm,REQUESTS(NUM_REQUEST),IERR)
       NUM_REQUEST = NUM_REQUEST+1 
       CALL MPI_IRECV(RECV_BUFFER_RIGHT(:,:,K-NROW+1:K),ISIZE*NROW,MPI_F,RHALO(K),2000000+K, &
                      masnum_mpi_comm,REQUESTS(NUM_REQUEST),IERR)
       NROW = 0
     ENDIF
   ENDDO

   NROW = 0
   DO K=ISLAT,IELAT
     NROW = NROW+1
     TRANSFER_DATA = .false.
     IF (K.EQ.IELAT) TRANSFER_DATA = .true.
     IF (K < IELAT) THEN
       IF (LHALO(K).NE.LHALO(K+1)) TRANSFER_DATA = .true.
     ENDIF
     IF (TRANSFER_DATA) THEN
       NUM_REQUEST = NUM_REQUEST+1 
       CALL MPI_ISEND(SEND_BUFFER_LEFT(:,:,K-NROW+1:K),ISIZE*NROW,MPI_F,LHALO(K),2000000+K,  &
                      masnum_mpi_comm,REQUESTS(NUM_REQUEST),IERR)
       NUM_REQUEST = NUM_REQUEST+1 
       CALL MPI_IRECV(RECV_BUFFER_LEFT(:,:,K-NROW+1:K),ISIZE*NROW,MPI_F,LHALO(K),4000000+K,  &
                      masnum_mpi_comm,REQUESTS(NUM_REQUEST),IERR)
       NROW = 0
     ENDIF
   ENDDO

   DO K = 1, NUM_REQUEST
     CALL MPI_WAIT(REQUESTS(K), status, ierr)
   ENDDO

!  from left to right exchange data
   if (.false.) then
   DO K=ISLAT,IELAT 
   CALL MPI_SENDRECV(ARR(1,1,IELON,K),ISIZE,MPI_F,RHALO(K),1000+K,                  &
                     ARR(1,1,ISLON-1,K),ISIZE,MPI_F,LHALO(K),1000+K,                &
                     masnum_mpi_comm,STATUS,IERR)
   END DO
!  from right to left exchange data
   DO K=ISLAT,IELAT
   CALL MPI_SENDRECV(ARR(1,1,ISLON,K),ISIZE,MPI_F,LHALO(K),1000+K,                  &
                     ARR(1,1,IELON+1,K),ISIZE,MPI_F,RHALO(K),1000+K,                &
                     masnum_mpi_comm,STATUS,IERR)
   END DO

   DO K=ISLAT,IELAT 
   DO I=1,KL
   DO J=1,JL
     if (ARR(I,J,ISLON-1,K) .ne. RECV_BUFFER_LEFT(I,J,K)) write(*,*) 'ee1', K, ARR(I,J,ISLON-1,K), RECV_BUFFER_LEFT(I,J,K)
     if (ARR(I,J,IELON+1,K) .ne. RECV_BUFFER_RIGHT(I,J,K)) write(*,*) 'ee2', K, ARR(I,J,IELON+1,K), RECV_BUFFER_RIGHT(I,J,K)
   ENDDO
   ENDDO
   ENDDO

   endif

   DO K=ISLAT,IELAT 
     ARR(:,:,ISLON-1,K) = RECV_BUFFER_LEFT(:,:,K)
     ARR(:,:,IELON+1,K) = RECV_BUFFER_RIGHT(:,:,K)
   ENDDO

   END SUBROUTINE COMM_LEFT_RIGHT
!************************************************************************
   SUBROUTINE COMM_CORNER(ARR)
   IMPLICIT NONE
   REAL(kind_r4),DIMENSION(KL,JL,ISLON-LE:IELON+LE,ISLAT-LE:IELAT+LE) :: ARR
   INTEGER ISIZE,K,SR_QUEST(16),NUM_QUESTS

   ISIZE=KL*JL

!   DO K=1,2
!   WRITE(6,*) "REC",MYID,K,MYID+NEIGHBOR_LON(K)+NEIGHBOR_LAT(K),NEIGHBOR(K)
!   CALL MPI_RECV(ARR(1,1,CORNER_REC(1,K),CORNER_REC(2,K)),ISIZE,MPI_F,  &
!        NEIGHBOR(K),MYID+NEIGHBOR_LON(K)+NEIGHBOR_LAT(K),masnum_mpi_comm,  &
!        STATUS,IERR)
!   END DO

   NUM_QUESTS = 0
   DO K=1,NUM_CORNER_SEND
     NUM_QUESTS = NUM_QUESTS+1
     CALL MPI_ISEND(ARR(1,1,CORNER_SEND(1,K),CORNER_SEND(2,K)),ISIZE,MPI_F, &
          CORNER_SEND(3,K),CORNER_SEND(3,K)+CORNER_SEND(1,K)+CORNER_SEND(2,K),&
          masnum_mpi_comm,SR_QUEST(NUM_QUESTS),IERR)
   END DO

   DO K=1,4
     NUM_QUESTS = NUM_QUESTS+1
     CALL MPI_IRECV(ARR(1,1,CORNER_REC(1,K),CORNER_REC(2,K)),ISIZE,MPI_F,  &
          NEIGHBOR(K),MYID+NEIGHBOR_LON(K)+NEIGHBOR_LAT(K),masnum_mpi_comm,   &
          SR_QUEST(NUM_QUESTS),IERR)
   END DO

   DO K=1,NUM_QUESTS
     CALL MPI_WAIT(SR_QUEST(K), status, ierr)
   ENDDO

   if (.false.) then
   DO K=1,NUM_CORNER_SEND
   CALL MPI_SEND(ARR(1,1,CORNER_SEND(1,K),CORNER_SEND(2,K)),ISIZE,MPI_F, &
        CORNER_SEND(3,K),CORNER_SEND(3,K)+CORNER_SEND(1,K)+CORNER_SEND(2,K),&
        masnum_mpi_comm,IERR)
   END DO

   DO K=1,4
   CALL MPI_RECV(ARR(1,1,CORNER_REC(1,K),CORNER_REC(2,K)),ISIZE,MPI_F,  &
        NEIGHBOR(K),MYID+NEIGHBOR_LON(K)+NEIGHBOR_LAT(K),masnum_mpi_comm,   &
        STATUS,IERR)
   END DO

   endif
  
   END SUBROUTINE COMM_CORNER
!************************************************************************
!************************************************************************
   SUBROUTINE COMM_DATA_2D(ARR)
   IMPLICIT NONE

   REAL(kind_r4),DIMENSION(ISLON-LE:IELON+LE,ISLAT-LE:IELAT+LE) :: ARR

   IF(NUMYCPU>1)CALL COMM_UP_BOTTOM_2D(ARR)
   CALL COMM_LEFT_RIGHT_2D(ARR)
   IF(NUMYCPU>1)CALL COMM_CORNER_2D(ARR)
!   IF(NUMXCPU>1.AND.NUMYCPU>1)CALL
!   UPDATE_4D_BOTTOMLEFT_UPRIGHT_SGI(ARR,ISLAT-1)
!   IF(NUMXCPU>1.AND.NUMYCPU>1)CALL
!   UPDATE_4D_BOTTOMLEFT_UPRIGHT_SGI(ARR,IELAT+1)
   RETURN
   END SUBROUTINE COMM_DATA_2D
!************************************************************************
   SUBROUTINE COMM_UP_BOTTOM_2D(ARR)
   IMPLICIT NONE
   REAL(kind_r4),DIMENSION(ISLON-LE:IELON+LE,ISLAT-LE:IELAT+LE) :: ARR

   INTEGER :: ISIZE

!  FROM BOTTOM TO UP EXCHANGE DATA
   ISIZE=IELON-ISLON+1
   CALL MPI_SENDRECV(arr(islon,ielat),isize,MPI_REAL,IUP,10000,     &
                     arr(islon,islat-1),isize,MPI_REAL,IDOWN,10000, &
                      MPI_COMM_WORLD,status,ierr)
!  FROM UP TO BOTTOM EXCHANGE DATA
   CALL MPI_SENDRECV(arr(islon,islat),isize,MPI_REAL,IDOWN,10000,   &
                     arr(islon,ielat+1),isize,MPI_REAL,IUP,10000,   &
                     MPI_COMM_WORLD,status,ierr)
   END SUBROUTINE COMM_UP_BOTTOM_2D
!************************************************************************
   SUBROUTINE COMM_LEFT_RIGHT_2D(ARR)
   IMPLICIT NONE
   REAL(kind_r4),DIMENSION(ISLON-LE:IELON+LE,ISLAT-LE:IELAT+LE) :: ARR

   INTEGER K,ISIZE
   ISIZE=1
!  from left to right exchange data
   DO K=ISLAT,IELAT
   CALL MPI_SENDRECV(ARR(IELON,K),ISIZE,MPI_REAL,RHALO(K),10000+K,  &
                     ARR(ISLON-1,K),ISIZE,MPI_REAL,LHALO(K),10000+K,&
                     MPI_COMM_WORLD,STATUS,IERR)
   END DO
!  from right to left exchange data
   DO K=ISLAT,IELAT
   CALL MPI_SENDRECV(ARR(ISLON,K),ISIZE,MPI_REAL,LHALO(K),10000+K,  &
                     ARR(IELON+1,K),ISIZE,MPI_REAL,RHALO(K),10000+K,&
                     MPI_COMM_WORLD,STATUS,IERR)
   END DO
   END SUBROUTINE COMM_LEFT_RIGHT_2D
!************************************************************************
   SUBROUTINE COMM_CORNER_2D(ARR)
   IMPLICIT NONE
   REAL(kind_r4),DIMENSION(ISLON-LE:IELON+LE,ISLAT-LE:IELAT+LE) :: ARR

   INTEGER ISIZE,K
   ISIZE=1

!   DO K=1,2
!   PRINT*,"REC",MYID,K,MYID+NEIGHBOR_LON(K)+NEIGHBOR_LAT(K),NEIGHBOR(K)
!   CALL MPI_RECV(ARR(1,1,CORNER_REC(1,K),CORNER_REC(2,K)),ISIZE,MPI_REAL,  &
!        NEIGHBOR(K),MYID+NEIGHBOR_LON(K)+NEIGHBOR_LAT(K),MPI_COMM_WORLD,  &
!        STATUS,IERR)
!   END DO

   DO K=1,NUM_CORNER_SEND
   CALL MPI_SEND(ARR(CORNER_SEND(1,K),CORNER_SEND(2,K)),ISIZE,MPI_REAL, &
        CORNER_SEND(3,K),CORNER_SEND(3,K)+CORNER_SEND(1,K)+CORNER_SEND(2,K),&
        MPI_COMM_WORLD,IERR)
   END DO

   DO K=1,4
   CALL MPI_RECV(ARR(CORNER_REC(1,K),CORNER_REC(2,K)),ISIZE,MPI_REAL,  &
        NEIGHBOR(K),MYID+NEIGHBOR_LON(K)+NEIGHBOR_LAT(K),MPI_COMM_WORLD,   &
        STATUS,IERR)
   END DO


   END SUBROUTINE COMM_CORNER_2D
!************************************************************************
   SUBROUTINE COMPTEST(H,LL) 
   IMPLICIT NONE
   INTEGER,INTENT(in) :: LL
   REAL(kind_r4),DIMENSION(ISLON-LE:IELON+LE,ISLAT-LE:IELAT+LE) :: H
   
   INTEGER UNITS,I,K,HALOID
   REAL B(LL),O(LL)
   CHARACTER*100 InFile 
   REAL(kind_r4) :: ZERO
   
   IF(KIND_R4==4)ZERO=1.E-6
   IF(KIND_R4==8)ZERO=1.E-14
!  =================================================
!   WRITE TEST DATA
!   LEFT
!   IF(Ileft>=0)THEN
   DO K=ISLAT,IELAT
   IF(LHALO(K)>=0)THEN
!   UNITS=100+Ileft
   UNITS=100+LHALO(K)
   INFILE='boud.XXX.XXX'
   WRITE(INFILE(6:8),'(I3.3)')UNITS
   WRITE(INFILE(10:12),'(I3.3)')MYID
   OPEN(UNITS,FILE=INFILE,STATUS='unknown',POSITION='append')
   WRITE(UNITS,'(<LL>f15.6,1x,I)')H(ISLON:ISLON+LL-1,K),LHALO(K)
   CLOSE(UNITS)
   END IF
   END DO 
!   RIGHT
!   IF(Iright>=0)THEN
   DO K=ISLAT,IELAT
   IF(RHALO(K)>=0)THEN
!   UNITS=200+Iright
   UNITS=200+RHALO(K)
   INFILE='boud.XXX.XXX'
   WRITE(INFILE(6:8),'(I3.3)')UNITS
   WRITE(INFILE(10:12),'(I3.3)')MYID
   OPEN(UNITS,FILE=INFILE,STATUS='unknown',POSITION='append')
   WRITE(UNITS,'(<LL>f15.6,1x,I)')H(IELON-LL+1:IELON,K),RHALO(K)
   CLOSE(UNITS)
   END IF
   END DO 
!   UPPER
   IF(Iup>=0)THEN
   UNITS=400+Iup
   INFILE='boud.XXX'
   WRITE(INFILE(6:8),'(I3.3)')UNITS
   OPEN(UNITS,FILE=INFILE,STATUS='unknown')
   DO K=ISLON,IELON
   WRITE(UNITS,'(<LL>f15.6)')H(K,IELAT-LL+1:IELAT)
   END DO 
   CLOSE(UNITS)
   END IF
!   DOWN
   IF(Idown>=0)THEN
   UNITS=300+Idown
   INFILE='boud.XXX'
   WRITE(INFILE(6:8),'(I3.3)')UNITS
   OPEN(UNITS,FILE=INFILE,STATUS='unknown')
   DO K=ISLON,IELON
   WRITE(UNITS,'(<LL>f15.6)')H(K,ISLAT:ISLAT+LL-1)
   END DO 
   CLOSE(UNITS)
   END IF
!   UPPER-LEFT
   IF(NUM_CORNER_SEND/=MPI_PROC_NULL)THEN
   DO K=1,NUM_CORNER_SEND
   INFILE='boud.XXX.XXXX.XXXX'
   WRITE(INFILE,'(a5,i3.3,a1,i4.4,a1,i4.4)')              &
   'boud.',CORNER_SEND(3,K),'.',CORNER_SEND(2,K),'.',CORNER_SEND(1,K)
   OPEN(UNITS,FILE=INFILE,STATUS='unknown')
   WRITE(UNITS,'(f15.6)')H(CORNER_SEND(1,K),CORNER_SEND(2,K))
   CLOSE(UNITS)
   END DO
   END IF 
   CALL BARRIER
!   FOR CENTER SUB-DOMAIN AND COMPARING THE ARIFICAL
!   BOUNDARY
!   compare upper boundary
   IF(IUP>=0)THEN
   INFILE='boud.XXX'
   WRITE(INFILE(6:8),'(I3.3)')300+MYID
   OPEN(110,file=INFILE,status='old')
   DO K=ISLON,IELON
   READ(110,'(<LL>f15.6)')B
   O=H(K,IELAT+1:IELAT+LL)
   DO I=1,LL
   IF(ABS(B(I)-O(I))>ZERO)THEN
   WRITE(*,'(3I,2f)')K,IElAT+I,I,B(I),O(I)
   CALL PAR_END
   END IF
   END DO
   END DO
   CLOSE(110,status='delete')
   END IF
   CALL BARRIER
   WRITE(6,*) "UPPER BOUNDARY IS CONSIST WITH THE NEIGHBOR"
   
   IF(IDOWN>=0)THEN
!   compare lower boundary
   INFILE='boud.XXX'
   WRITE(INFILE(6:8),'(I3.3)')400+MYID
   OPEN(110,file=INFILE,status='old')
   DO K=ISLON,IELON
   READ(110,'(<LL>f15.6)')B
   O=H(K,ISLAT-LL:ISLAT-1)
   DO I=1,LL
   IF(ABS(B(I)-O(I))>ZERO)THEN
   WRITE(*,'(3I,2f)')K,IElAT+I,I,B(I),O(I)
   CALL PAR_END
   END IF
   END DO
   END DO
   CLOSE(110,status='delete')
   END IF
   CALL BARRIER
   WRITE(6,*) "LOWER BOUNDARY IS CONSIST WITH THE NEIGHBOR"
   
   DO K=ISLAT,IELAT
!   IF(IRIGHT>=0)THEN
   IF(RHALO(K)>=0)THEN
!   compare right boundary
   IF(K==ISLAT)THEN
   INFILE='boud.XXX.XXX'
   WRITE(INFILE(6:8),'(I3.3)')100+MYID
   WRITE(INFILE(10:12),'(I3.3)')RHALO(K)
   OPEN(110,file=INFILE,status='old')
   ELSE IF(RHALO(K)/=RHALO(K-1).AND.K>ISLAT)THEN
   CLOSE(110,status='delete')
   INFILE='boud.XXX.XXX'
   WRITE(INFILE(6:8),'(I3.3)')100+MYID
   WRITE(INFILE(10:12),'(I3.3)')RHALO(K)
   OPEN(110,file=INFILE,status='old')
   END IF
!   DO K=ISLAT,IELAT
   READ(110,'(<LL>f15.6,I)')B,HALOID
   O=H(IELON+1:IELON+LL,K)
   DO I=1,LL
   IF(ABS(B(I)-O(I))>ZERO)THEN
   WRITE(*,'(3I,2f)')K,IElAT+I,I,B(I),O(I)
   CALL PAR_END
   END IF
   END DO
!   END DO
   IF(K==IELAT)CLOSE(110,status='delete')
   END IF
   END DO
   CALL BARRIER
   WRITE(6,*) "RIGHT BOUNDARY IS CONSIST WITH THE NEIGHBOR"
   
   DO K=ISLAT,IELAT
!   IF(Ileft>=0)THEN
   IF(LHALO(K)>=0)THEN
!   compare left boundary
   IF(K==ISLAT)THEN
   INFILE='boud.XXX.XXX'
   WRITE(INFILE(6:8),'(I3.3)')200+MYID
   WRITE(INFILE(10:12),'(I3.3)')LHALO(K)
   OPEN(110,file=INFILE,status='old')
   ELSE IF(LHALO(K)/=LHALO(K-1).AND.K>ISLAT)THEN
   CLOSE(110,status='delete')
   INFILE='boud.XXX.XXX'
   WRITE(INFILE(6:8),'(I3.3)')200+MYID
   WRITE(INFILE(10:12),'(I3.3)')LHALO(K)
   OPEN(110,file=INFILE,status='old')
   END IF
!   DO K=ISLAT,IELAT
   READ(110,'(<LL>f15.6,i)')B,HALOID
   O=H(ISLON-LL:ISLON-1,K)
   DO I=1,LL
   IF(ABS(B(I)-O(I))>ZERO)THEN
   WRITE(*,'(3I,2f)')K,IElAT+I,I,B(I),O(I)
   CALL PAR_END
   END IF
   END DO
!   END DO
   IF(K==IELAT)CLOSE(110,status='delete')
   END IF
   END DO
   CALL BARRIER
   WRITE(6,*) "LEFT BOUNDARY IS CONSIST WITH THE NEIGHBOR"
   
   IF(NEIGHBOR(4)>=0)THEN
!   compare upleft boundary
   WRITE(INFILE,'(a5,i3.3,a1,i4.4,a1,i4.4)')              &
   'boud.',MYID,'.',NEIGHBOR_LAT(4),'.',NEIGHBOR_LON(4)
   OPEN(110,file=INFILE,status='old')
   READ(110,'(f15.6)')B(1)
   O(1)=H(CORNER_REC(1,4),CORNER_REC(2,4))
   DO I=1,LL
   IF(ABS(B(I)-O(I))>ZERO)THEN
   WRITE(*,'(3I,2f)')K,IElAT+I,I,B(I),O(I)
   CALL PAR_END
   END IF
   END DO
   CLOSE(110,status='delete')
   END IF
   CALL BARRIER
   WRITE(6,*) "UPLEFT BOUNDARY IS CONSIST WITH THE NEIGHBOR"
   
   IF(NEIGHBOR(3)>=0)THEN
!   compare upright boundary
   WRITE(INFILE,'(a5,i3.3,a1,i4.4,a1,i4.4)')              &
   'boud.',MYID,'.',NEIGHBOR_LAT(3),'.',NEIGHBOR_LON(3)
   OPEN(110,file=INFILE,status='old')
   READ(110,'(f15.6)')B(1)
   O(1)=H(CORNER_REC(1,3),CORNER_REC(2,3))
   DO I=1,LL
   IF(ABS(B(I)-O(I))>ZERO)THEN
   WRITE(*,'(3I,2f)')K,IElAT+I,I,B(I),O(I)
   CALL PAR_END
   END IF
   END DO
   CLOSE(110,status='delete')
   END IF
   CALL BARRIER
   WRITE(6,*) "UPRIGHT BOUNDARY IS CONSIST WITH THE NEIGHBOR"
   
   IF(NEIGHBOR(2)>=0)THEN
!   compare lowerright boundary
   WRITE(INFILE,'(a5,i3.3,a1,i4.4,a1,i4.4)')              &
   'boud.',MYID,'.',NEIGHBOR_LAT(2),'.',NEIGHBOR_LON(2)
   OPEN(110,file=INFILE,status='old')
   READ(110,'(f15.6)')B(1)
   O(1)=H(CORNER_REC(1,2),CORNER_REC(2,2))
   DO I=1,LL
   IF(ABS(B(I)-O(I))>ZERO)THEN
   WRITE(*,'(3I,2f)')K,IElAT+I,I,B(I),O(I)
   CALL PAR_END
   END IF
   END DO
   CLOSE(110,status='delete')
   END IF
   CALL BARRIER
   WRITE(6,*) "LOWERRIGHT BOUNDARY IS CONSIST WITH THE NEIGHBOR"
   
   IF(NEIGHBOR(1)>=0)THEN
!   compare lowerleft boundary
   WRITE(INFILE,'(a5,i3.3,a1,i4.4,a1,i4.4)')              &
   'boud.',MYID,'.',NEIGHBOR_LAT(1),'.',NEIGHBOR_LON(1)
   OPEN(110,file=INFILE,status='old')
   READ(110,'(f15.6)')B(1)
   O(1)=H(CORNER_REC(1,1),CORNER_REC(2,1))
   DO I=1,LL
   IF(ABS(B(I)-O(I))>ZERO)THEN
   WRITE(*,'(3I,2f)')K,IElAT+I,I,B(I),O(I)
   CALL PAR_END
   END IF
   END DO
   CLOSE(110,status='delete')
   END IF
   CALL BARRIER
   WRITE(6,*) "LOWERLEFT BOUNDARY IS CONSIST WITH THE NEIGHBOR"
   RETURN 
   END SUBROUTINE COMPTEST
!************************************************************************
   FUNCTION EXTREM_0D(VAR,OPC)
   IMPLICIT NONE
   REAL(kind_r4),INTENT(IN) :: VAR
   REAL(kind_r4) :: EXTREM_0D
   CHARACTER(LEN=*),OPTIONAL :: OPC
!  LOCAL
   INTEGER MASTER,OP,L1,L2
   REAL(kind_r4) :: MAXV
   MASTER=0;OP=MPI_MAX

   IF(PRESENT(OPC))THEN
   L1=0;L2=0
   L1=INDEX(OPC,"MIN")
   L2=INDEX(OPC,"min")
   IF(L1/=0.OR.L2/=0)OP=MPI_MIN
   END IF

   CALL MPI_REDUCE(VAR,MAXV,1,MPI_F,OP,MASTER,masnum_mpi_comm,IERR)
   EXTREM_0D=MAXV
   RETURN
   END FUNCTION EXTREM_0D
!  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
   FUNCTION EXTREM_1D(VAR,OPC)
   IMPLICIT NONE
   REAL(kind_r4),INTENT(IN),DIMENSION(:) :: VAR
   REAL(kind_r4) :: EXTREM_1D
   CHARACTER(LEN=*),OPTIONAL :: OPC
!  LOCAL
   INTEGER MASTER,OP,L1,L2
   REAL(kind_r4) :: MAXV,GMAX
   MASTER=0;OP=MPI_MAX
   MAXV=MAXVAL(VAR)

   IF(PRESENT(OPC))THEN
   L1=0;L2=0
   L1=INDEX(OPC,"MIN")
   L2=INDEX(OPC,"min")
   IF(L1/=0.OR.L2/=0)THEN
   OP=MPI_MIN
   MAXV=MINVAL(VAR)
   END IF
   END IF
   CALL MPI_REDUCE(MAXV,GMAX,1,MPI_F,OP,MASTER,masnum_mpi_comm,IERR)
   EXTREM_1D=GMAX
   RETURN
   END FUNCTION EXTREM_1D
!  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
   FUNCTION EXTREM_2D(VAR,OPC)
   IMPLICIT NONE
   REAL(kind_r4),INTENT(IN),DIMENSION(:,:) :: VAR
   REAL(kind_r4) :: EXTREM_2D
   CHARACTER(LEN=*),OPTIONAL :: OPC
!  LOCAL
   INTEGER MASTER,OP,L1,L2
   REAL(kind_r4) :: MAXV,GMAX
   MASTER=0;OP=MPI_MAX
   MAXV=MAXVAL(VAR)

   IF(PRESENT(OPC))THEN
   L1=0;L2=0
   L1=INDEX(OPC,"MIN")
   L2=INDEX(OPC,"min")
   IF(L1/=0.OR.L2/=0)THEN
   OP=MPI_MIN
   MAXV=MINVAL(VAR)
   END IF
   END IF
   CALL MPI_REDUCE(MAXV,GMAX,1,MPI_F,OP,MASTER,masnum_mpi_comm,IERR)
   EXTREM_2D=GMAX
   RETURN
   END FUNCTION EXTREM_2D
!  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
   FUNCTION EXTREM_3D(VAR,OPC)
   IMPLICIT NONE
   REAL(kind_r4),INTENT(IN),DIMENSION(:,:,:) :: VAR
   REAL(kind_r4) :: EXTREM_3D
   CHARACTER(LEN=*),OPTIONAL :: OPC
!  LOCAL
   INTEGER MASTER,OP,L1,L2
   REAL(kind_r4) :: MAXV,GMAX
   MASTER=0;OP=MPI_MAX
   MAXV=MAXVAL(VAR)

   IF(PRESENT(OPC))THEN
   L1=0;L2=0
   L1=INDEX(OPC,"MIN")
   L2=INDEX(OPC,"min")
   IF(L1/=0.OR.L2/=0)THEN
   OP=MPI_MIN
   MAXV=MINVAL(VAR)
   END IF
   END IF
   CALL MPI_REDUCE(MAXV,GMAX,1,MPI_F,OP,MASTER,masnum_mpi_comm,IERR)
   EXTREM_3D=GMAX
   RETURN
   END FUNCTION EXTREM_3D
!  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
   FUNCTION EXTREM_4D(VAR,OPC)
   IMPLICIT NONE
   REAL(kind_r4),INTENT(IN),DIMENSION(:,:,:,:) :: VAR
   REAL(kind_r4) :: EXTREM_4D
   CHARACTER(LEN=*),OPTIONAL :: OPC
!  LOCAL
   INTEGER MASTER,OP,L1,L2
   REAL(kind_r4) :: MAXV,GMAX
   MASTER=0;OP=MPI_MAX
   MAXV=MAXVAL(VAR)

   IF(PRESENT(OPC))THEN
   L1=0;L2=0
   L1=INDEX(OPC,"MIN")
   L2=INDEX(OPC,"min")
   IF(L1/=0.OR.L2/=0)THEN
   OP=MPI_MIN
   MAXV=MINVAL(VAR)
   END IF
   END IF
   CALL MPI_REDUCE(MAXV,GMAX,1,MPI_F,OP,MASTER,masnum_mpi_comm,IERR)
   EXTREM_4D=GMAX
   RETURN
   END FUNCTION EXTREM_4D
!  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!************************************************************************
END MODULE PARALLEL_MOD_WAVE
