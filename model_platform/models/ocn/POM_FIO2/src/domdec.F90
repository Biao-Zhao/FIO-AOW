 SUBROUTINE DOMDEC 
!=======================================================================
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!     THE BELOW ROUTINE IS SUBAREA PROGRESS.
!     WROTE BY GUANSO. WANG 14-JAN-2008.
!     EMAIL:  wanggs@fio.org.cn
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!    IN PROJECT  SETTING LINK,  ADD MPICH.LIB
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!         --------------------------------------------------------------
!         |             |               |              |               |
!         |             |               |              |               |
!         |             |               |              |               |
!JJ=ILATCPU      3      |      7        |      11      |        15     |
!         |             |               |              |               |
!         |             |               |              |               |
!         |             |               |              |               |
!         -------------------------------------------------------------- 
!         |             |               |              |               |
!         |             |               |              |               |
!         |             |               |              |               |                 
!         |      2      |       6       |      10      |        14     |                 
!         |             |               |              |               |
!         |             |               |              |               |
!         |             |               |(IELON,IELAT) |               |
!  IYCPU  --------------------------------------------------------------
!ILATCPU  |             |AAAAAAAAAAAAAAA|              |               |
!         |             |AAAAAAAAAAAAAAA|              |               |
!         |             |AAAAAAAAAAAAAAA|              |               |
!         |      1      |AAAAAAA5AAAAAAA|       9      |        13     |  
!         |             |AAAAAAAAAAAAAAA|              |               | 
!         |             |AAAAAAAAAAAAAAA|              |               |
!         |             |AAAAAAAAAAAAAAA|              |               |
!         --------------------------------------------------------------
!         |(ISLON,ISLAT)|               |              |               |
!         |             |               |              |               |
!         |             |               |              |               |
!JJ=1     |      0      |       4       |       8      |        12     |
!         |             |               |              |               |
!         |             |               |              |               |
!         |             |               |              |               |
!         --------------------------------------------------------------  
!             II=1             IXCPU ILONCPU             II=ILONCPU
!=======================================================================
 USE PARALLEL_MOD, only:gather_int,bcast_int,barrier,par_end
 USE data_kind_mod
 USE LIMS, ONLY : NUMXCPU,NUMYCPU,IM,JM,MYID,NPROCS
 USE CONTROL, ONLY : ISLON,IELON,ISLAT,IELAT,       &
                     JSLON,JELON,JSLAT,JELAT,       &
                     KSLON,KELON,KSLAT,KELAT,       &
                     LSLON,LELON,LSLAT,LELAT,       &
                     MSLON,MELON,MSLAT,MELAT,       &
                     NSLON,NELON,NSLAT,NELAT,       &
                     L1SLON,L1ELON,L1SLAT,L1ELAT,   &
                     NEIGHBOR,IUP,IDOWN,IRIGHT,ILEFT, &
                     MSR,NEIGHBOR_ID,IPART !,OCN_COMM_WORLD
 USE CONST, ONLY : LE
 IMPLICIT NONE
 INCLUDE 'mpif.h'
 INTEGER(kind_in) IXCPU,IYCPU,II,JJ
 INTEGER(kind_in) IMPART,IMRMAIN,JMPART,JMRMAIN 
 !INTEGER,DIMENSION(7,NPROCS) :: IPART
 INTEGER,DIMENSION(7) :: IJIPART
 INTEGER LIM,LJM
 
 IXCPU=NUMXCPU
 IYCPU=NUMYCPU
 IF(NPROCS/=IXCPU*IYCPU)THEN
 WRITE(6,*),"ERROR NUMNODES"
 WRITE(6,*),"PLEASE ENTER RIGHT NUMBER NODES",      &
        " IT SHOULD BE :",IXCPU*IYCPU
! CALL BARRIER
 CALL PAR_END
 END IF 
 ALLOCATE(IPART(7,NPROCS))
 IPART=0

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!SUBAREA
 II=MYID/IYCPU+1
 JJ=MOD(MYID,IYCPU)+1

 IMPART=IM/IXCPU
 IMRMAIN=MOD(IM,IXCPU)
 IF(IMRMAIN>=II)THEN
 IMPART=IMPART+1
 END IF

 JMPART=JM/IYCPU  
 JMRMAIN=MOD(JM,IYCPU)
 IF(JMRMAIN>=JJ)THEN
 JMPART=JMPART+1
 END IF
 

 ISLON=(II-1)*IMPART+1
 IELON=II*IMPART
 IF(IMRMAIN<II)THEN
 ISLON=(II-1)*IMPART+1+IMRMAIN
 IELON=II*IMPART+IMRMAIN
 END IF
 
 ISLAT=(JJ-1)*JMPART+1
 IELAT=JJ*JMPART
 IF(JMRMAIN<JJ)THEN
 ISLAT=(JJ-1)*JMPART+1+JMRMAIN
 IELAT=JJ*JMPART+JMRMAIN
 END IF
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 JSLON=ISLON
 JELON=IELON
 JSLAT=ISLAT
 JELAT=IELAT

 KSLON=ISLON
 KELON=IELON
 KSLAT=ISLAT
 KELAT=IELAT

 LSLON=ISLON-1
 LELON=IELON+1
 LSLAT=ISLAT-1
 LELAT=IELAT+1
 IF(LSLAT==0)LSLAT=1
 IF(LELAT==JM+1)LELAT=JM

 L1SLON=ISLON-1
 L1ELON=IELON+1
 L1SLAT=ISLAT-1
 L1ELAT=IELAT+1

 MSLON=ISLON-2
 MELON=IELON+2
 MSLAT=ISLAT-2
 MELAT=IELAT+2
 IF(MSLAT==-1)MSLAT=1
 IF(MELAT==JM+2)MELAT=JM

 NSLON=ISLON-1
 NELON=IELON+1
 NSLAT=ISLAT-1
 NELAT=IELAT+1
 IF(NSLAT==0)NSLAT=1
 IF(NELAT==JM+1)NELAT=JM

 IF(ISLON==1) THEN
 JSLON=2
 KSLON=3
 LSLON=2
 L1SLON=2
 MSLON=2
 NSLON=2
 END IF
 
 IF(IELON==IM)THEN
 JELON=IM-1
 KELON=IM-2
 LELON=IM
 L1ELON=IM-1
 MELON=IM
 NELON=IM-1
 END IF

 IF(ISLAT==1)THEN
 JSLAT=2
 KSLAT=3
 LSLAT=2
 L1SLAT=2
 MSLAT=2
 NSLAT=2
 END IF

 IF(IELAT==JM)THEN
 JELAT=JM-1
 KELAT=JM-2
 LELAT=JM
 L1ELAT=JM-1
 MELAT=JM
 NELAT=JM-1
 END IF

 IPART(1,MYID+1)=MYID
 IPART(2,MYID+1)=ISLON
 IPART(3,MYID+1)=IELON
 IPART(4,MYID+1)=ISLAT
 IPART(5,MYID+1)=IELAT
 LIM=IELON-ISLON+1+2*LE
 LJM=IELAT-ISLAT+1+2*LE
 IPART(6,MYID+1)=LIM 
 IPART(7,MYID+1)=LJM 
 IJIPART=IPART(:,MYID+1)
 CALL GATHER_INT(IJIPART,IPART(:,MYID+1))
 CALL BCAST_INT(IPART)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
 NEIGHBOR(1)=MYID-1-IYCPU
 NEIGHBOR(2)=MYID-1+IYCPU
 NEIGHBOR(3)=MYID+1+IYCPU
 NEIGHBOR(4)=MYID+1-IYCPU
 
 ILEFT=MYID-IYCPU
 IRIGHT=MYID+IYCPU
 IUP=MYID+1
 IDOWN=MYID-1 
 
 NEIGHBOR_ID(1,1)=ISLON;NEIGHBOR_ID(2,1)=ISLAT
 NEIGHBOR_ID(1,2)=IELON;NEIGHBOR_ID(2,2)=ISLAT
 NEIGHBOR_ID(1,3)=IELON;NEIGHBOR_ID(2,3)=IELAT
 NEIGHBOR_ID(1,4)=ISLON;NEIGHBOR_ID(2,4)=IELAT
 
 IF(II==1)THEN
   ILEFT=MYID+(IXCPU-1)*IYCPU
   NEIGHBOR(1)=ILEFT-1
   NEIGHBOR(4)=ILEFT+1
   IF(JJ.EQ.1)NEIGHBOR(1)=MPI_PROC_NULL
   IF(JJ.EQ.IYCPU)NEIGHBOR(4)=MPI_PROC_NULL
 END IF
 IF(II==IXCPU)THEN
   IRIGHT=MYID-(IXCPU-1)*IYCPU
   NEIGHBOR(2)=IRIGHT-1
   NEIGHBOR(3)=IRIGHT+1
   IF(JJ.EQ.1)NEIGHBOR(2)=MPI_PROC_NULL
   IF(JJ.EQ.IYCPU)NEIGHBOR(3)=MPI_PROC_NULL
 END IF
 IF(JJ==1)THEN
   IDOWN=MPI_PROC_NULL
   NEIGHBOR(1)=MPI_PROC_NULL
   NEIGHBOR(2)=MPI_PROC_NULL
 END IF
 IF(JJ==IYCPU)THEN
   IUP=MPI_PROC_NULL
   NEIGHBOR(3)=MPI_PROC_NULL
   NEIGHBOR(4)=MPI_PROC_NULL
 END IF   
 DO II=1,NPROCS
 WRITE(6,'(i3,8i8,2i8)')IPART(:,II)
 END DO
 RETURN
 END 
