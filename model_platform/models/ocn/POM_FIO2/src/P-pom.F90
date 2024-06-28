!======================================================================
!CVS: $Id: P-pom.F90_program,v 1.1 2013/06/19 02:51:34 wgs Exp $
!CVS: $Source: /soa04/users/wgs/.mycvsroot/intercomm/ocean/P-pom.F90_program,v $
!CVS: $Name:  $
!======================================================================
 PROGRAM POM 
!==============================================================================!
!  INCLUDE MODULES                                                             !
!==============================================================================!
 USE parallel_mod 
 use LIMS
 use ALL_VAR
 use CONTROL
 USE CCPL_interface_mod, only: CCPL_finalize                           !zhaobiao,c-coupler2
 USE coupling_pom_mod                                                  !zhaobiao,c-coupler2 
 IMPLICIT NONE
 include 'mpif.h'
 REAL STIME,ENDTIME,RUNTIME
!==============================================================================!
!   SETUP PARALLEL ENVIRONMENT                                                 !
!==============================================================================!
 SERIAL = .TRUE.
 PAR    = .FALSE.
 MSR    = .TRUE.
 MYID   = 0
 NPROCS = 1
 
 pom_mpi_comm = CCPL_NULL_COMM                          !zhaobiao,c-coupler2
 !CALL c_coupler_initialize(pom_mpi_comm,.true.)
 CALL register_pom_frame_coupling_configuration         !zhaobiao,c-coupler2

 CALL SET_MPI_ENV(MYID,NPROCS,SERIAL,PAR,MSR) 
!==============================================================================!
!   SETUP MODEL RUN                                                            !
!==============================================================================!
! 
 CALL CPU_TIME(STIME)
!
!  READ PARAMETERS CONTROLLING MODEL RUN
!
 CALL DATA_RUN
 WRITE(6,*),IM,JM,KB,ZSS
!
!  DECOMPOSE DOMAIN BY ELEMENTS USING METIS
!
 CALL DOMDEC
!
! CALL do_coupling_register_decomp
! CALL do_coupling_register_data_buffer
!
! CORE SETCTION
!
 CALL POM_CORE
!
!  CLOSE PARALLEL ENVIRONMENT
!
 CALL CPU_TIME(ENDTIME)
 RUNTIME=ENDTIME-STIME
 STIME=EXTREMUM(RUNTIME,'MAX')
 WRITE(6,*)"MODEL RUN TIME: ",STIME
!
! CALL c_coupler_finalize()
CALL CCPL_finalize(.true.,annotation='CCPL_finalize')   !zhaobiao, c-coupler2

END PROGRAM POM
