!======================================================================
!CVS: $Id: wave.F90_program,v 1.1 2012/09/20 03:14:50 wgs Exp $
!CVS: $Source: /soa04/users/wgs/.mycvsroot/intercomm/wave/wave.F90_program,v $
!CVS: $Name:  $
!======================================================================
PROGRAM MASNUM_WAVE
!==============================================================================!
!  INCLUDE MODULES                                                             !
!==============================================================================!
 USE parallel_mod_wave 
 use LIMS_WAVE
 use ALL_VAR_WAVE
 use CONTROL_WAVE
 use CONST_WAVE
 use CCPL_interface_mod                       !zhaobiao, c-coupler2
 use coupling_wave_mod

 IMPLICIT NONE
 REAL STIME,ENDTIME
!==============================================================================!
!   SETUP PARALLEL ENVIRONMENT                                                 !
!==============================================================================!
 SERIAL = .TRUE.
 PAR    = .FALSE.
 MSR    = .TRUE.
 MYID   = 0
 NPROCS = 1

 masnum_mpi_comm = CCPL_NULL_COMM                          !zhaobiao,c-coupler2
 CALL register_masnum_frame_coupling_configuration         !zhaobiao,c-coupler2
 
 CALL SET_MPI_ENV(MYID,NPROCS,SERIAL,PAR,MSR) 
 !!CALL CCPL_start_normal_timing(masnum_state_variables(masnum_grid_id)%comp_id, "calculate initial")
!==============================================================================!
!   SETUP MODEL RUN                                                            !
!==============================================================================!
!
!  READ PARAMETERS CONTROLLING MODEL RUN
!
 CALL DATA_RUN_WAVE
!
!  DECOMPOSE DOMAIN BY ELEMENTS USING METIS
!
 CALL DOMDEC_WAVE
!
!read topography data
!
 CALL SETTOPOG
!
! INITIAL WAVE PARAMETER
!
 CALL SETWAVE
!
!
!
 CALL NLWEIGHT
!
!
! ALLOCATE  MEMORY FOR THE ALLOCATABLE VARIABLES
!
  CALL ALLOCATE_VARIABLE_WAVE
!
!
 CALL register_component_coupling_configuration !zhaobiao,c-coupler2
!
! GET THE START WALL TIME
!
 CALL cpu_time(stime)
!!CALL CCPL_stop_normal_timing(masnum_state_variables(masnum_grid_id)%comp_id, "calculate initial")
!!CALL CCPL_start_normal_timing(masnum_state_variables(masnum_grid_id)%comp_id, "calculate allrun")
 CALL READWI

 100  continue
!
! GET THE END WALL TIME
!
 CALL cpu_time(ENDTIME)
!!CALL CCPL_stop_normal_timing(masnum_state_variables(masnum_grid_id)%comp_id, "calculate allrun")
 CALL BARRIER 
 IF(MSR)WRITE(6,*) myid,"working time ",endtime-stime
!
!  CLOSE PARALLEL ENVIRONMENT
!
 CALL PAR_END
END
