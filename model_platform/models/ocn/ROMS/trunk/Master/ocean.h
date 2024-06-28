      PROGRAM ocean
!
!svn $Id: ocean.h 889 2018-02-10 03:32:52Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2018 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  Regional Ocean Model System (ROMS)                                  !
!  Terrain-following Ocean Model System (TOMS)                         !
!                                                                      !
!  Master program to execute  ROMS/TOMS  drivers in ocean mode only    !
!  without coupling (sequential or concurrent) to  any  atmospheric    !
!  model.                                                              !
!                                                                      !
!  This ocean model solves the free surface, hydrostatic, primitive    !
!  equations  over  variable  topography  using  stretched terrain-    !
!  following coordinates in the vertical and orthogonal curvilinear    !
!  coordinates in the horizontal.                                      !
!                                                                      !
!  Nonlinear Model Developers:                                         !
!                                                                      !
!  Dr. Hernan G. Arango                                                !
!    Institute of Marine and Coastal Sciences                          !
!    Rutgers University, New Brunswick, NJ, USA                        !
!    (arango@marine.rutgers.edu)                                       !
!                                                                      !
!  Dr. Alexander F. Shchepetkin                                        !
!    Institute of Geophysics and Planetary Physics                     !
!    UCLA, Los Angeles, CA, USA                                        !
!    (alex@atmos.ucla.edu)                                             !
!                                                                      !
!  Dr. John C. Warner                                                  !
!    U.S. Geological Survey                                            !
!    Woods Hole, MA, USA                                               !
!    (jcwarner@usgs.gov)                                               !
!                                                                      !
!  Tangent linear and Adjoint Models and Algorithms Developers:        !
!                                                                      !
!    Dr. Hernan G. Arango    (arango@marine.rutgers.edu)               !
!    Dr. Bruce Cornuelle     (bcornuelle@ucsd.edu)                     !
!    Dr. Emanuele Di Lorenzo (edl@eas.gatech.edu)                      !
!    Dr. Arthur J. Miller    (ajmiller@ucsd.edu)                       !
!    Dr. Andrew M. Moore     (ammoore@ucsc.edu)                        !
!    Dr. Brian Powell        (powellb@uscs.edu)                        !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_iounits
      USE mod_scalars
!
      USE ocean_control_mod, ONLY : ROMS_initialize
      USE ocean_control_mod, ONLY : ROMS_run
      USE ocean_control_mod, ONLY : ROMS_finalize
# ifdef C_COUPLER
      USE CCPL_interface_mod, only: CCPL_finalize                         !zhaobiao, c-coupler2
      USE coupling_roms_mod                                               !zhaobiao, c-coupler2
# endif
!
      implicit none
!
!  Local variable declarations.
!
      logical, save :: first

      integer :: ng, MyError

#ifdef DISTRIBUTE
# ifdef MPI
!
!-----------------------------------------------------------------------
!  Initialize distributed-memory MPI configuration.
!-----------------------------------------------------------------------
!
# ifdef C_COUPLER
      CALL register_roms_frame_coupling_configuration         !zhaobiao,c-coupler2
# endif
      IF (OCN_COMM_WORLD .ne. -1) THEN                        !zhaobiao,c-coupler2
          MyError=0                                           !zhaobiao,c-coupler2 
      ELSE                                                    !zhaobiao,c-coupler2
          CALL mpi_init (MyError)                             !zhaobiao,c-coupler2          
      END IF                                                  !zhaobiao,c-coupler2

      IF (MyError.ne.0) THEN
        WRITE (stdout,10)
  10    FORMAT (/,' ROMS/TOMS - Unable to initialize MPI.')
        exit_flag=6
      END IF
!
!  Get rank of the local process in the group associated with the
!  comunicator.
!
# ifdef C_COUPLER
     CALL mpi_comm_rank (OCN_COMM_WORLD, MyRank, MyError)    !zhaobiao,c-coupler2
#else
     CALL mpi_comm_rank (MPI_COMM_WORLD, MyRank, MyError)    
#endif
      IF (MyError.ne.0) THEN
        WRITE (stdout,20)
  20    FORMAT (/,' ROMS/TOMS - Unable to inquire rank of local',       &
     &              ' processor.')
        exit_flag=6
      END IF
# endif
#endif
!
!-----------------------------------------------------------------------
!  Initialize ocean internal and external parameters and state
!  variables for all nested grids, if applicable.
!-----------------------------------------------------------------------
!
      IF (exit_flag.eq.NoError) THEN
        first=.TRUE.
# ifdef C_COUPLER
        CALL ROMS_initialize (first, OCN_COMM_WORLD)       !zhaobiao,c-coupler2
# else
       CALL ROMS_initialize (first)
# endif
      END IF
!
!-----------------------------------------------------------------------
!  Time-step ocean model over all nested grids, if applicable, by the
!  specified time interval in seconds.
!-----------------------------------------------------------------------
!
      IF (exit_flag.eq.NoError) THEN
        run_time=0.0_r8
        DO ng=1,Ngrids
          run_time=MAX(run_time, dt(ng)*ntimes(ng))
        END DO
        CALL ROMS_run (run_time)
      END IF
!
!-----------------------------------------------------------------------
!  Terminate ocean model execution: flush and close all IO files.
!-----------------------------------------------------------------------
!
      CALL ROMS_finalize
     
      IF (exit_flag.eq.NoError) THEN
#if defined DISTRIBUTE && defined MPI
#ifdef C_COUPLER
      CALL CCPL_finalize(.false.,annotation="ROMS CALL CCPL_finalize")   !zhaobiao, c-coupler2
      CALL mpi_finalize (MyError)
#endif
#endif
      END IF

      END PROGRAM ocean
