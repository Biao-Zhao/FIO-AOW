      MODULE coupling_wave_mod
      USE DATA_KIND_MOD_WAVE, ONLY: KIND_IN
      USE CONTROL_WAVE, ONLY: ISLON, IELON, ISLAT, IELAT, DELTTM, DX,   &
                              DY, ISCURRENT 
      USE LIMS_WAVE, ONLY: IM, JM, KB
      USE ALL_VAR_WAVE,ONLY: XGRID, YGRID,NSP, ZBV, BV, H1_3, TPF,              &
                             UORBITAL, VORBITAL, USTOKES, VSTOKES,      &
                             RA, BBR, UST, TAUINX, TAUINY,TAUDSX,TAUDSY,&
                             UXX, UXY, UYX, UYY, UX, UY, WX, WY
      USE PARALLEL_MOD_WAVE, ONLY: masnum_mpi_comm, COMM_DATA_2D
      USE CCPL_interface_mod                       !zhaobiao, c-coupler2
      implicit none
!=======================================================================================================
!                              Main program                                                            =
!     This subroutine is the interface program for coupling between MASNUM and C-Coupler  created by  =
!     Dr. Biao Zhao                                                                                    =
!                                                                                                      =
!                                                                          Biao Zhao 2017.03.01        =
!=======================================================================================================
      type, private :: masnum_state_var
      integer               :: comp_id
      integer               :: parent_comp_id
      integer               :: time_step
      integer               :: grid_H2D_id
      integer               :: grid_V1D_id
      integer               :: grid_3D_id
      integer               :: decomp_id
      integer               :: timer_id
      integer               :: num_local_cells
      integer               :: num_global_cells
      integer,  allocatable :: local_cell_global_indexes(:)
      real,     allocatable :: center_lats(:), center_lons(:)
      real,     allocatable :: lats_vertexes(:,:), lons_vertexes(:,:)
      integer,  allocatable :: mask(:)
      logical               :: initialization
      real,     allocatable :: bv(:, :, :)
      real,     allocatable :: bbv(:, :, :)
      real,     allocatable :: ssu_from_ocean(:, :)    ! sea surface current u from ocean 
      real,     allocatable :: ssv_from_ocean(:, :)    ! sea surface current v from ocean
      real,     allocatable :: ustar_from_atm(:,:)
      real,     allocatable :: u10_from_atm(:,:)
      real,     allocatable :: v10_from_atm(:,:)
      real,     allocatable :: whitecap_fraction(:,:)
      real,     allocatable :: u_stokes_srf(:,:)
      real,     allocatable :: v_stokes_srf(:,:)
      real,     allocatable :: u_orbital_srf(:,:)
      real,     allocatable :: v_orbital_srf(:,:)
      real,     allocatable :: break_volume(:,:)
      real,     allocatable :: significant_wave_height(:,:)
      real,     allocatable :: peak_wave_length(:,:)
      real,     allocatable :: peak_wave_period(:,:)
      real,     allocatable :: u_wave_stress(:,:)
      real,     allocatable :: v_wave_stress(:,:)
      real,     allocatable  :: tau_in(:,:)
      end type masnum_state_var

      type(masnum_state_var), public :: masnum_state_variables(100)
      integer,             public    :: masnum_frame_id
      integer,             public    :: masnum_grid_id
      logical,             public    :: atm_coupled,ocean_coupled
      character(len=512),  public    :: log_file_name
      logical,             public    :: wave_log 
      CONTAINS




      SUBROUTINE register_masnum_frame_coupling_configuration
      implicit none
      integer             :: parent_comp_id
      parent_comp_id     = -1
      masnum_frame_id       = CCPL_register_component(parent_comp_id, "masnum","wave", masnum_mpi_comm,.false.,change_dir=.true., annotation= "register masnum FRAME to c-coupler")
      wave_log        = .false.
      wave_log          =   CCPL_get_comp_log_file_name(masnum_frame_id,log_file_name, annotation="get the wave logfile name of masnum")       
      open(6,file=trim(log_file_name),status="UNKNOWN")
      END SUBROUTINE register_masnum_frame_coupling_configuration

      SUBROUTINE register_component_coupling_configuration
      implicit none
      integer               :: parent_comp_id
      integer               :: comp_id
      character(len=1024)   :: annotation
      character(len=80)     :: comp_name
      integer               :: grid_H2D_id, grid_V1D_id, grid_3D_id
      integer               :: decomp_id
      integer               :: field_mark_ocn, field_mark_wave,field_mark_atm
      integer               :: field_id_u10, field_id_v10, field_id_ust
      integer               :: field_id_sss, field_id_sst, field_id_ssu,field_id_ssv
      integer               :: field_id_bv, field_id_bbv,field_id_u_wave_stress, field_id_v_wave_stress
      integer               :: field_id_ustokes, field_id_vstokes, field_id_uorb, field_id_vorb, field_id_whitecap, field_id_breakrate, field_id_hwave, field_id_pwave, field_id_lwavep
      integer               :: filed_u_wave_stress, filed_v_wave_stress, field_id_tau_in
      integer               :: timer_id, import_interface_id, export_interface_id
      integer, allocatable  :: fields_id(:)
      integer               :: num_comps, individual_or_family(100)
      character(len=1024)   :: comps_full_names(12)
      INTEGER(KIND_IN), ALLOCATABLE :: LANDMASK(:)
      integer               :: I,J,n

      field_mark_ocn  = 0
      field_mark_wave = 1
      field_mark_atm  = 2
!=======================================================================================================================================================================
      comp_name         =   "MASNUM_TOP_d01"
      annotation        =   "component MASNUM_TOP_d01 start registration"
      parent_comp_id    =   masnum_frame_id
      comp_id           =   CCPL_register_component(parent_comp_id, comp_name,"wave", masnum_mpi_comm,change_dir=.true., annotation=annotation) 
      masnum_grid_id = 1
      masnum_state_variables(masnum_grid_id)%comp_id        = comp_id
      masnum_state_variables(masnum_grid_id)%time_step      = DELTTM*60      !???? 
      masnum_state_variables(masnum_grid_id)%parent_comp_id = masnum_frame_id
      masnum_state_variables(masnum_grid_id)%initialization = .true.
!=======================================================================================================================================================================
 
      CALL CCPL_set_normal_time_step(comp_id, masnum_state_variables(masnum_grid_id)%time_step)
     
!=======================================================================================================================================================================
      CALL masnum_grid_parallel_decomposition
!      ALLOCATE(LANDMASK(IM*JM))
!      n = 0
!      DO J=1,JM
!      DO I=1,IM
!      n = n + 1
!      IF(NSP(I,J)/=0)THEN
!         LANDMASK(n)=1
!      ELSE
!         LANDMASK(n)=0
!      END IF
!      ENDDO
!      ENDDO
!      grid_H2D_id    =   CCPL_register_H2D_grid_via_global_data(comp_id, "masnum_grid_via_local", "LON_LAT", "degrees", "cyclic", IM, JM,                             &
!                                                               -999999.0, -999999.0, -999999.0, -999999.0, X(1:IM), Y,&
!                                                               landmask, annotation="register masnum H2D grid")
!      DEALLOCATE(LANDMASK)

!      grid_H2D_id    =   CCPL_register_H2D_grid_via_local_data(comp_id, "masnum_grid_via_local", "LON_LAT", "degrees", "acyclic", masnum_state_variables(masnum_grid_id)%num_global_cells,                             &
!                                                               masnum_state_variables(masnum_grid_id)%num_local_cells, masnum_state_variables(masnum_grid_id)%local_cell_global_indexes,                              &
!                                                               -999999., -999999., -999999., -999999., masnum_state_variables(masnum_grid_id)%center_lons, masnum_state_variables(masnum_grid_id)%center_lats,&
!                                                               masnum_state_variables(masnum_grid_id)%mask, annotation="register masnum H2D grid")
      grid_H2D_id    =   CCPL_register_H2D_grid_via_local_data(comp_id, "masnum_grid_via_local", "LON_LAT", "degrees", "acyclic", masnum_state_variables(masnum_grid_id)%num_global_cells,                             &
                                                               masnum_state_variables(masnum_grid_id)%num_local_cells, masnum_state_variables(masnum_grid_id)%local_cell_global_indexes,                              &
                                                               -999999., -999999., -999999., -999999., masnum_state_variables(masnum_grid_id)%center_lons, masnum_state_variables(masnum_grid_id)%center_lats,        &
                                                               mask=masnum_state_variables(masnum_grid_id)%mask, vertex_lon=masnum_state_variables(masnum_grid_id)%lons_vertexes,                                        &
                                                               vertex_lat=masnum_state_variables(masnum_grid_id)%lats_vertexes, annotation="register masnum H2D grid")

      masnum_state_variables(masnum_grid_id)%grid_H2D_id    = grid_H2D_id
     !register vertical grid
      grid_V1D_id     =  CCPL_register_V1D_Z_grid_via_model_data(comp_id, "masnum_V1D_grid", "meters",ZBV*(-1.0), annotation="register a v1d grid for masnum")
      grid_3D_id      =  CCPL_register_MD_grid_via_multi_grids(comp_id, "masnumm_zlevel_3D_grid", grid_H2D_id, grid_V1D_id, annotation="register a 3-d grid for masnum")
      masnum_state_variables(masnum_grid_id)%grid_V1D_id    = grid_V1D_id 
      masnum_state_variables(masnum_grid_id)%grid_3D_id    = grid_3D_id
!=======================================================================================================================================================================
      decomp_id      = CCPL_register_normal_parallel_decomp("decomp_masnum_grid", masnum_state_variables(masnum_grid_id)%grid_H2D_id, masnum_state_variables(masnum_grid_id)%num_local_cells, &
                                                       masnum_state_variables(masnum_grid_id)%local_cell_global_indexes, annotation="decompose masnum grid")
      masnum_state_variables(masnum_grid_id)%decomp_id      = decomp_id
!=======================================================================================================================================================================
      CALL allocate_coupling_buf_fields
      field_id_ssu      = CCPL_register_field_instance(masnum_state_variables(masnum_grid_id)%ssu_from_ocean, "ssu", masnum_state_variables(masnum_grid_id)%decomp_id, masnum_state_variables(masnum_grid_id)%grid_H2D_id, &
                                                      field_mark_ocn, usage_tag=CCPL_TAG_CPL_REST, field_unit="m s-1", annotation="register field instance of ssu_from_ocean")
      field_id_ssv      = CCPL_register_field_instance(masnum_state_variables(masnum_grid_id)%ssv_from_ocean, "ssv", masnum_state_variables(masnum_grid_id)%decomp_id, masnum_state_variables(masnum_grid_id)%grid_H2D_id, &
                                                       field_mark_ocn, usage_tag=CCPL_TAG_CPL_REST, field_unit="m s-1", annotation="register field instance of ssv_from_ocean")

      field_id_u10      = CCPL_register_field_instance(masnum_state_variables(masnum_grid_id)%u10_from_atm, "u10", masnum_state_variables(masnum_grid_id)%decomp_id, masnum_state_variables(masnum_grid_id)%grid_H2D_id, &
                                                       field_mark_atm, usage_tag=CCPL_TAG_CPL_REST, field_unit="m s-1", annotation="register field instance of u component wind speed at 10 meters")
      field_id_v10      = CCPL_register_field_instance(masnum_state_variables(masnum_grid_id)%v10_from_atm, "v10", masnum_state_variables(masnum_grid_id)%decomp_id, masnum_state_variables(masnum_grid_id)%grid_H2D_id, &
                                                       field_mark_atm, usage_tag=CCPL_TAG_CPL_REST, field_unit="m s-1", annotation="register field instance of v component wind speed at 10 meters")
      field_id_ust      = CCPL_register_field_instance(masnum_state_variables(masnum_grid_id)%ustar_from_atm, "ust", masnum_state_variables(masnum_grid_id)%decomp_id, masnum_state_variables(masnum_grid_id)%grid_H2D_id, &
                                                       field_mark_atm,usage_tag=CCPL_TAG_CPL_REST, field_unit="m s-1", annotation="register field instance of friction velocity")

      field_id_bv       = CCPL_register_field_instance(masnum_state_variables(masnum_grid_id)%bv, "bv", masnum_state_variables(masnum_grid_id)%decomp_id, masnum_state_variables(masnum_grid_id)%grid_3D_id, field_mark_wave, &
                                                       usage_tag=CCPL_TAG_CPL_REST, field_unit="unitless",annotation="register field instance of none breaking wave induced mixing coefficient")
      field_id_bbv      = CCPL_register_field_instance(masnum_state_variables(masnum_grid_id)%bbv, "bbv", masnum_state_variables(masnum_grid_id)%decomp_id, masnum_state_variables(masnum_grid_id)%grid_3D_id, field_mark_wave, &
                                                       usage_tag=CCPL_TAG_CPL_REST, field_unit="unitless", annotation="register field instance of breaking wave induced mixing coefficient")
      field_id_u_wave_stress   = CCPL_register_field_instance(masnum_state_variables(masnum_grid_id)%u_wave_stress, "tauwx", masnum_state_variables(masnum_grid_id)%decomp_id, masnum_state_variables(masnum_grid_id)%grid_H2D_id,&
                                                        field_mark_wave, usage_tag=CCPL_TAG_CPL_REST, field_unit="m s-1", annotation="register field instance of u component of wave induced stress")
      field_id_v_wave_stress   = CCPL_register_field_instance(masnum_state_variables(masnum_grid_id)%v_wave_stress, "tauwy", masnum_state_variables(masnum_grid_id)%decomp_id, masnum_state_variables(masnum_grid_id)%grid_H2D_id,&
                                                         field_mark_wave, usage_tag=CCPL_TAG_CPL_REST, field_unit="m s-1", annotation="register field instance of v component of wave induced stress")
      field_id_ustokes  = CCPL_register_field_instance(masnum_state_variables(masnum_grid_id)%u_stokes_srf, "u_stokes_srf", masnum_state_variables(masnum_grid_id)%decomp_id, masnum_state_variables(masnum_grid_id)%grid_H2D_id, &
                                                       field_mark_wave, usage_tag=CCPL_TAG_CPL_REST, field_unit="m s-1", annotation="register field instance of u component of stokes drift")
      field_id_vstokes  = CCPL_register_field_instance(masnum_state_variables(masnum_grid_id)%v_stokes_srf, "v_stokes_srf", masnum_state_variables(masnum_grid_id)%decomp_id, masnum_state_variables(masnum_grid_id)%grid_H2D_id, &
                                                       field_mark_wave, usage_tag=CCPL_TAG_CPL_REST, field_unit="m s-1", annotation="register field instance of v component of stokes drift")
      field_id_uorb     = CCPL_register_field_instance(masnum_state_variables(masnum_grid_id)%u_orbital_srf, "u_orbital_srf", masnum_state_variables(masnum_grid_id)%decomp_id, masnum_state_variables(masnum_grid_id)%grid_H2D_id,&
                                                       field_mark_wave, usage_tag=CCPL_TAG_CPL_REST, field_unit="m s-1", annotation="register field instance of u component of orbital speed")
      field_id_vorb     = CCPL_register_field_instance(masnum_state_variables(masnum_grid_id)%v_orbital_srf, "v_orbital_srf", masnum_state_variables(masnum_grid_id)%decomp_id, masnum_state_variables(masnum_grid_id)%grid_H2D_id,&
                                                       field_mark_wave, usage_tag=CCPL_TAG_CPL_REST, field_unit="m s-1", annotation="register field instance of v component of orbital speed")
      field_id_whitecap = CCPL_register_field_instance(masnum_state_variables(masnum_grid_id)%whitecap_fraction,"whitecap_fraction",masnum_state_variables(masnum_grid_id)%decomp_id, &
                                                       masnum_state_variables(masnum_grid_id)%grid_H2D_id,field_mark_wave, usage_tag=CCPL_TAG_CPL_REST, field_unit="unitless", annotation="register field instance of whitecape fraction")
      field_id_breakrate= CCPL_register_field_instance(masnum_state_variables(masnum_grid_id)%break_volume, "break_volume", masnum_state_variables(masnum_grid_id)%decomp_id, masnum_state_variables(masnum_grid_id)%grid_H2D_id, &
                                                       field_mark_wave, usage_tag=CCPL_TAG_CPL_REST, field_unit="m3 m-2 s-1 ", annotation="register field instance of breaking rate")
      field_id_hwave    = CCPL_register_field_instance(masnum_state_variables(masnum_grid_id)%significant_wave_height, "significant_wave_height", masnum_state_variables(masnum_grid_id)%decomp_id,&
                                                       masnum_state_variables(masnum_grid_id)%grid_H2D_id, field_mark_wave, usage_tag=CCPL_TAG_CPL_REST, field_unit="m", annotation="register field instance of significant wave height")
      field_id_lwavep   = CCPL_register_field_instance(masnum_state_variables(masnum_grid_id)%peak_wave_length, "peak_wave_length", masnum_state_variables(masnum_grid_id)%decomp_id,&
                                                       masnum_state_variables(masnum_grid_id)%grid_H2D_id,field_mark_wave, usage_tag=CCPL_TAG_CPL_REST, field_unit="m", annotation="register field instance of peak wave length")
      field_id_pwave    = CCPL_register_field_instance(masnum_state_variables(masnum_grid_id)%peak_wave_period, "peak_wave_period", masnum_state_variables(masnum_grid_id)%decomp_id, &
                                                       masnum_state_variables(masnum_grid_id)%grid_H2D_id, field_mark_wave, usage_tag=CCPL_TAG_CPL_REST, field_unit="second", annotation="register field instance of peak wave period")
      field_id_tau_in   = CCPL_register_field_instance(masnum_state_variables(masnum_grid_id)%tau_in, "tau_in", masnum_state_variables(masnum_grid_id)%decomp_id, masnum_state_variables(masnum_grid_id)%grid_H2D_id, field_mark_wave, &
                                                       usage_tag=CCPL_TAG_CPL_REST, field_unit="N", annotation="register field instance of wave induced stress")
!=======================================================================================================================================================================================================================================
!                                           Define a single timer that is a periodic timer for the given component model
      timer_id = CCPL_define_single_timer(comp_id, "seconds", 300, 0, 0, annotation="define a single timer for comp_id")
      !timer_id = CCPL_define_single_timer(comp_id, "steps", 1, 0, 0, annotation="define a single timer for comp_id")
      masnum_state_variables(masnum_grid_id)%timer_id = timer_id
!=======================================================================================================================================================================================================================================
      allocate(fields_id(13))
      fields_id(1)  = field_id_ssu
      fields_id(2)  = field_id_ssv
      import_interface_id = CCPL_register_import_interface("WAVE_receive_from_OCN", 2, fields_id, timer_id, 1, annotation="register interface for receiving data from OCN")
      fields_id(1)  = field_id_u10
      fields_id(2)  = field_id_v10
      fields_id(3)  = field_id_ust
      import_interface_id = CCPL_register_import_interface("WAVE_receive_from_ATM", 3, fields_id, timer_id, 1, annotation="register interface for receiving data from ATM")

      fields_id(1)  = field_id_ustokes
      fields_id(2)  = field_id_vstokes
      fields_id(3)  = field_id_uorb
      fields_id(4)  = field_id_vorb
      fields_id(5)  = field_id_whitecap
      fields_id(6)  = field_id_breakrate
      fields_id(7)  = field_id_hwave
      fields_id(8)  = field_id_lwavep
      fields_id(9)  = field_id_pwave
      fields_id(10) = field_id_tau_in
      export_interface_id = CCPL_register_export_interface("WAVE_send_to_ATM", 10, fields_id, timer_id, annotation="register interface for sending WAVE data to ATM")
      fields_id(1)  = field_id_bv
      fields_id(2)  = field_id_bbv
      fields_id(3)  = field_id_u_wave_stress
      fields_id(4)  = field_id_v_wave_stress
      export_interface_id = CCPL_register_export_interface("WAVE_send_to_OCN", 4, fields_id, timer_id, annotation="register interface for sending data to OCN")
      deallocate(fields_id)
!============================================ coupling generation ===============================================================================================================================================

      CALL CCPL_get_configurable_comps_full_names(comp_id, "external_comps_for_coupling_generation", num_comps, comps_full_names, individual_or_family, annotation="test CCPL_get_configurable_comps_full_names")
      CALL CCPL_do_external_coupling_generation(num_comps, comps_full_names, individual_or_family)

!============================================ check which model will be coupled =================================================================================================================================
      ocean_coupled  = .false.
      atm_coupled    = .false.
      ocean_coupled  = CCPL_is_comp_type_coupled(comp_id,"ocn", annotation="wave is coupling with ocean")
      atm_coupled    = CCPL_is_comp_type_coupled(comp_id,"atm", annotation="wave is coupling with atm")
!=======================================================================================================================================================================================================================================
      END SUBROUTINE register_component_coupling_configuration



      SUBROUTINE run_coupling
      implicit none
      integer               :: i
      integer               :: field_update_status(13)
      logical               :: interface_status, timer_status
      IF(masnum_state_variables(masnum_grid_id)%initialization)THEN
        CALL send_coupling_fields
        IF(atm_coupled)   interface_status = CCPL_execute_interface_using_name(masnum_state_variables(masnum_grid_id)%comp_id, "WAVE_send_to_ATM", .true., annotation="initialization,execute WAVE_send_to_ATM")
        IF(ocean_coupled) interface_status = CCPL_execute_interface_using_name(masnum_state_variables(masnum_grid_id)%comp_id, "WAVE_send_to_OCN", .true., annotation="initialization,execute WAVE_send_to_OCN")
          
        IF(atm_coupled)   interface_status = CCPL_execute_interface_using_name(masnum_state_variables(masnum_grid_id)%comp_id, "WAVE_receive_from_ATM", .true., field_update_status,annotation="initialize,execute WAVE_receive_from_ATM")
        IF(ocean_coupled) interface_status = CCPL_execute_interface_using_name(masnum_state_variables(masnum_grid_id)%comp_id, "WAVE_receive_from_OCN", .true., field_update_status,annotation="initialize,execute WAVE_receive_from_OCN")
        CALL receive_coupling_fields
      ELSE
        timer_status = CCPL_is_timer_on(masnum_state_variables(masnum_grid_id)%timer_id,annotation="check whether the timer is on")
        CALL send_coupling_fields
        IF(atm_coupled)   interface_status = CCPL_execute_interface_using_name(masnum_state_variables(masnum_grid_id)%comp_id, "WAVE_send_to_ATM", .false., annotation="integrate,execute WAVE_send_to_ATM")
        IF(ocean_coupled) interface_status = CCPL_execute_interface_using_name(masnum_state_variables(masnum_grid_id)%comp_id, "WAVE_send_to_OCN", .false., annotation="integrate,execute WAVE_send_to_OCN")

        IF(atm_coupled)   interface_status = CCPL_execute_interface_using_name(masnum_state_variables(masnum_grid_id)%comp_id, "WAVE_receive_from_ATM", .false., field_update_status,annotation="integrate,execute WAVE_receive_from_ATM")
        IF(ocean_coupled) interface_status = CCPL_execute_interface_using_name(masnum_state_variables(masnum_grid_id)%comp_id, "WAVE_receive_from_OCN", .false.,field_update_status,annotation="integrate,execute  WAVE_receive_from_OCN")
        IF(timer_status)THEN
        CALL receive_coupling_fields
        END IF
        CALL CCPL_advance_time(masnum_state_variables(masnum_grid_id)%comp_id)
        CALL CCPL_do_restart_write_IO(masnum_state_variables(masnum_grid_id)%comp_id,.false.)
      END IF
      masnum_state_variables(masnum_grid_id)%initialization = .false.

      END SUBROUTINE run_coupling




      subroutine send_coupling_fields
      implicit none
      integer i, j, k
      ! send fields !
      do j=ISLAT,IELAT
      do i=ISLON,IELON
      IF(NSP(i,j)==0)CYCLE 
      do k=1,KB 
        masnum_state_variables(masnum_grid_id)%bv(i,j,k) = BV(I,J,K)
        masnum_state_variables(masnum_grid_id)%bbv(i,j,k) = BV(I,J,K)
      end do
         masnum_state_variables(masnum_grid_id)%significant_wave_height(i,j)=H1_3(i,j)
         masnum_state_variables(masnum_grid_id)%peak_wave_period(i,j)=TPF(i,j)
         masnum_state_variables(masnum_grid_id)%whitecap_fraction(i,j)=RA(i,j)
         masnum_state_variables(masnum_grid_id)%u_stokes_srf(i,j)=USTOKES(i,j)
         masnum_state_variables(masnum_grid_id)%v_stokes_srf(i,j)=VSTOKES(i,j)
         masnum_state_variables(masnum_grid_id)%u_orbital_srf(i,j)=UORBITAL(i,j)
         masnum_state_variables(masnum_grid_id)%v_orbital_srf(i,j)=VORBITAL(i,j)
         masnum_state_variables(masnum_grid_id)%break_volume(i,j)=BBR(i,j)
         masnum_state_variables(masnum_grid_id)%peak_wave_length(i,j)=9.81*TPF(i,j)**2/(2*3.14)
         masnum_state_variables(masnum_grid_id)%u_wave_stress(i,j)=TAUINX(i,j)+TAUDSX(i,j)               !zhaobiao,tau_in_x+tau_diss_x,based on Janssen 2012
         masnum_state_variables(masnum_grid_id)%v_wave_stress(i,j)=TAUINY(i,j)+TAUDSY(i,j)               !zhaobiao,tau_in_y+tau_diss_y,based on Janssen 2012
         masnum_state_variables(masnum_grid_id)%tau_in(i,j)=SQRT(TAUINX(i,j)**2+TAUINY(i,j)**2)          !zhaobiao,scalar of wave-induced stress, based on Janssen 2012
      end do
      end do
      end subroutine send_coupling_fields

      subroutine receive_coupling_fields
      implicit none
      integer i, j, k
      ! send fields !
      do j=ISLAT,IELAT
         do i=ISLON,IELON
            if(NSP(i,j)==0)cycle
            ! receive fields !
              WX(i,j) = masnum_state_variables(masnum_grid_id)%u10_from_atm(I,J) 
              WY(i,j) = masnum_state_variables(masnum_grid_id)%v10_from_atm(I,J)
              UST(i,j)= masnum_state_variables(masnum_grid_id)%ustar_from_atm(I,J)  
            if(ISCURRENT) then
              UX(i,j) = masnum_state_variables(masnum_grid_id)%ssu_from_ocean(I,J)
              UY(i,j) = masnum_state_variables(masnum_grid_id)%ssv_from_ocean(I,J)
            end if
         end do
      end do
      if(ISCURRENT) then
        CALL COMM_DATA_2D(UX)
        CALL COMM_DATA_2D(UY)
        do j=ISLAT,IELAT
        do i=ISLON,IELON
        if(NSP(i,j)/=1)cycle
         !-------------------- inner points  ------------------------!
           UXX(i,j)=(UX(i+1,j)-UX(i-1,j))/(DX(i+1,j)+DX(I,J))
           UXY(i,j)=(UX(i,j+1)-UX(i,j-1))/(DY(i,j+1)+DY(I,J))
           UYX(i,j)=(UY(i+1,j)-UY(i-1,j))/(DX(i+1,j)+DX(I,J))
           UYY(i,j)=(UY(i,j+1)-UY(i,j-1))/(DY(i,j+1)+DY(I,J))
         !-----------------------------------------------------------!
        end do
        end do
      end if
      end subroutine receive_coupling_fields



      subroutine masnum_grid_parallel_decomposition
      implicit none
      integer                    :: num_local_rows, num_local_cols, num_local_cells
      integer                    :: num_global_rows, num_global_cols, num_global_cells
      integer                    ::  i, j, n
      num_local_rows = IELAT-ISLAT+1
      num_local_cols = IELON-ISLON+1
      num_local_cells = num_local_rows*num_local_cols
      num_global_rows = JM
      num_global_cols = IM
      num_global_cells = IM*JM
      masnum_state_variables(masnum_grid_id)%num_local_cells   = num_local_cells
      masnum_state_variables(masnum_grid_id)%num_global_cells  = num_global_cells

      IF (num_local_cells .gt. 0) THEN
         allocate(masnum_state_variables(masnum_grid_id)%local_cell_global_indexes(num_local_cells))
         allocate(masnum_state_variables(masnum_grid_id)%center_lats(num_local_cells))
         allocate(masnum_state_variables(masnum_grid_id)%center_lons(num_local_cells))
         allocate(masnum_state_variables(masnum_grid_id)%lons_vertexes(4,num_local_cells))
         allocate(masnum_state_variables(masnum_grid_id)%lats_vertexes(4,num_local_cells))
         allocate(masnum_state_variables(masnum_grid_id)%mask(num_local_cells))
         masnum_state_variables(masnum_grid_id)%mask= 0
         n = 0
         do j=ISLAT,IELAT
         do i=ISLON,IELON
              n = n+1
              masnum_state_variables(masnum_grid_id)%local_cell_global_indexes(n) = i+(j-1)*IM
              masnum_state_variables(masnum_grid_id)%center_lats(n) = YGRID(i,j)
              masnum_state_variables(masnum_grid_id)%center_lons(n) = XGRID(i,j)
              if(nsp(i,j).eq.1 .or. nsp(i,j).eq.2)then
                 masnum_state_variables(masnum_grid_id)%mask(n) = 1
              end if
!for iner points
              if(j.ne.1 .and. j.ne.JM .and. i.ne.1 .and. i.ne.IM) then
              masnum_state_variables(masnum_grid_id)%lons_vertexes(1,n) = (XGRID(i-1,j)+XGRID(i,j))/2 
              masnum_state_variables(masnum_grid_id)%lats_vertexes(1,n) = (YGRID(i,j-1)+YGRID(i,j))/2 

              masnum_state_variables(masnum_grid_id)%lons_vertexes(2,n) = (XGRID(i-1,j)+XGRID(i,j))/2 
              masnum_state_variables(masnum_grid_id)%lats_vertexes(2,n) = (YGRID(i,j+1)+YGRID(i,j))/2 

              masnum_state_variables(masnum_grid_id)%lons_vertexes(3,n) = (XGRID(i+1,j)+XGRID(i,j))/2 
              masnum_state_variables(masnum_grid_id)%lats_vertexes(3,n) = (YGRID(i,j+1)+YGRID(i,j))/2

              masnum_state_variables(masnum_grid_id)%lons_vertexes(4,n) = (XGRID(i+1,j)+XGRID(i,j))/2 
              masnum_state_variables(masnum_grid_id)%lats_vertexes(4,n) = (YGRID(i,j-1)+YGRID(i,j))/2

              end if
!for four boundary lines
              if(j.eq.1 .and. i.ne.1 .and. i.ne.IM) then
              masnum_state_variables(masnum_grid_id)%lons_vertexes(1,n) = XGRID(i,j)
              masnum_state_variables(masnum_grid_id)%lats_vertexes(1,n) = YGRID(i,j)

              masnum_state_variables(masnum_grid_id)%lons_vertexes(2,n) = (XGRID(i-1,j)+XGRID(i,j))/2 
              masnum_state_variables(masnum_grid_id)%lats_vertexes(2,n) = (YGRID(i,j+1)+YGRID(i,j))/2 

              masnum_state_variables(masnum_grid_id)%lons_vertexes(3,n) = (XGRID(i+1,j)+XGRID(i,j))/2 
              masnum_state_variables(masnum_grid_id)%lats_vertexes(3,n) = (YGRID(i,j+1)+YGRID(i,j))/2

              masnum_state_variables(masnum_grid_id)%lons_vertexes(4,n) = XGRID(i,j)
              masnum_state_variables(masnum_grid_id)%lats_vertexes(4,n) = YGRID(i,j)
              end if

              if(j.eq.JM .and. i.ne.1 .and. i.ne.IM) then
              masnum_state_variables(masnum_grid_id)%lons_vertexes(1,n) = (XGRID(i-1,j)+XGRID(i,j))/2 
              masnum_state_variables(masnum_grid_id)%lats_vertexes(1,n) = (YGRID(i,j-1)+YGRID(i,j))/2

              masnum_state_variables(masnum_grid_id)%lons_vertexes(2,n) = XGRID(i,j)
              masnum_state_variables(masnum_grid_id)%lats_vertexes(2,n) = YGRID(i,j)

              masnum_state_variables(masnum_grid_id)%lons_vertexes(3,n) = XGRID(i,j)
              masnum_state_variables(masnum_grid_id)%lats_vertexes(3,n) = YGRID(i,j)

              masnum_state_variables(masnum_grid_id)%lons_vertexes(4,n) = (XGRID(i+1,j)+XGRID(i,j))/2 
              masnum_state_variables(masnum_grid_id)%lats_vertexes(4,n) = (YGRID(i,j-1)+YGRID(i,j))/2
              end if

              if(i.eq.1 .and. j.ne.1 .and. j.ne.JM) then
              masnum_state_variables(masnum_grid_id)%lons_vertexes(1,n) = XGRID(i,j)
              masnum_state_variables(masnum_grid_id)%lats_vertexes(1,n) = YGRID(i,j)

              masnum_state_variables(masnum_grid_id)%lons_vertexes(2,n) = XGRID(i,j)
              masnum_state_variables(masnum_grid_id)%lats_vertexes(2,n) = YGRID(i,j)

              masnum_state_variables(masnum_grid_id)%lons_vertexes(3,n) = (XGRID(i+1,j)+XGRID(i,j))/2 
              masnum_state_variables(masnum_grid_id)%lats_vertexes(3,n) = (YGRID(i,j+1)+YGRID(i,j))/2

              masnum_state_variables(masnum_grid_id)%lons_vertexes(4,n) = (XGRID(i+1,j)+XGRID(i,j))/2 
              masnum_state_variables(masnum_grid_id)%lats_vertexes(4,n) = (YGRID(i,j-1)+YGRID(i,j))/2
              end if

              if(i.eq.IM .and. j.ne.1 .and. j.ne.JM) then
              masnum_state_variables(masnum_grid_id)%lons_vertexes(1,n) = (XGRID(i-1,j)+XGRID(i,j))/2 
              masnum_state_variables(masnum_grid_id)%lats_vertexes(1,n) = (YGRID(i,j-1)+YGRID(i,j))/2 

              masnum_state_variables(masnum_grid_id)%lons_vertexes(2,n) = (XGRID(i-1,j)+XGRID(i,j))/2 
              masnum_state_variables(masnum_grid_id)%lats_vertexes(2,n) = (YGRID(i,j+1)+YGRID(i,j))/2

              masnum_state_variables(masnum_grid_id)%lons_vertexes(3,n) = XGRID(i,j)
              masnum_state_variables(masnum_grid_id)%lats_vertexes(3,n) = YGRID(i,j)

              masnum_state_variables(masnum_grid_id)%lons_vertexes(4,n) = XGRID(i,j)
              masnum_state_variables(masnum_grid_id)%lats_vertexes(4,n) = YGRID(i,j)
              end if

! for four corner points
              if(j.eq.1 .and. i.eq.1) then
              masnum_state_variables(masnum_grid_id)%lons_vertexes(1,n) = XGRID(i,j)
              masnum_state_variables(masnum_grid_id)%lats_vertexes(1,n) = YGRID(i,j)

              masnum_state_variables(masnum_grid_id)%lons_vertexes(2,n) = XGRID(i,j)
              masnum_state_variables(masnum_grid_id)%lats_vertexes(2,n) = YGRID(i,j)

              masnum_state_variables(masnum_grid_id)%lons_vertexes(3,n) = (XGRID(i+1,j)+XGRID(i,j))/2 
              masnum_state_variables(masnum_grid_id)%lats_vertexes(3,n) = (YGRID(i,j+1)+YGRID(i,j))/2

              masnum_state_variables(masnum_grid_id)%lons_vertexes(4,n) = XGRID(i,j)
              masnum_state_variables(masnum_grid_id)%lats_vertexes(4,n) = YGRID(i,j)
              end if

              if(j.eq.JM .and. i.eq.1) then
              masnum_state_variables(masnum_grid_id)%lons_vertexes(1,n) = XGRID(i,j)
              masnum_state_variables(masnum_grid_id)%lats_vertexes(1,n) = YGRID(i,j)

              masnum_state_variables(masnum_grid_id)%lons_vertexes(2,n) = XGRID(i,j)
              masnum_state_variables(masnum_grid_id)%lats_vertexes(2,n) = YGRID(i,j)

              masnum_state_variables(masnum_grid_id)%lons_vertexes(3,n) = XGRID(i,j)
              masnum_state_variables(masnum_grid_id)%lats_vertexes(3,n) = YGRID(i,j)

              masnum_state_variables(masnum_grid_id)%lons_vertexes(4,n) = (XGRID(i+1,j)+XGRID(i,j))/2 
              masnum_state_variables(masnum_grid_id)%lats_vertexes(4,n) = (YGRID(i,j-1)+YGRID(i,j))/2
              end if

              if(j.eq.1 .and. i.eq.IM) then
              masnum_state_variables(masnum_grid_id)%lons_vertexes(1,n) = XGRID(i,j)
              masnum_state_variables(masnum_grid_id)%lats_vertexes(1,n) = YGRID(i,j)

              masnum_state_variables(masnum_grid_id)%lons_vertexes(2,n) = (XGRID(i-1,j)+XGRID(i,j))/2 
              masnum_state_variables(masnum_grid_id)%lats_vertexes(2,n) = (YGRID(i,j+1)+YGRID(i,j))/2

              masnum_state_variables(masnum_grid_id)%lons_vertexes(3,n) = XGRID(i,j)
              masnum_state_variables(masnum_grid_id)%lats_vertexes(3,n) = YGRID(i,j)

              masnum_state_variables(masnum_grid_id)%lons_vertexes(4,n) = XGRID(i,j)
              masnum_state_variables(masnum_grid_id)%lats_vertexes(4,n) = YGRID(i,j)
              end if

              if(j.eq.JM .and. i.eq.IM) then
              masnum_state_variables(masnum_grid_id)%lons_vertexes(1,n) = (XGRID(i-1,j)+XGRID(i,j))/2 
              masnum_state_variables(masnum_grid_id)%lats_vertexes(1,n) = (YGRID(i,j-1)+YGRID(i,j))/2

              masnum_state_variables(masnum_grid_id)%lons_vertexes(2,n) = XGRID(i,j)
              masnum_state_variables(masnum_grid_id)%lats_vertexes(2,n) = YGRID(i,j)

              masnum_state_variables(masnum_grid_id)%lons_vertexes(3,n) = XGRID(i,j)
              masnum_state_variables(masnum_grid_id)%lats_vertexes(3,n) = YGRID(i,j)

              masnum_state_variables(masnum_grid_id)%lons_vertexes(4,n) = XGRID(i,j)
              masnum_state_variables(masnum_grid_id)%lats_vertexes(4,n) = YGRID(i,j)
              end if
              !write(6,*) "center_lon",masnum_state_variables(masnum_grid_id)%center_lons(n)
              !write(6,*) "vertexes_lon",masnum_state_variables(masnum_grid_id)%lons_vertexes(:,n) 
         end do
         end do
      END IF
      end subroutine masnum_grid_parallel_decomposition
      



      subroutine allocate_coupling_buf_fields
      implicit none
      ALLOCATE(masnum_state_variables(masnum_grid_id)%bv(ISLON:IELON,ISLAT:IELAT,KB))
      ALLOCATE(masnum_state_variables(masnum_grid_id)%bbv(ISLON:IELON,ISLAT:IELAT,KB))
      ALLOCATE(masnum_state_variables(masnum_grid_id)%ssu_from_ocean(ISLON:IELON,ISLAT:IELAT))
      ALLOCATE(masnum_state_variables(masnum_grid_id)%ssv_from_ocean(ISLON:IELON,ISLAT:IELAT))
      ALLOCATE(masnum_state_variables(masnum_grid_id)%ustar_from_atm(ISLON:IELON,ISLAT:IELAT))
      ALLOCATE(masnum_state_variables(masnum_grid_id)%u10_from_atm(ISLON:IELON,ISLAT:IELAT))
      ALLOCATE(masnum_state_variables(masnum_grid_id)%v10_from_atm(ISLON:IELON,ISLAT:IELAT))
      ALLOCATE(masnum_state_variables(masnum_grid_id)%whitecap_fraction(ISLON:IELON,ISLAT:IELAT))
      ALLOCATE(masnum_state_variables(masnum_grid_id)%u_stokes_srf(ISLON:IELON,ISLAT:IELAT))
      ALLOCATE(masnum_state_variables(masnum_grid_id)%v_stokes_srf(ISLON:IELON,ISLAT:IELAT))
      ALLOCATE(masnum_state_variables(masnum_grid_id)%u_orbital_srf(ISLON:IELON,ISLAT:IELAT))
      ALLOCATE(masnum_state_variables(masnum_grid_id)%v_orbital_srf(ISLON:IELON,ISLAT:IELAT))
      ALLOCATE(masnum_state_variables(masnum_grid_id)%break_volume(ISLON:IELON,ISLAT:IELAT))
      ALLOCATE(masnum_state_variables(masnum_grid_id)%significant_wave_height(ISLON:IELON,ISLAT:IELAT))
      ALLOCATE(masnum_state_variables(masnum_grid_id)%peak_wave_length(ISLON:IELON,ISLAT:IELAT))
      ALLOCATE(masnum_state_variables(masnum_grid_id)%peak_wave_period(ISLON:IELON,ISLAT:IELAT))
      ALLOCATE(masnum_state_variables(masnum_grid_id)%u_wave_stress(ISLON:IELON,ISLAT:IELAT))
      ALLOCATE(masnum_state_variables(masnum_grid_id)%v_wave_stress(ISLON:IELON,ISLAT:IELAT))
      ALLOCATE(masnum_state_variables(masnum_grid_id)%tau_in(ISLON:IELON,ISLAT:IELAT))    
      masnum_state_variables(masnum_grid_id)%bv=0.0
      masnum_state_variables(masnum_grid_id)%bbv=0.0
      masnum_state_variables(masnum_grid_id)%ssu_from_ocean = 0.0
      masnum_state_variables(masnum_grid_id)%ssv_from_ocean = 0.0
      masnum_state_variables(masnum_grid_id)%u10_from_atm = 0.0
      masnum_state_variables(masnum_grid_id)%v10_from_atm = 0.0
      masnum_state_variables(masnum_grid_id)%ustar_from_atm = 0.0
      masnum_state_variables(masnum_grid_id)%whitecap_fraction = 0.0
      masnum_state_variables(masnum_grid_id)%u_stokes_srf = 0.0
      masnum_state_variables(masnum_grid_id)%v_stokes_srf = 0.0
      masnum_state_variables(masnum_grid_id)%u_orbital_srf = 0.0
      masnum_state_variables(masnum_grid_id)%v_orbital_srf = 0.0
      masnum_state_variables(masnum_grid_id)%break_volume  = 0.0
      masnum_state_variables(masnum_grid_id)%significant_wave_height = 0.0
      masnum_state_variables(masnum_grid_id)%peak_wave_length = 0.0
      masnum_state_variables(masnum_grid_id)%peak_wave_period = 0.0
      masnum_state_variables(masnum_grid_id)%u_wave_stress = 0.0
      masnum_state_variables(masnum_grid_id)%v_wave_stress = 0.0
      masnum_state_variables(masnum_grid_id)%tau_in = 0.0

      end subroutine allocate_coupling_buf_fields


      SUBROUTINE release_masnum_buf
      implicit none
      IF(ALLOCATED(masnum_state_variables(masnum_grid_id)%bv))  DEALLOCATE(masnum_state_variables(masnum_grid_id)%bv)
      IF(ALLOCATED(masnum_state_variables(masnum_grid_id)%bbv)) DEALLOCATE(masnum_state_variables(masnum_grid_id)%bbv)
      IF(ALLOCATED(masnum_state_variables(masnum_grid_id)%ssu_from_ocean)) DEALLOCATE(masnum_state_variables(masnum_grid_id)%ssu_from_ocean)
      IF(ALLOCATED(masnum_state_variables(masnum_grid_id)%ssv_from_ocean)) DEALLOCATE(masnum_state_variables(masnum_grid_id)%ssv_from_ocean)
      IF(ALLOCATED(masnum_state_variables(masnum_grid_id)%u10_from_atm)) DEALLOCATE(masnum_state_variables(masnum_grid_id)%u10_from_atm)
      IF(ALLOCATED(masnum_state_variables(masnum_grid_id)%v10_from_atm)) DEALLOCATE(masnum_state_variables(masnum_grid_id)%v10_from_atm)
      IF(ALLOCATED(masnum_state_variables(masnum_grid_id)%ustar_from_atm)) DEALLOCATE(masnum_state_variables(masnum_grid_id)%ustar_from_atm)
      IF(ALLOCATED(masnum_state_variables(masnum_grid_id)%whitecap_fraction)) DEALLOCATE(masnum_state_variables(masnum_grid_id)%whitecap_fraction)
      IF(ALLOCATED(masnum_state_variables(masnum_grid_id)%u_stokes_srf)) DEALLOCATE(masnum_state_variables(masnum_grid_id)%u_stokes_srf)
      IF(ALLOCATED(masnum_state_variables(masnum_grid_id)%v_stokes_srf)) DEALLOCATE(masnum_state_variables(masnum_grid_id)%v_stokes_srf)
      IF(ALLOCATED(masnum_state_variables(masnum_grid_id)%u_stokes_srf)) DEALLOCATE(masnum_state_variables(masnum_grid_id)%u_orbital_srf)
      IF(ALLOCATED(masnum_state_variables(masnum_grid_id)%v_stokes_srf)) DEALLOCATE(masnum_state_variables(masnum_grid_id)%v_orbital_srf)
      IF(ALLOCATED(masnum_state_variables(masnum_grid_id)%break_volume)) DEALLOCATE(masnum_state_variables(masnum_grid_id)%break_volume)
      IF(ALLOCATED(masnum_state_variables(masnum_grid_id)%significant_wave_height)) DEALLOCATE(masnum_state_variables(masnum_grid_id)%significant_wave_height)
      IF(ALLOCATED(masnum_state_variables(masnum_grid_id)%peak_wave_length)) DEALLOCATE(masnum_state_variables(masnum_grid_id)%peak_wave_length)
      IF(ALLOCATED(masnum_state_variables(masnum_grid_id)%peak_wave_period)) DEALLOCATE(masnum_state_variables(masnum_grid_id)%peak_wave_period)
      IF(ALLOCATED(masnum_state_variables(masnum_grid_id)%u_wave_stress)) DEALLOCATE(masnum_state_variables(masnum_grid_id)%u_wave_stress)
      IF(ALLOCATED(masnum_state_variables(masnum_grid_id)%v_wave_stress)) DEALLOCATE(masnum_state_variables(masnum_grid_id)%v_wave_stress)
      IF(ALLOCATED(masnum_state_variables(masnum_grid_id)%tau_in)) DEALLOCATE(masnum_state_variables(masnum_grid_id)%tau_in)
      IF(ALLOCATED(masnum_state_variables(masnum_grid_id)%local_cell_global_indexes)) DEALLOCATE(masnum_state_variables(masnum_grid_id)%local_cell_global_indexes)
      IF(ALLOCATED(masnum_state_variables(masnum_grid_id)%center_lats)) DEALLOCATE(masnum_state_variables(masnum_grid_id)%center_lats)
      IF(ALLOCATED(masnum_state_variables(masnum_grid_id)%center_lons)) DEALLOCATE(masnum_state_variables(masnum_grid_id)%center_lons)
      IF(ALLOCATED(masnum_state_variables(masnum_grid_id)%mask)) DEALLOCATE(masnum_state_variables(masnum_grid_id)%mask)
      END  SUBROUTINE release_masnum_buf

      END MODULE coupling_wave_mod
