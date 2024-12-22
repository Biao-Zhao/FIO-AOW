#include "cppdefs.h"
MODULE coupling_roms_mod
      USE CCPL_interface_mod                       
      USE mod_iounits
      USE mod_parallel, ONLY: OCN_COMM_WORLD,Master,MyRank
      USE mod_scalars, ONLY: dt, itemp, isalt, Cp, rho0, EWperiodic, NSperiodic, StefBo, emmiss, SCALARS, Vtransform,hc
      USE mod_param, ONLY: BOUNDS, Lm, Mm, N, DOMAIN, iNLM, NghostPoints
      USE mod_grid, ONLY: GRID
      USE mod_ocean, ONLY: OCEAN
      USE mod_stepping, ONLY: nrhs,nstp
      USE mod_kinds, ONLY: r8
      USE mod_forces, ONLY: FORCES
      USE exchange_2d_mod, ONLY : exchange_r2d_tile, exchange_u2d_tile, exchange_v2d_tile 
      USE exchange_3d_mod, ONLY : exchange_w3d_tile
#ifdef DISTRIBUTE
      USE mp_exchange_mod, ONLY : mp_exchange2d, mp_exchange3d
#endif
      implicit none
      type, private :: roms_state_var
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
      real(r8),     allocatable :: center_lats(:), center_lons(:)
      real(r8),     allocatable :: lats_vertexes(:,:), lons_vertexes(:,:)
      integer,  allocatable :: mask(:)
      logical               :: initialization
      real(r8), allocatable :: topography(:, :)
      real(r8), allocatable :: z_w(:, :, :)
      real(r8), allocatable :: sst(:, :)
      real(r8), allocatable :: sss(:, :)
      real(r8), allocatable :: ssv(:, :)
      real(r8), allocatable :: ssu(:, :)
      real(r8), allocatable :: bv_from_wave(:, :, :)
      real(r8), allocatable :: bbv_from_wave(:, :, :)
      real(r8), allocatable :: tauwx_from_wave(:, :)
      real(r8), allocatable :: tauwy_from_wave(:, :)
      real(r8), allocatable :: u10_from_atm(:, :)
      real(r8), allocatable :: v10_from_atm(:, :)
      real(r8), allocatable :: rhoa_from_atm(:, :)
      real(r8), allocatable :: ust_from_atm(:, :)
      real(r8), allocatable :: t2_from_atm(:, :)
      real(r8), allocatable :: q2_from_atm(:, :)
      real(r8), allocatable :: qfx_from_atm(:, :)
      real(r8), allocatable :: LT_from_atm(:, :)
      real(r8), allocatable :: ST_from_atm(:, :)
      real(r8), allocatable :: SLP_from_atm(:, :)
      real(r8), allocatable :: rain_from_atm(:, :)
      real(r8), allocatable :: DSW_from_atm(:, :)
      real(r8), allocatable :: LW_from_atm(:, :)
      real(r8), allocatable :: ssu_rho(:,:)
      real(r8), allocatable :: ssv_rho(:,:)
      end type roms_state_var

      type(roms_state_var), public :: roms_state_variables(100)
      integer,              public :: roms_frame_id
      logical,              public :: atm_coupled,wave_coupled     
      character(len=512),   public :: log_file_name
      logical,              public :: ocean_log
      contains
!
      SUBROUTINE register_roms_frame_coupling_configuration
      implicit none
      integer             :: parent_comp_id
      parent_comp_id      = -1
      OCN_COMM_WORLD      = CCPL_NULL_COMM
      roms_frame_id       = CCPL_register_component(parent_comp_id, "roms","ocn", OCN_COMM_WORLD,.false.,change_dir=.true., annotation= "register ROMS FRAME to c-coupler")
      ocean_log            = .false.
      ocean_log            = CCPL_get_comp_log_file_name(roms_frame_id,log_file_name, annotation="get the ocean logfile name of roms")
      open(stdout,file=trim(log_file_name),status="UNKNOWN")
      END SUBROUTINE register_roms_frame_coupling_configuration
!
!
!
      SUBROUTINE register_component_coupling_configuration (ng, parent_ng, tile)
      implicit none
      integer, intent(in) :: ng, parent_ng, tile
      integer               :: roms_grid_id
      integer               :: parent_comp_id
      integer               :: comp_id
      character(len=1024)   :: annotation
      character(len=80)     :: comp_name
      character(len=20)     :: nestid
      integer               :: grid_H2D_id, grid_V1D_id, grid_3D_id
      integer               :: decomp_id
      integer               :: field_mark_ocn, field_mark_wave, field_mark_atm
      integer               :: field_id_z_w, field_id_topo,field_id_sss, field_id_sst, field_id_ssu, field_id_ssv
      integer               :: field_id_bv, field_id_bbv, field_id_tauwx, field_id_tauwy
      integer               :: field_id_u10, field_id_v10, field_id_ust, field_id_rhoa, field_id_qfx, field_id_sh, field_id_lh, field_id_t2, field_id_q2, field_id_rain, field_id_mslp, field_id_swdown, field_id_lwdown      
      integer               :: timer_id, import_interface_id, export_interface_id
      integer, allocatable  :: fields_id(:)
      integer               :: num_comps, individual_or_family(12)
      character(len=1024)   :: comps_full_names(12)      
      field_mark_ocn  = 0
      field_mark_wave = 1
      field_mark_atm  = 2
!==============================================Register the component model using CCPL_register_component =====================================================================================
      IF(ng .eq. 1)THEN
        write(nestid,"(I2)") ng
        annotation        =   "component "//"ROMS_TOP_d0"//trim(adjustl(nestid))//" start registration"
        parent_comp_id    =   roms_frame_id
        comp_name         =   "ROMS_TOP_d0"//trim(adjustl(nestid))
      ELSE
        write(nestid,"(I2)") ng
        annotation        =   "component "//"ROMS_NEST_d0"//trim(adjustl(nestid))//" start registration"
        parent_comp_id    =   roms_state_variables(parent_ng)%comp_id        
        comp_name         =   "ROMS_NEST_d0"//trim(adjustl(nestid))
      END IF
      comp_id     =   CCPL_register_component(parent_comp_id, comp_name, "ocn", OCN_COMM_WORLD,change_dir=.true., annotation=annotation)
      roms_grid_id = ng
      roms_state_variables(roms_grid_id)%comp_id        = comp_id
      roms_state_variables(roms_grid_id)%time_step      = dt(ng)
      roms_state_variables(roms_grid_id)%parent_comp_id = parent_comp_id
      roms_state_variables(roms_grid_id)%initialization = .true.
!===============================================================================================================================================================================================
     
      CALL CCPL_set_normal_time_step(comp_id, roms_state_variables(roms_grid_id)%time_step)

!=============================================== Register roms H2D grid using CCPL_register_H2D_grid_via_local_data =============================================================================
      CALL roms_grid_parallel_decomposition(ng,tile)
      grid_H2D_id    =   CCPL_register_H2D_grid_via_local_data(comp_id, "roms_grid_via_local", "LON_LAT", "degrees", "acyclic", roms_state_variables(roms_grid_id)%num_global_cells, roms_state_variables(roms_grid_id)%num_local_cells, roms_state_variables(roms_grid_id)%local_cell_global_indexes, -999999., -999999., -999999., -999999., roms_state_variables(roms_grid_id)%center_lons, roms_state_variables(roms_grid_id)%center_lats, roms_state_variables(roms_grid_id)%mask, annotation="register roms  H2D  grid")

      !grid_H2D_id    =   CCPL_register_H2D_grid_via_local_data(comp_id, "roms_grid_via_local", "LON_LAT", "degrees", "acyclic", roms_state_variables(roms_grid_id)%num_global_cells, roms_state_variables(roms_grid_id)%num_local_cells, roms_state_variables(roms_grid_id)%local_cell_global_indexes, -999999., -999999., -999999., -999999., roms_state_variables(roms_grid_id)%center_lons, roms_state_variables(roms_grid_id)%center_lats, mask=roms_state_variables(roms_grid_id)%mask, vertex_lon=roms_state_variables(roms_grid_id)%lons_vertexes, vertex_lat=roms_state_variables(roms_grid_id)%lats_vertexes,annotation="register roms H2D grid")


      roms_state_variables(roms_grid_id)%grid_H2D_id    = grid_H2D_id
      !3D decomposition and register
      grid_V1D_id     =  CCPL_register_V1D_grid_without_data(comp_id, "roms_V1D_grid", "meter", N(ng)+1, annotation="register a v1d grid for roms")
      grid_3D_id      =  CCPL_register_MD_grid_via_multi_grids(comp_id, "roms_slevel_3D_grid", grid_H2D_id, grid_V1D_id, annotation="register a 3-d grid for roms")

      roms_state_variables(roms_grid_id)%grid_V1D_id    = grid_V1D_id
      roms_state_variables(roms_grid_id)%grid_3D_id    = grid_3D_id


!================================================================================================================================================================================================
      decomp_id      =   CCPL_register_normal_parallel_decomp("decomp_roms_grid", roms_state_variables(roms_grid_id)%grid_H2D_id, roms_state_variables(roms_grid_id)%num_local_cells, roms_state_variables(roms_grid_id)%local_cell_global_indexes, annotation="decompose roms grid")
      roms_state_variables(roms_grid_id)%decomp_id      = decomp_id
!================================================================================================================================================================================================
      CALL allocate_coupling_buf_fields(ng,tile)
      !field_id_topo       = CCPL_register_field_instance(roms_state_variables(roms_grid_id)%topography,"topography", roms_state_variables(roms_grid_id)%decomp_id, roms_state_variables(roms_grid_id)%grid_H2D_id, field_mark_ocn, usage_tag=CCPL_TAG_CPL_REST,field_unit="meter",annotation="register field instance of topo")
      field_id_z_w       = CCPL_register_field_instance(roms_state_variables(roms_grid_id)%z_w,"z_w",roms_state_variables(roms_grid_id)%decomp_id,roms_state_variables(roms_grid_id)%grid_3D_id, field_mark_ocn,usage_tag=CCPL_TAG_CPL_REST,field_unit="meter",annotation="register field instance of z_w")

      !CALL CCPL_set_3D_grid_constant_surface_field(roms_state_variables(roms_grid_id)%grid_3D_id, field_id_topo,annotation="set bottom field of a 3-D grid")
      CAll  CCPL_set_3D_grid_3D_vertical_coord_field(roms_state_variables(roms_grid_id)%grid_3D_id, field_id_z_w, "variable",annotation="set variable depth of every layer in vertical")

      field_id_sss      = CCPL_register_field_instance(roms_state_variables(roms_grid_id)%SSS, "sss", roms_state_variables(roms_grid_id)%decomp_id, roms_state_variables(roms_grid_id)%grid_H2D_id, field_mark_ocn, usage_tag=CCPL_TAG_CPL_REST, field_unit="Psu", annotation="register field instance of SSS")
      field_id_sst      = CCPL_register_field_instance(roms_state_variables(roms_grid_id)%SST, "sst", roms_state_variables(roms_grid_id)%decomp_id, roms_state_variables(roms_grid_id)%grid_H2D_id, field_mark_ocn, usage_tag=CCPL_TAG_CPL_REST, field_unit="Celsius", annotation="register field instance of SST")
      field_id_ssu      = CCPL_register_field_instance(roms_state_variables(roms_grid_id)%SSU, "ssu", roms_state_variables(roms_grid_id)%decomp_id, roms_state_variables(roms_grid_id)%grid_H2D_id, field_mark_ocn, usage_tag=CCPL_TAG_CPL_REST, field_unit="m s-1", annotation="register field instance of SSU")
      field_id_ssv      = CCPL_register_field_instance(roms_state_variables(roms_grid_id)%SSV, "ssv", roms_state_variables(roms_grid_id)%decomp_id, roms_state_variables(roms_grid_id)%grid_H2D_id, field_mark_ocn, usage_tag=CCPL_TAG_CPL_REST, field_unit="m s-1", annotation="register field instance of SSV")

      field_id_u10      = CCPL_register_field_instance(roms_state_variables(roms_grid_id)%u10_from_atm, "u10", roms_state_variables(roms_grid_id)%decomp_id, roms_state_variables(roms_grid_id)%grid_H2D_id, field_mark_atm, usage_tag=CCPL_TAG_CPL_REST, field_unit="m s-1", annotation="register field instance of u component wind speed at 10 meters")
      field_id_v10      = CCPL_register_field_instance(roms_state_variables(roms_grid_id)%v10_from_atm, "v10", roms_state_variables(roms_grid_id)%decomp_id, roms_state_variables(roms_grid_id)%grid_H2D_id, field_mark_atm, usage_tag=CCPL_TAG_CPL_REST, field_unit="m s-1", annotation="register field instance of v component wind speed at 10 meters")
      field_id_ust      = CCPL_register_field_instance(roms_state_variables(roms_grid_id)%ust_from_atm, "ust", roms_state_variables(roms_grid_id)%decomp_id, roms_state_variables(roms_grid_id)%grid_H2D_id, field_mark_atm, usage_tag=CCPL_TAG_CPL_REST, field_unit="m s-1", annotation="register field instance of friction velocity")
      field_id_rhoa     = CCPL_register_field_instance(roms_state_variables(roms_grid_id)%rhoa_from_atm, "rhoa", roms_state_variables(roms_grid_id)%decomp_id, roms_state_variables(roms_grid_id)%grid_H2D_id, field_mark_atm, usage_tag=CCPL_TAG_CPL_REST, field_unit="kg m-3", annotation="register field instance of surface air density")
      field_id_t2       = CCPL_register_field_instance(roms_state_variables(roms_grid_id)%t2_from_atm, "t2", roms_state_variables(roms_grid_id)%decomp_id, roms_state_variables(roms_grid_id)%grid_H2D_id, field_mark_atm, usage_tag=CCPL_TAG_CPL_REST, field_unit="K", annotation="register field instance of air temperature at 2 meters")
      field_id_q2       = CCPL_register_field_instance(roms_state_variables(roms_grid_id)%q2_from_atm, "q2", roms_state_variables(roms_grid_id)%decomp_id, roms_state_variables(roms_grid_id)%grid_H2D_id, field_mark_atm, usage_tag=CCPL_TAG_CPL_REST, field_unit="kg kg-1", annotation="register field instance of air specific humidity at 2 meters")
      field_id_qfx      = CCPL_register_field_instance(roms_state_variables(roms_grid_id)%qfx_from_atm, "qfx", roms_state_variables(roms_grid_id)%decomp_id, roms_state_variables(roms_grid_id)%grid_H2D_id, field_mark_atm, usage_tag=CCPL_TAG_CPL_REST, field_unit="kg m-2 s-1", annotation="register field instance of surface moisture flux")
      field_id_sh       = CCPL_register_field_instance(roms_state_variables(roms_grid_id)%ST_from_atm, "sh", roms_state_variables(roms_grid_id)%decomp_id, roms_state_variables(roms_grid_id)%grid_H2D_id, field_mark_atm, usage_tag=CCPL_TAG_CPL_REST, field_unit="W m-2 ", annotation="register field instance of sensible heat flux")
      field_id_lh       = CCPL_register_field_instance(roms_state_variables(roms_grid_id)%LT_from_atm, "lh", roms_state_variables(roms_grid_id)%decomp_id, roms_state_variables(roms_grid_id)%grid_H2D_id, field_mark_atm, usage_tag=CCPL_TAG_CPL_REST, field_unit="W m-2", annotation="register field instance of latent heat flux")
      field_id_rain     = CCPL_register_field_instance(roms_state_variables(roms_grid_id)%rain_from_atm, "rain", roms_state_variables(roms_grid_id)%decomp_id, roms_state_variables(roms_grid_id)%grid_H2D_id, field_mark_atm, usage_tag=CCPL_TAG_CPL_REST, field_unit="mm s-1", annotation="register field instance of  precipitation")
      field_id_mslp     = CCPL_register_field_instance(roms_state_variables(roms_grid_id)%SLP_from_atm, "mslp", roms_state_variables(roms_grid_id)%decomp_id, roms_state_variables(roms_grid_id)%grid_H2D_id, field_mark_atm, usage_tag=CCPL_TAG_CPL_REST, field_unit="Pasca", annotation="register field instance of mean sea level pressure")
      field_id_swdown   = CCPL_register_field_instance(roms_state_variables(roms_grid_id)%DSW_from_atm, "swdown", roms_state_variables(roms_grid_id)%decomp_id, roms_state_variables(roms_grid_id)%grid_H2D_id, field_mark_atm, usage_tag=CCPL_TAG_CPL_REST, field_unit="W m-2", annotation="register field instance of net short wave flux")
      field_id_lwdown   = CCPL_register_field_instance(roms_state_variables(roms_grid_id)%LW_from_atm, "lwdown", roms_state_variables(roms_grid_id)%decomp_id, roms_state_variables(roms_grid_id)%grid_H2D_id, field_mark_atm, usage_tag=CCPL_TAG_CPL_REST, field_unit="W m-2", annotation="register field instance of downward  long wave flux")

      field_id_bv      = CCPL_register_field_instance(roms_state_variables(roms_grid_id)%bv_from_wave, "bv", roms_state_variables(roms_grid_id)%decomp_id, roms_state_variables(roms_grid_id)%grid_3D_id, field_mark_wave, usage_tag=CCPL_TAG_CPL_REST, field_unit="meter", annotation="register field instance of none breaking wave induced mixing coefficient")
      field_id_bbv     = CCPL_register_field_instance(roms_state_variables(roms_grid_id)%bbv_from_wave, "bbv", roms_state_variables(roms_grid_id)%decomp_id, roms_state_variables(roms_grid_id)%grid_3D_id, field_mark_wave, usage_tag=CCPL_TAG_CPL_REST, field_unit="meter", annotation="register field instance of breaking wave induced mixing coefficient")
      field_id_tauwx   = CCPL_register_field_instance(roms_state_variables(roms_grid_id)%tauwx_from_wave, "tauwx", roms_state_variables(roms_grid_id)%decomp_id,roms_state_variables(roms_grid_id)%grid_H2D_id,field_mark_wave, usage_tag=CCPL_TAG_CPL_REST, field_unit="m s-1", annotation="register field instance of u component of wave induced stress")
      field_id_tauwy   = CCPL_register_field_instance(roms_state_variables(roms_grid_id)%tauwy_from_wave, "tauwy", roms_state_variables(roms_grid_id)%decomp_id,roms_state_variables(roms_grid_id)%grid_H2D_id,field_mark_wave,  usage_tag=CCPL_TAG_CPL_REST, field_unit="m s-1", annotation="register field instance of v component of wave induced stress")
!================================================================================================================================================================================================
!                                                    Define a single timer that  is a periodic timer for a given component model
      timer_id = CCPL_define_single_timer(comp_id, "seconds", 3600, 0, 0, annotation="define a single timer for roms")
      !timer_id = CCPL_define_single_timer(comp_id, "steps", 1, 0, 0, annotation="define a single timer for comp_id")
      roms_state_variables(roms_grid_id)%timer_id        = timer_id

!================================================================================================================================================================================================
      allocate(fields_id(13))
      fields_id(1)  = field_id_ssu
      fields_id(2)  = field_id_ssv
      fields_id(3)  = field_id_sss
      fields_id(4)  = field_id_sst
      export_interface_id = CCPL_register_export_interface("OCN_send_to_ATM", 4, fields_id, timer_id, annotation="register interface for sending OCN data to ATM")
      export_interface_id = CCPL_register_export_interface("OCN_send_to_WAVE", 2, fields_id, timer_id, annotation="register interface for sending OCN data to WAVE")

      fields_id(1)  = field_id_u10
      fields_id(2)  = field_id_v10
      fields_id(3)  = field_id_ust
      fields_id(4)  = field_id_rain
      fields_id(5)  = field_id_t2
      fields_id(6)  = field_id_q2
      fields_id(7)  = field_id_qfx
      fields_id(8)  = field_id_sh
      fields_id(9)  = field_id_lh
      fields_id(10) = field_id_rhoa
      fields_id(11) = field_id_mslp
      fields_id(12) = field_id_swdown
      fields_id(13) = field_id_lwdown
      import_interface_id = CCPL_register_import_interface("OCN_receive_from_ATM", 13, fields_id, timer_id, 1, annotation="register interface for receiving data from ATM") 

      fields_id(1)  = field_id_bv
      fields_id(2)  = field_id_bbv
      fields_id(3)  = field_id_tauwx
      fields_id(4)  = field_id_tauwy
      import_interface_id = CCPL_register_import_interface("OCN_receive_from_WAVE", 4, fields_id, timer_id, 1, annotation="register interface for receiving data from WAVE")
      deallocate(fields_id)
!====================================================================== coupling generation =====================================================================================================

      CALL CCPL_get_configurable_comps_full_names(comp_id, "external_comps_for_coupling_generation", num_comps, comps_full_names, individual_or_family, annotation="test CCPL_get_configurable_comps_full_names")      
      write(stdout,*)  num_comps,comps_full_names(1:num_comps),individual_or_family(1:num_comps)
      CALL CCPL_do_external_coupling_generation(num_comps, comps_full_names(1:num_comps), individual_or_family(1:num_comps))

!================================================ check which model component will be coupled ===================================================================================================
      wave_coupled   = .false.
      atm_coupled    = .false.
      wave_coupled   = CCPL_is_comp_type_coupled(comp_id,"wave", annotation="ocean is coupling with wave")
      atm_coupled    = CCPL_is_comp_type_coupled(comp_id,"atm", annotation="ocean is coupling with atm")
!================================================================================================================================================================================================
      END SUBROUTINE register_component_coupling_configuration
!

!
      SUBROUTINE run_coupling (ng, tile)
      implicit none
      integer, intent(in) :: ng, tile
      integer             :: roms_grid_id
      integer             :: field_update_status(13)
      logical             :: interface_status, timer_status
      roms_grid_id = ng
      IF(roms_state_variables(roms_grid_id)%initialization)THEN
        CALL send_coupling_fields(ng, tile)
        IF(atm_coupled)  interface_status = CCPL_execute_interface_using_name(roms_state_variables(roms_grid_id)%comp_id, "OCN_send_to_ATM", .true., annotation="initialization,execute OCN_send_to_ATM")
        IF(wave_coupled) interface_status = CCPL_execute_interface_using_name(roms_state_variables(roms_grid_id)%comp_id, "OCN_send_to_WAVE", .true., annotation="initialization,execute OCN_send_to_WAVE")
        
        IF(atm_coupled)  interface_status = CCPL_execute_interface_using_name(roms_state_variables(roms_grid_id)%comp_id, "OCN_receive_from_ATM", .true., field_update_status, annotation="initialization,execute OCN_receive_from_ATM")
        IF(wave_coupled) interface_status = CCPL_execute_interface_using_name(roms_state_variables(roms_grid_id)%comp_id, "OCN_receive_from_WAVE", .true., field_update_status, annotation="initialization,execute OCN_receive_from_WAVE")
        CALL receive_coupling_fields(ng, tile)
      ELSE
        timer_status = CCPL_is_timer_on(roms_state_variables(roms_grid_id)%timer_id,annotation="check whether the timer is on")
        !IF(timer_status)THEN 
        !write(stdout,*) "zb test timer_status",timer_status
        CALL send_coupling_fields(ng, tile)
        !END IF
        IF(atm_coupled)  interface_status = CCPL_execute_interface_using_name(roms_state_variables(roms_grid_id)%comp_id, "OCN_send_to_ATM", .false., annotation="integrate,execute OCN_send_to_ATM")
        IF(wave_coupled) interface_status = CCPL_execute_interface_using_name(roms_state_variables(roms_grid_id)%comp_id, "OCN_send_to_WAVE", .false., annotation="integrate,execute OCN_send_to_WAVE")
        
        IF(atm_coupled)  interface_status = CCPL_execute_interface_using_name(roms_state_variables(roms_grid_id)%comp_id, "OCN_receive_from_ATM", .false.,field_update_status,annotation="integrate,execute OCN_receive_from_ATM")
        IF(wave_coupled) interface_status = CCPL_execute_interface_using_name(roms_state_variables(roms_grid_id)%comp_id, "OCN_receive_from_WAVE", .false.,field_update_status,annotation="integrate,execute OCN_receive_from_WAVE")
        !IF(timer_status)THEN
        CALL receive_coupling_fields(ng, tile)
        !END IF
        CALL CCPL_advance_time(roms_state_variables(roms_grid_id)%comp_id)
        call CCPL_do_restart_write_IO(roms_state_variables(roms_grid_id)%comp_id,.false.)
      END IF
      roms_state_variables(roms_grid_id)%initialization = .false.
      
      END SUBROUTINE run_coupling
!
!
      SUBROUTINE roms_grid_parallel_decomposition (ng, tile)
      implicit none
      integer, intent(in) :: ng, tile
      integer             :: roms_grid_id
      integer                    :: num_local_rows, num_local_cols, num_local_cells
      integer                    :: num_global_rows, num_global_cols, num_global_cells
      integer                    ::  i, j, n
      integer                    :: Istr, Iend, Jstr, Jend
      integer                    :: IstrR, IendR, JstrR, JendR
      roms_grid_id = ng
      Istr=BOUNDS(ng)%Istr(tile)
      Iend=BOUNDS(ng)%Iend(tile)
      Jstr=BOUNDS(ng)%Jstr(tile)
      Jend=BOUNDS(ng)%Jend(tile)
!
      IF (DOMAIN(ng)%Western_Edge(tile)) THEN
        IstrR=BOUNDS(ng)%Istr(tile)-1
      ELSE
        IstrR=BOUNDS(ng)%Istr(tile)
      END IF
      IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
        IendR=BOUNDS(ng)%Iend(tile)+1
      ELSE
        IendR=BOUNDS(ng)%Iend(tile)
      END IF
      IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
        JstrR=BOUNDS(ng)%Jstr(tile)-1
      ELSE
        JstrR=BOUNDS(ng)%Jstr(tile)
      END IF
      IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
        JendR=BOUNDS(ng)%Jend(tile)+1
      ELSE
        JendR=BOUNDS(ng)%Jend(tile)
      END IF



      num_local_rows = JendR-JstrR+1
      num_local_cols = IendR-IstrR+1
      num_local_cells = num_local_rows*num_local_cols
      num_global_rows = Mm(ng)+2
      num_global_cols = Lm(ng)+2  
      num_global_cells = num_global_rows*num_global_cols 
      roms_state_variables(roms_grid_id)%num_local_cells   = num_local_cells
      roms_state_variables(roms_grid_id)%num_global_cells  = num_global_cells
      IF (num_local_cells .gt. 0) THEN
         allocate(roms_state_variables(roms_grid_id)%local_cell_global_indexes(num_local_cells))
         allocate(roms_state_variables(roms_grid_id)%center_lats(num_local_cells))
         allocate(roms_state_variables(roms_grid_id)%center_lons(num_local_cells))
         allocate(roms_state_variables(roms_grid_id)%lons_vertexes(4,num_local_cells))
         allocate(roms_state_variables(roms_grid_id)%lats_vertexes(4,num_local_cells))
         allocate(roms_state_variables(roms_grid_id)%mask(num_local_cells))
         roms_state_variables(roms_grid_id)%mask = 0
         n = 0
         do j=JstrR,JendR
         do i=IstrR,IendR
              n = n+1
              roms_state_variables(roms_grid_id)%local_cell_global_indexes(n) = j*(Lm(ng)+2)+i+1 
              roms_state_variables(roms_grid_id)%center_lats(n) = GRID(ng)%latr(i,j)
              roms_state_variables(roms_grid_id)%center_lons(n) = GRID(ng)%lonr(i,j)
              if(GRID(ng)%rmask(i,j).eq.1)then
                roms_state_variables(roms_grid_id)%mask(n) = 1
              end if
!for iner points
              roms_state_variables(roms_grid_id)%lons_vertexes(1,n) = (GRID(ng)%lonr(i-1,j)+GRID(ng)%lonr(i,j))/2 
              roms_state_variables(roms_grid_id)%lats_vertexes(1,n) = (GRID(ng)%latr(i,j-1)+GRID(ng)%latr(i,j))/2 

              roms_state_variables(roms_grid_id)%lons_vertexes(2,n) = (GRID(ng)%lonr(i-1,j)+GRID(ng)%lonr(i,j))/2
              roms_state_variables(roms_grid_id)%lats_vertexes(2,n) = (GRID(ng)%latr(i,j+1)+GRID(ng)%latr(i,j))/2 

              roms_state_variables(roms_grid_id)%lons_vertexes(3,n) = (GRID(ng)%lonr(i+1,j)+GRID(ng)%lonr(i,j))/2
              roms_state_variables(roms_grid_id)%lats_vertexes(3,n) = (GRID(ng)%latr(i,j+1)+GRID(ng)%latr(i,j))/2


              roms_state_variables(roms_grid_id)%lons_vertexes(4,n) = (GRID(ng)%lonr(i+1,j)+GRID(ng)%lonr(i,j))/2
              roms_state_variables(roms_grid_id)%lats_vertexes(4,n) = (GRID(ng)%latr(i,j-1)+GRID(ng)%latr(i,j))/2

!for four boundary lines
              if(j.eq.JstrR) then
              roms_state_variables(roms_grid_id)%lons_vertexes(1,n) = GRID(ng)%lonr(i,j)
              roms_state_variables(roms_grid_id)%lats_vertexes(1,n) = GRID(ng)%latr(i,j)

              roms_state_variables(roms_grid_id)%lons_vertexes(4,n) = GRID(ng)%lonr(i,j)
              roms_state_variables(roms_grid_id)%lats_vertexes(4,n) = GRID(ng)%latr(i,j)
              end if

              if(j.eq.JendR) then
              roms_state_variables(roms_grid_id)%lons_vertexes(2,n) = GRID(ng)%lonr(i,j)
              roms_state_variables(roms_grid_id)%lats_vertexes(2,n) = GRID(ng)%latr(i,j)

              roms_state_variables(roms_grid_id)%lons_vertexes(3,n) = GRID(ng)%lonr(i,j)
              roms_state_variables(roms_grid_id)%lats_vertexes(3,n) = GRID(ng)%latr(i,j)
              end if

              if(i.eq.IstrR) then
              roms_state_variables(roms_grid_id)%lons_vertexes(1,n) = GRID(ng)%lonr(i,j)
              roms_state_variables(roms_grid_id)%lats_vertexes(1,n) = GRID(ng)%latr(i,j)

              roms_state_variables(roms_grid_id)%lons_vertexes(2,n) = GRID(ng)%lonr(i,j)
              roms_state_variables(roms_grid_id)%lats_vertexes(2,n) = GRID(ng)%latr(i,j)
              end if

              if(i.eq.IendR) then
              roms_state_variables(roms_grid_id)%lons_vertexes(3,n) = GRID(ng)%lonr(i,j)
              roms_state_variables(roms_grid_id)%lats_vertexes(3,n) = GRID(ng)%latr(i,j)

              roms_state_variables(roms_grid_id)%lons_vertexes(4,n) = GRID(ng)%lonr(i,j)
              roms_state_variables(roms_grid_id)%lats_vertexes(4,n) = GRID(ng)%latr(i,j)
              end if
! for four corner points
              if(j.eq.JstrR .and. i.eq.IstrR) then
              roms_state_variables(roms_grid_id)%lons_vertexes(1,n) = GRID(ng)%lonr(i,j) 
              roms_state_variables(roms_grid_id)%lats_vertexes(1,n) = GRID(ng)%latr(i,j)

              roms_state_variables(roms_grid_id)%lons_vertexes(2,n) = GRID(ng)%lonr(i,j)
              roms_state_variables(roms_grid_id)%lats_vertexes(2,n) = GRID(ng)%latr(i,j)

              roms_state_variables(roms_grid_id)%lons_vertexes(4,n) = GRID(ng)%lonr(i,j)
              roms_state_variables(roms_grid_id)%lats_vertexes(4,n) = GRID(ng)%latr(i,j)
              end if

              if(j.eq.JendR .and. i.eq.IstrR) then
              roms_state_variables(roms_grid_id)%lons_vertexes(1,n) = GRID(ng)%lonr(i,j)
              roms_state_variables(roms_grid_id)%lats_vertexes(1,n) = GRID(ng)%latr(i,j)

              roms_state_variables(roms_grid_id)%lons_vertexes(2,n) = GRID(ng)%lonr(i,j)
              roms_state_variables(roms_grid_id)%lats_vertexes(2,n) = GRID(ng)%latr(i,j)

              roms_state_variables(roms_grid_id)%lons_vertexes(3,n) = GRID(ng)%lonr(i,j)
              roms_state_variables(roms_grid_id)%lats_vertexes(3,n) = GRID(ng)%latr(i,j)
              end if

              if(j.eq.JstrR .and. i.eq.IendR) then
              roms_state_variables(roms_grid_id)%lons_vertexes(1,n) = GRID(ng)%lonr(i,j)
              roms_state_variables(roms_grid_id)%lats_vertexes(1,n) = GRID(ng)%latr(i,j)

              roms_state_variables(roms_grid_id)%lons_vertexes(3,n) = GRID(ng)%lonr(i,j)
              roms_state_variables(roms_grid_id)%lats_vertexes(3,n) = GRID(ng)%latr(i,j)

              roms_state_variables(roms_grid_id)%lons_vertexes(4,n) = GRID(ng)%lonr(i,j)
              roms_state_variables(roms_grid_id)%lats_vertexes(4,n) = GRID(ng)%latr(i,j)
              end if

              if(j.eq.JendR .and. i.eq.IendR) then
              roms_state_variables(roms_grid_id)%lons_vertexes(2,n) = GRID(ng)%lonr(i,j)
              roms_state_variables(roms_grid_id)%lats_vertexes(2,n) = GRID(ng)%latr(i,j)

              roms_state_variables(roms_grid_id)%lons_vertexes(3,n) = GRID(ng)%lonr(i,j)
              roms_state_variables(roms_grid_id)%lats_vertexes(3,n) = GRID(ng)%latr(i,j)

              roms_state_variables(roms_grid_id)%lons_vertexes(4,n) = GRID(ng)%lonr(i,j)
              roms_state_variables(roms_grid_id)%lats_vertexes(4,n) = GRID(ng)%latr(i,j)
              end if

         end do
         end do
      END IF
      END SUBROUTINE roms_grid_parallel_decomposition
!
!
      SUBROUTINE send_coupling_fields (ng, tile)
!=======================================================================
!                                                                      !
!  This subroutine acquires the coupling data streams between ocean    !
!  and atmosphere models. Currently, the following data streams are    !
!  coded:                                                              !
!                                                                      !
!     (...) WRF  units                                                 !
!     [...] ROMS units                                                 !
!                                                                      !
!     Fields export to WRF and MASNUM models:                          !
!                                                                      !
!     * SST     Sea Surface temperature (Celsius), [Celsius]           !
!     * SSS     Sea Surface salt        (PSU), [PSU]                   !
!     * SSU     Sea Surface  U-current speed (m/s), [m/s]              !
!     * SSV     Sea Surface V-current speed (m/s), [m/s]               !
!                                                                      !
!=======================================================================

      implicit none
      integer, intent(in) :: ng, tile
      integer             :: roms_grid_id
      integer             :: i,j,k
      integer             :: Istr, Iend, Jstr, Jend
      integer             :: IstrR, IendR, JstrR, JendR
      roms_grid_id = ng
      Istr=BOUNDS(ng)%Istr(tile)
      Iend=BOUNDS(ng)%Iend(tile)
      Jstr=BOUNDS(ng)%Jstr(tile)
      Jend=BOUNDS(ng)%Jend(tile)
!
      IF (DOMAIN(ng)%Western_Edge(tile)) THEN
        IstrR=BOUNDS(ng)%Istr(tile)-1
      ELSE
        IstrR=BOUNDS(ng)%Istr(tile)
      END IF
      IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
        IendR=BOUNDS(ng)%Iend(tile)+1
      ELSE
        IendR=BOUNDS(ng)%Iend(tile)
      END IF
      IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
        JstrR=BOUNDS(ng)%Jstr(tile)-1
      ELSE
        JstrR=BOUNDS(ng)%Jstr(tile)
      END IF
      IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
        JendR=BOUNDS(ng)%Jend(tile)+1
      ELSE
        JendR=BOUNDS(ng)%Jend(tile)
      END IF
      ! send fields !
      DO J=JstrR,JendR
      DO I=Istr,Iend
         roms_state_variables(roms_grid_id)%ssu_rho(I,J) = 0.5_r8*(OCEAN(ng)%u(i,j,N(ng),nstp(ng))+OCEAN(ng)%u(i+1,j,N(ng),nstp(ng)))                                 !transform U-type variable to RHO-type
      END DO
      END DO
      IF (DOMAIN(ng)%Western_Edge(tile)) THEN
      DO j=Jstr,Jend
         roms_state_variables(roms_grid_id)%ssu_rho(Istr-1,j)=roms_state_variables(roms_grid_id)%ssu_rho(Istr,j)
      END DO
      END IF
      IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
      DO j=Jstr,Jend
         roms_state_variables(roms_grid_id)%ssu_rho(Iend+1,j)=roms_state_variables(roms_grid_id)%ssu_rho(Iend,j)
      END DO
      END IF
      IF (DOMAIN(ng)%SouthWest_Corner(tile)) THEN
          roms_state_variables(roms_grid_id)%ssu_rho(Istr-1,Jstr-1)=0.5_r8*(roms_state_variables(roms_grid_id)%ssu_rho(Istr,Jstr-1)+roms_state_variables(roms_grid_id)%ssu_rho(Istr-1,Jstr))
      END IF
      IF (DOMAIN(ng)%SouthEast_Corner(tile)) THEN
          roms_state_variables(roms_grid_id)%ssu_rho(Iend+1,Jstr-1)=0.5_r8*(roms_state_variables(roms_grid_id)%ssu_rho(Iend,Jstr-1)+roms_state_variables(roms_grid_id)%ssu_rho(Iend+1,Jstr))
      END IF
      IF (DOMAIN(ng)%NorthWest_Corner(tile)) THEN
          roms_state_variables(roms_grid_id)%ssu_rho(Istr-1,Jend+1)=0.5_r8*(roms_state_variables(roms_grid_id)%ssu_rho(Istr-1,Jend)+roms_state_variables(roms_grid_id)%ssu_rho(Istr,Jend+1))
      END IF
      IF (DOMAIN(ng)%NorthEast_Corner(tile)) THEN
          roms_state_variables(roms_grid_id)%ssu_rho(Iend+1,Jend+1)=0.5_r8*(roms_state_variables(roms_grid_id)%ssu_rho(Iend+1,Jend)+roms_state_variables(roms_grid_id)%ssu_rho(Iend,Jend+1))
      END IF


      DO J=Jstr,Jend
      DO I=IstrR,IendR
         roms_state_variables(roms_grid_id)%ssv_rho(I,J) = 0.5_r8*(OCEAN(ng)%v(i,j,N(ng),nstp(ng))+OCEAN(ng)%v(i,j+1,N(ng),nstp(ng)))                                 !transform V-type variable to RHO-type      
      END DO
      END DO
      IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
      DO i=Istr,Iend
         roms_state_variables(roms_grid_id)%ssv_rho(i,Jend+1)=roms_state_variables(roms_grid_id)%ssv_rho(i,Jend)
      END DO
      END IF
      IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
      DO i=Istr,Iend
         roms_state_variables(roms_grid_id)%ssv_rho(i,Jstr-1)=roms_state_variables(roms_grid_id)%ssv_rho(i,Jstr)
      END DO
      END IF
      IF (DOMAIN(ng)%SouthWest_Corner(tile)) THEN
         roms_state_variables(roms_grid_id)%ssv_rho(Istr-1,Jstr-1)=0.5_r8*(roms_state_variables(roms_grid_id)%ssv_rho(Istr,Jstr-1)+roms_state_variables(roms_grid_id)%ssv_rho(Istr-1,Jstr))
      END IF
      IF (DOMAIN(ng)%SouthEast_Corner(tile)) THEN
         roms_state_variables(roms_grid_id)%ssv_rho(Iend+1,Jstr-1)=0.5_r8*(roms_state_variables(roms_grid_id)%ssv_rho(Iend,Jstr-1)+roms_state_variables(roms_grid_id)%ssv_rho(Iend+1,Jstr))
      END IF
      IF (DOMAIN(ng)%NorthWest_Corner(tile)) THEN
         roms_state_variables(roms_grid_id)%ssv_rho(Istr-1,Jend+1)=0.5_r8*(roms_state_variables(roms_grid_id)%ssv_rho(Istr-1,Jend)+roms_state_variables(roms_grid_id)%ssv_rho(Istr,Jend+1))
      END IF
      IF (DOMAIN(ng)%NorthEast_Corner(tile)) THEN
         roms_state_variables(roms_grid_id)%ssv_rho(Iend+1,Jend+1)=0.5_r8*(roms_state_variables(roms_grid_id)%ssv_rho(Iend+1,Jend)+roms_state_variables(roms_grid_id)%ssv_rho(Iend,Jend+1))
      END IF

      DO J=JstrR,JendR
      DO I=IstrR,IendR     
#ifdef CURVGRID 
         roms_state_variables(roms_grid_id)%ssu(I,J) = roms_state_variables(roms_grid_id)%ssu_rho(I,J)*GRID(ng)%CosAngler(i,j)-roms_state_variables(roms_grid_id)%ssv_rho(I,J)*GRID(ng)%SinAngler(i,j)            !Rotate variable from curvgrid to lon-lat grid
         roms_state_variables(roms_grid_id)%ssv(I,J) = roms_state_variables(roms_grid_id)%ssv_rho(I,J)*GRID(ng)%CosAngler(i,j)+roms_state_variables(roms_grid_id)%ssu_rho(I,J)*GRID(ng)%SinAngler(i,j)            !Rotate variable from curvgrid to lon-lat grid
#else
         roms_state_variables(roms_grid_id)%ssu(I,J) = roms_state_variables(roms_grid_id)%ssu_rho(I,J)
         roms_state_variables(roms_grid_id)%ssv(I,J) = roms_state_variables(roms_grid_id)%ssv_rho(I,J)
#endif
      END DO
      END DO

      DO J=JstrR,JendR
      DO I=IstrR,IendR
         roms_state_variables(roms_grid_id)%sst(I,J) = OCEAN(ng)%t(i,j,N(ng),nstp(ng),itemp)                                       !SST from ROMS
         roms_state_variables(roms_grid_id)%sss(I,J) = OCEAN(ng)%t(i,j,N(ng),nstp(ng),isalt)                                       !SSS from ROMS
      END DO
      END DO

      RETURN
      END SUBROUTINE send_coupling_fields
!
!
      SUBROUTINE receive_coupling_fields (ng, tile)
!=========================================================================================
!                                                                                        !
!  This subroutine acquires the coupling data streams between ocean                      !
!  and atmosphere models. Currently, the following data streams are                      !
!  coded:                                                                                !
!                                                                                        !
!     (...) WRF  units                                                                   !
!     [...] ROMS units                                                                   !
!     {...} MASNUM units                                                                 !
!     Fields From  WRF model:                                                            !
!                                                                                        !
!     * DSW_from_atm     Downward Net shortwave radiation (Watts/m2), [Celsius m/s]      !
!     * LW_from_atm      Downward Long wave raditaion (Watts/m2), [Celsius m/s]          !
!     * LT_from_atm      Latent heat flux (Watts/m2), [Celsius m/s]                      !
!     * ST_from_atm      Sensible heat flux (Watts/m2), [Celsius m/s]                    !
!     * MSLP             Mean Sea Level Pressure (Pa), [mb]                              !
!     * Q2               Speicific humidity (2 m) (kg/kg), [g/kg]                        !
!     * T2               Surface (2 m) air temperature (Kelvin), [Celsius]               !
!     * U10_from_atm     Surface (10 m) U-wind speed (m/s), [m/s]                        !
!     * V10_from_atm     Surface (10 m) V-wind speed (m/s), [m/s]                        !
!     * rain             Precipitation (mm/s), [kg/m2/s]                                 !
!     * qfx_from_atm     Upward moisture flux at sea surface (kg/m2/s) [kg/m2/s]         !
!     * Taux             wind u stress [m2/s2]                                           !    
!     * Tauy             wind v stress [m2/s2]                                           !  
!     * Tauwx            wave-induced stress {kg/m/s2} [m2/s2]                           !
!     * Tauwy            wave-induced stess {kg/m/s2} [m2/s2]                            !
!=========================================================================================
      implicit none
      integer, intent(in) :: ng, tile
      integer             :: roms_grid_id
      integer             :: i,j,k
      integer             :: Istr, Iend, Jstr, Jend
      integer             :: IstrR, IendR, JstrR, JendR
      real(r8) :: fac,BBR,windspd,windu,windv,UST,taux,tauy,tauwx,tauwy
      real(r8), parameter :: eps=1.0e-10
#include "tile.h"
      roms_grid_id = ng
      Istr=BOUNDS(ng)%Istr(tile)
      Iend=BOUNDS(ng)%Iend(tile)
      Jstr=BOUNDS(ng)%Jstr(tile)
      Jend=BOUNDS(ng)%Jend(tile)
!
      IF (DOMAIN(ng)%Western_Edge(tile)) THEN
        IstrR=BOUNDS(ng)%Istr(tile)-1
      ELSE
        IstrR=BOUNDS(ng)%Istr(tile)
      END IF
      IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
        IendR=BOUNDS(ng)%Iend(tile)+1
      ELSE
        IendR=BOUNDS(ng)%Iend(tile)
      END IF
      IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
        JstrR=BOUNDS(ng)%Jstr(tile)-1
      ELSE
        JstrR=BOUNDS(ng)%Jstr(tile)
      END IF
      IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
        JendR=BOUNDS(ng)%Jend(tile)+1
      ELSE
        JendR=BOUNDS(ng)%Jend(tile)
      END IF
      ! receive fields !
      fac=1.0_r8/(rho0*Cp)
      DO J=JstrR,JendR
      DO I=IstrR,IendR
         FORCES(ng)%srflx(i,j) = roms_state_variables(roms_grid_id)%DSW_from_atm(I,J)*fac
         BBR=OCEAN(ng)%t(i,j,N(ng),nstp(ng),itemp)+273.16_r8
         BBR=BBR*BBR*BBR*BBR
         BBR=emmiss*StefBo*BBR  !upward longwave radiation flux
         FORCES(ng)%lrflx(i,j) = roms_state_variables(roms_grid_id)%LW_from_atm(i,j)*fac-BBR*fac  !net longwave radiation flux
         FORCES(ng)%shflx(i,j) = roms_state_variables(roms_grid_id)%ST_from_atm(i,j)*fac
         FORCES(ng)%lhflx(i,j) = roms_state_variables(roms_grid_id)%LT_from_atm(i,j)*fac
         FORCES(ng)%Pair(i,j)  = roms_state_variables(roms_grid_id)%SLP_from_atm(i,j)*0.01_r8
         FORCES(ng)%Hair(i,j)  = roms_state_variables(roms_grid_id)%q2_from_atm(i,j)*1000.0_r8
         FORCES(ng)%Tair(i,j)  = roms_state_variables(roms_grid_id)%t2_from_atm(i,j)-273.15_r8
         FORCES(ng)%rain(i,j)  = roms_state_variables(roms_grid_id)%rain_from_atm(i,j)*rho0/1000.0_r8
         FORCES(ng)%evap(i,j)  = roms_state_variables(roms_grid_id)%qfx_from_atm(i,j)
         Do k=0,N(ng)
         FORCES(ng)%Bv(i,j,k)=roms_state_variables(roms_grid_id)%bv_from_wave(i,j,k)
         END DO
      END DO
      END DO

      DO J=Jstr-1,JendR
      DO I=Istr-1,IendR 
         windu = roms_state_variables(roms_grid_id)%U10_from_atm(i,j)
         windv = roms_state_variables(roms_grid_id)%V10_from_atm(i,j)
         windspd  = sqrt(windu*windu+windv*windv)+eps
         UST   = roms_state_variables(roms_grid_id)%rhoa_from_atm(i,j)*roms_state_variables(roms_grid_id)%ust_from_atm(i,j)*roms_state_variables(roms_grid_id)%ust_from_atm(i,j)
         taux  = (UST*windu/windspd)/rho0
         tauy  = (UST*windv/windspd)/rho0
         tauwx = roms_state_variables(roms_grid_id)%tauwx_from_wave(i,j)/rho0
         tauwy = roms_state_variables(roms_grid_id)%tauwy_from_wave(i,j)/rho0
#ifdef CURVGRID
         FORCES(ng)%Uwind(i,j)  = windu*GRID(ng)%CosAngler(i,j)+windv*GRID(ng)%SinAngler(i,j)
         FORCES(ng)%Vwind(i,j)  = windv*GRID(ng)%CosAngler(i,j)-windu*GRID(ng)%SinAngler(i,j)
         FORCES(ng)%Taux(i,j)   = taux*GRID(ng)%CosAngler(i,j)+tauy*GRID(ng)%SinAngler(i,j)
         FORCES(ng)%Tauy(i,j)   = tauy*GRID(ng)%CosAngler(i,j)-taux*GRID(ng)%SinAngler(i,j)
         FORCES(ng)%Tauwx(i,j)  = tauwx*GRID(ng)%CosAngler(i,j)+tauwy*GRID(ng)%SinAngler(i,j)
         FORCES(ng)%Tauwy(i,j)  = tauwy*GRID(ng)%CosAngler(i,j)-tauwx*GRID(ng)%SinAngler(i,j)
#else
         FORCES(ng)%Uwind(i,j) = windu
         FORCES(ng)%Vwind(i,j) = windv
         FORCES(ng)%Taux(i,j)  = taux
         FORCES(ng)%Tauy(i,j)  = tauy
         FORCES(ng)%Tauwx(i,j) = tauwx
         FORCES(ng)%Tauwy(i,j) = tauwy
#endif
      END DO
      END DO
      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
!
!-----------------------------------------------------------------------
!  Apply periodic boundary conditions.
!-----------------------------------------------------------------------
!
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          FORCES(ng)%srflx)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          FORCES(ng)%lrflx)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          FORCES(ng)%lhflx)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          FORCES(ng)%shflx)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          FORCES(ng)%Taux)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          FORCES(ng)%Tauy)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          FORCES(ng)%Pair)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          FORCES(ng)%Hair)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          FORCES(ng)%Tair)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          FORCES(ng)%Uwind)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          FORCES(ng)%Vwind)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          FORCES(ng)%rain)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          FORCES(ng)%evap)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          FORCES(ng)%Tauwx)
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          FORCES(ng)%Tauwy)
       CALL exchange_w3d_tile  (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj, 0, N(ng),           &
     &                          FORCES(ng)%Bv)
     END IF
#ifdef DISTRIBUTE
!-----------------------------------------------------------------------
!  Exchange tile boundaries.
!-----------------------------------------------------------------------
!
      CALL mp_exchange2d (ng, tile, iNLM, 2,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    FORCES(ng)%srflx, FORCES(ng)%lrflx)
      CALL mp_exchange2d (ng, tile, iNLM, 2,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    FORCES(ng)%lhflx, FORCES(ng)%shflx)
      CALL mp_exchange2d (ng, tile, iNLM, 2,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    FORCES(ng)%Taux, FORCES(ng)%Tauy)
      CALL mp_exchange2d (ng, tile, iNLM, 1,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    FORCES(ng)%Pair)
      CALL mp_exchange2d (ng, tile, iNLM, 2,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    FORCES(ng)%Hair, FORCES(ng)%Tair)
      CALL mp_exchange2d (ng, tile, iNLM, 2,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    FORCES(ng)%Uwind, FORCES(ng)%Vwind)
      CALL mp_exchange2d (ng, tile, iNLM, 1,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    FORCES(ng)%rain)
      CALL mp_exchange2d (ng, tile, iNLM, 1,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    FORCES(ng)%evap)
      CALL mp_exchange2d (ng, tile, iNLM, 2,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    FORCES(ng)%Tauwx, FORCES(ng)%Tauwy)
      CALL mp_exchange3d (ng, tile, iNLM, 1,                            &
     &                    LBi, UBi, LBj, UBj, 0, N(ng),                 &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    FORCES(ng)%Bv)
#endif
      RETURN
      END SUBROUTINE receive_coupling_fields
!
!
      SUBROUTINE allocate_coupling_buf_fields (ng, tile)
      implicit none
      integer, intent(in) :: ng, tile
      integer             :: roms_grid_id
      integer                    :: Istr, Iend, Jstr, Jend
      integer                    :: IstrR, IendR, JstrR, JendR
      integer             :: i,j,k
      real                :: hwater, hinv, cff_w, cff1_w, cff2_w  
      roms_grid_id = ng
      Istr=BOUNDS(ng)%Istr(tile)
      Iend=BOUNDS(ng)%Iend(tile)
      Jstr=BOUNDS(ng)%Jstr(tile)
      Jend=BOUNDS(ng)%Jend(tile)
!
      IF (DOMAIN(ng)%Western_Edge(tile)) THEN
        IstrR=BOUNDS(ng)%Istr(tile)-1
      ELSE
        IstrR=BOUNDS(ng)%Istr(tile)
      END IF
      IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
        IendR=BOUNDS(ng)%Iend(tile)+1
      ELSE
        IendR=BOUNDS(ng)%Iend(tile)
      END IF
      IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
        JstrR=BOUNDS(ng)%Jstr(tile)-1
      ELSE
        JstrR=BOUNDS(ng)%Jstr(tile)
      END IF
      IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
        JendR=BOUNDS(ng)%Jend(tile)+1
      ELSE
        JendR=BOUNDS(ng)%Jend(tile)
      END IF

      allocate(roms_state_variables(roms_grid_id)%z_w(IstrR:IendR,JstrR:JendR,0:N(ng)))
      allocate(roms_state_variables(roms_grid_id)%bv_from_wave(IstrR:IendR,JstrR:JendR,0:N(ng)))
      allocate(roms_state_variables(roms_grid_id)%bbv_from_wave(IstrR:IendR,JstrR:JendR,0:N(ng)))
      allocate(roms_state_variables(roms_grid_id)%tauwx_from_wave(IstrR:IendR,JstrR:JendR)) 
      allocate(roms_state_variables(roms_grid_id)%tauwy_from_wave(IstrR:IendR,JstrR:JendR)) 
      allocate(roms_state_variables(roms_grid_id)%sst(IstrR:IendR,JstrR:JendR))
      allocate(roms_state_variables(roms_grid_id)%sss(IstrR:IendR,JstrR:JendR))
      allocate(roms_state_variables(roms_grid_id)%ssu(IstrR:IendR,JstrR:JendR))
      allocate(roms_state_variables(roms_grid_id)%ssv(IstrR:IendR,JstrR:JendR))
      allocate(roms_state_variables(roms_grid_id)%u10_from_atm(IstrR:IendR,JstrR:JendR))
      allocate(roms_state_variables(roms_grid_id)%v10_from_atm(IstrR:IendR,JstrR:JendR))
      allocate(roms_state_variables(roms_grid_id)%rhoa_from_atm(IstrR:IendR,JstrR:JendR)) 
      allocate(roms_state_variables(roms_grid_id)%ust_from_atm(IstrR:IendR,JstrR:JendR))
      allocate(roms_state_variables(roms_grid_id)%t2_from_atm(IstrR:IendR,JstrR:JendR))
      allocate(roms_state_variables(roms_grid_id)%q2_from_atm(IstrR:IendR,JstrR:JendR))
      allocate(roms_state_variables(roms_grid_id)%qfx_from_atm(IstrR:IendR,JstrR:JendR))   
      allocate(roms_state_variables(roms_grid_id)%LT_from_atm(IstrR:IendR,JstrR:JendR))
      allocate(roms_state_variables(roms_grid_id)%ST_from_atm(IstrR:IendR,JstrR:JendR))
      allocate(roms_state_variables(roms_grid_id)%SLP_from_atm(IstrR:IendR,JstrR:JendR))
      allocate(roms_state_variables(roms_grid_id)%rain_from_atm(IstrR:IendR,JstrR:JendR))
      allocate(roms_state_variables(roms_grid_id)%DSW_from_atm(IstrR:IendR,JstrR:JendR))
      allocate(roms_state_variables(roms_grid_id)%LW_from_atm(IstrR:IendR,JstrR:JendR))
      allocate(roms_state_variables(roms_grid_id)%ssu_rho(IstrR:IendR,JstrR:JendR))
      allocate(roms_state_variables(roms_grid_id)%ssv_rho(IstrR:IendR,JstrR:JendR))      
      DO J=JstrR,JendR
        DO I=IstrR,IendR
          DO k=0,N(ng)
           roms_state_variables(roms_grid_id)%z_w(I,J,K)        = GRID(ng)%z_w(I,J,K)
          END DO
        END DO
      END DO
!      IF (Vtransform(ng).eq.1) THEN
!        DO j=JstrR,JendR
!          DO i=IstrR,IendR
!           DO k=1,N(ng)
!              hwater=GRID(ng)%h(I,J)
!              hinv=1.0_r8/hwater
!              cff_w=hc(ng)*(SCALARS(ng)%sc_w(k)-SCALARS(ng)%Cs_w(k))
!              cff1_w=SCALARS(ng)%Cs_w(k)
!              roms_state_variables(roms_grid_id)%z_w(i,j,k)=cff_w+cff1_w*hwater
!            END DO
!              roms_state_variables(roms_grid_id)%z_w(i,j,0)=-1*hwater
!          END DO
!        END DO
!      ELSE IF (Vtransform(ng).eq.2) THEN
!        DO j=JstrR,JendR
!          DO i=IstrR,IendR
!            DO k=1,N(ng)
!              hwater=GRID(ng)%h(I,J)
!              hinv=1.0_r8/(hc(ng)+hwater)
!              cff_w=hc(ng)*SCALARS(ng)%sc_w(k)
!              cff1_w=SCALARS(ng)%Cs_w(k)
!              cff2_w=(cff_w+cff1_w*hwater)*hinv
!              roms_state_variables(roms_grid_id)%z_w(i,j,k)=hwater*cff2_w
!            END DO
!              roms_state_variables(roms_grid_id)%z_w(i,j,0)=-1*hwater
!          END DO
!        END DO
!      END IF

      roms_state_variables(roms_grid_id)%bv_from_wave      = 0.0
      roms_state_variables(roms_grid_id)%bbv_from_wave     = 0.0
      roms_state_variables(roms_grid_id)%tauwx_from_wave   = 0.0
      roms_state_variables(roms_grid_id)%tauwy_from_wave   = 0.0
      roms_state_variables(roms_grid_id)%sst               = 0.0
      roms_state_variables(roms_grid_id)%sss               = 0.0
      roms_state_variables(roms_grid_id)%ssu               = 0.0
      roms_state_variables(roms_grid_id)%ssv               = 0.0
      roms_state_variables(roms_grid_id)%u10_from_atm      = 0.0
      roms_state_variables(roms_grid_id)%v10_from_atm      = 0.0
      roms_state_variables(roms_grid_id)%rhoa_from_atm     = 0.0
      roms_state_variables(roms_grid_id)%ust_from_atm      = 0.0
      roms_state_variables(roms_grid_id)%t2_from_atm       = 0.0
      roms_state_variables(roms_grid_id)%q2_from_atm       = 0.0
      roms_state_variables(roms_grid_id)%qfx_from_atm      = 0.0
      roms_state_variables(roms_grid_id)%LT_from_atm       = 0.0
      roms_state_variables(roms_grid_id)%ST_from_atm       = 0.0
      roms_state_variables(roms_grid_id)%SLP_from_atm      = 0.0
      roms_state_variables(roms_grid_id)%rain_from_atm     = 0.0
      roms_state_variables(roms_grid_id)%DSW_from_atm      = 0.0
      roms_state_variables(roms_grid_id)%LW_from_atm       = 0.0  
      roms_state_variables(roms_grid_id)%ssu_rho           = 0.0
      roms_state_variables(roms_grid_id)%ssv_rho           = 0.0
      END SUBROUTINE allocate_coupling_buf_fields
!
!
      SUBROUTINE release_roms_buf(ng)
      implicit none
      integer, intent(in) :: ng
      integer             :: roms_grid_id
      roms_grid_id = ng
      IF(ALLOCATED(roms_state_variables(roms_grid_id)%topography))      DEALLOCATE(roms_state_variables(roms_grid_id)%topography)
      IF(ALLOCATED(roms_state_variables(roms_grid_id)%z_w))             DEALLOCATE(roms_state_variables(roms_grid_id)%z_w)
      IF(ALLOCATED(roms_state_variables(roms_grid_id)%bv_from_wave))    DEALLOCATE(roms_state_variables(roms_grid_id)%bv_from_wave)
      IF(ALLOCATED(roms_state_variables(roms_grid_id)%bbv_from_wave))   DEALLOCATE(roms_state_variables(roms_grid_id)%bbv_from_wave)
      IF(ALLOCATED(roms_state_variables(roms_grid_id)%tauwx_from_wave)) DEALLOCATE(roms_state_variables(roms_grid_id)%tauwx_from_wave)
      IF(ALLOCATED(roms_state_variables(roms_grid_id)%tauwy_from_wave)) DEALLOCATE(roms_state_variables(roms_grid_id)%tauwy_from_wave)
      IF(ALLOCATED(roms_state_variables(roms_grid_id)%ssu))             DEALLOCATE(roms_state_variables(roms_grid_id)%ssu)
      IF(ALLOCATED(roms_state_variables(roms_grid_id)%ssv))             DEALLOCATE(roms_state_variables(roms_grid_id)%ssv)
      IF(ALLOCATED(roms_state_variables(roms_grid_id)%sst))             DEALLOCATE(roms_state_variables(roms_grid_id)%sst)
      IF(ALLOCATED(roms_state_variables(roms_grid_id)%sss))             DEALLOCATE(roms_state_variables(roms_grid_id)%sss)
      IF(ALLOCATED(roms_state_variables(roms_grid_id)%u10_from_atm))    DEALLOCATE(roms_state_variables(roms_grid_id)%u10_from_atm)
      IF(ALLOCATED(roms_state_variables(roms_grid_id)%v10_from_atm))    DEALLOCATE(roms_state_variables(roms_grid_id)%v10_from_atm)
      IF(ALLOCATED(roms_state_variables(roms_grid_id)%rhoa_from_atm))   DEALLOCATE(roms_state_variables(roms_grid_id)%rhoa_from_atm)
      IF(ALLOCATED(roms_state_variables(roms_grid_id)%ust_from_atm))    DEALLOCATE(roms_state_variables(roms_grid_id)%ust_from_atm)
      IF(ALLOCATED(roms_state_variables(roms_grid_id)%t2_from_atm))     DEALLOCATE(roms_state_variables(roms_grid_id)%t2_from_atm)
      IF(ALLOCATED(roms_state_variables(roms_grid_id)%q2_from_atm))     DEALLOCATE(roms_state_variables(roms_grid_id)%q2_from_atm)
      IF(ALLOCATED(roms_state_variables(roms_grid_id)%qfx_from_atm))    DEALLOCATE(roms_state_variables(roms_grid_id)%qfx_from_atm)
      IF(ALLOCATED(roms_state_variables(roms_grid_id)%LT_from_atm))     DEALLOCATE(roms_state_variables(roms_grid_id)%LT_from_atm)
      IF(ALLOCATED(roms_state_variables(roms_grid_id)%ST_from_atm))     DEALLOCATE(roms_state_variables(roms_grid_id)%ST_from_atm)
      IF(ALLOCATED(roms_state_variables(roms_grid_id)%SLP_from_atm))    DEALLOCATE(roms_state_variables(roms_grid_id)%SLP_from_atm)
      IF(ALLOCATED(roms_state_variables(roms_grid_id)%rain_from_atm))   DEALLOCATE(roms_state_variables(roms_grid_id)%rain_from_atm)
      IF(ALLOCATED(roms_state_variables(roms_grid_id)%DSW_from_atm))    DEALLOCATE(roms_state_variables(roms_grid_id)%DSW_from_atm)
      IF(ALLOCATED(roms_state_variables(roms_grid_id)%LW_from_atm))     DEALLOCATE(roms_state_variables(roms_grid_id)%LW_from_atm)
      IF(ALLOCATED(roms_state_variables(roms_grid_id)%ssu_rho))         DEALLOCATE(roms_state_variables(roms_grid_id)%ssu_rho)
      IF(ALLOCATED(roms_state_variables(roms_grid_id)%ssv_rho))         DEALLOCATE(roms_state_variables(roms_grid_id)%ssv_rho)
      END  SUBROUTINE release_roms_buf

END MODULE coupling_roms_mod
