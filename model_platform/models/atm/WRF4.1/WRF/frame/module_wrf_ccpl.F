MODULE module_wrf_ccpl
 
USE module_domain_type
USE CCPL_interface_mod
USE module_dm , only: local_communicator
type, private :: wrf_state_var
   integer               :: comp_id
   integer               :: parent_comp_id
   integer               :: time_step
   integer               :: grid_H2D_id
   integer               :: decomp_id
   integer               :: timer_id
   integer               :: num_local_cells
   integer               :: num_global_cells
   integer,  allocatable :: local_cell_global_indexes(:)
   real,     allocatable :: center_lats(:), center_lons(:)
   real,     allocatable :: lats_vertexes(:,:), lons_vertexes(:,:)
   integer,  allocatable :: mask(:)
   logical               :: initialization
   real, allocatable :: U10_buf(:,:)
   real, allocatable :: V10_buf(:,:)
   real, allocatable :: RHOA_buf(:,:)
   real, allocatable :: UST_buf(:,:)
   real, allocatable :: T2_buf(:,:)
   real, allocatable :: Q2_buf(:,:)
   real, allocatable :: QFX_buf(:,:)
   real, allocatable :: LH_buf(:,:)
   real, allocatable :: SH_buf(:,:)
   real, allocatable :: RAIN_buf(:,:)
   real, allocatable :: SWDOWN_buf(:,:)
   real, allocatable :: LWDOWN_buf(:,:)
   real, allocatable :: MSLP_buf(:,:)  

   real, allocatable :: SST_buf(:,:)
   real, allocatable :: SSS_buf(:,:)
   real, allocatable :: SSU_buf(:,:)
   real, allocatable :: SSV_buf(:,:)
   
   real, allocatable :: whitecap_fraction_buf(:,:)
   real, allocatable :: u_orbital_srf_buf(:,:)
   real, allocatable :: v_orbital_srf_buf(:,:)
   real, allocatable :: u_stokes_srf_buf(:,:)
   real, allocatable :: v_stokes_srf_buf(:,:)
   real, allocatable :: break_volume_buf(:,:)
   real, allocatable :: tau_in_buf(:,:)
   real, allocatable :: significant_wave_height_buf(:,:)
   real, allocatable :: peak_wave_length_buf(:,:)
   real, allocatable :: peak_wave_period_buf(:,:)

end type wrf_state_var

   type(wrf_state_var), public :: wrf_state_variables(100)
   integer                     :: wrf_frame_id
   character(len=800), private :: message
   logical                     :: wave_coupled,ocean_coupled
   character(len=512),  public    :: log_file_name
   logical,             public    :: atm_log
contains 

SUBROUTINE register_wrf_frame_coupling_configuration
implicit none
integer             :: parent_comp_id
     parent_comp_id     = -1
     local_communicator = CCPL_NULL_COMM
     wrf_frame_id       = CCPL_register_component(parent_comp_id, "wrf", "atm", local_communicator,.false.,change_dir=.true., annotation= "register WRF FRAME to c-coupler")
     atm_log            = .false.
     atm_log            = CCPL_get_comp_log_file_name(wrf_frame_id,log_file_name,annotation="get the atm logfile name of wrf")
     open(6,file=trim(log_file_name),status="UNKNOWN")
END SUBROUTINE register_wrf_frame_coupling_configuration

 
SUBROUTINE register_component_coupling_configuration (wrf_grid)
implicit none
  type(domain), pointer :: wrf_grid
  integer               :: parent_comp_id
  integer               :: wrf_grid_id
  integer               :: comp_id
  character(len=1024)   :: annotation
  character(len=20)     :: nestid
  character(len=80)     :: comp_name
  integer               :: grid_H2D_id
  integer               :: decomp_id
  integer               :: field_mark_ocn, field_mark_wave, field_mark_atm 
  integer               :: field_id_sss, field_id_sst, field_id_ssu, field_id_ssv
  integer               :: field_id_ustokes, field_id_vstokes, field_id_uorb, field_id_vorb, field_id_whitecap, field_id_breakrate, field_id_hwave, field_id_pwave, field_id_lwavep, field_id_tau_in
  integer               :: field_id_u10, field_id_v10, field_id_ust, field_id_rhoa, field_id_qfx, field_id_sh, field_id_lh, field_id_t2, field_id_q2, field_id_rain, field_id_mslp, field_id_swdown, field_id_lwdown
  integer               :: timer_id, import_interface_id, export_interface_id
  integer, allocatable  :: fields_id(:)
  integer               :: num_comps, individual_or_family(12)
  character(len=1024)   :: comps_full_names(12)
  field_mark_ocn  = 0
  field_mark_wave = 1
  field_mark_atm  = 2

!==============================================Register the component model using CCPL_register_component ================== ===================================================================
  IF(wrf_grid%id .eq. 1)THEN
     write(nestid,"(I2)") wrf_grid%id
     annotation        =   "component "//"WRF_TOP_d0"//trim(adjustl(nestid))//" start registration"
     parent_comp_id    =   wrf_frame_id
     comp_name         =   "WRF_TOP_d0"//trim(adjustl(nestid))
  ELSE
    write(nestid,"(I2)") wrf_grid%id
    annotation        =   "component "//"WRF_NEST_d0"//trim(adjustl(nestid))//" start registration"
    parent_comp_id    =   wrf_state_variables(wrf_grid%parent_id)%comp_id      
    comp_name         =   "WRF_NEST_d0"//trim(adjustl(nestid))   
  END IF
  comp_id     =   CCPL_register_component(parent_comp_id, comp_name, "atm", local_communicator,change_dir=.true., annotation=annotation)
  write(message,*) trim(comp_name)," comp_id: ", comp_id, " domain:", wrf_grid%id, " parent comp_id: ",parent_comp_id
  CALL wrf_debug(100,message)
  wrf_grid_id = wrf_grid%id
  wrf_state_variables(wrf_grid_id)%comp_id        = comp_id
  wrf_state_variables(wrf_grid_id)%time_step      = wrf_grid%dt
  wrf_state_variables(wrf_grid_id)%parent_comp_id      = parent_comp_id
  wrf_state_variables(wrf_grid_id)%initialization = .true.
!===============================================================================================================================================================================================


  CALL CCPL_set_normal_time_step(comp_id, wrf_state_variables(wrf_grid_id)%time_step)

 
!=================================================Register wrf H2D grid using CCPL_register_H2D_grid_via_local_data =============================================================================
  CALL wrf_grid_parallel_decomposition(wrf_grid)
!  grid_H2D_id    =   CCPL_register_H2D_grid_via_local_data(comp_id, "wrf_grid_via_local", "LON_LAT", "degrees", "acyclic", wrf_state_variables(wrf_grid_id)%num_global_cells, &
!                                                           wrf_state_variables(wrf_grid_id)%num_local_cells, wrf_state_variables(wrf_grid_id)%local_cell_global_indexes,      &
!                                                           -999999., -999999., -999999., -999999., wrf_state_variables(wrf_grid_id)%center_lons,                              &
!                                                           wrf_state_variables(wrf_grid_id)%center_lats, wrf_state_variables(wrf_grid_id)%mask,                               &
!                                                           annotation="register wrf  H2D  grid")
  grid_H2D_id    =   CCPL_register_H2D_grid_via_local_data(comp_id, "wrf_grid_via_local", "LON_LAT", "degrees", "acyclic",wrf_state_variables(wrf_grid_id)%num_global_cells,  &
                                                           wrf_state_variables(wrf_grid_id)%num_local_cells, wrf_state_variables(wrf_grid_id)%local_cell_global_indexes,      &
                                                           -999999., -999999.,-999999., -999999., wrf_state_variables(wrf_grid_id)%center_lons,                               &
                                                           wrf_state_variables(wrf_grid_id)%center_lats, mask=wrf_state_variables(wrf_grid_id)%mask,                          &
                                                           vertex_lon=wrf_state_variables(wrf_grid_id)%lons_vertexes,                                                         &
                                                           vertex_lat=wrf_state_variables(wrf_grid_id)%lats_vertexes,                                                         &
                                                           annotation="register wrf H2D grid")

  wrf_state_variables(wrf_grid_id)%grid_H2D_id    = grid_H2D_id
!================================================================================================================================================================================================



!================================================== Register a new parallel decomposition of an H2D grid ========================================================================================
  decomp_id      =   CCPL_register_normal_parallel_decomp("decomp_wrf_grid", wrf_state_variables(wrf_grid_id)%grid_H2D_id, wrf_state_variables(wrf_grid_id)%num_local_cells, &
                                                   wrf_state_variables(wrf_grid_id)%local_cell_global_indexes, annotation="decompose wrf grid")
  wrf_state_variables(wrf_grid_id)%decomp_id      = decomp_id
!================================================================================================================================================================================================



!================================================== Register fields using  CCPL_register_field_instance =========================================================================================
  CALL allocate_coupling_buf_fields(wrf_grid)
  field_id_sss      =   CCPL_register_field_instance(wrf_state_variables(wrf_grid_id)%SSS_buf, "sss", wrf_state_variables(wrf_grid_id)%decomp_id, wrf_state_variables(wrf_grid_id)%grid_H2D_id, field_mark_ocn, &
                                                      usage_tag=CCPL_TAG_CPL_REST, field_unit="Psu", annotation="register field instance of SSS") 
  field_id_sst      =   CCPL_register_field_instance(wrf_state_variables(wrf_grid_id)%SST_buf, "sst", wrf_state_variables(wrf_grid_id)%decomp_id, wrf_state_variables(wrf_grid_id)%grid_H2D_id, field_mark_ocn, &
                                                      usage_tag=CCPL_TAG_CPL_REST, field_unit="Celsius", annotation="register field instance of SST")
  field_id_ssu      =   CCPL_register_field_instance(wrf_state_variables(wrf_grid_id)%SSU_buf, "ssu", wrf_state_variables(wrf_grid_id)%decomp_id, wrf_state_variables(wrf_grid_id)%grid_H2D_id, field_mark_ocn, &
                                                      usage_tag=CCPL_TAG_CPL_REST, field_unit="m s-1", annotation="register field instance of SSU")
  field_id_ssv      =   CCPL_register_field_instance(wrf_state_variables(wrf_grid_id)%SSV_buf, "ssv", wrf_state_variables(wrf_grid_id)%decomp_id, wrf_state_variables(wrf_grid_id)%grid_H2D_id, field_mark_ocn, &
                                                      usage_tag=CCPL_TAG_CPL_REST, field_unit="m s-1", annotation="register field instance of SSV")

  field_id_ustokes  =   CCPL_register_field_instance(wrf_state_variables(wrf_grid_id)%u_stokes_srf_buf, "u_stokes_srf", wrf_state_variables(wrf_grid_id)%decomp_id, wrf_state_variables(wrf_grid_id)%grid_H2D_id, field_mark_wave,       &
                                                     usage_tag=CCPL_TAG_CPL_REST, field_unit="m s-1", annotation="register field instance of u component of stokes drift")
  field_id_vstokes  =   CCPL_register_field_instance(wrf_state_variables(wrf_grid_id)%v_stokes_srf_buf, "v_stokes_srf", wrf_state_variables(wrf_grid_id)%decomp_id, wrf_state_variables(wrf_grid_id)%grid_H2D_id, field_mark_wave,       &
                                                     usage_tag=CCPL_TAG_CPL_REST, field_unit="m s-1", annotation="register field instance of v component of stokes drift")
  field_id_uorb     =   CCPL_register_field_instance(wrf_state_variables(wrf_grid_id)%u_orbital_srf_buf, "u_orbital_srf", wrf_state_variables(wrf_grid_id)%decomp_id, wrf_state_variables(wrf_grid_id)%grid_H2D_id, field_mark_wave,     &
                                                     usage_tag=CCPL_TAG_CPL_REST, field_unit="m s-1", annotation="register field instance of u component of orbital speed")
  field_id_vorb     =   CCPL_register_field_instance(wrf_state_variables(wrf_grid_id)%v_orbital_srf_buf, "v_orbital_srf", wrf_state_variables(wrf_grid_id)%decomp_id, wrf_state_variables(wrf_grid_id)%grid_H2D_id, field_mark_wave,     &
                                                     usage_tag=CCPL_TAG_CPL_REST, field_unit="m s-1", annotation="register field instance of v component of orbital speed")
  field_id_whitecap =   CCPL_register_field_instance(wrf_state_variables(wrf_grid_id)%whitecap_fraction_buf,"whitecap_fraction",wrf_state_variables(wrf_grid_id)%decomp_id,wrf_state_variables(wrf_grid_id)%grid_H2D_id,field_mark_wave, &
                                                     usage_tag=CCPL_TAG_CPL_REST, field_unit="unitless", annotation="register field instance of whitecape fraction")
  field_id_breakrate=   CCPL_register_field_instance(wrf_state_variables(wrf_grid_id)%break_volume_buf, "break_volume", wrf_state_variables(wrf_grid_id)%decomp_id, wrf_state_variables(wrf_grid_id)%grid_H2D_id, field_mark_wave,       &
                                                     usage_tag=CCPL_TAG_CPL_REST, field_unit="m3 m-2 s-1 ", annotation="register field instance of breaking rate")
  field_id_hwave    =   CCPL_register_field_instance(wrf_state_variables(wrf_grid_id)%significant_wave_height_buf, "significant_wave_height", wrf_state_variables(wrf_grid_id)%decomp_id, wrf_state_variables(wrf_grid_id)%grid_H2D_id,  &
                                                     field_mark_wave, usage_tag=CCPL_TAG_CPL_REST, field_unit="m", annotation="register field instance of significant wave height")
  field_id_lwavep   =   CCPL_register_field_instance(wrf_state_variables(wrf_grid_id)%peak_wave_length_buf, "peak_wave_length", wrf_state_variables(wrf_grid_id)%decomp_id, wrf_state_variables(wrf_grid_id)%grid_H2D_id,                &
                                                     field_mark_wave, usage_tag=CCPL_TAG_CPL_REST, field_unit="m", annotation="register field instance of peak wave length")
  field_id_pwave    =   CCPL_register_field_instance(wrf_state_variables(wrf_grid_id)%peak_wave_period_buf, "peak_wave_period", wrf_state_variables(wrf_grid_id)%decomp_id, wrf_state_variables(wrf_grid_id)%grid_H2D_id,                &
                                                     field_mark_wave, usage_tag=CCPL_TAG_CPL_REST, field_unit="second", annotation="register field instance of peak wave period")
  field_id_tau_in   =   CCPL_register_field_instance(wrf_state_variables(wrf_grid_id)%tau_in_buf, "tau_in", wrf_state_variables(wrf_grid_id)%decomp_id, wrf_state_variables(wrf_grid_id)%grid_H2D_id, field_mark_wave,                   &
                                                     usage_tag=CCPL_TAG_CPL_REST, field_unit="N", annotation="register field instance of wave induced stress")
  
  
  field_id_u10      =   CCPL_register_field_instance(wrf_state_variables(wrf_grid_id)%U10_buf, "u10", wrf_state_variables(wrf_grid_id)%decomp_id, wrf_state_variables(wrf_grid_id)%grid_H2D_id, field_mark_atm,       &
                                                     usage_tag=CCPL_TAG_CPL_REST, field_unit="m s-1", annotation="register field instance of u component wind speed at 10 meters")
  field_id_v10      =   CCPL_register_field_instance(wrf_state_variables(wrf_grid_id)%V10_buf, "v10", wrf_state_variables(wrf_grid_id)%decomp_id, wrf_state_variables(wrf_grid_id)%grid_H2D_id, field_mark_atm,       &
                                                     usage_tag=CCPL_TAG_CPL_REST, field_unit="m s-1", annotation="register field instance of v component wind speed at 10 meters")
  field_id_ust      =   CCPL_register_field_instance(wrf_state_variables(wrf_grid_id)%UST_buf, "ust", wrf_state_variables(wrf_grid_id)%decomp_id, wrf_state_variables(wrf_grid_id)%grid_H2D_id, field_mark_atm,       &
                                                     usage_tag=CCPL_TAG_CPL_REST, field_unit="m s-1", annotation="register field instance of friction velocity")
  field_id_rhoa     =   CCPL_register_field_instance(wrf_state_variables(wrf_grid_id)%RHOA_buf, "rhoa", wrf_state_variables(wrf_grid_id)%decomp_id, wrf_state_variables(wrf_grid_id)%grid_H2D_id, field_mark_atm,     &
                                                     usage_tag=CCPL_TAG_CPL_REST, field_unit="kg m-3", annotation="register field instance of surface air density")
  field_id_t2       =   CCPL_register_field_instance(wrf_state_variables(wrf_grid_id)%T2_buf, "t2", wrf_state_variables(wrf_grid_id)%decomp_id, wrf_state_variables(wrf_grid_id)%grid_H2D_id, field_mark_atm,         &
                                                     usage_tag=CCPL_TAG_CPL_REST, field_unit="K", annotation="register field instance of air temperature at 2 meters")
  field_id_q2       =   CCPL_register_field_instance(wrf_state_variables(wrf_grid_id)%Q2_buf, "q2", wrf_state_variables(wrf_grid_id)%decomp_id, wrf_state_variables(wrf_grid_id)%grid_H2D_id, field_mark_atm,         &
                                                     usage_tag=CCPL_TAG_CPL_REST, field_unit="kg kg-1", annotation="register field instance of air specific humidity at 2 meters")
  field_id_qfx      =   CCPL_register_field_instance(wrf_state_variables(wrf_grid_id)%QFX_buf, "qfx", wrf_state_variables(wrf_grid_id)%decomp_id, wrf_state_variables(wrf_grid_id)%grid_H2D_id, field_mark_atm,       &
                                                     usage_tag=CCPL_TAG_CPL_REST, field_unit="kg m-2 s-1", annotation="register field instance of surface moisture flux")
  field_id_sh       =   CCPL_register_field_instance(wrf_state_variables(wrf_grid_id)%SH_buf, "sh", wrf_state_variables(wrf_grid_id)%decomp_id, wrf_state_variables(wrf_grid_id)%grid_H2D_id, field_mark_atm,         &
                                                     usage_tag=CCPL_TAG_CPL_REST, field_unit="W m-2 ", annotation="register field instance of sensible heat flux")
  field_id_lh       =   CCPL_register_field_instance(wrf_state_variables(wrf_grid_id)%LH_buf, "lh", wrf_state_variables(wrf_grid_id)%decomp_id, wrf_state_variables(wrf_grid_id)%grid_H2D_id, field_mark_atm,         &
                                                     usage_tag=CCPL_TAG_CPL_REST, field_unit="W m-2", annotation="register field instance of latent heat flux")
  field_id_rain     =   CCPL_register_field_instance(wrf_state_variables(wrf_grid_id)%RAIN_buf, "rain", wrf_state_variables(wrf_grid_id)%decomp_id, wrf_state_variables(wrf_grid_id)%grid_H2D_id, field_mark_atm,     &
                                                     usage_tag=CCPL_TAG_CPL_REST, field_unit="mm s-1", annotation="register field instance of  precipitation")
  field_id_mslp     =   CCPL_register_field_instance(wrf_state_variables(wrf_grid_id)%MSLP_buf, "mslp", wrf_state_variables(wrf_grid_id)%decomp_id, wrf_state_variables(wrf_grid_id)%grid_H2D_id, field_mark_atm,     &
                                                     usage_tag=CCPL_TAG_CPL_REST, field_unit="Pasca", annotation="register field instance of mean sea level pressure")
  field_id_swdown   =   CCPL_register_field_instance(wrf_state_variables(wrf_grid_id)%SWDOWN_buf, "swdown", wrf_state_variables(wrf_grid_id)%decomp_id, wrf_state_variables(wrf_grid_id)%grid_H2D_id, field_mark_atm, &
                                                     usage_tag=CCPL_TAG_CPL_REST, field_unit="W m-2", annotation="register field instance of net short wave flux ")
  field_id_lwdown   =   CCPL_register_field_instance(wrf_state_variables(wrf_grid_id)%LWDOWN_buf, "lwdown", wrf_state_variables(wrf_grid_id)%decomp_id, wrf_state_variables(wrf_grid_id)%grid_H2D_id, field_mark_atm, &
                                                     usage_tag=CCPL_TAG_CPL_REST, field_unit="W m-2", annotation="register field instance of downward  long wave flux ")
!================================================================================================================================================================================================================
!                                                    Define a single timer that is a periodic timer for the given component model
  timer_id = CCPL_define_single_timer(comp_id, "seconds", 300, 0, 0, annotation="define a single timer for comp_id") 
  !timer_id = CCPL_define_single_timer(comp_id, "steps", 1, 0, 0, annotation="define a single timer for comp_id")
  wrf_state_variables(wrf_grid_id)%timer_id        = timer_id
!=================================================== Register  import and export interface for a number of field instances ======================================================================================
  allocate(fields_id(13))
  fields_id(1)  = field_id_sss
  fields_id(2)  = field_id_sst
  fields_id(3)  = field_id_ssu
  fields_id(4)  = field_id_ssv 
  import_interface_id = CCPL_register_import_interface("ATM_receive_from_OCN", 4, fields_id, timer_id, 1, annotation="register interface for receiving data from OCN")
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
  import_interface_id = CCPL_register_import_interface("ATM_receive_from_WAVE", 10, fields_id, timer_id, 1, annotation="register interface for receiving data from WAVE")

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
  export_interface_id = CCPL_register_export_interface("ATM_send_to_OCN", 13, fields_id, timer_id, annotation="register interface for sending ATM data to OCN")
  export_interface_id = CCPL_register_export_interface("ATM_send_to_WAVE", 4, fields_id, timer_id, annotation="register interface for sending ATM data to WAVE")
  deallocate(fields_id)
!=========================================== coupling generation ============================================================================================================================================

  CALL CCPL_get_configurable_comps_full_names(comp_id, "external_comps_for_coupling_generation", num_comps, comps_full_names, individual_or_family, annotation="test CCPL_get_configurable_comps_full_names")
  write(*,*)"zhaobiao,",num_comps,comps_full_names(1:num_comps),individual_or_family(1:num_comps)
  CALL CCPL_do_external_coupling_generation(num_comps, comps_full_names(1:num_comps), individual_or_family(1:num_comps))

  num_comps=0 
  CALL CCPL_get_configurable_comps_full_names(comp_id,"external_comps_for_coupling_generation_ocean_oneway_nest", num_comps, comps_full_names, individual_or_family, annotation="test CCPL_get_configurable_comps_full_names for wrf coupled with ocean nest grid")
  if(num_comps.gt.1) then
       write(*,*)"zhaobiao,test",num_comps,comps_full_names(1:num_comps),individual_or_family(1:num_comps)
       CALL CCPL_do_external_coupling_generation(num_comps, comps_full_names(1:num_comps), individual_or_family(1:num_comps))
  end if
!=========================================== check wich model will be coupled ===============================================================================================================================
  wave_coupled   = .false.
  ocean_coupled  = .false.
  wave_coupled  = CCPL_is_comp_type_coupled(comp_id,"wave", annotation="atm is coupling with wave")
  ocean_coupled = CCPL_is_comp_type_coupled(comp_id,"ocn", annotation="atm is coupling with ocean") 
!================================================================================================================================================================================================================

END SUBROUTINE register_component_coupling_configuration 





SUBROUTINE run_coupling(wrf_grid)
  implicit none
  type(domain), pointer :: wrf_grid
  integer               :: wrf_grid_id
  integer               :: i
  integer               :: field_update_status(13)
  logical               :: interface_status, timer_status
  wrf_grid_id = wrf_grid%id
  IF(wrf_state_variables(wrf_grid_id)%initialization)THEN
       CALL send_coupling_fields(wrf_grid)
       IF(ocean_coupled) interface_status = CCPL_execute_interface_using_name(wrf_state_variables(wrf_grid_id)%comp_id, "ATM_send_to_OCN", .true., annotation="initialization,execute ATM_send_to_OCN")
       IF(wave_coupled)  interface_status = CCPL_execute_interface_using_name(wrf_state_variables(wrf_grid_id)%comp_id, "ATM_send_to_WAVE", .true., annotation="initialization,execute ATM_send_to_WAVE")
       
       IF(ocean_coupled) interface_status = CCPL_execute_interface_using_name(wrf_state_variables(wrf_grid_id)%comp_id, "ATM_receive_from_OCN", .true., field_update_status, annotation="initialization,execute ATM_receive_from_OCN")
       IF(wave_coupled)  interface_status = CCPL_execute_interface_using_name(wrf_state_variables(wrf_grid_id)%comp_id, "ATM_receive_from_WAVE", .true., field_update_status, annotation="initialization,execute ATM_receive_from_WAVE")
       CALL receive_coupling_fields(wrf_grid) 
       
       write(message,*)"zhaobiao,run_coupling,initialization, domain ", wrf_grid%id
       CALL wrf_debug(100,message)
  ELSE
       timer_status = CCPL_is_timer_on(wrf_state_variables(wrf_grid_id)%timer_id,annotation="check whether the timer is on")
       !IF(timer_status)THEN 
       !write(message,*) "zb test timer_status",timer_status
       !CALL wrf_debug(0,message)
       CALL send_coupling_fields(wrf_grid)
       !END IF
       IF(ocean_coupled) interface_status = CCPL_execute_interface_using_name(wrf_state_variables(wrf_grid_id)%comp_id, "ATM_send_to_OCN", .false., annotation="integrate,execute ATM_send_to_OCN")
       IF(wave_coupled)  interface_status = CCPL_execute_interface_using_name(wrf_state_variables(wrf_grid_id)%comp_id, "ATM_send_to_WAVE", .false., annotation="integrate,execute ATM_send_to_WAVE")
       
       IF(ocean_coupled) interface_status = CCPL_execute_interface_using_name(wrf_state_variables(wrf_grid_id)%comp_id, "ATM_receive_from_OCN", .false., field_update_status,annotation="integrate,execute ATM_receive_from_OCN")
       IF(wave_coupled)  interface_status = CCPL_execute_interface_using_name(wrf_state_variables(wrf_grid_id)%comp_id, "ATM_receive_from_WAVE", .false., field_update_status,annotation="integrate,execute ATM_receive_from_WAVE")
       !IF(timer_status)THEN
       CALL receive_coupling_fields(wrf_grid)
       !END IF
       CALL CCPL_advance_time(wrf_state_variables(wrf_grid_id)%comp_id)       
       CALL CCPL_do_restart_write_IO(wrf_state_variables(wrf_grid_id)%comp_id,.false.)     
       write(message,*)"zhaobiao,run_coupling,integrate,domain ", wrf_grid%id
       CALL wrf_debug(100,message)
  END IF
  wrf_state_variables(wrf_grid_id)%initialization = .false.

END SUBROUTINE run_coupling






SUBROUTINE wrf_grid_parallel_decomposition(wrf_grid_temp)
   implicit none
   TYPE(domain) , POINTER :: wrf_grid_temp
   integer                :: wrf_grid_temp_id
   integer                :: num_local_rows, num_local_cols, num_local_cells
   integer                :: num_global_rows, num_global_cols, num_global_cells
   integer                :: i, j, n
   integer                :: iep, isp, jep, jsp, ied, isd, jed, jsd

   isd = wrf_grid_temp%sd31
   ied = wrf_grid_temp%ed31-1
   isp = wrf_grid_temp%sp31
   iep = MIN(wrf_grid_temp%ep31,ied)
   jsd = wrf_grid_temp%sd33
   jed = wrf_grid_temp%ed33-1
   jsp = wrf_grid_temp%sp33
   jep = MIN(wrf_grid_temp%ep33,jed)
   wrf_grid_temp_id = wrf_grid_temp%id
   
   num_local_rows   = jep - jsp + 1
   num_local_cols   = iep - isp + 1
   num_global_rows  = jed - jsd + 1
   num_global_cols  = ied - isd + 1
   num_local_cells  = num_local_rows  * num_local_cols
   num_global_cells = num_global_rows * num_global_cols
   wrf_state_variables(wrf_grid_temp_id)%num_local_cells   = num_local_cells
   wrf_state_variables(wrf_grid_temp_id)%num_global_cells  = num_global_cells
   write(message,*)"zhaobiao, number of cells for row and column: ",num_global_rows,num_global_cols,num_local_rows,num_local_cols
   CALL wrf_debug(100,message)


   IF (wrf_grid_temp%sd32 .ne. wrf_grid_temp%sp32 .or. wrf_grid_temp%ed32 .ne. wrf_grid_temp%ep32)THEN
      CALL CCPL_report_error(wrf_state_variables(wrf_grid_temp_id)%comp_id, .false., "the parallel decomposition on vertical direction is used in WRF")  
   END IF

   IF (num_local_cells .gt. 0) THEN
      allocate(wrf_state_variables(wrf_grid_temp_id)%local_cell_global_indexes(num_local_cells))
      allocate(wrf_state_variables(wrf_grid_temp_id)%center_lats(num_local_cells))
      allocate(wrf_state_variables(wrf_grid_temp_id)%center_lons(num_local_cells))
      allocate(wrf_state_variables(wrf_grid_temp_id)%lons_vertexes(4,num_local_cells))
      allocate(wrf_state_variables(wrf_grid_temp_id)%lats_vertexes(4,num_local_cells))
      allocate(wrf_state_variables(wrf_grid_temp_id)%mask(num_local_cells))
      wrf_state_variables(wrf_grid_temp_id)%mask = 0
      n = 0
      DO j = jsp, jep
         DO i = isp, iep
            n = n + 1
            wrf_state_variables(wrf_grid_temp_id)%local_cell_global_indexes(n) = (j-1)*num_global_cols+i
            wrf_state_variables(wrf_grid_temp_id)%center_lats(n)               = wrf_grid_temp%xlat(i,j)
            wrf_state_variables(wrf_grid_temp_id)%center_lons(n)               = wrf_grid_temp%xlong(i,j)
            IF(wrf_grid_temp%landmask(i,j).eq.0 .or. wrf_grid_temp%xice(i,j).eq.1 .or. wrf_grid_temp%lakeMask(i,j).eq.1)THEN
              wrf_state_variables(wrf_grid_temp_id)%mask(n) = 1
            END IF
!for iner points
              wrf_state_variables(wrf_grid_temp_id)%lons_vertexes(1,n) = (wrf_grid_temp%xlong(i-1,j) + wrf_grid_temp%xlong(i,j))/2
              wrf_state_variables(wrf_grid_temp_id)%lats_vertexes(1,n) = (wrf_grid_temp%xlat(i,j-1)  + wrf_grid_temp%xlat(i,j))/2
              wrf_state_variables(wrf_grid_temp_id)%lons_vertexes(2,n) = (wrf_grid_temp%xlong(i-1,j) + wrf_grid_temp%xlong(i,j))/2
              wrf_state_variables(wrf_grid_temp_id)%lats_vertexes(2,n) = (wrf_grid_temp%xlat(i,j+1)  + wrf_grid_temp%xlat(i,j))/2
              wrf_state_variables(wrf_grid_temp_id)%lons_vertexes(3,n) = (wrf_grid_temp%xlong(i+1,j) + wrf_grid_temp%xlong(i,j))/2
              wrf_state_variables(wrf_grid_temp_id)%lats_vertexes(3,n) = (wrf_grid_temp%xlat(i,j+1)  + wrf_grid_temp%xlat(i,j))/2
              wrf_state_variables(wrf_grid_temp_id)%lons_vertexes(4,n) = (wrf_grid_temp%xlong(i+1,j) + wrf_grid_temp%xlong(i,j))/2
              wrf_state_variables(wrf_grid_temp_id)%lats_vertexes(4,n) = (wrf_grid_temp%xlat(i,j-1)  + wrf_grid_temp%xlat(i,j))/2
!for four boundary lines
              if(j.eq.jsp) then
              wrf_state_variables(wrf_grid_temp_id)%lons_vertexes(1,n) = wrf_grid_temp%xlong(i,j)
              wrf_state_variables(wrf_grid_temp_id)%lats_vertexes(1,n) = wrf_grid_temp%xlat(i,j)
              wrf_state_variables(wrf_grid_temp_id)%lons_vertexes(4,n) = wrf_grid_temp%xlong(i,j)
              wrf_state_variables(wrf_grid_temp_id)%lats_vertexes(4,n) = wrf_grid_temp%xlat(i,j)
              end if
              if(j.eq.jep) then
              wrf_state_variables(wrf_grid_temp_id)%lons_vertexes(2,n) = wrf_grid_temp%xlong(i,j)
              wrf_state_variables(wrf_grid_temp_id)%lats_vertexes(2,n) = wrf_grid_temp%xlat(i,j)
              wrf_state_variables(wrf_grid_temp_id)%lons_vertexes(3,n) = wrf_grid_temp%xlong(i,j)
              wrf_state_variables(wrf_grid_temp_id)%lats_vertexes(3,n) = wrf_grid_temp%xlat(i,j)
              end if
              if(i.eq.isp) then
              wrf_state_variables(wrf_grid_temp_id)%lons_vertexes(1,n) = wrf_grid_temp%xlong(i,j)
              wrf_state_variables(wrf_grid_temp_id)%lats_vertexes(1,n) = wrf_grid_temp%xlat(i,j)
              wrf_state_variables(wrf_grid_temp_id)%lons_vertexes(2,n) = wrf_grid_temp%xlong(i,j)
              wrf_state_variables(wrf_grid_temp_id)%lats_vertexes(2,n) = wrf_grid_temp%xlat(i,j)
              end if
              if(i.eq.iep) then
              wrf_state_variables(wrf_grid_temp_id)%lons_vertexes(3,n) = wrf_grid_temp%xlong(i,j)
              wrf_state_variables(wrf_grid_temp_id)%lats_vertexes(3,n) = wrf_grid_temp%xlat(i,j)
              wrf_state_variables(wrf_grid_temp_id)%lons_vertexes(4,n) = wrf_grid_temp%xlong(i,j)
              wrf_state_variables(wrf_grid_temp_id)%lats_vertexes(4,n) = wrf_grid_temp%xlat(i,j)
              end if
! for four corner points
              if(j.eq.jsp .and. i.eq.isp) then
              wrf_state_variables(wrf_grid_temp_id)%lons_vertexes(1,n) = wrf_grid_temp%xlong(i,j)
              wrf_state_variables(wrf_grid_temp_id)%lats_vertexes(1,n) = wrf_grid_temp%xlat(i,j)
              wrf_state_variables(wrf_grid_temp_id)%lons_vertexes(2,n) = wrf_grid_temp%xlong(i,j)
              wrf_state_variables(wrf_grid_temp_id)%lats_vertexes(2,n) = wrf_grid_temp%xlat(i,j)
              wrf_state_variables(wrf_grid_temp_id)%lons_vertexes(4,n) = wrf_grid_temp%xlong(i,j)
              wrf_state_variables(wrf_grid_temp_id)%lats_vertexes(4,n) = wrf_grid_temp%xlat(i,j)
              end if
              if(j.eq.jep .and. i.eq.isp) then
              wrf_state_variables(wrf_grid_temp_id)%lons_vertexes(1,n) = wrf_grid_temp%xlong(i,j)
              wrf_state_variables(wrf_grid_temp_id)%lats_vertexes(1,n) = wrf_grid_temp%xlat(i,j)
              wrf_state_variables(wrf_grid_temp_id)%lons_vertexes(2,n) = wrf_grid_temp%xlong(i,j)
              wrf_state_variables(wrf_grid_temp_id)%lats_vertexes(2,n) = wrf_grid_temp%xlat(i,j)
              wrf_state_variables(wrf_grid_temp_id)%lons_vertexes(3,n) = wrf_grid_temp%xlong(i,j)
              wrf_state_variables(wrf_grid_temp_id)%lats_vertexes(3,n) = wrf_grid_temp%xlat(i,j)
              end if
              if(j.eq.jsp .and. i.eq.iep) then
              wrf_state_variables(wrf_grid_temp_id)%lons_vertexes(1,n) = wrf_grid_temp%xlong(i,j)
              wrf_state_variables(wrf_grid_temp_id)%lats_vertexes(1,n) = wrf_grid_temp%xlat(i,j)
              wrf_state_variables(wrf_grid_temp_id)%lons_vertexes(3,n) = wrf_grid_temp%xlong(i,j)
              wrf_state_variables(wrf_grid_temp_id)%lats_vertexes(3,n) = wrf_grid_temp%xlat(i,j)
              wrf_state_variables(wrf_grid_temp_id)%lons_vertexes(4,n) = wrf_grid_temp%xlong(i,j)
              wrf_state_variables(wrf_grid_temp_id)%lats_vertexes(4,n) = wrf_grid_temp%xlat(i,j)
              end if
              if(j.eq.jep .and. i.eq.iep) then
              wrf_state_variables(wrf_grid_temp_id)%lons_vertexes(2,n) = wrf_grid_temp%xlong(i,j)
              wrf_state_variables(wrf_grid_temp_id)%lats_vertexes(2,n) = wrf_grid_temp%xlat(i,j)
              wrf_state_variables(wrf_grid_temp_id)%lons_vertexes(3,n) = wrf_grid_temp%xlong(i,j)
              wrf_state_variables(wrf_grid_temp_id)%lats_vertexes(3,n) = wrf_grid_temp%xlat(i,j)
              wrf_state_variables(wrf_grid_temp_id)%lons_vertexes(4,n) = wrf_grid_temp%xlong(i,j)
              wrf_state_variables(wrf_grid_temp_id)%lats_vertexes(4,n) = wrf_grid_temp%xlat(i,j)
              end if

         END DO
      END DO
   END IF

END SUBROUTINE wrf_grid_parallel_decomposition




SUBROUTINE allocate_coupling_buf_fields(wrf_grid_temp)
   implicit none
   TYPE(domain) , POINTER :: wrf_grid_temp
   integer                :: wrf_grid_temp_id
   integer                :: i, j
   integer                :: iep, isp, jep, jsp, ied, isd, jed, jsd
   isd = wrf_grid_temp%sd31
   ied = wrf_grid_temp%ed31-1
   isp = wrf_grid_temp%sp31
   iep = MIN(wrf_grid_temp%ep31,ied)
   jsd = wrf_grid_temp%sd33
   jed = wrf_grid_temp%ed33-1
   jsp = wrf_grid_temp%sp33
   jep = MIN(wrf_grid_temp%ep33,jed)
   wrf_grid_temp_id = wrf_grid_temp%id  
 
      IF (wrf_state_variables(wrf_grid_temp_id)%num_local_cells .gt. 0) THEN
        ALLOCATE(wrf_state_variables(wrf_grid_temp_id)%U10_buf  (isp:iep,jsp:jep))
        ALLOCATE(wrf_state_variables(wrf_grid_temp_id)%V10_buf  (isp:iep,jsp:jep))
        ALLOCATE(wrf_state_variables(wrf_grid_temp_id)%RHOA_buf  (isp:iep,jsp:jep))
        ALLOCATE(wrf_state_variables(wrf_grid_temp_id)%Q2_buf    (isp:iep,jsp:jep))
        ALLOCATE(wrf_state_variables(wrf_grid_temp_id)%RAIN_buf    (isp:iep,jsp:jep))
        ALLOCATE(wrf_state_variables(wrf_grid_temp_id)%MSLP_buf  (isp:iep,jsp:jep))
        ALLOCATE(wrf_state_variables(wrf_grid_temp_id)%T2_buf(isp:iep,jsp:jep))
        ALLOCATE(wrf_state_variables(wrf_grid_temp_id)%UST_buf  (isp:iep,jsp:jep))
        ALLOCATE(wrf_state_variables(wrf_grid_temp_id)%SH_buf  (isp:iep,jsp:jep))
        ALLOCATE(wrf_state_variables(wrf_grid_temp_id)%LH_buf  (isp:iep,jsp:jep))
        ALLOCATE(wrf_state_variables(wrf_grid_temp_id)%QFX_buf  (isp:iep,jsp:jep))
        ALLOCATE(wrf_state_variables(wrf_grid_temp_id)%LWDOWN_buf  (isp:iep,jsp:jep))
        ALLOCATE(wrf_state_variables(wrf_grid_temp_id)%SWDOWN_buf  (isp:iep,jsp:jep))

        ALLOCATE(wrf_state_variables(wrf_grid_temp_id)%SST_buf(isp:iep,jsp:jep))
        ALLOCATE(wrf_state_variables(wrf_grid_temp_id)%SSS_buf(isp:iep,jsp:jep))
        ALLOCATE(wrf_state_variables(wrf_grid_temp_id)%SSU_buf(isp:iep,jsp:jep))
        ALLOCATE(wrf_state_variables(wrf_grid_temp_id)%SSV_buf(isp:iep,jsp:jep))

        ALLOCATE(wrf_state_variables(wrf_grid_temp_id)%u_stokes_srf_buf(isp:iep,jsp:jep))
        ALLOCATE(wrf_state_variables(wrf_grid_temp_id)%v_stokes_srf_buf(isp:iep,jsp:jep))
        ALLOCATE(wrf_state_variables(wrf_grid_temp_id)%u_orbital_srf_buf(isp:iep,jsp:jep))
        ALLOCATE(wrf_state_variables(wrf_grid_temp_id)%v_orbital_srf_buf(isp:iep,jsp:jep))
        ALLOCATE(wrf_state_variables(wrf_grid_temp_id)%whitecap_fraction_buf(isp:iep,jsp:jep))
        ALLOCATE(wrf_state_variables(wrf_grid_temp_id)%break_volume_buf(isp:iep,jsp:jep))
        ALLOCATE(wrf_state_variables(wrf_grid_temp_id)%significant_wave_height_buf(isp:iep,jsp:jep))
        ALLOCATE(wrf_state_variables(wrf_grid_temp_id)%peak_wave_length_buf(isp:iep,jsp:jep))
        ALLOCATE(wrf_state_variables(wrf_grid_temp_id)%peak_wave_period_buf(isp:iep,jsp:jep))
        ALLOCATE(wrf_state_variables(wrf_grid_temp_id)%tau_in_buf(isp:iep,jsp:jep))
        
        wrf_state_variables(wrf_grid_temp_id)%U10_buf = 0.0
        wrf_state_variables(wrf_grid_temp_id)%V10_buf = 0.0 
        wrf_state_variables(wrf_grid_temp_id)%RHOA_buf = 0.0
        wrf_state_variables(wrf_grid_temp_id)%Q2_buf = 0.0
        wrf_state_variables(wrf_grid_temp_id)%RAIN_buf = 0.0
        wrf_state_variables(wrf_grid_temp_id)%MSLP_buf = 0.0
        wrf_state_variables(wrf_grid_temp_id)%T2_buf = 0.0 
        wrf_state_variables(wrf_grid_temp_id)%UST_buf = 0.0
        wrf_state_variables(wrf_grid_temp_id)%SH_buf = 0.0
        wrf_state_variables(wrf_grid_temp_id)%LH_buf = 0.0
        wrf_state_variables(wrf_grid_temp_id)%QFX_buf = 0.0
        wrf_state_variables(wrf_grid_temp_id)%LWDOWN_buf = 0.0
        wrf_state_variables(wrf_grid_temp_id)%SWDOWN_buf = 0.0 
        
        wrf_state_variables(wrf_grid_temp_id)%SSS_buf = 0.0
        wrf_state_variables(wrf_grid_temp_id)%SST_buf = 0.0
        wrf_state_variables(wrf_grid_temp_id)%SSU_buf = 0.0
        wrf_state_variables(wrf_grid_temp_id)%SSV_buf = 0.0
        
        wrf_state_variables(wrf_grid_temp_id)%u_stokes_srf_buf = 0.0
        wrf_state_variables(wrf_grid_temp_id)%v_stokes_srf_buf = 0.0
        wrf_state_variables(wrf_grid_temp_id)%u_orbital_srf_buf = 0.0
        wrf_state_variables(wrf_grid_temp_id)%v_orbital_srf_buf = 0.0
        wrf_state_variables(wrf_grid_temp_id)%whitecap_fraction_buf = 0.0
        wrf_state_variables(wrf_grid_temp_id)%break_volume_buf = 0.0
        wrf_state_variables(wrf_grid_temp_id)%significant_wave_height_buf = 0.0
        wrf_state_variables(wrf_grid_temp_id)%peak_wave_length_buf = 0.0
        wrf_state_variables(wrf_grid_temp_id)%peak_wave_period_buf = 0.0 
        wrf_state_variables(wrf_grid_temp_id)%tau_in_buf = 0.0
       END IF
      
END SUBROUTINE allocate_coupling_buf_fields



SUBROUTINE send_coupling_fields(wrf_grid_temp)
   implicit none
   TYPE(domain) , POINTER :: wrf_grid_temp
   integer                :: wrf_grid_temp_id
   integer                :: i, j
   integer                :: iep, isp, jep, jsp, ied, isd, jed, jsd
   real,parameter         :: eps=1.0e-10
   isd = wrf_grid_temp%sd31
   ied = wrf_grid_temp%ed31-1
   isp = wrf_grid_temp%sp31
   iep = MIN(wrf_grid_temp%ep31,ied)
   jsd = wrf_grid_temp%sd33
   jed = wrf_grid_temp%ed33-1
   jsp = wrf_grid_temp%sp33
   jep = MIN(wrf_grid_temp%ep33,jed)
   wrf_grid_temp_id = wrf_grid_temp%id
        DO j = jsp, jep
         DO i = isp, iep
            !tansfer u10 and v10 from wrf grid to earth grid when wrf use lambert or polar projection
            wrf_state_variables(wrf_grid_temp_id)%U10_buf(i,j)    = wrf_grid_temp%u10(i,j)*wrf_grid_temp%cosa(i,j)-wrf_grid_temp%v10(i,j)*wrf_grid_temp%sina(i,j)
            wrf_state_variables(wrf_grid_temp_id)%V10_buf(i,j)    = wrf_grid_temp%v10(i,j)*wrf_grid_temp%cosa(i,j)+wrf_grid_temp%u10(i,j)*wrf_grid_temp%sina(i,j)
            wrf_state_variables(wrf_grid_temp_id)%UST_buf(i,j)    = wrf_grid_temp%ust(i,j)
            wrf_state_variables(wrf_grid_temp_id)%RHOA_buf(i,j)   = 1.0/(wrf_grid_temp%alt(i,1,j)+eps)
            wrf_state_variables(wrf_grid_temp_id)%T2_buf(i,j)     = wrf_grid_temp%t2(i,j)
            wrf_state_variables(wrf_grid_temp_id)%Q2_buf(i,j)     = wrf_grid_temp%q2(i,j)/(wrf_grid_temp%q2(i,j)+1) !convert water mixing ratio to specific humidity
            wrf_state_variables(wrf_grid_temp_id)%QFX_buf(i,j)    = wrf_grid_temp%qfx(i,j)
            wrf_state_variables(wrf_grid_temp_id)%LH_buf(i,j)     = -1.0*wrf_grid_temp%lh(i,j)
            wrf_state_variables(wrf_grid_temp_id)%SH_buf(i,j)     = -1.0*wrf_grid_temp%hfx(i,j)
            wrf_state_variables(wrf_grid_temp_id)%SWDOWN_buf(i,j) = wrf_grid_temp%gsw(i,j)
            wrf_state_variables(wrf_grid_temp_id)%LWDOWN_buf(i,j) = wrf_grid_temp%glw(i,j)
            wrf_state_variables(wrf_grid_temp_id)%MSLP_buf(i,j)   = wrf_grid_temp%psfc(i,j)*exp((9.81*wrf_grid_temp%ht(i,j))/(287.0*wrf_grid_temp%t2(i,j)*(1.0+0.61*wrf_grid_temp%q2(i,j))))
            wrf_state_variables(wrf_grid_temp_id)%RAIN_buf(i,j)   =(wrf_grid_temp%raincv(i,j)+wrf_grid_temp%rainncv(i,j))/wrf_grid_temp%dt       !time step averaged precipitation
         END DO
        END DO
       write(message,*)"zhaobiao,send_coupling_fields,domain",wrf_grid_temp%id
       CALL wrf_debug(100,message)
END SUBROUTINE send_coupling_fields



SUBROUTINE receive_coupling_fields(wrf_grid_temp)
   implicit none
   TYPE(domain) , POINTER :: wrf_grid_temp
   integer                :: wrf_grid_temp_id
   integer                :: i, j
   integer                :: iep, isp, jep, jsp, ied, isd, jed, jsd
   real,parameter         :: eps=1.0e-10 
   isd = wrf_grid_temp%sd31
   ied = wrf_grid_temp%ed31-1
   isp = wrf_grid_temp%sp31
   iep = MIN(wrf_grid_temp%ep31,ied)
   jsd = wrf_grid_temp%sd33
   jed = wrf_grid_temp%ed33-1
   jsp = wrf_grid_temp%sp33
   jep = MIN(wrf_grid_temp%ep33,jed)
   wrf_grid_temp_id = wrf_grid_temp%id  

      IF (wrf_state_variables(wrf_grid_temp_id)%num_local_cells .gt. 0) THEN

        DO j = jsp, jep
         DO i = isp, iep
            if(wrf_grid_temp%lakeMask(i,j).ne.1 .and. wrf_grid_temp%landmask(i,j).eq.0)then
            wrf_grid_temp%sst(i,j)          =    wrf_state_variables(wrf_grid_temp_id)%SST_buf(i,j)+273.15
            wrf_grid_temp%sss(i,j)          =    wrf_state_variables(wrf_grid_temp_id)%SSS_buf(i,j)
            wrf_grid_temp%uoce(i,j)         =    wrf_state_variables(wrf_grid_temp_id)%SSU_buf(i,j)*wrf_grid_temp%cosa(i,j)+wrf_state_variables(wrf_grid_temp_id)%SSV_buf(i,j)*wrf_grid_temp%sina(i,j)
            wrf_grid_temp%voce(i,j)         =    wrf_state_variables(wrf_grid_temp_id)%SSV_buf(i,j)*wrf_grid_temp%cosa(i,j)-wrf_state_variables(wrf_grid_temp_id)%SSU_buf(i,j)*wrf_grid_temp%sina(i,j)
            
            wrf_grid_temp%ustokes(i,j)      =    wrf_state_variables(wrf_grid_temp_id)%u_stokes_srf_buf(i,j)*wrf_grid_temp%cosa(i,j)+wrf_state_variables(wrf_grid_temp_id)%v_stokes_srf_buf(i,j)*wrf_grid_temp%sina(i,j)
            wrf_grid_temp%vstokes(i,j)      =    wrf_state_variables(wrf_grid_temp_id)%v_stokes_srf_buf(i,j)*wrf_grid_temp%cosa(i,j)-wrf_state_variables(wrf_grid_temp_id)%u_stokes_srf_buf(i,j)*wrf_grid_temp%sina(i,j)
            wrf_grid_temp%uorb(i,j)         =    wrf_state_variables(wrf_grid_temp_id)%u_orbital_srf_buf(i,j)*wrf_grid_temp%cosa(i,j)+wrf_state_variables(wrf_grid_temp_id)%v_orbital_srf_buf(i,j)*wrf_grid_temp%sina(i,j)
            wrf_grid_temp%vorb(i,j)         =    wrf_state_variables(wrf_grid_temp_id)%v_orbital_srf_buf(i,j)*wrf_grid_temp%cosa(i,j)-wrf_state_variables(wrf_grid_temp_id)%u_orbital_srf_buf(i,j)*wrf_grid_temp%sina(i,j)
            wrf_grid_temp%whitecap(i,j)     =    wrf_state_variables(wrf_grid_temp_id)%whitecap_fraction_buf(i,j)
            wrf_grid_temp%breakrate(i,j)    =    wrf_state_variables(wrf_grid_temp_id)%break_volume_buf(i,j)
            wrf_grid_temp%hwave(i,j)        =    wrf_state_variables(wrf_grid_temp_id)%significant_wave_height_buf(i,j)
            wrf_grid_temp%lwavep(i,j)       =    wrf_state_variables(wrf_grid_temp_id)%peak_wave_length_buf(i,j)
            wrf_grid_temp%pwave(i,j)        =    wrf_state_variables(wrf_grid_temp_id)%peak_wave_period_buf(i,j)
            wrf_grid_temp%tau_in(i,j)       =    wrf_state_variables(wrf_grid_temp_id)%tau_in_buf(i,j)
            end if

            wrf_grid_temp%mslp(i,j)         =    wrf_grid_temp%psfc(i,j)*exp((9.81*wrf_grid_temp%ht(i,j))/(287.0*wrf_grid_temp%t2(i,j)*(1.0+0.61*wrf_grid_temp%q2(i,j)))) 
            wrf_grid_temp%rhoa(i,j)         =    1.0/(wrf_grid_temp%alt(i,1,j)+eps)                  
         END DO
        END DO
       END IF
       write(message,*)"zhaobiao,receive_coupling_fields,domain ",wrf_grid_temp%id
       CALL wrf_debug(100,message)

END SUBROUTINE receive_coupling_fields



SUBROUTINE release_wrf_buf
USE module_configure, ONLY : model_config_rec 
implicit none
integer  :: i
 DO i=1,model_config_rec%max_dom
   IF(ALLOCATED(wrf_state_variables(i)%U10_buf)) DEALLOCATE(wrf_state_variables(i)%U10_buf)
   IF(ALLOCATED(wrf_state_variables(i)%V10_buf)) DEALLOCATE(wrf_state_variables(i)%V10_buf)
   IF(ALLOCATED(wrf_state_variables(i)%RHOA_buf)) DEALLOCATE(wrf_state_variables(i)%RHOA_buf)
   IF(ALLOCATED(wrf_state_variables(i)%UST_buf)) DEALLOCATE(wrf_state_variables(i)%UST_buf)
   IF(ALLOCATED(wrf_state_variables(i)%T2_buf)) DEALLOCATE(wrf_state_variables(i)%T2_buf)
   IF(ALLOCATED(wrf_state_variables(i)%Q2_buf)) DEALLOCATE(wrf_state_variables(i)%Q2_buf)
   IF(ALLOCATED(wrf_state_variables(i)%QFX_buf)) DEALLOCATE(wrf_state_variables(i)%QFX_buf)
   IF(ALLOCATED(wrf_state_variables(i)%LH_buf)) DEALLOCATE(wrf_state_variables(i)%LH_buf)
   IF(ALLOCATED(wrf_state_variables(i)%SH_buf)) DEALLOCATE(wrf_state_variables(i)%SH_buf)
   IF(ALLOCATED(wrf_state_variables(i)%RAIN_buf)) DEALLOCATE(wrf_state_variables(i)%RAIN_buf)
   IF(ALLOCATED(wrf_state_variables(i)%SWDOWN_buf)) DEALLOCATE(wrf_state_variables(i)%SWDOWN_buf)
   IF(ALLOCATED(wrf_state_variables(i)%LWDOWN_buf)) DEALLOCATE(wrf_state_variables(i)%LWDOWN_buf)
   IF(ALLOCATED(wrf_state_variables(i)%MSLP_buf)) DEALLOCATE(wrf_state_variables(i)%MSLP_buf)
   
   IF(ALLOCATED(wrf_state_variables(i)%SST_buf)) DEALLOCATE(wrf_state_variables(i)%SST_buf)
   IF(ALLOCATED(wrf_state_variables(i)%SSS_buf)) DEALLOCATE(wrf_state_variables(i)%SSS_buf)
   IF(ALLOCATED(wrf_state_variables(i)%SSU_buf)) DEALLOCATE(wrf_state_variables(i)%SSU_buf)
   IF(ALLOCATED(wrf_state_variables(i)%SSV_buf)) DEALLOCATE(wrf_state_variables(i)%SSV_buf)
   
   IF(ALLOCATED(wrf_state_variables(i)%whitecap_fraction_buf)) DEALLOCATE(wrf_state_variables(i)%whitecap_fraction_buf)
   IF(ALLOCATED(wrf_state_variables(i)%u_orbital_srf_buf)) DEALLOCATE(wrf_state_variables(i)%u_orbital_srf_buf)
   IF(ALLOCATED(wrf_state_variables(i)%v_orbital_srf_buf)) DEALLOCATE(wrf_state_variables(i)%v_orbital_srf_buf)
   IF(ALLOCATED(wrf_state_variables(i)%u_stokes_srf_buf)) DEALLOCATE(wrf_state_variables(i)%u_stokes_srf_buf)
   IF(ALLOCATED(wrf_state_variables(i)%v_stokes_srf_buf)) DEALLOCATE(wrf_state_variables(i)%v_stokes_srf_buf)
   IF(ALLOCATED(wrf_state_variables(i)%break_volume_buf)) DEALLOCATE(wrf_state_variables(i)%break_volume_buf)
   IF(ALLOCATED(wrf_state_variables(i)%tau_in_buf)) DEALLOCATE(wrf_state_variables(i)%tau_in_buf)
   IF(ALLOCATED(wrf_state_variables(i)%significant_wave_height_buf)) DEALLOCATE(wrf_state_variables(i)%significant_wave_height_buf)
   IF(ALLOCATED(wrf_state_variables(i)%peak_wave_length_buf)) DEALLOCATE(wrf_state_variables(i)%peak_wave_length_buf)
   IF(ALLOCATED(wrf_state_variables(i)%peak_wave_period_buf)) DEALLOCATE(wrf_state_variables(i)%peak_wave_period_buf)
  
   IF(ALLOCATED(wrf_state_variables(i)%local_cell_global_indexes)) DEALLOCATE(wrf_state_variables(i)%local_cell_global_indexes)
   IF(ALLOCATED(wrf_state_variables(i)%center_lats)) DEALLOCATE(wrf_state_variables(i)%center_lats)
   IF(ALLOCATED(wrf_state_variables(i)%center_lons)) DEALLOCATE(wrf_state_variables(i)%center_lons)
   IF(ALLOCATED(wrf_state_variables(i)%mask)) DEALLOCATE(wrf_state_variables(i)%mask)
 END DO
END SUBROUTINE release_wrf_buf



END MODULE module_wrf_ccpl 
