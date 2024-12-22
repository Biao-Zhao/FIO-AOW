      MODULE coupling_pom_mod

      use control, only: islon, ielon, islat, ielat, BVTYPE,DTI
      use lims, only: im, jm, kb, LON_DOM,LAT_DOM 
      use CCPL_interface_mod                       !zhaobiao, c-coupler2
      use all_var
      use parallel_mod, ONLY: pom_mpi_comm
      implicit none
!=======================================================================================================
!                              Coupling Interface                                                      =
!     This subroutine is the interface gropgram for coupling between POM and C-Coupler  created by     =
!     Dr. Biao Zhao                                                                                    =
!                                                                                                      =
!                                                                          Zhao Biao 2017.07.11        =
!=======================================================================================================
      type, private :: pom_state_var
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
      integer,  allocatable ::  mask(:)
      logical               :: initialization
      real, allocatable :: topography(:, :)
      real, allocatable :: sst(:, :)
      real, allocatable :: sss(:, :)
      real, allocatable :: ssv(:, :)
      real, allocatable :: ssu(:, :)
      real, allocatable :: bv_from_wave(:, :, :)
      real, allocatable :: bbv_from_wave(:, :, :)
      real, allocatable :: tauwx_from_wave(:, :)
      real, allocatable :: tauwy_from_wave(:, :)
      real, allocatable :: u10_from_atm(:, :)
      real, allocatable :: v10_from_atm(:, :)
      real, allocatable :: rhoa_from_atm(:, :)      
      real, allocatable :: ust_from_atm(:, :)    
      real, allocatable :: t2_from_atm(:, :)
      real, allocatable :: q2_from_atm(:, :)
      real, allocatable :: qfx_from_atm(:, :)        
      real, allocatable :: LT_from_atm(:, :)
      real, allocatable :: ST_from_atm(:, :)
      real, allocatable :: SLP_from_atm(:, :)
      real, allocatable :: rain_from_atm(:, :)
      real, allocatable :: DSW_from_atm(:, :)
      real, allocatable :: LW_from_atm(:, :)
      end type pom_state_var

      type(pom_state_var), public :: pom_state_variables(100)
      integer,             public :: pom_frame_id
      integer,             public :: pom_grid_id
      logical,             public :: atm_coupled,wave_coupled
      character(len=512),  public    :: log_file_name
      logical,             public    :: ocean_log
      CONTAINS



      SUBROUTINE register_pom_frame_coupling_configuration
      implicit none
      integer             :: parent_comp_id
      parent_comp_id     = -1
      pom_frame_id       = CCPL_register_component(parent_comp_id, "pom","ocn", pom_mpi_comm,.false.,.true., annotation= "register POM FRAME to c-coupler")
      ocean_log           = .false.
      ocean_log           = CCPL_get_comp_log_file_name(pom_frame_id,log_file_name,annotation="get the ocean logfile name of pom")       
      open(6,file=trim(log_file_name),status="UNKNOWN")
      END SUBROUTINE register_pom_frame_coupling_configuration


      SUBROUTINE register_component_coupling_configuration
      implicit none
      integer               :: parent_comp_id
      integer               :: comp_id
      character(len=1024)   :: annotation
      character(len=80)     :: comp_name
      integer               :: grid_H2D_id, grid_V1D_id, grid_3D_id
      integer               :: decomp_id
      integer               :: field_mark_ocn, field_mark_wave, field_mark_atm
      integer               :: field_id_topo,field_id_sss, field_id_sst, field_id_ssu, field_id_ssv
      integer               :: field_id_bv, field_id_bbv, field_id_tauwx, field_id_tauwy
      integer               :: field_id_u10, field_id_v10, field_id_ust, field_id_rhoa, field_id_qfx, field_id_sh, field_id_lh, field_id_t2, field_id_q2, field_id_rain, field_id_mslp, field_id_swdown, field_id_lwdown
      integer               :: timer_id, import_interface_id, export_interface_id
      integer, allocatable  :: fields_id(:)
      integer               :: num_comps, individual_or_family(100)
      character(len=1024)   :: comps_full_names(12)
 
      field_mark_ocn  = 0
      field_mark_wave = 1
      field_mark_atm  = 2
!=========================================================================================================================================================================================================
      comp_name         =   "POM_TOP_d01"
      annotation        =   "component POM_TOP_d01 start registration"
      parent_comp_id     =   pom_frame_id
      comp_id           =   CCPL_register_component(parent_comp_id, comp_name,"ocn", pom_mpi_comm,.true., annotation=annotation) 
      pom_grid_id = 1
      pom_state_variables(pom_grid_id)%comp_id        = comp_id
      pom_state_variables(pom_grid_id)%time_step      = DTI       
      pom_state_variables(pom_grid_id)%parent_comp_id = pom_frame_id
      pom_state_variables(pom_grid_id)%initialization = .true.
!=========================================================================================================================================================================================================

      CALL CCPL_set_normal_time_step(comp_id, pom_state_variables(pom_grid_id)%time_step)

!======================================================== register H2D and V3D grid for pom ==============================================================================================================
      CALL pom_grid_parallel_decomposition
!      grid_H2D_id    =   CCPL_register_H2D_grid_via_local_data(comp_id, "pom_grid_via_local", "LON_LAT", "degrees", "acyclic", pom_state_variables(pom_grid_id)%num_global_cells, &
!                                                               pom_state_variables(pom_grid_id)%num_local_cells, pom_state_variables(pom_grid_id)%local_cell_global_indexes, &
!                                                               LON_DOM(1), LON_DOM(2), LAT_DOM(1), LAT_DOM(2), pom_state_variables(pom_grid_id)%center_lons, pom_state_variables(pom_grid_id)%center_lats,&
!                                                               pom_state_variables(pom_grid_id)%mask, annotation="register pom H2D grid")
      grid_H2D_id    =   CCPL_register_H2D_grid_via_local_data(comp_id, "pom_grid_via_local", "LON_LAT", "degrees", "acyclic", pom_state_variables(pom_grid_id)%num_global_cells,                          &
                                                               pom_state_variables(pom_grid_id)%num_local_cells, pom_state_variables(pom_grid_id)%local_cell_global_indexes,                              &
                                                               -999999., -999999., -999999., -999999., pom_state_variables(pom_grid_id)%center_lons, pom_state_variables(pom_grid_id)%center_lats,&
                                                               mask=pom_state_variables(pom_grid_id)%mask, vertex_lon=pom_state_variables(pom_grid_id)%lons_vertexes,&
                                                               vertex_lat=pom_state_variables(pom_grid_id)%lats_vertexes,annotation="register pom H2D grid")
      pom_state_variables(pom_grid_id)%grid_H2D_id    = grid_H2D_id

      !3D decomposition ???????????
      grid_V1D_id     =  CCPL_register_V1D_SIGMA_grid_via_model_data(comp_id, "pom_V1D_grid", "unitless", 0.0, ZZ, annotation="register a v1d grid for pom")
      grid_3D_id      =  CCPL_register_MD_grid_via_multi_grids(comp_id, "pom_sigmalevel_3D_grid", grid_H2D_id, grid_V1D_id, annotation="register a 3-d grid for pom")
      
      pom_state_variables(pom_grid_id)%grid_V1D_id    = grid_V1D_id
      pom_state_variables(pom_grid_id)%grid_3D_id    = grid_3D_id





!=========================================================================================================================================================================================================
      decomp_id      =   CCPL_register_normal_parallel_decomp("decomp_pom_grid", pom_state_variables(pom_grid_id)%grid_H2D_id, pom_state_variables(pom_grid_id)%num_local_cells, &
                                                       pom_state_variables(pom_grid_id)%local_cell_global_indexes, annotation="decompose pom grid")
      pom_state_variables(pom_grid_id)%decomp_id      = decomp_id
!=========================================================================================================================================================================================================
      CALL allocate_coupling_buf_fields
      
      field_id_topo       = CCPL_register_field_instance(pom_state_variables(pom_grid_id)%topography, "topography", pom_state_variables(pom_grid_id)%decomp_id, pom_state_variables(pom_grid_id)%grid_H2D_id, field_mark_ocn, &
                                                        usage_tag=CCPL_TAG_CPL_REST,field_unit="meter", annotation="register field instance of topo")

      CALL CCPL_set_3D_grid_constant_surface_field(pom_state_variables(pom_grid_id)%grid_3D_id, field_id_topo,annotation="set bottom field of a 3-D grid")

      field_id_sss      = CCPL_register_field_instance(pom_state_variables(pom_grid_id)%SSS, "sss", pom_state_variables(pom_grid_id)%decomp_id, pom_state_variables(pom_grid_id)%grid_H2D_id, field_mark_ocn, &
                                                       usage_tag=CCPL_TAG_CPL_REST,field_unit="Psu", annotation="register field instance of SSS")
      field_id_sst      = CCPL_register_field_instance(pom_state_variables(pom_grid_id)%SST, "sst", pom_state_variables(pom_grid_id)%decomp_id, pom_state_variables(pom_grid_id)%grid_H2D_id, field_mark_ocn, &
                                                       usage_tag=CCPL_TAG_CPL_REST,field_unit="Celsius", annotation="register field instance of SST")
      field_id_ssu      = CCPL_register_field_instance(pom_state_variables(pom_grid_id)%SSU, "ssu", pom_state_variables(pom_grid_id)%decomp_id, pom_state_variables(pom_grid_id)%grid_H2D_id, field_mark_ocn, &
                                                       usage_tag=CCPL_TAG_CPL_REST,field_unit="m s-1", annotation="register field instance of SSU")
      field_id_ssv      = CCPL_register_field_instance(pom_state_variables(pom_grid_id)%SSV, "ssv", pom_state_variables(pom_grid_id)%decomp_id, pom_state_variables(pom_grid_id)%grid_H2D_id, field_mark_ocn, &
                                                       usage_tag=CCPL_TAG_CPL_REST,field_unit="m s-1", annotation="register field instance of SSV")


      field_id_u10      = CCPL_register_field_instance(pom_state_variables(pom_grid_id)%u10_from_atm, "u10", pom_state_variables(pom_grid_id)%decomp_id, pom_state_variables(pom_grid_id)%grid_H2D_id, field_mark_atm,       &
                                                       usage_tag=CCPL_TAG_CPL_REST,field_unit="m s-1", annotation="register field instance of u component wind speed at 10 meters")
      field_id_v10      = CCPL_register_field_instance(pom_state_variables(pom_grid_id)%v10_from_atm, "v10", pom_state_variables(pom_grid_id)%decomp_id, pom_state_variables(pom_grid_id)%grid_H2D_id, field_mark_atm,       &
                                                       usage_tag=CCPL_TAG_CPL_REST,field_unit="m s-1", annotation="register field instance of v component wind speed at 10 meters")
      field_id_ust      = CCPL_register_field_instance(pom_state_variables(pom_grid_id)%ust_from_atm, "ust", pom_state_variables(pom_grid_id)%decomp_id, pom_state_variables(pom_grid_id)%grid_H2D_id, field_mark_atm,       &
                                                       usage_tag=CCPL_TAG_CPL_REST,field_unit="m s-1", annotation="register field instance of friction velocity")
      field_id_rhoa     = CCPL_register_field_instance(pom_state_variables(pom_grid_id)%rhoa_from_atm, "rhoa", pom_state_variables(pom_grid_id)%decomp_id, pom_state_variables(pom_grid_id)%grid_H2D_id, field_mark_atm,     &
                                                       usage_tag=CCPL_TAG_CPL_REST,field_unit="kg m-3", annotation="register field instance of surface air density")
      field_id_t2       = CCPL_register_field_instance(pom_state_variables(pom_grid_id)%t2_from_atm, "t2", pom_state_variables(pom_grid_id)%decomp_id, pom_state_variables(pom_grid_id)%grid_H2D_id, field_mark_atm,         &
                                                       usage_tag=CCPL_TAG_CPL_REST,field_unit="K", annotation="register field instance of air temperature at 2 meters")
      field_id_q2       = CCPL_register_field_instance(pom_state_variables(pom_grid_id)%q2_from_atm, "q2", pom_state_variables(pom_grid_id)%decomp_id, pom_state_variables(pom_grid_id)%grid_H2D_id, field_mark_atm,         &
                                                       usage_tag=CCPL_TAG_CPL_REST,field_unit="kg kg-1", annotation="register field instance of air specific humidity at 2 meters")
      field_id_qfx      = CCPL_register_field_instance(pom_state_variables(pom_grid_id)%qfx_from_atm, "qfx", pom_state_variables(pom_grid_id)%decomp_id, pom_state_variables(pom_grid_id)%grid_H2D_id, field_mark_atm,       &
                                                       usage_tag=CCPL_TAG_CPL_REST,field_unit="kg m-2 s-1", annotation="register field instance of surface moisture flux")
      field_id_sh       = CCPL_register_field_instance(pom_state_variables(pom_grid_id)%ST_from_atm, "sh", pom_state_variables(pom_grid_id)%decomp_id, pom_state_variables(pom_grid_id)%grid_H2D_id, field_mark_atm,         &
                                                       usage_tag=CCPL_TAG_CPL_REST,field_unit="W m-2 ", annotation="register field instance of sensible heat flux")
      field_id_lh       = CCPL_register_field_instance(pom_state_variables(pom_grid_id)%LT_from_atm, "lh", pom_state_variables(pom_grid_id)%decomp_id, pom_state_variables(pom_grid_id)%grid_H2D_id, field_mark_atm,         &
                                                       usage_tag=CCPL_TAG_CPL_REST,field_unit="W m-2", annotation="register field instance of latent heat flux")
      field_id_rain     = CCPL_register_field_instance(pom_state_variables(pom_grid_id)%rain_from_atm, "rain", pom_state_variables(pom_grid_id)%decomp_id, pom_state_variables(pom_grid_id)%grid_H2D_id, field_mark_atm,     &
                                                       usage_tag=CCPL_TAG_CPL_REST,field_unit="mm s-1", annotation="register field instance of  precipitation")
      field_id_mslp     = CCPL_register_field_instance(pom_state_variables(pom_grid_id)%SLP_from_atm, "mslp", pom_state_variables(pom_grid_id)%decomp_id, pom_state_variables(pom_grid_id)%grid_H2D_id, field_mark_atm,     &
                                                       usage_tag=CCPL_TAG_CPL_REST,field_unit="Pasca", annotation="register field instance of mean sea level pressure")
      field_id_swdown   = CCPL_register_field_instance(pom_state_variables(pom_grid_id)%DSW_from_atm, "swdown", pom_state_variables(pom_grid_id)%decomp_id, pom_state_variables(pom_grid_id)%grid_H2D_id, field_mark_atm, &
                                                       usage_tag=CCPL_TAG_CPL_REST,field_unit="W m-2", annotation="register field instance of net short wave flux")
      field_id_lwdown   = CCPL_register_field_instance(pom_state_variables(pom_grid_id)%LW_from_atm, "lwdown", pom_state_variables(pom_grid_id)%decomp_id, pom_state_variables(pom_grid_id)%grid_H2D_id, field_mark_atm, &
                                                       usage_tag=CCPL_TAG_CPL_REST,field_unit="W m-2", annotation="register field instance of downward  long wave flux")

      field_id_bv      = CCPL_register_field_instance(pom_state_variables(pom_grid_id)%bv_from_wave, "bv", pom_state_variables(pom_grid_id)%decomp_id, pom_state_variables(pom_grid_id)%grid_3D_id, field_mark_wave, &
                                                      usage_tag=CCPL_TAG_CPL_REST,field_unit= "unitless",annotation="register field instance of none breaking wave induced mixing coefficient")
      field_id_bbv     = CCPL_register_field_instance(pom_state_variables(pom_grid_id)%bbv_from_wave, "bbv", pom_state_variables(pom_grid_id)%decomp_id, pom_state_variables(pom_grid_id)%grid_3D_id, field_mark_wave, &
                                                      usage_tag=CCPL_TAG_CPL_REST,field_unit= "unitless", annotation="register field instance of breaking wave induced mixing coefficient")
      field_id_tauwx   = CCPL_register_field_instance(pom_state_variables(pom_grid_id)%tauwx_from_wave, "tauwx", pom_state_variables(pom_grid_id)%decomp_id, pom_state_variables(pom_grid_id)%grid_H2D_id, field_mark_wave, &
                                                      usage_tag=CCPL_TAG_CPL_REST,field_unit="m s-1", annotation="register field instance of u component of wave induced stress")
      field_id_tauwy   = CCPL_register_field_instance(pom_state_variables(pom_grid_id)%tauwy_from_wave, "tauwy", pom_state_variables(pom_grid_id)%decomp_id, pom_state_variables(pom_grid_id)%grid_H2D_id, field_mark_wave, &
                                                      usage_tag=CCPL_TAG_CPL_REST, field_unit="m s-1", annotation="register field instance of v component of wave induced stress")
!===============================================================================================================================================================================================================================
!                                                    Define a single timer that is a periodic timer for the given component model
      timer_id = CCPL_define_single_timer(comp_id, "seconds", 300, 0, 0, annotation="define a single timer for comp_id")
      !timer_id = CCPL_define_single_timer(comp_id, "steps", 1, 0, 0, annotation="define a single timer for comp_id")
      pom_state_variables(pom_grid_id)%timer_id        = timer_id
!===============================================================================================================================================================================================================================
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
!=============================================== coupling generation ===========================================================================================================================================
                              
      CALL CCPL_get_configurable_comps_full_names(comp_id, "external_comps_for_coupling_generation", num_comps, comps_full_names, individual_or_family, annotation="test CCPL_get_configurable_comps_full_names")
      CALL CCPL_do_external_coupling_generation(num_comps, comps_full_names, individual_or_family)

!=============================================== check which model will be coupled =============================================================================================================================
      wave_coupled   = .false.
      atm_coupled    = .false.
      wave_coupled   = CCPL_is_comp_type_coupled(comp_id,"wave", annotation="ocean is coupling with wave")
      atm_coupled    = CCPL_is_comp_type_coupled(comp_id,"atm", annotation="ocean is coupling with atm")
!================================================================================================================================================================================================================

      END SUBROUTINE register_component_coupling_configuration



      SUBROUTINE run_coupling
      implicit none
      integer               :: i
      integer               :: field_update_status(13)
      logical               :: interface_status, timer_status
      IF(pom_state_variables(pom_grid_id)%initialization)THEN
        CALL send_coupling_fields
        IF(atm_coupled)  interface_status =  CCPL_execute_interface_using_name(pom_state_variables(pom_grid_id)%comp_id, "OCN_send_to_ATM", .true., annotation="initialization,execute OCN_send_to_ATM")
        IF(wave_coupled) interface_status =  CCPL_execute_interface_using_name(pom_state_variables(pom_grid_id)%comp_id, "OCN_send_to_WAVE", .true., annotation="initialization,execute OCN_send_to_WAVE")
        
        IF(atm_coupled)  interface_status =  CCPL_execute_interface_using_name(pom_state_variables(pom_grid_id)%comp_id, "OCN_receive_from_ATM", .true., field_update_status, annotation="initialization,execute OCN_receive_from_ATM")
        IF(wave_coupled) interface_status =  CCPL_execute_interface_using_name(pom_state_variables(pom_grid_id)%comp_id, "OCN_receive_from_WAVE", .true., field_update_status, annotation="initialization,execute OCN_receive_from_WAVE")
        CALL receive_coupling_fields
      ELSE
        timer_status = CCPL_is_timer_on(pom_state_variables(pom_grid_id)%timer_id,annotation="check whether the timer is on")
        !IF(timer_status)THEN 
        !write(6,*) "zb test timer_status",timer_status
        CALL send_coupling_fields
        !END IF
        IF(atm_coupled)  interface_status =  CCPL_execute_interface_using_name(pom_state_variables(pom_grid_id)%comp_id, "OCN_send_to_ATM", .false., annotation="integrate,execute OCN_send_to_ATM")
        IF(wave_coupled) interface_status =  CCPL_execute_interface_using_name(pom_state_variables(pom_grid_id)%comp_id, "OCN_send_to_WAVE", .false., annotation="integrate,execute OCN_send_to_WAVE")
        
        IF(atm_coupled)  interface_status =  CCPL_execute_interface_using_name(pom_state_variables(pom_grid_id)%comp_id, "OCN_receive_from_ATM", .false., field_update_status,annotation="integrate,execute OCN_receive_from_ATM")
        IF(wave_coupled) interface_status =  CCPL_execute_interface_using_name(pom_state_variables(pom_grid_id)%comp_id, "OCN_receive_from_WAVE", .false.,field_update_status,annotation="integrate,execute OCN_receive_from_WAVE")
        !IF(timer_status)THEN
        CALL receive_coupling_fields
        !END IF
        CALL CCPL_advance_time(pom_state_variables(pom_grid_id)%comp_id)
        CALL CCPL_do_restart_write_IO(pom_state_variables(pom_grid_id)%comp_id,.false.)
      END IF
      pom_state_variables(pom_grid_id)%initialization = .false.

      END SUBROUTINE run_coupling



      subroutine send_coupling_fields
      implicit none
      integer i,j,k
      ! send fields !
      do J=ISLAT,IELAT
      do I=ISLON,IELON
         pom_state_variables(pom_grid_id)%ssu(I,J) = U(I,J,1)*DUM(I,J)!I*J+I+J
         pom_state_variables(pom_grid_id)%ssv(I,J) = V(I,J,1)*DVM(I,J)!I*J+I+J
         pom_state_variables(pom_grid_id)%sst(I,J) = T(I,J,1)*FSM(I,J)!I*J+I+J
         pom_state_variables(pom_grid_id)%sss(I,J) = S(I,J,1)*FSM(I,J)!I*J+I+J
      enddo
      enddo
      end subroutine send_coupling_fields


      subroutine receive_coupling_fields
      implicit none
      integer i,j,k
      !receive fields !
      IF(BVTYPE) THEN
        do K=1,KB                 
         do J=ISLAT,IELAT
          do I=ISLON,IELON
             BV(i,j,k)=pom_state_variables(pom_grid_id)%bv_from_wave(i,j,k)
          end do
         end do
        end do
!          WRITE(6,*) "using BV(957,598,1) is :",BV(957,598,1)
      ELSE
          BV = 0.0
!          WRITE(6,*) "have not used BV(957,598,1) is :",BV(957,598,1)
      END IF

      end subroutine receive_coupling_fields



      subroutine pom_grid_parallel_decomposition
      implicit none
      integer                    :: num_local_rows, num_local_cols, num_local_cells
      integer                    :: num_global_rows, num_global_cols, num_global_cells
      integer                    ::  i, j, n, I1,I2,J1,J2

      num_local_rows = ielat-islat+1
      num_local_cols = ielon-islon+1
      num_local_cells = num_local_rows*num_local_cols
      num_global_rows = jm
      num_global_cols = im    
      num_global_cells = im*jm 
      pom_state_variables(pom_grid_id)%num_local_cells   = num_local_cells
      pom_state_variables(pom_grid_id)%num_global_cells  = num_global_cells
 
      IF (num_local_cells .gt. 0) THEN
         allocate(pom_state_variables(pom_grid_id)%local_cell_global_indexes(num_local_cells))
         allocate(pom_state_variables(pom_grid_id)%center_lats(num_local_cells))
         allocate(pom_state_variables(pom_grid_id)%center_lons(num_local_cells))
         allocate(pom_state_variables(pom_grid_id)%lons_vertexes(4,num_local_cells))
         allocate(pom_state_variables(pom_grid_id)%lats_vertexes(4,num_local_cells))
         allocate(pom_state_variables(pom_grid_id)%mask(num_local_cells))
         pom_state_variables(pom_grid_id)%mask = 0
         n = 0
         do j=islat,ielat
         do i=islon,ielon
              n = n+1
              pom_state_variables(pom_grid_id)%local_cell_global_indexes(n) = i+(j-1)*im
              pom_state_variables(pom_grid_id)%center_lats(n) = Y(j)
              pom_state_variables(pom_grid_id)%center_lons(n) = X(i)
              if(FSM(i,j).eq.1)then
                pom_state_variables(pom_grid_id)%mask(n) = 1
              end if
              
!for iner points
              if(j.ne.1 .and. j.ne.jm .and. i.ne.1 .and. i.ne.im) then
              pom_state_variables(pom_grid_id)%lons_vertexes(1,n) = (X(i-1)+X(i))/2
              pom_state_variables(pom_grid_id)%lats_vertexes(1,n) = (Y(j-1)+Y(j))/2

              pom_state_variables(pom_grid_id)%lons_vertexes(2,n) = (X(i-1)+X(i))/2
              pom_state_variables(pom_grid_id)%lats_vertexes(2,n) = (Y(j+1)+Y(j))/2

              pom_state_variables(pom_grid_id)%lons_vertexes(3,n) = (X(i+1)+X(i))/2
              pom_state_variables(pom_grid_id)%lats_vertexes(3,n) = (Y(j+1)+Y(j))/2

              pom_state_variables(pom_grid_id)%lons_vertexes(4,n) = (X(i+1)+X(i))/2
              pom_state_variables(pom_grid_id)%lats_vertexes(4,n) = (Y(j-1)+Y(j))/2

              end if
!for four boundary lines
              if(j.eq.1 .and. i.ne.1 .and. i.ne.im) then
              pom_state_variables(pom_grid_id)%lons_vertexes(1,n) = X(i)
              pom_state_variables(pom_grid_id)%lats_vertexes(1,n) = Y(j)

              pom_state_variables(pom_grid_id)%lons_vertexes(2,n) = (X(i-1)+X(i))/2
              pom_state_variables(pom_grid_id)%lats_vertexes(2,n) = (Y(j+1)+Y(j))/2

              pom_state_variables(pom_grid_id)%lons_vertexes(3,n) = (X(i+1)+X(i))/2
              pom_state_variables(pom_grid_id)%lats_vertexes(3,n) = (Y(j+1)+Y(j))/2

              pom_state_variables(pom_grid_id)%lons_vertexes(4,n) = X(i)
              pom_state_variables(pom_grid_id)%lats_vertexes(4,n) = Y(j)
              end if
             
              if(j.eq.jm .and. i.ne.1 .and. i.ne.im) then
              pom_state_variables(pom_grid_id)%lons_vertexes(1,n) = (X(i-1)+X(i))/2
              pom_state_variables(pom_grid_id)%lats_vertexes(1,n) = (Y(j-1)+Y(j))/2

              pom_state_variables(pom_grid_id)%lons_vertexes(2,n) = X(i)
              pom_state_variables(pom_grid_id)%lats_vertexes(2,n) = Y(j)

              pom_state_variables(pom_grid_id)%lons_vertexes(3,n) = X(i)
              pom_state_variables(pom_grid_id)%lats_vertexes(3,n) = Y(j)


              pom_state_variables(pom_grid_id)%lons_vertexes(4,n) = (X(i+1)+X(i))/2
              pom_state_variables(pom_grid_id)%lats_vertexes(4,n) = (Y(j-1)+Y(j))/2
              end if
   
              if(i.eq.1 .and. j.ne.1 .and. j.ne.jm) then
              pom_state_variables(pom_grid_id)%lons_vertexes(1,n) = X(i)
              pom_state_variables(pom_grid_id)%lats_vertexes(1,n) = Y(j)

              pom_state_variables(pom_grid_id)%lons_vertexes(2,n) = X(i)
              pom_state_variables(pom_grid_id)%lats_vertexes(2,n) = Y(j)

              pom_state_variables(pom_grid_id)%lons_vertexes(3,n) = (X(i+1)+X(i))/2
              pom_state_variables(pom_grid_id)%lats_vertexes(3,n) = (Y(j+1)+Y(j))/2

              pom_state_variables(pom_grid_id)%lons_vertexes(4,n) = (X(i+1)+X(i))/2
              pom_state_variables(pom_grid_id)%lats_vertexes(4,n) = (Y(j-1)+Y(j))/2
              end if

              if(i.eq.im .and. j.ne.1 .and. j.ne.jm) then
              pom_state_variables(pom_grid_id)%lons_vertexes(1,n) = (X(i-1)+X(i))/2
              pom_state_variables(pom_grid_id)%lats_vertexes(1,n) = (Y(j-1)+Y(j))/2

              pom_state_variables(pom_grid_id)%lons_vertexes(2,n) = (X(i-1)+X(i))/2
              pom_state_variables(pom_grid_id)%lats_vertexes(2,n) = (Y(j+1)+Y(j))/2

              pom_state_variables(pom_grid_id)%lons_vertexes(3,n) = X(i)
              pom_state_variables(pom_grid_id)%lats_vertexes(3,n) = Y(j)

              pom_state_variables(pom_grid_id)%lons_vertexes(4,n) = X(i)
              pom_state_variables(pom_grid_id)%lats_vertexes(4,n) = Y(j)
              end if

! for four corner points
              if(j.eq.1 .and. i.eq.1) then
              pom_state_variables(pom_grid_id)%lons_vertexes(1,n) = X(i)
              pom_state_variables(pom_grid_id)%lats_vertexes(1,n) = Y(j)

              pom_state_variables(pom_grid_id)%lons_vertexes(2,n) = X(i)
              pom_state_variables(pom_grid_id)%lats_vertexes(2,n) = Y(j)

              pom_state_variables(pom_grid_id)%lons_vertexes(3,n) = (X(i+1)+X(i))/2
              pom_state_variables(pom_grid_id)%lats_vertexes(3,n) = (Y(j+1)+Y(j))/2

              pom_state_variables(pom_grid_id)%lons_vertexes(4,n) = X(i)
              pom_state_variables(pom_grid_id)%lats_vertexes(4,n) = Y(j)
              end if

              if(j.eq.jm .and. i.eq.1) then
              pom_state_variables(pom_grid_id)%lons_vertexes(1,n) = X(i)
              pom_state_variables(pom_grid_id)%lats_vertexes(1,n) = Y(j)

              pom_state_variables(pom_grid_id)%lons_vertexes(2,n) = X(i)
              pom_state_variables(pom_grid_id)%lats_vertexes(2,n) = Y(j)

              pom_state_variables(pom_grid_id)%lons_vertexes(3,n) = X(i)
              pom_state_variables(pom_grid_id)%lats_vertexes(3,n) = Y(j)

              pom_state_variables(pom_grid_id)%lons_vertexes(4,n) = (X(i+1)+X(i))/2
              pom_state_variables(pom_grid_id)%lats_vertexes(4,n) = (Y(j-1)+Y(j))/2
              end if

              if(j.eq.1 .and. i.eq.im) then
              pom_state_variables(pom_grid_id)%lons_vertexes(1,n) = X(i)
              pom_state_variables(pom_grid_id)%lats_vertexes(1,n) = Y(j)

              pom_state_variables(pom_grid_id)%lons_vertexes(2,n) = (X(i-1)+X(i))/2
              pom_state_variables(pom_grid_id)%lats_vertexes(2,n) = (Y(j+1)+Y(j))/2

              pom_state_variables(pom_grid_id)%lons_vertexes(3,n) = X(i)
              pom_state_variables(pom_grid_id)%lats_vertexes(3,n) = Y(j)

              pom_state_variables(pom_grid_id)%lons_vertexes(4,n) = X(i)
              pom_state_variables(pom_grid_id)%lats_vertexes(4,n) = Y(j)
              end if

              if(j.eq.jm .and. i.eq.im) then
              pom_state_variables(pom_grid_id)%lons_vertexes(1,n) =(X(i-1)+X(i))/2
              pom_state_variables(pom_grid_id)%lats_vertexes(1,n) = (Y(j-1)+Y(j))/2

              pom_state_variables(pom_grid_id)%lons_vertexes(2,n) = X(i)
              pom_state_variables(pom_grid_id)%lats_vertexes(2,n) = Y(j)

              pom_state_variables(pom_grid_id)%lons_vertexes(3,n) = X(i)
              pom_state_variables(pom_grid_id)%lats_vertexes(3,n) = Y(j)

              pom_state_variables(pom_grid_id)%lons_vertexes(4,n) = X(i)
              pom_state_variables(pom_grid_id)%lats_vertexes(4,n) = Y(j)
              end if
              !write(6,*) "center_lon",pom_state_variables(pom_grid_id)%center_lons(n)
              !write(6,*) "vertexes_lon",pom_state_variables(pom_grid_id)%lons_vertexes(:,n) 
         end do
         end do
      END IF
      end subroutine pom_grid_parallel_decomposition



      subroutine allocate_coupling_buf_fields
      implicit none
      integer                    ::  i, j
      allocate(pom_state_variables(pom_grid_id)%topography(ISLON:IELON,ISLAT:IELAT))
      allocate(pom_state_variables(pom_grid_id)%bv_from_wave(ISLON:IELON,ISLAT:IELAT,KB))
      allocate(pom_state_variables(pom_grid_id)%bbv_from_wave(ISLON:IELON,ISLAT:IELAT,KB))
      allocate(pom_state_variables(pom_grid_id)%tauwx_from_wave(ISLON:IELON,ISLAT:IELAT)) 
      allocate(pom_state_variables(pom_grid_id)%tauwy_from_wave(ISLON:IELON,ISLAT:IELAT)) 
      allocate(pom_state_variables(pom_grid_id)%sst(ISLON:IELON,ISLAT:IELAT))
      allocate(pom_state_variables(pom_grid_id)%sss(ISLON:IELON,ISLAT:IELAT))
      allocate(pom_state_variables(pom_grid_id)%ssu(ISLON:IELON,ISLAT:IELAT))
      allocate(pom_state_variables(pom_grid_id)%ssv(ISLON:IELON,ISLAT:IELAT))
      allocate(pom_state_variables(pom_grid_id)%u10_from_atm(ISLON:IELON,ISLAT:IELAT))
      allocate(pom_state_variables(pom_grid_id)%v10_from_atm(ISLON:IELON,ISLAT:IELAT))
      allocate(pom_state_variables(pom_grid_id)%rhoa_from_atm(ISLON:IELON,ISLAT:IELAT)) 
      allocate(pom_state_variables(pom_grid_id)%ust_from_atm(ISLON:IELON,ISLAT:IELAT))
      allocate(pom_state_variables(pom_grid_id)%t2_from_atm(ISLON:IELON,ISLAT:IELAT))
      allocate(pom_state_variables(pom_grid_id)%q2_from_atm(ISLON:IELON,ISLAT:IELAT))
      allocate(pom_state_variables(pom_grid_id)%qfx_from_atm(ISLON:IELON,ISLAT:IELAT))   
      allocate(pom_state_variables(pom_grid_id)%LT_from_atm(ISLON:IELON,ISLAT:IELAT))
      allocate(pom_state_variables(pom_grid_id)%ST_from_atm(ISLON:IELON,ISLAT:IELAT))
      allocate(pom_state_variables(pom_grid_id)%SLP_from_atm(ISLON:IELON,ISLAT:IELAT))
      allocate(pom_state_variables(pom_grid_id)%rain_from_atm(ISLON:IELON,ISLAT:IELAT))
      allocate(pom_state_variables(pom_grid_id)%DSW_from_atm(ISLON:IELON,ISLAT:IELAT))
      allocate(pom_state_variables(pom_grid_id)%LW_from_atm(ISLON:IELON,ISLAT:IELAT))
    
      do J=ISLAT,IELAT
      do I=ISLON,IELON
         pom_state_variables(pom_grid_id)%topography(I,J) = H(I,J)*FSM(I,J)
      enddo
      enddo 
      pom_state_variables(pom_grid_id)%bv_from_wave = 0.0
      pom_state_variables(pom_grid_id)%bbv_from_wave = 0.0
      pom_state_variables(pom_grid_id)%tauwx_from_wave = 0.0
      pom_state_variables(pom_grid_id)%tauwy_from_wave = 0.0
      pom_state_variables(pom_grid_id)%sst = 0.0
      pom_state_variables(pom_grid_id)%sss = 0.0
      pom_state_variables(pom_grid_id)%ssu = 0.0
      pom_state_variables(pom_grid_id)%ssv = 0.0
      pom_state_variables(pom_grid_id)%u10_from_atm = 0.0
      pom_state_variables(pom_grid_id)%v10_from_atm = 0.0
      pom_state_variables(pom_grid_id)%rhoa_from_atm = 0.0
      pom_state_variables(pom_grid_id)%ust_from_atm = 0.0
      pom_state_variables(pom_grid_id)%t2_from_atm = 0.0
      pom_state_variables(pom_grid_id)%q2_from_atm = 0.0
      pom_state_variables(pom_grid_id)%qfx_from_atm = 0.0
      pom_state_variables(pom_grid_id)%LT_from_atm = 0.0
      pom_state_variables(pom_grid_id)%ST_from_atm = 0.0
      pom_state_variables(pom_grid_id)%SLP_from_atm = 0.0
      pom_state_variables(pom_grid_id)%rain_from_atm = 0.0
      pom_state_variables(pom_grid_id)%DSW_from_atm = 0.0
      pom_state_variables(pom_grid_id)%LW_from_atm = 0.0

      end subroutine allocate_coupling_buf_fields

      SUBROUTINE release_pom_buf
      implicit none
      IF(ALLOCATED(pom_state_variables(pom_grid_id)%topography)) DEALLOCATE(pom_state_variables(pom_grid_id)%topography)
      IF(ALLOCATED(pom_state_variables(pom_grid_id)%bv_from_wave)) DEALLOCATE(pom_state_variables(pom_grid_id)%bv_from_wave)
      IF(ALLOCATED(pom_state_variables(pom_grid_id)%bbv_from_wave)) DEALLOCATE(pom_state_variables(pom_grid_id)%bbv_from_wave)
      IF(ALLOCATED(pom_state_variables(pom_grid_id)%tauwx_from_wave)) DEALLOCATE(pom_state_variables(pom_grid_id)%tauwx_from_wave)
      IF(ALLOCATED(pom_state_variables(pom_grid_id)%tauwy_from_wave)) DEALLOCATE(pom_state_variables(pom_grid_id)%tauwy_from_wave)
      IF(ALLOCATED(pom_state_variables(pom_grid_id)%ssu)) DEALLOCATE(pom_state_variables(pom_grid_id)%ssu)
      IF(ALLOCATED(pom_state_variables(pom_grid_id)%ssv)) DEALLOCATE(pom_state_variables(pom_grid_id)%ssv)
      IF(ALLOCATED(pom_state_variables(pom_grid_id)%sst)) DEALLOCATE(pom_state_variables(pom_grid_id)%sst)
      IF(ALLOCATED(pom_state_variables(pom_grid_id)%sss)) DEALLOCATE(pom_state_variables(pom_grid_id)%sss)
      IF(ALLOCATED(pom_state_variables(pom_grid_id)%u10_from_atm)) DEALLOCATE(pom_state_variables(pom_grid_id)%u10_from_atm)
      IF(ALLOCATED(pom_state_variables(pom_grid_id)%v10_from_atm)) DEALLOCATE(pom_state_variables(pom_grid_id)%v10_from_atm)
      IF(ALLOCATED(pom_state_variables(pom_grid_id)%rhoa_from_atm)) DEALLOCATE(pom_state_variables(pom_grid_id)%rhoa_from_atm)
      IF(ALLOCATED(pom_state_variables(pom_grid_id)%ust_from_atm)) DEALLOCATE(pom_state_variables(pom_grid_id)%ust_from_atm)
      IF(ALLOCATED(pom_state_variables(pom_grid_id)%t2_from_atm)) DEALLOCATE(pom_state_variables(pom_grid_id)%t2_from_atm)
      IF(ALLOCATED(pom_state_variables(pom_grid_id)%q2_from_atm)) DEALLOCATE(pom_state_variables(pom_grid_id)%q2_from_atm)
      IF(ALLOCATED(pom_state_variables(pom_grid_id)%qfx_from_atm)) DEALLOCATE(pom_state_variables(pom_grid_id)%qfx_from_atm)
      IF(ALLOCATED(pom_state_variables(pom_grid_id)%LT_from_atm)) DEALLOCATE(pom_state_variables(pom_grid_id)%LT_from_atm)
      IF(ALLOCATED(pom_state_variables(pom_grid_id)%ST_from_atm)) DEALLOCATE(pom_state_variables(pom_grid_id)%ST_from_atm)
      IF(ALLOCATED(pom_state_variables(pom_grid_id)%SLP_from_atm)) DEALLOCATE(pom_state_variables(pom_grid_id)%SLP_from_atm)
      IF(ALLOCATED(pom_state_variables(pom_grid_id)%rain_from_atm)) DEALLOCATE(pom_state_variables(pom_grid_id)%rain_from_atm)
      IF(ALLOCATED(pom_state_variables(pom_grid_id)%DSW_from_atm)) DEALLOCATE(pom_state_variables(pom_grid_id)%DSW_from_atm)
      IF(ALLOCATED(pom_state_variables(pom_grid_id)%LW_from_atm)) DEALLOCATE(pom_state_variables(pom_grid_id)%LW_from_atm)
      END  SUBROUTINE release_pom_buf



      END MODULE coupling_pom_mod
