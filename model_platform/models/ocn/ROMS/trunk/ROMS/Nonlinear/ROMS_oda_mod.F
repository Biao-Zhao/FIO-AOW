! In main3d.F
!   use ROMS_oda_mod  ! add at begining.
! ...
!   call ROMS_oda  ! in the main loop for time

#include "cppdefs.h"
#define RAMPDAYS 1.0
#define TIMESTEP 45
#define BETA  10.0
#undef ADJUV3D
      module ROMS_oda_mod
      USE mod_param
      USE mod_grid
      USE mod_ncparam
      USE mod_netcdf
      USE mod_ocean
      USE mod_parallel
      USE mod_scalars
      USE mod_sources
      USE mod_stepping
      USE mod_iounits, ONLY : stdout 
      USE nf_fread2d_mod, ONLY : nf_fread2d
      USE nf_fread3d_mod, ONLY : nf_fread3d

      implicit none

      public :: ROMS_oda
      private

      integer :: oda_init=0
      integer :: imethod,maxcalls,icalls
      real(r8) :: ramp_days,time_interval,alpha

      real(r8),pointer :: adj_el(:,:)
      real(r8),pointer :: adj_u(:,:,:)
      real(r8),pointer :: adj_v(:,:,:)
      real(r8),pointer :: adj_t(:,:,:)
      real(r8),pointer :: adj_s(:,:,:)

      integer :: LBi, UBi, LBj, UBj
      logical :: ualive,valive,talive,salive,ealive

      contains
!-----------------------------------------------------------------------------
      subroutine ROMS_oda_init(ng)
        integer, intent(in) :: ng
        logical :: ext

        if(oda_init/=0)return
        ! init parameters  (Default)
        imethod=0  ! 0 for adding adj at one time. 1 for adding by step
        inquire(file='mtd001',exist=ext)
        if(ext)imethod=1
        inquire(file='mtd002',exist=ext)
        if(ext)imethod=2

        oda_init=1
        if(imethod==0)then
          !oda_init=0
          oda_init=-1
          return
        endif
        
        ramp_days=RAMPDAYS
        !time_interval=30.0 !time step of ROMS
        time_interval=TIMESTEP

        maxcalls=ramp_days*86400/time_interval
        alpha=time_interval/(ramp_days*86400.*BETA)
        icalls=0

        LBi=LBOUND(GRID(ng)%h,DIM=1)
        UBi=UBOUND(GRID(ng)%h,DIM=1)
        LBj=LBOUND(GRID(ng)%h,DIM=2)
        UBj=UBOUND(GRID(ng)%h,DIM=2)
        allocate (adj_el(LBi:UBi,LBj:UBj))
        allocate (adj_u(LBi:UBi,LBj:UBj,N(ng)))
        allocate (adj_v(LBi:UBi,LBj:UBj,N(ng)))
        allocate (adj_t(LBi:UBi,LBj:UBj,N(ng)))
        allocate (adj_s(LBi:UBi,LBj:UBj,N(ng)))
        adj_el=0.0;adj_u=0.0;adj_v=0.0;adj_t=0.0;adj_s=0.0
      end subroutine ROMS_oda_init
!-----------------------------------------------------------------------------
      subroutine ROMS_oda_final
        if(associated(adj_el)) deallocate(adj_el)
        if(associated(adj_u) ) deallocate(adj_u)
        if(associated(adj_v) ) deallocate(adj_v)
        if(associated(adj_t) ) deallocate(adj_t)
        if(associated(adj_s) ) deallocate(adj_s) 
      end subroutine ROMS_oda_final
!-----------------------------------------------------------------------------
      subroutine ROMS_oda(ng)
        integer, intent(in) :: ng
        integer :: ns_t 

        if(oda_init<0)return
        if(ng/=1)return
        call ROMS_oda_init(ng)
        if(oda_init==0)return

        if(icalls>maxcalls)then
          call ROMS_oda_final
          oda_init=-2
          return
        endif

        ! read in adjustments from EAKF
        icalls=icalls+1
        if(icalls==1)call readadjs(ng)
        if(oda_init==0)then
          write(stdout,*) "ODA ERR No Adjust-data"
          oda_init=-3
          return
        endif
        !write(stdout,*) "ODA icalls=",icalls
        ! add the adj data
        DO ns_t=1,3
          if(ealive)then
            OCEAN(ng) % zeta(:,:,ns_t)=OCEAN(ng)%zeta(:,:,ns_t) &
                                       *(1.0-alpha)       &
     &                                    +adj_el(:,:)*alpha      
          endif
#ifdef SOLVE3D
          if(talive)then
            OCEAN(ng) % t(:,:,:,ns_t,1)=                                &
     &                                OCEAN(ng) % t(:,:,:,ns_t,1)       &
                                       *(1.0-alpha)       &
     &                                   +adj_t(:,:,:)*alpha
          endif
          if(salive)then
            OCEAN(ng) % t(:,:,:,ns_t,2)=                                &
     &                                OCEAN(ng) % t(:,:,:,ns_t,2)       &
                                       *(1.0-alpha)       &
     &                                    +adj_s(:,:,:)*alpha       
          endif
# endif
        END DO
#ifdef SOLVE3D
#ifdef ADJUV3D
        DO ns_t=1,2
          if(ualive)then
            OCEAN(ng) % u(:,:,:,ns_t)=                                  &
     &                                OCEAN(ng) % u(:,:,:,ns_t)         &
                                       *(1.0-alpha)       &
     &                                    +adj_u(:,:,:)*alpha
            !write(*,*),'u',alpha,maxval(adj_u)
          endif
          if(valive)then
            OCEAN(ng) % v(:,:,:,ns_t)=                                  &
     &                                OCEAN(ng) % v(:,:,:,ns_t)         &
                                       *(1.0-alpha)       &
     &                                    +adj_v(:,:,:)*alpha  
          endif
        END DO
#endif
# endif
      end subroutine ROMS_oda
!-----------------------------------------------------------------------------
      subroutine readadjs(ng)
        integer, intent(in) :: ng
        ! read adj_t, adj_s, adj_u, adj_v, adj_el
        real(r8), parameter :: Fscl = 1.0_r8
        real(r8) :: Fmin, Fmax
        character(len=200) :: pfname
        logical :: Lregrid
        integer :: gtype,Vsize(4),is,ncid,Vid, adstatus,alive,i,j,k
!---------------------------------zeta---------------------------------
        Lregrid=.FALSE.
        DO is=1,4
          Vsize(is)=0
        END DO
!---------------------------------zeta---------------------------------
        pfname='input_data/adj/curr_zeta_'//time_code(ng)(1:4)//        &
     &  time_code(ng)(6:7)//time_code(ng)(9:10)//time_code(ng)(12:13)// &
     &  '.nc'
        inquire(file=pfname, exist=ealive)
        if (ealive) then
          call netcdf_open (ng, iNLM, pfname, 1, ncid)
          call netcdf_inq_varid (ng, iNLM, pfname, 'zeta',ncid, Vid)
          gtype=r2dvar
          adstatus=nf_fread2d(ng, iNLM, pfname, ncid,                   &
     &                                 'zeta', Vid,                     &
     &                            0, gtype, Vsize,                      &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            Fscl, Fmin, Fmax,                     &
#ifdef MASKING
     &                           GRID(ng) % rmask,                      &
#endif
     &                            adj_el, Lregrid)
          write(stdout,*),'ODA reading zeta: ',pfname,maxval(adj_el)
        else
          oda_init=0
        endif
!---------------------------------u--------------------------------------
#ifdef ADJUV3D
        pfname='input_data/adj/curr_u_'//time_code(ng)(1:4)//        &
     &  time_code(ng)(6:7)//time_code(ng)(9:10)//time_code(ng)(12:13)// &
     &  '.nc'
        inquire(file=pfname, exist=ualive)
        if (ualive) then
          call netcdf_open (ng, iNLM, pfname, 1, ncid)
          call netcdf_inq_varid (ng, iNLM, pfname, 'u',ncid, Vid)
          gtype=u3dvar
          adstatus=nf_fread3d(ng, iNLM, pfname, ncid,                   &
     &                            'u', Vid,                             &
     &                            0, gtype, Vsize,                      &
     &                            LBi, UBi, LBj, UBj, 1,N(ng),          &
     &                            Fscl, Fmin, Fmax,                     &
#ifdef MASKING
     &                           GRID(ng) % umask,                      &
#endif
     &                            adj_u)
          write(stdout,*),'ODA reading u: ',pfname,maxval(adj_u)
        else
          oda_init=0
        endif
!---------------------------------v--------------------------------------
        pfname='input_data/adj/curr_v_'//time_code(ng)(1:4)//        &
     &  time_code(ng)(6:7)//time_code(ng)(9:10)//time_code(ng)(12:13)// &
     &  '.nc'
        inquire(file=pfname, exist=valive)
        if (valive) then
          call netcdf_open (ng, iNLM, pfname, 1, ncid)
          call netcdf_inq_varid (ng, iNLM, pfname, 'v',ncid, Vid)
          gtype=v3dvar
          adstatus=nf_fread3d(ng, iNLM, pfname, ncid,                   &
     &                            'v', Vid,                             &
     &                            0, gtype, Vsize,                      &
     &                            LBi, UBi, LBj, UBj, 1,N(ng),          &
     &                            Fscl, Fmin, Fmax,                     &
#ifdef MASKING
     &                           GRID(ng) % vmask,                      &
#endif
     &                            adj_v)
         write(stdout,*),'ODA reading v: ',pfname,Fmax,maxval(adj_v)
        else
          oda_init=0
        endif
#endif
!---------------------------------temp--------------------------------------
        pfname='input_data/adj/curr_temp_'//time_code(ng)(1:4)//        &
     &  time_code(ng)(6:7)//time_code(ng)(9:10)//time_code(ng)(12:13)// &
     &  '.nc'
        inquire(file=pfname, exist=talive)
        if (talive) then
          call netcdf_open (ng, iNLM, pfname, 1, ncid)
          call netcdf_inq_varid (ng, iNLM, pfname, 'temp',ncid, Vid)
          gtype=r3dvar
          adstatus=nf_fread3d(ng, iNLM, pfname, ncid,                   &
     &                            'temp', Vid,                          &
     &                            0, gtype, Vsize,                      &
     &                            LBi, UBi, LBj, UBj, 1,N(ng),          &
     &                            Fscl, Fmin, Fmax,                     &
#ifdef MASKING
     &                           GRID(ng) % rmask,                      &
#endif
     &                            adj_t)
          write(stdout,*),'ODA reading temp',pfname,Fmax,maxval(adj_t)
        else
          oda_init=0
        endif
!---------------------------------salt--------------------------------------
        pfname='input_data/adj/curr_salt_'//time_code(ng)(1:4)//        &
     &  time_code(ng)(6:7)//time_code(ng)(9:10)//time_code(ng)(12:13)// &
     &  '.nc'
        inquire(file=pfname, exist=salive)
        if (salive) then
          call netcdf_open (ng, iNLM, pfname, 1, ncid)
          call netcdf_inq_varid (ng, iNLM, pfname, 'salt',ncid, Vid)
          gtype=r3dvar
          adstatus=nf_fread3d(ng, iNLM, pfname, ncid,                   &
     &                            'salt', Vid,                          &
     &                            0, gtype, Vsize,                      &
     &                            LBi, UBi, LBj, UBj, 1,N(ng),          &
     &                            Fscl, Fmin, Fmax,                     &
#ifdef MASKING
     &                           GRID(ng) % rmask,                      &
#endif
     &                            adj_s)
         write(stdout,*),'ODA readig salinity',pfname,Fmax,maxval(adj_s)
        else
          oda_init=0
        endif
      end subroutine readadjs
!-----------------------------------------------------------------------
      end module ROMS_oda_mod
