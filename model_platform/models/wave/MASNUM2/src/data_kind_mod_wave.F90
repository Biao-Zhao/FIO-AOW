!======================================================================
!CVS: $Id: data_kind_mod_wave.F90,v 1.1 2013/06/20 08:53:25 wgs Exp $
!CVS: $Source: /soa04/users/wgs/.mycvsroot/intercomm/wave/data_kind_mod_wave.F90,v $
!CVS: $Name:  $
!======================================================================
MODULE data_kind_mod_wave
   implicit none
   SAVE
   !----------------------------------------------------------------------------
   ! precision/kind constants add data public
   !----------------------------------------------------------------------------
   integer,parameter :: KIND_R16= selected_real_kind(24) ! 16 byte real
   integer,parameter :: KIND_R8 = selected_real_kind(12) ! 8 byte real
   integer,parameter :: KIND_R4 = selected_real_kind( 6) ! 4 byte real
   integer,parameter :: KIND_RN = kind(1.0)              ! native real
   integer,parameter :: KIND_I8 = selected_int_kind (13) ! 8 byte integer
   integer,parameter :: KIND_I4 = selected_int_kind ( 6) ! 4 byte integer
   integer,parameter :: KIND_IN = kind(1)                ! native integer
   integer,parameter :: KIND_CL = 256                    ! long char
   integer,parameter :: KIND_CS = 80                     ! short char
   integer :: MPI_F,MPI_I

END MODULE data_kind_mod_wave
