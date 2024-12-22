module sea_surface_albedo
real, parameter, private   :: PI=3.1415927
real, parameter, private   :: rad2deg=180.0/PI
real, parameter, private   :: deg2rad=PI/180.0
contains


!=======================================================================================================
!                              Broad band sea surface albedo                                           =
!     This subroutine is used to substitute the constant sea surface albedo in WRF by Huang (2019)     =
!     and payne (2018)                                                                                 =
!                                                                                                      =
!                                                                                                      =
!                                                             added by Dr. Zhao Biao 2019.05.11        =
!     References:                                                                                      =
!     Payne, R. E. (1972). Albedo of the sea surface. Journal of the Atmospheric Sciences, 29(5),      =
!     959–970. https://doi.org/10.1175/1520-0469(1972)029<0959:AOTSS>2.0.CO;2                          =
!                                                                                                      =
!     Huang, C. J., Qiao, F., Chen, S., Xue, Y., & Guo, J. (2019). Observation and parameterization    =
!     of broadband sea surface albedo. Journal of Geophysical Research: Oceans, 124, 4480–4491.        =
!     https://doi.org/10.1029/2018JC014444                                                             =
! =======================================================================================================    


subroutine albedo_huang(coszen,tra,u10,v10,t2,q2,psfc,hwave,albedo)
implicit none
real,   intent(in)    :: coszen,tra
real,   intent(in)    :: u10,v10,t2,q2,psfc,hwave
real,   intent(out)   :: albedo
!-------------------------------------------------------------------------------------------------------------------------------------------------------------
real                :: albedo_clr,albedo_cld
real                :: zenith,csza,transmittance,sgwh,es,e0,qs,qv,pres,wspd10,whitecap
character(len=800) :: message
real                :: albedo_peace,albedo_whitecap=0.465       !base on Gregory Sinnett et al.(2016),Methods in Oceanography
!coszen               cosine of zenith angel                           (0-1)
!transmittance        transmittance of whole layer of atmosphere       (0-1)
!u10                  u component of wind speed at 10 meter            (m/s)
!v10                  v component of wind speed at 10 meter            (m/s)
!t2                   air temperature at 2 meter                       (kelvin)
!q2                   Water vapor mixing ratio at 2meter               (kg kg-1)
!psfc                 air surface pressure                             (pa)
!hwave                significant wave height                          (m)
!whitecap             whitecape fraction                               (0-1)
zenith=rad2deg*acos(coszen)
!if(zenith>80.0) zenith=80.0
csza=cos(zenith*deg2rad)

sgwh=hwave
if(sgwh>2.0)   sgwh=2.0                         ! upper limit is 2 meter

wspd10=sqrt(u10**2+v10**2)
whitecap=2.95*(wspd10**3.52)*1.0e-6
if(wspd10>12.0) wspd10=12.0                             ! upper limit is 12 m/s

transmittance=tra
if(transmittance<0.0) transmittance=0.0
if(transmittance>1.0) transmittance=1.0

pres=psfc/100.0                               ! conver from Pa to hPa
es=6.112*exp(17.67*(t2-273.15)/(t2-29.65))    ! saturation water vapor pressure (hPa)
qs=0.622*es/(pres-es)
qv=q2/(1+q2)                                  ! convert waveter vapor mixing ratio to specific humidity
e0=es*qv/qs/10.0                              ! rh=e0/es*100%=qv/qs*100%, conver from hPa to kPa
if(e0>3.5) e0=3.5                                ! upper limit is 3.5

albedo_clr  = 0.041/(csza**(4.0/3)+0.083)+0.0025*wspd10-0.005*e0-0.0015
albedo_cld  = 0.046+0.021*sgwh-0.006*e0

if(transmittance<0.25 .and. transmittance>=0.0)then
  albedo_peace = albedo_cld
else if(transmittance>=0.55 .and. transmittance<=1.0)then
  albedo_peace = albedo_clr
else if(transmittance<0.55 .and. transmittance>=0.25)then
  if(transmittance>=0.25 .and. transmittance<0.3) transmittance=0.3
  albedo_peace = (transmittance-0.25)/0.3*albedo_clr+(0.55-transmittance)/0.3*albedo_cld
end if

albedo=whitecap*albedo_whitecap+(1-whitecap)*albedo_peace

write(message,*) 'zeinith',zenith,'whitecap',whitecap,' hwave',hwave,' albedo_peace',albedo_peace, ' albedo', albedo
CALL wrf_debug(0,message)

end subroutine albedo_huang
       
subroutine albedo_payne(coszen,tra,albedo)
implicit none
real,   intent(in)  :: coszen
real,   intent(in)  :: tra
real,   intent(out) :: albedo
!-----------------------------------------------------------  local variables ----------------------------------------------------------------------------------
real,   dimension(21,46) :: albtab
real,   dimension(46)    :: zentab
real,   dimension(21)    :: transtab
integer                  :: i,j
integer                  :: in_ew,in_ew1,in_ew2
integer                  :: in_ns,in_ns1,in_ns2
real                     :: zenith,transmittance
real                     :: dzen_pre,dzen_now
real                     :: dtra_pre,dtra_now
real                     :: albedo1,albedo2
character(len=800)       :: message

data zentab/ 90,88,86,84,82,80,78,76,74,72,70,68,66,64,62,60,58,56,54,52,50,48,46,44,42,40,38,36,34,32,30,28,26,24,22,20,18,16,14,12,10,8,6,4,2,0/

data transtab/ .00,.05,.10,.15,.20,.25,.30,.35,.40,.45,.50,.55,.60,.65,.70,.75,.80,.85,.90,.95,1.00/ 
data albtab/ 0.061, 0.062, 0.072, 0.087, 0.115, 0.263, 0.235, 0.318, 0.395, 0.472, 0.542, 0.604, 0.655, 0.693, 0.719, 0.732, 0.730, 0.681, 0.581, 0.453, 0.425, &
             0.061, 0.062, 0.070, 0.083, 0.108, 0.145, 0.198, 0.263, 0.336, 0.415, 0.487, 0.547, 0.595, 0.631, 0.656, 0.670, 0.652, 0.602, 0.494, 0.398, 0.370, &
             0.061, 0.061, 0.068, 0.079, 0.098, 0.130, 0.174, 0.228, 0.290, 0.357, 0.424, 0.498, 0.556, 0.588, 0.603, 0.592, 0.556, 0.488, 0.393, 0.342, 0.325, &
             0.061, 0.061, 0.065, 0.073, 0.086, 0.110, 0.150, 0.192, 0.248, 0.306, 0.360, 0.407, 0.444, 0.469, 0.480, 0.474, 0.444, 0.386, 0.333, 0.301, 0.290, &
             0.061, 0.061, 0.065, 0.070, 0.082, 0.101, 0.131, 0.168, 0.208, 0.252, 0.295, 0.331, 0.358, 0.375, 0.385, 0.377, 0.356, 0.320, 0.288, 0.266, 0.255, &
             0.061, 0.061, 0.063, 0.068, 0.077, 0.092, 0.114, 0.143, 0.176, 0.210, 0.242, 0.272, 0.288, 0.296, 0.300, 0.291, 0.273, 0.252, 0.237, 0.266, 0.220, & 
             0.061, 0.061, 0.062, 0.066, 0.072, 0.084, 0.103, 0.127, 0.151, 0.176, 0.198, 0.219, 0.236, 0.245, 0.250, 0.246, 0.235, 0.222, 0.211, 0.205, 0.200, & 
             0.061, 0.061, 0.061, 0.065, 0.071, 0.079, 0.094, 0.113, 0.134, 0.154, 0.173, 0.185, 0.190, 0.193, 0.193, 0.190, 0.188, 0.185, 0.182, 0.180, 0.178, &
             0.061, 0.061, 0.061, 0.064, 0.067, 0.072, 0.083, 0.099, 0.117, 0.135, 0.150, 0.160, 0.164, 0.165, 0.164, 0.162, 0.160, 0.159, 0.158, 0.157, 0.157, &
             0.061, 0.061, 0.061, 0.063, 0.067, 0.072, 0.080, 0.092, 0.107, 0.125, 0.136, 0.141, 0.145, 0.145, 0.145, 0.144, 0.143, 0.142, 0.141, 0.140, 0.140, &
             0.061, 0.061, 0.061, 0.062, 0.065, 0.068, 0.074, 0.084, 0.097, 0.111, 0.121, 0.127, 0.130, 0.131, 0.131, 0.130, 0.129, 0.127, 0.126, 0.125, 0.122, &
             0.061, 0.061, 0.061, 0.061, 0.063, 0.067, 0.074, 0.082, 0.091, 0.102, 0.110, 0.116, 0.119, 0.118, 0.116, 0.114, 0.113, 0.111, 0.110, 0.109, 0.108, &
             0.061, 0.061, 0.061, 0.061, 0.062, 0.064, 0.070, 0.076, 0.085, 0.094, 0.101, 0.105, 0.107, 0.106, 0.103, 0.100, 0.097, 0.096, 0.095, 0.095, 0.095, &
             0.061, 0.061, 0.061, 0.060, 0.061, 0.063, 0.067, 0.072, 0.079, 0.086, 0.093, 0.097, 0.097, 0.097, 0.092, 0.088, 0.086, 0.084, 0.083, 0.083, 0.083, &
             0.061, 0.061, 0.061, 0.060, 0.061, 0.062, 0.065, 0.070, 0.075, 0.081, 0.086, 0.089, 0.090, 0.088, 0.084, 0.080, 0.077, 0.075, 0.074, 0.074, 0.074, &
             0.061, 0.061, 0.061, 0.060, 0.060, 0.061, 0.064, 0.067, 0.071, 0.076, 0.081, 0.083, 0.084, 0.081, 0.076, 0.072, 0.069, 0.067, 0.066, 0.065, 0.065, &
             0.061, 0.061, 0.060, 0.060, 0.060, 0.061, 0.063, 0.065, 0.068, 0.072, 0.076, 0.077, 0.076, 0.074, 0.071, 0.067, 0.064, 0.062, 0.061, 0.061, 0.061, &
             0.061, 0.061, 0.061, 0.060, 0.060, 0.061, 0.062, 0.064, 0.067, 0.071, 0.073, 0.074, 0.073, 0.069, 0.065, 0.062, 0.060, 0.058, 0.057, 0.057, 0.056, &
             0.061, 0.061, 0.060, 0.060, 0.060, 0.060, 0.061, 0.062, 0.065, 0.068, 0.069, 0.069, 0.068, 0.065, 0.061, 0.058, 0.055, 0.054, 0.053, 0.052, 0.052, &
             0.061, 0.061, 0.060, 0.060, 0.060, 0.060, 0.060, 0.062, 0.063, 0.066, 0.067, 0.066, 0.064, 0.061, 0.057, 0.054, 0.051, 0.050, 0.049, 0.048, 0.048, &
             0.061, 0.061, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.062, 0.065, 0.065, 0.063, 0.060, 0.057, 0.054, 0.050, 0.047, 0.046, 0.045, 0.044, 0.044, &
             0.061, 0.061, 0.060, 0.060, 0.060, 0.060, 0.060, 0.060, 0.061, 0.063, 0.064, 0.061, 0.058, 0.055, 0.051, 0.047, 0.044, 0.042, 0.041, 0.040, 0.040, &
             0.061, 0.061, 0.060, 0.060, 0.060, 0.060, 0.059, 0.060, 0.060, 0.062, 0.062, 0.059, 0.056, 0.052, 0.049, 0.045, 0.042, 0.040, 0.039, 0.038, 0.038, &
             0.061, 0.061, 0.060, 0.060, 0.060, 0.059, 0.059, 0.059, 0.060, 0.061, 0.060, 0.057, 0.054, 0.050, 0.047, 0.043, 0.039, 0.036, 0.034, 0.033, 0.033, &
             0.061, 0.061, 0.060, 0.060, 0.059, 0.059, 0.059, 0.059, 0.060, 0.060, 0.059, 0.056, 0.053, 0.049, 0.045, 0.041, 0.037, 0.035, 0.033, 0.032, 0.032, &
             0.061, 0.061, 0.060, 0.060, 0.060, 0.059, 0.059, 0.059, 0.059, 0.059, 0.058, 0.055, 0.051, 0.047, 0.043, 0.039, 0.035, 0.033, 0.032, 0.031, 0.031, &
             0.061, 0.061, 0.060, 0.060, 0.060, 0.059, 0.059, 0.059, 0.059, 0.058, 0.057, 0.054, 0.050, 0.046, 0.043, 0.039, 0.035, 0.032, 0.031, 0.030, 0.030, &
             0.061, 0.061, 0.060, 0.060, 0.060, 0.059, 0.059, 0.059, 0.058, 0.057, 0.056, 0.053, 0.049, 0.046, 0.042, 0.038, 0.035, 0.032, 0.030, 0.029, 0.029, &
             0.061, 0.061, 0.060, 0.060, 0.060, 0.059, 0.059, 0.059, 0.058, 0.057, 0.055, 0.053, 0.048, 0.044, 0.041, 0.037, 0.034, 0.031, 0.029, 0.028, 0.028, &
             0.061, 0.061, 0.060, 0.060, 0.060, 0.059, 0.059, 0.059, 0.058, 0.057, 0.055, 0.052, 0.048, 0.044, 0.040, 0.036, 0.033, 0.030, 0.028, 0.027, 0.027, &
             0.061, 0.061, 0.060, 0.060, 0.060, 0.059, 0.059, 0.058, 0.057, 0.056, 0.054, 0.051, 0.047, 0.043, 0.039, 0.036, 0.033, 0.030, 0.028, 0.027, 0.026, &
             0.061, 0.061, 0.060, 0.060, 0.060, 0.059, 0.059, 0.058, 0.057, 0.055, 0.053, 0.050, 0.046, 0.042, 0.039, 0.035, 0.032, 0.030, 0.028, 0.026, 0.026, &
             0.061, 0.061, 0.060, 0.060, 0.060, 0.059, 0.059, 0.058, 0.057, 0.055, 0.053, 0.050, 0.046, 0.042, 0.038, 0.035, 0.032, 0.030, 0.028, 0.026, 0.026, &
             0.061, 0.061, 0.060, 0.060, 0.060, 0.059, 0.059, 0.058, 0.057, 0.055, 0.052, 0.049, 0.045, 0.041, 0.038, 0.034, 0.032, 0.029, 0.027, 0.026, 0.026, &
             0.061, 0.061, 0.060, 0.060, 0.060, 0.059, 0.059, 0.058, 0.057, 0.055, 0.052, 0.049, 0.045, 0.041, 0.037, 0.033, 0.029, 0.027, 0.026, 0.025, 0.025, &
             0.061, 0.061, 0.060, 0.060, 0.060, 0.059, 0.059, 0.058, 0.057, 0.055, 0.052, 0.049, 0.045, 0.040, 0.036, 0.032, 0.029, 0.027, 0.026, 0.025, 0.025, &
             0.061, 0.061, 0.060, 0.060, 0.060, 0.059, 0.059, 0.058, 0.057, 0.055, 0.051, 0.048, 0.044, 0.040, 0.036, 0.032, 0.029, 0.027, 0.026, 0.025, 0.026, &
             0.061, 0.061, 0.060, 0.060, 0.060, 0.059, 0.059, 0.058, 0.056, 0.054, 0.051, 0.047, 0.043, 0.039, 0.035, 0.032, 0.029, 0.027, 0.025, 0.025, 0.025, &
             0.061, 0.061, 0.060, 0.060, 0.060, 0.059, 0.059, 0.057, 0.056, 0.053, 0.050, 0.047, 0.043, 0.039, 0.035, 0.031, 0.028, 0.026, 0.025, 0.025, 0.025, &
             0.061, 0.061, 0.060, 0.060, 0.060, 0.059, 0.059, 0.058, 0.056, 0.054, 0.050, 0.047, 0.043, 0.039, 0.034, 0.031, 0.028, 0.026, 0.025, 0.025, 0.025, &
             0.061, 0.061, 0.060, 0.060, 0.060, 0.059, 0.059, 0.058, 0.056, 0.053, 0.050, 0.046, 0.042, 0.038, 0.034, 0.031, 0.028, 0.026, 0.025, 0.025, 0.025, &
             0.061, 0.061, 0.060, 0.060, 0.060, 0.059, 0.059, 0.058, 0.056, 0.053, 0.050, 0.046, 0.042, 0.038, 0.034, 0.030, 0.028, 0.026, 0.025, 0.025, 0.025, &
             0.061, 0.061, 0.060, 0.060, 0.060, 0.059, 0.059, 0.058, 0.056, 0.054, 0.051, 0.047, 0.043, 0.038, 0.034, 0.030, 0.027, 0.026, 0.025, 0.025, 0.025, &
             0.061, 0.061, 0.060, 0.060, 0.059, 0.059, 0.058, 0.057, 0.056, 0.054, 0.050, 0.047, 0.042, 0.038, 0.034, 0.030, 0.027, 0.026, 0.025, 0.025, 0.025, &
             0.061, 0.061, 0.060, 0.060, 0.059, 0.059, 0.058, 0.057, 0.056, 0.053, 0.050, 0.046, 0.042, 0.038, 0.034, 0.030, 0.028, 0.026, 0.025, 0.025, 0.025, &
             0.061, 0.061, 0.060, 0.060, 0.059, 0.059, 0.058, 0.057, 0.055, 0.053, 0.050, 0.046, 0.042, 0.038, 0.034, 0.030, 0.028, 0.026, 0.025, 0.025, 0.025/
!-----------------------------------------------------------------------------------------------------------------------------------------------------------------
     zenith=rad2deg*acos(coszen)
     transmittance=tra
!     if(zenith>86.0) zenith=86.0
!     if(zenith<18.0) zenith=18.0 
     if(transmittance<0.0) transmittance=0.0
     if(transmittance>1.0) transmittance=1.0

     dzen_pre=2.0
     do j=1,46
        dzen_now=zentab(j)-zenith
        if(abs(dzen_now)<abs(dzen_pre))then
          dzen_pre=dzen_now
          in_ew=j
        else
          cycle
        end if 
     end do

     dtra_pre=0.05
     do i=1,21
        dtra_now=transmittance-transtab(i)
        if(abs(dtra_now)<abs(dtra_pre))then
          dtra_pre=dtra_now
          in_ns=i
        else
          cycle
        end if
     end do
 
     if (dzen_pre<0)then
        in_ew1=in_ew-1
        in_ew2=in_ew     
     else if(dzen_pre>0)then
        in_ew1=in_ew
        in_ew2=in_ew+1
     else if(dzen_pre==0)then
        in_ew1=in_ew
        in_ew2=in_ew        
     else
        write(message,*) 'can not find zenith angle in payne'
        CALL wrf_error_fatal(message)
     end if 


     if (dtra_pre<0)then
        in_ns1=in_ns-1
        in_ns2=in_ns
     else if(dtra_pre>0)then
        in_ns1=in_ns
        in_ns2=in_ns+1
     else if(dtra_pre==0)then
        in_ns1=in_ns
        in_ns2=in_ns     
     else
        write(message,*) 'can not find transmittance in payne'
        CALL wrf_error_fatal(message)
     end if

     if(in_ew1/=in_ew2 .and. in_ns1/=in_ns2)then
        albedo1 = (zentab(in_ew2)-zenith)/(zentab(in_ew2)-zentab(in_ew1))*albtab(in_ns1,in_ew1)+(zenith-zentab(in_ew1))/(zentab(in_ew2)-zentab(in_ew1))*albtab(in_ns1,in_ew2)
        albedo2 = (zentab(in_ew2)-zenith)/(zentab(in_ew2)-zentab(in_ew1))*albtab(in_ns2,in_ew1)+(zenith-zentab(in_ew1))/(zentab(in_ew2)-zentab(in_ew1))*albtab(in_ns2,in_ew2) 
        albedo  = (transtab(in_ns2)-transmittance)/(transtab(in_ns2)-transtab(in_ns1))*albedo1+(transmittance-transtab(in_ns1))/(transtab(in_ns2)-transtab(in_ns1))*albedo2
     else if(in_ew1==in_ew2 .and. in_ns1/=in_ns2)then
        albedo  = (transtab(in_ns2)-transmittance)/(transtab(in_ns2)-transtab(in_ns1))*albtab(in_ns1,in_ew1)+(transmittance-transtab(in_ns1))/(transtab(in_ns2)-transtab(in_ns1))*albtab(in_ns2,in_ew1)     
     else if(in_ew1/=in_ew2 .and. in_ns1==in_ns2)then
        albedo  = (zentab(in_ew2)-zenith)/(zentab(in_ew2)-zentab(in_ew1))*albtab(in_ns1,in_ew1)+(zenith-zentab(in_ew1))/(zentab(in_ew2)-zentab(in_ew1))*albtab(in_ns1,in_ew2)        
     else if(in_ew1==in_ew2 .and. in_ns1==in_ns2)then
        albedo=albtab(in_ns1,in_ew1)
     end if
     !write(*,*) "lower J=",in_ew1,"zenith=",zentab(in_ew1)
     !write(*,*) "upper J=",in_ew2,"zenith=",zentab(in_ew2)  
     !write(*,*) "lower I=",in_ns1,"transmit=",transtab(in_ns1)
     !write(*,*) "upper I=",in_ns2,"transmit=",transtab(in_ns2)
     !write(*,'(4f6.3)') albtab(in_ns1,in_ew1),albtab(in_ns1,in_ew2),albtab(in_ns2,in_ew1),albtab(in_ns2,in_ew2)
     !write(*,'(a,f6.3)') "albedo=",albedo,"transmittance",transmittance
     !write(message,*) "lower J=",in_ew1," zenith=",zentab(in_ew1)," upper J=",in_ew2," zenith=",zentab(in_ew2)," lower I=",in_ns1," transmit=",transtab(in_ns1), " upper I=",in_ns2," transmit=",transtab(in_ns2)," albedo=",albedo," transmittance",transmittance
      write(message,*)" albedo=",albedo," transmittance",transmittance, "zenith=",zenith
      CALL wrf_debug(0,message)
end subroutine albedo_payne
        
end module sea_surface_albedo
