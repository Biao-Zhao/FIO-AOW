!###############################################################################
!-------------------------------------------------------------------------------

  module time_mod

!-------------------------------------------------------------------------------
! ******************************************************************************
!-------------------------------------------------------------------------------
!                                                Copyright (C) 2005 Xunqiang Yin
!                                                MODULE NAME : time_mod
!                                                PRESENT VERSION : 2005-11-04
!
! --- USAGE : To convert time among different types: datestr, datevec, datenum.
! --- DEPEND: None
!
! --- NOTE for describing of subroutine / function : 
!  A. The parameters bracketed with [], means optional parameter.
!  B. The describe for the parameters of subroutine / function, started with: 
!   * It means input prameter;
!   # It means output prameter;
!   @ It means input and output prameter(it will be changed inside).
!
!-------------------------------------------------------------------------------
! ******************************************************************************
! ***                       INTERFACE DESCRIBE                               ***
! ******************************************************************************
!-------------------------------------------------------------------------------
!
!  1. datestr : Get the date in a string.
!
!     datestr(datevec_in)
!     datestr(datevec_in, datestr_form)
!     datestr(datenum_in, datenum_type)
!
!    # datestr      = a string contains a date with the formate of datestr_form.
!    * datestr_form = the indexs of the formate of datestr.
!    * datevec_in   = a vector contains : yyyy, mm, dd, HH, MM, SS.
!    * datenum_in   = a double precision number --- the counted time from 
!                     reference_date. (in days, hours, minutes or seconds)
!    * datenum_type = the unit type for datenum ( 1 = days, 2 = hours, 
!                              3 = minutes & 4 = seconds.)
!
!  2. datevec : Get the date in a vector.
!
!     datevec(datestr_in)
!     datevec(datestr_in, datestr_form)
!     datevec(datenum_in, datenum_type)
!
!    # datevec      = a vector contains : yyyy, mm, dd, HH, MM, SS.
!    * datestr_in   = a string contains a date with the formate of datestr_form.
!    * datestr_form = the indexs of the formate of datestr.
!    * datenum_in   = a double precision number --- the counted time from 
!                     reference_date. (in days, hours, minutes or seconds)
!    * datenum_type = the unit type for datenum ( 1 = days, 2 = hours, 
!                              3 = minutes & 4 = seconds.)
!
!  3. datenum : Get the date in a double precision number.
!
!     datenum(datestr_in, datenum_type)
!     datenum(datevec_in, datenum_type)
!     datenum(datestr_in, datestr_form, datenum_type)
!
!    # datenum      = a double precision number --- the counted time from 
!                     reference_date. (in days, hours, minutes or seconds)
!    * datestr_in   = a string contains a date with the formate of datestr_form.
!    * datevec_in   = a vector contains : yyyy, mm, dd, HH, MM, SS.
!    * datestr_form = the indexs of the formate of datestr.
!    * datenum_type = the unit type for datenum ( 1 = days, 2 = hours, 
!                              3 = minutes & 4 = seconds.)
!
!                                     --- Written by Xunqiang Yin, 2005-11-04
!
!-------------------------------------------------------------------------------
!
!  4. init_time_mod : Initialize for time_mod (Set refference date for counting).
!
!     init_time_mod(reference_date_in)
!     
!   * reference_date_in = New refference date for count. Default is 
!                         [0, 1, 1, 0, 0, 0] (0000-01-01 00:00:00).
!
!                                     --- Written by Xunqiang Yin, 2005-11-08
!
!-------------------------------------------------------------------------------
!
! --- List of datestr_form
!
!    Index            Format                   Example
!--------------------+------------------------+-----------------------
!       0             'dd-mmm-yyyy HH:MM:SS'   01-Mar-2000 15:45:17 
!       1             'dd-mmm-yyyy'            01-Mar-2000  
!       2             'mm/dd/yy'               03/01/00     
!       3             'mmm'                    Mar          
!       4             'm'                      M            
!       5             'mm'                     03            
!       6             'mm/dd'                  03/01        
!       7             'dd'                     01            
!       8             'ddd'                    Wed          
!       9             'd'                      W            
!      10             'yyyy'                   2000         
!      11             'yy'                     00           
!      12             'mmmyy'                  Mar00        
!      13             'HH:MM:SS'               15:45:17     
!      14             'HH:MM:SS PM'             3:45:17 PM  
!      15             'HH:MM'                  15:45        
!      16             'HH:MM PM'                3:45 PM     
!      17             'QQ-YY'                  Q1-96        
!      18             'QQ'                     Q1           
!      19             'dd/mm'                  01/03        
!      20             'dd/mm/yy'               01/03/00     
!      21             'mmm.dd,yyyy HH:MM:SS'   Mar.01,2000 15:45:17 
!      22             'mmm.dd,yyyy'            Mar.01,2000  
!      23             'mm/dd/yyyy'             03/01/2000 
!      24             'dd/mm/yyyy'             01/03/2000 
!      25             'yy/mm/dd'               00/03/01 
!      26             'yyyy/mm/dd'             2000/03/01 
!      27             'QQ-YYYY'                Q1-1996        
!      28             'mmmyyyy'                Mar2000        
!      29 (ISO 8601)  'yyyy-mm-dd'             2000-03-01
!      30 (ISO 8601)  'yyyymmddTHHMMSS'        20000301T154517 
!      31             'yyyy-mm-dd HH:MM:SS'    2000-03-01 15:45:17 
! ! --- Following the list of MATLAB
!
!      -1             'yyyymmddHHMMSS'        20000301154517 
!
! ******************************************************************************
!-------------------------------------------------------------------------------

  implicit none

!--------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  public init_time_mod, datestr, datevec, datenum
  private

!--------------------------------------------------------------------------------

  interface datestr
    module procedure datestr0, datestr1, datestr2
  end interface datestr

!--------------------------------------------------------------------------------

  interface datevec
    module procedure datevec0, datevec1, datevec2
  end interface datevec

!--------------------------------------------------------------------------------

  interface datenum
    module procedure datenum0, datenum1, datenum2
  end interface datenum

!-------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  integer, parameter :: &
    ConMonDays(12) = [0 , 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334]

  integer :: reference_date(6) = [0, 1, 0, 0, 0, 0]
  character ( len = 9 ), parameter, dimension(12) :: month_long = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  character ( len = 3 ), parameter, dimension(12) :: month_short = (/ &
    'Jan', 'Feb', 'Mar', 'Apr', &
    'May', 'Jun', 'Jul', 'Aug', &
    'Sep', 'Oct', 'Nov', 'Dec' /)

!--------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  contains

!--------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!*DeckYinxq : init_time_mod

  subroutine init_time_mod(reference_date_in)

  implicit none

  integer, intent(in) :: reference_date_in(:)

  reference_date(1:min(6, size(reference_date_in))) =               &
                reference_date_in(1:min(6, size(reference_date_in)))

  return

  end subroutine init_time_mod

!--------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!*DeckYinxq : datestr0

  character(len=19) function datestr0(datevec_in)

  implicit none

  integer, intent(in) :: datevec_in(:)

  integer :: date(6), datevec_in_length
  character(len=80) :: datestr_format

  date = 0; datevec_in_length = size(datevec_in)
  date(1:datevec_in_length) = datevec_in(1:datevec_in_length)

  datestr_format = '(i4.4,a,i2.2,a,i2.2,1x,i2.2,a,i2.2,a,i2.2)'

  write(datestr0, datestr_format)date(1), '-', date(2), '-', date(3), &
                                 date(4), ':', date(5), ':', date(6)

  return

  end function datestr0

!--------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!*DeckYinxq : datestr1

  character(len=50) function datestr1(datevec_in, datestr_form)

  implicit none

  integer, intent(in) :: datevec_in(:), datestr_form

  integer :: date(6), datevec_in_length
  character(len=80) :: datestr_format

  date = 0; datevec_in_length = size(datevec_in)
  date(1:datevec_in_length) = datevec_in(1:datevec_in_length)

  if     (datestr_form == 0) then

    ! --- Format : 'dd-mmm-yyyy HH:MM:SS'
    datestr_format = '(i2.2,a1,a3,a1,i4.4,1x,i2.2,a,i2.2,a,i2.2)'
    write(datestr1, datestr_format)date(3), '-', month_short(date(2)), '-', &
                                   date(1),                                 &
                                   date(4), ':', date(5), ':', date(6)

  elseif (datestr_form == 1) then

    ! --- Format : 'dd-mmm-yyyy'
    datestr_format = '(i2.2,a1,a3,a1,i4.4)'
    write(datestr1, datestr_format)date(3), '-', month_short(date(2)), '-', &
                                   date(1)
  
  elseif (datestr_form == 2) then

    ! --- Format : 'mm/dd/yy'
    datestr_format = '(i2.2,a,i2.2,a,i2.2)'
    write(datestr1, datestr_format)date(3), '/', date(2), '/', mod(date(1), 100)
  
  elseif (datestr_form == 3) then

    ! --- Format : 'mmm'
    datestr_format = '(a3)'
    write(datestr1, datestr_format)month_short(date(2))
  
  elseif (datestr_form == 4) then

    ! --- Format : 'm'
    datestr_format = '(a1)'
    write(datestr1, datestr_format)month_short(date(2))
  
  elseif (datestr_form == 5) then

    ! --- Format : 'mm'
    datestr_format = '(i2.2)'
    write(datestr1, datestr_format)date(2)
  
  elseif (datestr_form == 6) then

    ! --- Format : 'mm/dd'
    datestr_format = '(i2.2,a1,i2.2)'
    write(datestr1, datestr_format)date(2), '/', date(3)
  
  elseif (datestr_form == 7) then

    ! --- Format : 'dd'
    datestr_format = '(i2.2)'
    write(datestr1, datestr_format)date(3)
  
  elseif (datestr_form == 8 .or. datestr_form == 9) then

    ! --- Format : 'ddd', 'd'
    datestr_format = '(a)'
    write(datestr1, datestr_format)'for week day, not ready'
  
  elseif (datestr_form == 10) then

    ! --- Format : 'yyyy'
    datestr_format = '(i4.4)'
    write(datestr1, datestr_format)date(1)
  
  elseif (datestr_form == 11) then

    ! --- Format : 'yy'
    datestr_format = '(i2.2)'
    write(datestr1, datestr_format)mod(date(1), 100)
  
  elseif (datestr_form == 12) then

    ! --- Format : 'yy'
    datestr_format = '(a3,i2.2)'
    write(datestr1, datestr_format)month_short(date(2)), mod(date(1), 100)
  
  elseif (datestr_form == 13) then

    ! --- Format : 'HH:MM:SS'
    datestr_format = '(i2.2,a,i2.2,a,i2.2)'
    write(datestr1, datestr_format)date(4), ':', date(5), ':', date(6)
  
  elseif (datestr_form == 14) then

    ! --- Format : 'HH:MM:SS PM'
    datestr_format = '(i2.2,a,i2.2,a,i2.2,a3)'
    if(date(4) > 12) then
      write(datestr1, datestr_format)mod(date(4), 12), ':', date(5), ':',   &
                                     date(6), ' PM'
    else
      write(datestr1, datestr_format)mod(date(4), 12), ':', date(5), ':',   &
                                     date(6), ' AM'
    endif

  elseif (datestr_form == 15) then

    ! --- Format : 'HH:MM'
    datestr_format = '(i2.2,a,i2.2)'
    write(datestr1, datestr_format)date(4), ':', date(5)
  
  elseif (datestr_form == 16) then

    ! --- Format : 'HH:MM PM'
    datestr_format = '(i2.2,a,i2.2,a3)'
    if(date(4) > 12) then
      write(datestr1, datestr_format)mod(date(4), 12), ':', date(5), ' PM'
    else
      write(datestr1, datestr_format)mod(date(4), 12), ':', date(5), ' AM'
    endif

  elseif (datestr_form == 17 .or. datestr_form == 18) then

    ! --- Format : 'QQ-yy', 'QQ'
    datestr_format = '(a)'
    write(datestr1, datestr_format)'QQ, none'
  
  elseif (datestr_form == 19) then

    ! --- Format : 'dd/mm'
    datestr_format = '(i2.2,a,i2.2)'
    write(datestr1, datestr_format)date(3), '-', date(2)

  elseif (datestr_form == 20) then

    ! --- Format : 'dd/mm/yy'
    datestr_format = '(i2.2,a,i2.2,a,i2.2)'
    write(datestr1, datestr_format)date(3), '-', date(2), '-', mod(date(1), 100)

  elseif (datestr_form == 21) then

    ! --- Format : 'mmm.dd,yyyy HH:MM:SS'
    datestr_format = '(a3,a1,i2.2,a1,i4.4,1x,i2.2,a,i2.2,a,i2.2)'
    write(datestr1, datestr_format)month_short(date(2)), '.', date(3), '.',  &
                                   date(1),                                  &
                                   date(4), ':', date(5), ':', date(6)

  elseif (datestr_form == 22) then

    ! --- Format : 'mmm.dd,yyyy HH:MM:SS'
    datestr_format = '(a3,a1,i2.2,a1,i4.4)'
    write(datestr1, datestr_format)month_short(date(2)), '.', date(3), '.',  &
                                   date(1)

  elseif (datestr_form == 23) then

    ! --- Format : 'mm/dd/yyyy'
    datestr_format = '(i2.2,a,i2.2,a,i4.4)'
    write(datestr1, datestr_format)date(2), '/', date(3), '/', date(1)
  
  elseif (datestr_form == 24) then

    ! --- Format : 'dd/mm/yyyy'
    datestr_format = '(i2.2,a,i2.2,a,i4.4)'
    write(datestr1, datestr_format)date(3), '/', date(2), '/', date(1)
  
  elseif (datestr_form == 25) then

    ! --- Format : 'yy/mm/dd'
    datestr_format = '(i2.2,a,i2.2,a,i2.2)'
    write(datestr1, datestr_format)mod(date(1),100), '-', date(2), '-', date(3)
  
  elseif (datestr_form == 26) then

    ! --- Format : 'yyyy/mm/dd'
    datestr_format = '(i4.4,a,i2.2,a,i2.2)'
    write(datestr1, datestr_format)date(1), '/', date(2), '/', date(3)
  
  elseif (datestr_form == 27) then

    ! --- Format : 'QQ-YYYY'
    datestr_format = '(a)'
    write(datestr1, datestr_format)'QQ, none'
  
  elseif (datestr_form == 28) then

    ! --- Format : 'mmmyyyy'
    datestr_format = '(a3,i4.4)'
    write(datestr1, datestr_format)month_short(date(2)), date(1)
  
  elseif (datestr_form == 29) then

    ! --- Format : 'yyyy-mm-dd'
    datestr_format = '(i4.4,a,i2.2,a,i2.2)'
    write(datestr1, datestr_format)date(1), '-', date(2), '-', date(3)
  
  elseif (datestr_form == 30) then

    ! --- Format : 'yyyymmddTHHMMSS'
    datestr_format = '(i4.4,i2.2,i2.2,a1,i2.2,i2.2,i2.2)'
    write(datestr1, datestr_format)date(1), date(2), date(3), 'T', date(4), &
                                   date(5), date(6)
  
  elseif (datestr_form == 31) then

    ! --- Format : 'yyyy-mm-dd HH:MM:SS'
    datestr_format = '(i4.4,a,i2.2,a,i2.2,1x,i2.2,a,i2.2,a,i2.2)'
    write(datestr1, datestr_format)date(1), '-', date(2), '-', date(3),     &
                                   date(4), ':', date(5), ':', date(6)
  
  elseif (datestr_form == -1) then

    ! --- Format : 'yyyymmddHHMMSS'
    datestr_format = '(i4.4,i2.2,i2.2,i2.2,i2.2,i2.2)'
    write(datestr1, datestr_format)date(1), date(2), date(3),                & 
                                   date(4), date(5), date(6)
  
  else

   write(*, *)'Undefined datestr_form : ', datestr_form

  endif

  return

  end function datestr1

!--------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!*DeckYinxq : datestr2

  character(len=19) function datestr2(datenum_in, datenum_type)

  implicit none

  double precision, intent(in) :: datenum_in
  integer, intent(in) :: datenum_type

  datestr2 = datestr0(datevec2(datenum_in, datenum_type))

  return

  end function datestr2

!--------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!*DeckYinxq : datevec0

  function datevec0(datestr_in)

  implicit none

  character(len=*), intent(in) :: datestr_in
  integer, dimension(6) :: datevec0

  character(len=80) ::                                                      &
         datestr_format = '(i4.4,T6,i2.2,T9,i2.2,1x,i2.2,T15,i2.2,T18,i2.2)'

  read(datestr_in, datestr_format)datevec0

  return

  end function datevec0

!--------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!*DeckYinxq : datevec1

  function datevec1(datestr_in, datestr_form)

  implicit none

  character(len=*), intent(in) :: datestr_in
  character(len=len_trim(datestr_in)) :: datestrtmp
  integer, intent(in) :: datestr_form
  integer, dimension(6) :: datevec1
  integer i

  character(len=80) ::                                                        &
            datestr_format = '(i4.4,T6,i2.2,T9,i2.2,1x,i2.2,T15,i2.2,T18,i2.2)'

  datevec1 = 0
  if     (datestr_form == 29) then

    ! --- Format : 'yyyy-mm-dd'
    datestr_format = '(i4.4,T6,i2.2,T9,i2.2)'
    read(datestr_in, datestr_format)datevec1(1:3)

  elseif (datestr_form == 31) then

    ! --- Format : 'yyyy-mm-dd HH:MM:SS'
    datestr_format = '(i4.4,x,i2.2,x,i2.2,1x,i2.2,x,i2.2,x,i2.2)'
    datestrtmp=datestr_in
    do i=1,len_trim(datestrtmp)
    if(datestrtmp(I:I)=='-'.OR.datestrtmp(I:I)==':')datestrtmp(I:I)=' '
    end do 
   
    read(datestrtmp, datestr_format)datevec1

  
  else

   write(*, *)'Undefined datestr_form : ', datestr_form

  endif

  return

  end function datevec1

!--------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!*DeckYinxq : datevec2

  function datevec2(datenum_in, datenum_type)

  implicit none

  double precision, intent(in) :: datenum_in
  integer, intent(in) :: datenum_type
  integer, dimension(6) :: datevec2

  double precision :: datenum, temp_seconds, reference_seconds
  integer :: i, leaps

! --- 1. Reverse for time : HH, MM, SS

  reference_seconds = reference_date(4) * 3600  &
                    + reference_date(5) * 60 + reference_date(6)

  if (datenum_type == 1)then
    datenum      = int(datenum_in)
    temp_seconds = (datenum_in - datenum) * 86400. + reference_seconds
  endif

  if (datenum_type == 2)then
    datenum      = int(datenum_in /  24)
    temp_seconds = (datenum_in - datenum * 24) * 3600. + reference_seconds
  endif

  if (datenum_type == 3)then
    datenum      = int(datenum_in / 1440)
    temp_seconds = (datenum_in - datenum * 1440) * 60. + reference_seconds
  endif

  if (datenum_type == 4)then
    datenum      = int(datenum_in / 86400.)
    temp_seconds = datenum_in - datenum * 86400. + reference_seconds
  endif

  datevec2(4) = mod(temp_seconds / 3600., 60.)
  datevec2(5) = mod(temp_seconds / 60.  , 60.) 
  datevec2(6) = nint(mod(temp_seconds , 60.))

! --- Adjust hours, Xunqiang Yin, 2006-11-4 23:38

  if(datevec2(6) >= 60)then
    datevec2(6) = datevec2(6) - 60
    datevec2(5) = datevec2(5) + 1
  endif

  if(datevec2(5) >= 60)then
    datevec2(5) = datevec2(5) - 60
    datevec2(4) = datevec2(4) + 1
  endif

  if(datevec2(4) >= 24)then
    datevec2(4) = datevec2(4) - 24
    datenum = datenum + 1
  endif

! --- 2. Reverse for date : yyyy, mm, dd.

! --- 2.1 Reversr days and first guess of year.

  datenum = datenum + ConMonDays(reference_date(2))  &
          + reference_date(3)
  datevec2(1) = datenum / 365 + reference_date(1)

! --- 2.2 Count leaps.

  leaps = 0
  if (reference_date(2) > 2) then
    i = reference_date(1)
    if( (mod(i, 4) == 0 .and. mod(i, 100) /= 0) .or. &
        mod(i, 400) == 0)leaps = -1
  endif
  do i = reference_date(1), datevec2(1) - 1
    if( (mod(i, 4) == 0 .and. mod(i, 100) /= 0) .or. &
        mod(i, 400) == 0)leaps = leaps + 1
  enddo

! --- 2.3 Determine days and year.

  datevec2(3) = datenum - (datevec2(1) - reference_date(1)) * 365 - leaps

  do while(datevec2(3) <= 0)
    datevec2(1) = datevec2(1) - 1; i = datevec2(1);
    if( (mod(i, 4) == 0 .and. mod(i, 100) /= 0) .or. &
        mod(i, 400) == 0)leaps = leaps - 1
    datevec2(3) = datenum - (datevec2(1) - reference_date(1)) * 365 - leaps
  enddo

! --- 2.4 Determine month & day.

  i =  datevec2(1); leaps = 0; datevec2(2) = 1
  if((mod(i, 4) == 0 .and. mod(i, 100) /= 0) .or. mod(i, 400) == 0)leaps = 1
  do while(datevec2(3) > ConMonDays(datevec2(2)+1))
    datevec2(2) = datevec2(2) + 1
    if(datevec2(2) == 2 ) datevec2(3) = datevec2(3) - leaps
    if(datevec2(2) == 12) exit
  enddo
  datevec2(3) = datevec2(3) - ConMonDays(datevec2(2))

  if(datevec2(2) == 2 ) datevec2(3) = datevec2(3) + leaps

  return

  end function datevec2

!--------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!*DeckYinxq : datenum1

  double precision function datenum1(datevec_in, datenum_type)

  implicit none

  integer, intent(in) :: datevec_in(:)
  integer, intent(in) :: datenum_type

  integer :: date(6), leaps, i

  double precision :: temp_seconds, reference_seconds


  date = 0; date(1:size(datevec_in)) = datevec_in(:)

! --- count leap years during reference_date to date. 

  leaps = 0

  do i = reference_date(1), date(1) - 1
    if( (mod(i, 4) == 0 .and. mod(i, 100) /= 0) .or. &
        mod(i, 400) == 0)leaps = leaps + 1
  enddo

  if( date(2) > 2 )then
    i = date(1)
    if( (mod(i, 4) == 0 .and. mod(i, 100) /= 0) .or. &
        mod(i, 400) == 0)leaps = leaps + 1
  endif

! --- count days during reference_date to date

  datenum1 = (date(1) - reference_date(1)) * 365 + leaps            &
          + ConMonDays(date(2)) - ConMonDays(reference_date(2))  &
          + date(3) - reference_date(3)

! --- count time during reference_date to date

  reference_seconds = reference_date(4) * 3600  &
                    + reference_date(5) *60 + reference_date(6)
  temp_seconds = date(4) * 3600  + date(5) *60 + date(6) - reference_seconds

  if (datenum_type == 1) datenum1 = datenum1         + temp_seconds / 86400.
  if (datenum_type == 2) datenum1 = datenum1 *  24   + temp_seconds / 3600.
  if (datenum_type == 3) datenum1 = datenum1 * 1440  + temp_seconds / 60.
  if (datenum_type == 4) datenum1 = datenum1 * 86400.+ temp_seconds 

  return
  
  end function datenum1

!--------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!*DeckYinxq : datenum2

  double precision function datenum2(datestr_in, datestr_form, datenum_type)

  implicit none

  character(len=*), intent(in) :: datestr_in
  integer, intent(in) :: datestr_form, datenum_type

  datenum2 = datenum1(datevec1(datestr_in, datestr_form), datenum_type)

  return

  end function datenum2

!--------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!*DeckYinxq : datenum0

  double precision function datenum0(datestr_in, datenum_type)

  implicit none

  character(len=*), intent(in) :: datestr_in
  integer, intent(in) :: datenum_type

  datenum0 = datenum1(datevec0(datestr_in), datenum_type)

  return

  end function datenum0

!--------------------------------------------------------------------------------

  end module time_mod

!-------------------------------------------------------------------------------
!###############################################################################
