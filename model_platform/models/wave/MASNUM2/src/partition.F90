!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  The below routine is subarea progress.
!  wrote by Guanso. Wang 14-Jan-2008.
!  Email:  wanggs@fio.org.cn
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  SUBROUTINE PARTITION(MNSP,IXCPU,IYCPU,IM,JM,ILAT,ILON,COE)
  IMPLICIT NONE
  include 'mpif.h'
! arguments
  INTEGER,INTENT(IN) ::  IM,JM,IXCPU,IYCPU,COE
  INTEGER,DIMENSION(IM,JM),INTENT(IN) :: MNSP
  INTEGER,DIMENSION(IXCPU+1),INTENT(OUT) :: ILAT
  INTEGER,DIMENSION(IYCPU+1,IXCPU),INTENT(OUT) :: ILON
! local
  INTEGER,DIMENSION(IM,JM) :: COUNT_NSP
  INTEGER II,JJ,JJ1,JJ2,KK,KK1,ITT,K,ISIZE
  INTEGER GLAND
  
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! COUNT_NSP(II,1) STORAGE THE NUMBER OF OCEAN GRID IN EVERY LONGITUDE
! COUNT_NSP(721,361) STORAGE THE TOTAL NUMBER OF OCEAN GRID
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  COUNT_NSP= 0
  COUNT_NSP(:,1) = SUM(MNSP,DIM=2)
  COUNT_NSP(IM,JM)=SUM(COUNT_NSP(:,1))
  COUNT_NSP(IM,JM)=INT((IM*JM-COUNT_NSP(IM,JM))/COE)+1+COUNT_NSP(IM,JM)
!  WRITE(6,*) COUNT_NSP(IM,JM),SUM(COUNT_NSP(:,1))

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  SUBAREA ALONG THE LATITUDE
!  ILAT STORAGE PARTITION COORDINATE OF LONGITUDE 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  K=1
  ISIZE=INT(COUNT_NSP(IM,JM)/(IXCPU))+1
  JJ = 1
  DO II=1,IM
  GLAND = 0
  GLAND = SUM(COUNT_NSP(JJ:II,1))
  GLAND = GLAND + INT((JM*(II-JJ+1)-GLAND)/COE) + 1
  IF(GLAND-ISIZE>1)THEN
  JJ=II-1
  K=K+1
  ILAT(K)=JJ 
  END IF
  END DO
  ILAT(1)=1
  ILAT(IXCPU+1)=IM


  DO II = IXCPU,2,-1
  KK1 = ILAT(II+1)
  KK = ILAT(II)+1 

  GLAND = SUM(COUNT_NSP(KK:KK1,1))
  GLAND = GLAND+INT((JM*(KK1-KK+1)-GLAND)/COE)+1
  JJ = INT((GLAND-ISIZE)/(1.*JM)+0.5)
  JJ1 = 0
  
  DO WHILE(JJ>=1)
  JJ1 = JJ1 + JJ
  KK1 = ILAT(II+1)
  KK = ILAT(II)+JJ1+1
  GLAND = SUM(COUNT_NSP(KK:KK1,1))
  GLAND = GLAND+INT((JM*(KK1-KK+1)-GLAND)/COE)+1
  JJ = INT((GLAND-ISIZE)/(1.*JM)+0.5)
  END DO
  
  ILAT(II)=ILAT(II)+JJ1
  END DO

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  ILON STORAGE PARTED COORDINATE OF LATITUDE 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ILON(1,:)=0
  ILON(IYCPU+1,:)=JM
  DO II=1,IXCPU

   KK=ILAT(II)+1
   IF(II==1)KK=ILAT(II)
   KK1=ILAT(II+1)
   ISIZE=SUM(COUNT_NSP(KK:KK1,1))
   ISIZE=ISIZE+INT(((KK1-KK+1)*JM-ISIZE)/COE)+1
   ISIZE=ISIZE/IYCPU
   JJ1=1;ITT=1

   DO JJ=1,JM
    GLAND=0
    GLAND=SUM(SUM(MNSP(KK:KK1,JJ1:JJ),DIM=2))
    GLAND=GLAND+INT(((KK1-KK+1)*(JJ-JJ1+1)-GlAND)/COE)
    IF(GLAND-ISIZE>1)THEN
    JJ1=JJ-1
    ITT=ITT+1
    IF(ITT==IYCPU+1)JJ1=JM
    ILON(ITT,II)=JJ1
    END IF
   END DO
!  adjust
   DO JJ=IYCPU,2,-1
   JJ2 = ILON(JJ+1,II)
   JJ1 = ILON(JJ,II)+1
   GLAND = SUM(SUM(MNSP(KK:KK1,JJ1:JJ2),DIM=2))
   GLAND = GLAND+INT(((KK1-KK+1)*(JJ2-JJ1+1)-GLAND)/COE)
   ITT = INT((GLAND-ISIZE)/(1.*(KK1-KK+1))+0.5)
   K=0
   DO WHILE(ITT>=1)
   K=K+ITT
   JJ1=ILON(JJ,II)+K+1
   GLAND = SUM(SUM(MNSP(KK:KK1,JJ1:JJ2),DIM=2))
   GLAND = GLAND+INT(((KK1-KK+1)*(JJ2-JJ1+1)-GLAND)/COE)
   ITT = INT((GLAND-ISIZE)/(1.*(KK1-KK+1))+0.5)
   END DO
   ILON(JJ,II)=ILON(JJ,II)+K
  
   END DO
   
  END DO


  RETURN
  END 
