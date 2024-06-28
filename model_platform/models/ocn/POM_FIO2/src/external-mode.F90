 SUBROUTINE EXTERNAL_MODE
 use MOD_IO

 USE LIMS
 USE CONTROL 
 USE CONST, ONLY : GRAV,SMOTH
 USE ALL_VAR !, ONLY : FLUXUA,FLUXVA,D,DX,DY,UA,VA,ELF,ELB,ART,    &
             !        UAF,ADX2D,ADVUA,ARU,COR,EL,DRX2D,WUSURF,    &
             !        WUBOT,H,UAB,VAF,ADY2D,ADVVA,ARV,DRY2D,      &
             !        WVSURF,WVBOT,VAF,VAB,ETF,FSM,EGF,UTF,VTF
 USE PARALLEL_MOD
 USE CCPL_interface_mod, only : CCPL_report_error   !zhaobiao, C-Coupler2
 USE coupling_pom_mod                               !zhaobiao, C-Coupler2
 IMPLICIT NONE
 INTEGER IEXT,IMAX,JMAX
 REAL ALPHA,VMAXL,VAMAX,SCUVA
 CHARACTER*15 FLG_RES
 CHARACTER*100 OUTFILE_RES
 FLG_RES='_XXXX_XXXX_XXXX'

 DO IEXT=1,ISPLIT
 !>>>>>>>>>LOOP 8000 >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
! LOOP 405
! DO 405 J=2,JM
! DO 405 I=2,IM
 DO J=MSLAT,MELAT
 DO I=MSLON,MELON
 FLUXUA(I,J)=.25E0*(D(I,J)+D(I-1,J))*(DY(I,J)+DY(I-1,J))*UA(I,J)
 FLUXVA(I,J)=.25E0*(D(I,J)+D(I,J-1))*(DX(I,J)+DX(I,J-1))*VA(I,J)
 END DO
 END DO
! LOOP 410
! DO 410 J=2,JMM1
! DO 410 I=2,IMM1
 DO J=LSLAT,LELAT
 DO I=LSLON,LELON
 ELF(I,J)=ELB(I,J)                                               &
     -DTE2*(FLUXUA(I+1,J)-FLUXUA(I,J)+FLUXVA(I,J+1)-FLUXVA(I,J)) &
                     /ART(I,J)
 END DO
 END DO
 CALL BCOND(1)

 IF(MOD(IEXT,ISPADV)==0)THEN  
 CALL ADVAVE

 END IF

! Note that ALPHA = 0. is perfectly acceptable. The value, ALPHA = .225
! permits a longer time step.
 ALPHA=0.225     
! LOOP 420
! DO 420 J=2,JMM1
! DO 420 I=2,IM
 DO J=JSLAT,JELAT
 DO I=JSLON,IELON
 UAF(I,J)=ADX2D(I,J)+ADVUA(I,J)                                  &
     -ARU(I,J)*.25*(  COR(I,J)*D(I,J)*(VA(I,J+1)+VA(I,J))        &
               +COR(I-1,J)*D(I-1,J)*(VA(I-1,J+1)+VA(I-1,J)) )    &
          +.25E0*GRAV*(DY(I,J)+DY(I-1,J))*(D(I,J)+D(I-1,J))      &
              *( (1.E0-2.E0*ALPHA)*(EL(I,J)-EL(I-1,J))           &
             +ALPHA*(ELB(I,J)-ELB(I-1,J)+ELF(I,J)-ELF(I-1,J)) )  &
                +DRX2D(I,J)                                      &
       +ARU(I,J)*( WUSURF(I,J)-WUBOT(I,J)   )
 END DO
 END DO
! LOOP 425
! DO 425 J=2,JMM1
! DO 425 I=2,IM
 DO J=JSLAT,JELAT
 DO I=JSLON,IELON
 UAF(I,J)=                                                         &
          ((H(I,J)+ELB(I,J)+H(I-1,J)+ELB(I-1,J))*ARU(I,J)*UAB(I,J) &
                 -4.E0*DTE*UAF(I,J))                               &
         /((H(I,J)+ELF(I,J)+H(I-1,J)+ELF(I-1,J))*ARU(I,J))
 END DO
 END DO
! LOOP 430 	
! DO 430 J=2,JM
! DO 430 I=2,IMM1
 DO J=JSLAT,IELAT
 DO I=JSLON,JELON
 VAF(I,J)=ADY2D(I,J)+ADVVA(I,J)                                  &
     +ARV(I,J)*.25*(  COR(I,J)*D(I,J)*(UA(I+1,J)+UA(I,J))        &
                +COR(I,J-1)*D(I,J-1)*(UA(I+1,J-1)+UA(I,J-1)) )   &
          +.25E0*GRAV*(DX(I,J)+DX(I,J-1))*(D(I,J)+D(I,J-1))      &
              *( (1.E0-2.E0*ALPHA)*(EL(I,J)-EL(I,J-1))           &
             +ALPHA*(ELB(I,J)-ELB(I,J-1)+ELF(I,J)-ELF(I,J-1)) )  &
                +DRY2D(I,J)                                      &
     + ARV(I,J)*( WVSURF(I,J)-WVBOT(I,J)   )                      
 END DO
 END DO
! LOOP 435
! DO 435 J=2,JM
! DO 435 I=2,IMM1
 DO J=JSLAT,IELAT
 DO I=JSLON,JELON
 VAF(I,J)=                                                        &
         ((H(I,J)+ELB(I,J)+H(I,J-1)+ELB(I,J-1))*VAB(I,J)*ARV(I,J) &
               -4.E0*DTE*VAF(I,J))                                &
        /((H(I,J)+ELF(I,J)+H(I,J-1)+ELF(I,J-1))*ARV(I,J))
 END DO
 END DO
 CALL BCOND(2)
 IF(IEXT<ISPLIT-2)GO TO 440
 
 IF(IEXT==ISPLIT-2)THEN
   !LOOP 431
   !DO 431 J=1,JM
   !DO 431 I=1,IM
   DO J=ISLAT,IELAT
   DO I=ISLON,IELON
   ETF(I,J)=.25*SMOTH*ELF(I,J)
   END DO
   END DO
 ENDIF
 
 IF(IEXT==ISPLIT-1)THEN
   ! LOOP 432
   !DO 432 J=1,JM
   !DO 432 I=1,IM
   DO J=ISLAT,IELAT
   DO I=ISLON,IELON
   ETF(I,J)=ETF(I,J)+.5*(1.-.5*SMOTH)*ELF(I,J)
   END DO
   END DO
 ENDIF
 IF(IEXT==ISPLIT) THEN
   ! LOOP 433
   !DO 433 J=1,JM
   !DO 433 I=1,IM
   DO J=ISLAT,IELAT
   DO I=ISLON,IELON
   ETF(I,J)=(ETF(I,J)+.5*ELF(I,J))*FSM(I,J)
   END DO
   END DO
 ENDIF
 
 440  CONTINUE
!
! TEST FOR CFL VIOLATION. IF SO, PRINT AND STOP
!
 VMAXL=100.
 VAMAX=0.       
! LOOP 442
! DO 442 J=1,JM
! DO 442 I=1,IM
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
    SCUVA=SQRT(UAF(I,J)**2+VAF(I,J)**2)
    IF(SCUVA>=VAMAX)THEN
     VAMAX=scUVA
     IMAX=I
     JMAX=J
    END IF
 END DO
 END DO
 
 IF(VAMAX.GT.VMAXL)THEN
   WRITE(6,*),MYID,"WARNING,OVERFLOW!",IMAX,JMAX,VAMAX
   WRITE(FLG_RES,'(i4.4,a1,i4.4,a1,i4.4)'),MYID,'_',ISLON,'_',ISLAT
   OutFile_RES=trim(output)//TRIM(ModelNAME)//'.res.'//FLG_RES//'.ERROR.nc'
   CALL WRITE_RES(OutFile_RES)
   CALL CCPL_report_error(pom_state_variables(pom_grid_id)%comp_id,.false., "ERROR WARNING, OVERFLOW")  !zhaobiao, C-Coupler2
   RETURN
 END IF


! test ok 20130313 16:44:07
!    
! APPLY FILTER TO REMOVE TIME SPLIT. RESET TIME SEQUENCE.
! LOOP 445

! DO 445 J=1,JM
! DO 445 I=1,IM
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
 UA(I,J)=UA(I,J)+.5E0*SMOTH*(UAB(I,J)-2.E0*UA(I,J)+UAF(I,J))
 VA(I,J)=VA(I,J)+.5E0*SMOTH*(VAB(I,J)-2.E0*VA(I,J)+VAF(I,J))
 EL(I,J)=EL(I,J)+.5E0*SMOTH*(ELB(I,J)-2.E0*EL(I,J)+ELF(I,J))
 ELB(I,J)=EL(I,J)
 EL(I,J)=ELF(I,J)
 D(I,J)=H(I,J)+EL(I,J)
 UAB(I,J)=UA(I,J)
 UA(I,J)=UAF(I,J)
 VAB(I,J)=VA(I,J)
 VA(I,J)=VAF(I,J)
 END DO
 END DO

!****************************************************************
! UPDATE VARIABLE FOR EXTERNAL MODE
 CALL COMM_EXTERNAL_MODE 

!****************************************************************
 IF(IEXT==ISPLIT)CYCLE
 
! LOOP 450
! DO 450 J=2,JM
! DO 450 I=2,IM
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
 EGF(I,J)=EGF(I,J)+EL(I,J)*ISPI
 END DO
 END DO
 
 DO  J=ISLAT,IELAT
 DO  I=JSLON,IELON
 UTF(I,J)=UTF(I,J)+UA(I,J)*(D(I,J)+D(I-1,J))*ISP2I
 END DO
 END DO
 
 DO J=JSLAT,IELAT
 DO I=ISLON,IELON
 VTF(I,J)=VTF(I,J)+VA(I,J)*(D(I,J)+D(I,J-1))*ISP2I
 END DO
 END DO

 END DO
 RETURN
 END
