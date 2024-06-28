 SUBROUTINE ADVAVE
 USE LIMS
 USE CONTROL
 USE CONST
 USE ALL_VAR, ONLY : ADVUA,ADVVA,FLUXUA,FLUXVA,D,UA,UAB,TPS,AAM2D, &
                     VA,VAB,WUBOT,WVBOT,CBC,DX,DY,ARU,ARV
 IMPLICIT NONE      


!---------------------------------------------------------------------
!      CALCULATE U-ADVECTION & DIFFUSION
!---------------------------------------------------------------------
!-------- ADVECTIVE FLUXES -------------------------------------------
 ADVUA = ZERO
! LOOP 300
! DO 300 J=2,JM
! DO 300 I=2,IMM1
 DO J=LSLAT,LELAT
 DO I=LSLON,LELON
 FLUXUA(I,J)=.125E0*((D(I+1,J)+D(I,J))*UA(I+1,J)                 &
                  +(D(I,J)+D(I-1,J))*UA(I,J))                    &
                   *(UA(I+1,J)+UA(I,J))
 END DO
 END DO
! LOOP 400
! DO 400 J=2,JM
! DO 400 I=2,IM
 DO J=LSLAT,LELAT
 DO I=LSLON,LELON
 FLUXVA(I,J)=.125E0*((D(I,J)+D(I,J-1))*VA(I,J)                   &
                  +(D(I-1,J)+D(I-1,J-1))*VA(I-1,J))              &
                     *(UA(I,J)+UA(I,J-1))
 END DO
 END DO
!----------- ADD VISCOUS FLUXES ---------------------------------
! LOOP 460
! DO  460 J=2,JM
! DO  460 I=2,IMM1
 DO J=LSLAT,LELAT
 DO I=LSLON,LELON
 FLUXUA(I,J)=FLUXUA(I,J)                                         &
          -D(I,J)*2.E0*AAM2D(I,J)*(UAB(I+1,J)-UAB(I,J))/DX(I,J)
 END DO
 END DO

! LOOP 470 	      
! DO  470 J=2,JM
! DO  470 I=2,IM
 DO J=LSLAT,LELAT
 DO I=LSLON,LELON
 TPS(I,J)=.25E0*(D(I,J)+D(I-1,J)+D(I,J-1)+D(I-1,J-1))              &
             *(AAM2D(I,J)+AAM2D(I,J-1)+AAM2D(I-1,J)+AAM2D(I-1,J-1))&
                 *((UAB(I,J)-UAB(I,J-1))                           &
                 /(DY(I,J)+DY(I-1,J)+DY(I,J-1)+DY(I-1,J-1))        &
                  +(VAB(I,J)-VAB(I-1,J))                           &
                 /(DX(I,J)+DX(I-1,J)+DX(I,J-1)+DX(I-1,J-1)) )
 FLUXUA(I,J)=FLUXUA(I,J)*DY(I,J)
 FLUXVA(I,J)=(FLUXVA(I,J)-TPS(I,J))                                &
             *.25E0*(DX(I,J)+DX(I-1,J)+DX(I,J-1)+DX(I-1,J-1))
 END DO
 END DO
!----------------------------------------------------------------
! LOOP 480
! DO  480 J=2,JMM1
! DO  480 I=2,IMM1
 DO J=JSLAT,JELAT
 DO I=JSLON,JELON
 ADVUA(I,J)=FLUXUA(I,J)-FLUXUA(I-1,J)                              &
            +FLUXVA(I,J+1)-FLUXVA(I,J)
 END DO
 END DO
!----------------------------------------------------------------
! CALCULATE V-ADVECTION & DIFFUSION
!----------------------------------------------------------------
 ADVVA = ZERO
!
!---------ADVECTIVE FLUXES ----------------------------
! LOOP 700
! DO 700 J=2,JM
! DO 700 I=2,IM
 DO J=LSLAT,LELAT
 DO I=LSLON,LELON
 FLUXUA(I,J)=.125E0*((D(I,J)+D(I-1,J))*UA(I,J)                   & 
          +(D(I,J-1)+D(I-1,J-1))*UA(I,J-1))*                     &
                         (VA(I-1,J)+VA(I,J))
 END DO
 END DO
! LOOP 800 	
! DO 800 J=2,JMM1
! DO 800 I=2,IM
 DO J=LSLAT,LELAT
 DO I=LSLON,LELON
 FLUXVA(I,J)=.125E0*((D(I,J+1)+D(I,J))                           &
        *VA(I,J+1)+(D(I,J)+D(I,J-1))*VA(I,J))                    &
       *(VA(I,J+1)+VA(I,J))                                       
 END DO
 END DO
!------- ADD VISCOUS FLUXES -----------------------------------
! LOOP 860
! DO  860 J=2,JMM1
! DO  860 I=2,IM
 DO J=LSLAT,LELAT
 DO I=LSLON,LELON
 FLUXVA(I,J)=FLUXVA(I,J)                                        &
         -D(I,J)*2.E0*AAM2D(I,J)*(VAB(I,J+1)-VAB(I,J))/DY(I,J)
 END DO
 END DO
! LOOP 870
! DO  870 J=2,JM
! DO  870 I=2,IM
 DO J=LSLAT,LELAT
 DO I=LSLON,LELON
 FLUXVA(I,J)=FLUXVA(I,J)*DX(I,J)
 FLUXUA(I,J)=(FLUXUA(I,J)-TPS(I,J))                            &
              *.25E0*(DY(I,J)+DY(I-1,J)+DY(I,J-1)+DY(I-1,J-1))
 END DO
 END DO
!---------------------------------------------------------------
! LOOP 880
! DO  880 J=2,JMM1
! DO  880 I=2,IMM1
 DO J=JSLAT,JELAT
 DO I=JSLON,JELON
 ADVVA(I,J)=FLUXUA(I+1,J)-FLUXUA(I,J)                           &
           +FLUXVA(I,J)-FLUXVA(I,J-1)
 END DO
 END DO
!
!---------------------------------------------------------------
 IF(MODE/=2)RETURN
! LOOP 100
! DO 100 J=2,JMM1
! DO 100 I=2,IMM1 
 DO J=JSLAT,JELAT
 DO I=JSLON,JELON
 WUBOT(I,J)=-0.5E0*(CBC(I,J)+CBC(I-1,J))                        &
      *SQRT(UAB(I,J)**2+(.25E0*(VAB(I,J)                        &
      +VAB(I,J+1)+VAB(I-1,J)+VAB(I-1,J+1)))**2)*UAB(I,J)
 END DO
 END DO
! LOOP 102 
! DO 102 J=2,JMM1
! DO 102 I=2,IMM1
 DO J=JSLAT,JELAT
 DO I=JSLON,JELON
 WVBOT(I,J)=-0.5E0*(CBC(I,J)+CBC(I,J-1))                        &
      *SQRT((.25E0*(UAB(I,J)+UAB(I+1,J)                         &
      +UAB(I,J-1)+UAB(I+1,J-1)))**2+VAB(I,J)**2)*VAB(I,J)
 END DO
 END DO
! LOOP 120
! DO 120 J=2,JMM1
! DO 120 I=2,IMM1
 DO J=LSLAT,LELAT
 DO I=LSLON,LELON
 TPS(I,J)=.25*((VA(I,J+1)+VA(I,J))*(DY(I+1,J)-DY(I-1,J))        &
                -(UA(I+1,J)+UA(I,J))*(DX(I,J+1)-DX(I,J-1)) )    &
                 /(DX(I,J)*DY(I,J))
 END DO
 END DO
! LOOP 130
! DO 130 J=3,JMM2
! DO 130 I=3,IMM2
 DO J=KSLAT,KELAT
 DO I=KSLON,KELON
 ADVUA(I,J)=ADVUA(I,J)                                          &
     -ARU(I,J)*.25*(  TPS(I,J)*D(I,J)*(VA(I,J+1)+VA(I,J))       &
           +TPS(I-1,J)*D(I-1,J)*(VA(I-1,J+1)+VA(I-1,J)) )
 ADVVA(I,J)=ADVVA(I,J)                                          &
     +ARV(I,J)*.25*(  TPS(I,J)*D(I,J)*(UA(I+1,J)+UA(I,J))       &
           +TPS(I,J-1)*D(I,J-1)*(UA(I+1,J-1)+UA(I,J-1)) )
 END DO
 END DO
!****************************************************************
! 
 RETURN
 END
