!             
 SUBROUTINE VERTVL
 USE CONTROL
 USE LIMS
 USE CONST, ONLY : ZERO
 USE ALL_VAR, ONLY : W,U,V,ETF,ETB,DT,DX,DY,DZ,A,C
 IMPLICIT NONE

!************************************************************
! CALCULATE NEW VERTICAL VELOCITY
!
! REESTABLISH BOUNDARY CONDITIONS
!************************************************************
!
! XFLUX>>>A
! YFLUX>>>C
A=ZERO;C=ZERO
! LOOP 100 
! DO 100 J=2,JM
! DO 100 I=2,IM
 DO K=1,KBM1
 DO J=MSLAT,MELAT
 DO I=MSLON,MELON
 A(I,J,K)=.25E0*(DY(I,J)+DY(I-1,J))*(DT(I,J)+DT(I-1,J))*U(I,J,K)
 END DO
 END DO
 END DO
! LOOP 120 	
! DO 120 J=2,JM
! DO 120 I=1,IM
 DO K=1,KBM1
 DO J=MSLAT,MELAT
 DO I=ISLON-2,MELON
 C(I,J,K)=.25E0*(DX(I,J)+DX(I,J-1))*(DT(I,J)+DT(I,J-1))*V(I,J,K)
 END DO
 END DO
 END DO
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! THE FOLLOW CODE FROM POM2K  
!                        Nov. 15, 2009
! NOTE THAT, IF ONE WISHES TO INCLUDE FRESHWATER FLUX, THE
! SURFACE VELOCITY SHOULD BE SET TO VFLUX(I,J). SEE ALSO
! CHANGE MADE TO 2-D VOLUME CONSERVATION EQUATION WHICH
! CALCULATES ELF.
!
! DO J=2,JMM1
! DO I=2,IMM1
!   W(I,J,1)=0.5*(VFLUXB(I,J)+VFLUXF(I,J))
! END DO
! END DO
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

 !W(:,:,1) = ZERO
 W = ZERO
! LOOP 710 
! DO 710 J=2,JMM1
! DO 710 I=2,IMM1
 DO J=L1SLAT,L1ELAT
 DO I=L1SLON,L1ELON
 DO K=1,KBM1
 W(I,J,K+1)=W(I,J,K)                                                  &
     +DZ(K)*((A(I+1,J,K)-A(I,J,K)                                     &
             +C(I,J+1,K)-C(I,J,K))                                    &
             /(DX(I,J)*DY(I,J))                                       &
             +(ETF(I,J)-ETB(I,J))/DTI2 )
 END DO
 END DO
 END DO
 RETURN
 END    
