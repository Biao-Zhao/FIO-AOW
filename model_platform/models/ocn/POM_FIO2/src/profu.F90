 SUBROUTINE PROFU(DT2)
 USE DATA_KIND_MOD
 USE LIMS
 USE CONTROL
 USE CONST, ONLY : LE,UMOL,ONE
 USE ALL_VAR, ONLY : ETF,WUSURF,TPS,CBC,WUBOT,C,KM,A,EE,GG,UF,UB,VB,H,DZ,DZZ,DUM
 IMPLICIT NONE
 REAL(kind_r4) DT2

 REAL(kind_r4),DIMENSION(ISLON-LE:IELON+LE,ISLAT-LE:IELAT+LE) :: DH
 INTEGER(kind_in) KI
!***********************************************************************
!                                                                      *
!  THE FOLLOWING SECTION SOLVES THE EQUATION                     *
!   DT2*(KM*U')'-U=-UB                                           *
!                                                                      *
!***********************************************************************
!
 DH(:,:)=ONE
! LOOP 85 
! DO 85 J=2,JM
! DO 85 I=2,IM
 DO J=JSLAT,IELAT
 DO I=JSLON,IELON
 DH(I,J)=.5E0*(H(I,J)+ETF(I,J)+H(I-1,J)+ETF(I-1,J))
 END DO
 END DO
! LOOP 90      
! DO 90 J=2,JM
! DO 90 I=2,IM
 DO K=1,KB
 DO J=JSLAT,IELAT
 DO I=JSLON,IELON
 C(I,J,K)=(KM(I,J,K)+KM(I-1,J,K))*.5E0
 END DO
 END DO
 END DO
! LOOP 100      
! DO 100 J=1,JM
! DO 100 I=1,IM
 DO K=2,KBM1
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
 A(I,J,K-1)=-DT2*(C(I,J,K)+UMOL  )/(DZ(K-1)*DZZ(K-1)*DH(I,J)          &
      *DH(I,J))
 C(I,J,K)=-DT2*(C(I,J,K)+UMOL  )/(DZ(K)*DZZ(K-1)*DH(I,J)              &
      *DH(I,J))
 END DO
 END DO
 END DO
! LOOP 1011
! DO 1011 J=1,JM
! DO 1011 I=1,IM
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
 EE(I,J,1)=A(I,J,1)/(A(I,J,1)-1.E0)
 GG(I,J,1)=(-DT2*WUSURF(I,J)/(-DZ(1)*DH(I,J))-UF(I,J,1))              &
    /(A(I,J,1)-1.E0)
 END DO
 END DO

! LOOP 101      
! DO 101 J=1,JM
! DO 101 I=1,IM
 DO K=2,KBM2
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
 IF ((A(I,J,K)+C(I,J,K)*(1.-EE(I,J,K-1))-1.)==0.)THEN
 WRITE(*,*) "PROFU.F ZERO"
 GG(I,J,K)=1.E0/(A(I,J,K)+C(I,J,K)*(1.-EE(I,J,K-1))-1.+1e-6)
 ELSE
 GG(I,J,K)=1.E0/(A(I,J,K)+C(I,J,K)*(1.-EE(I,J,K-1))-1.)
 END IF 
 EE(I,J,K)=A(I,J,K)*GG(I,J,K)
 GG(I,J,K)=(C(I,J,K)*GG(I,J,K-1)-UF(I,J,K))*GG(I,J,K)
 END DO
 END DO
 END DO
! LOOP 102 	
! DO 102 J=2,JMM1
! DO 102 I=2,IMM1
 DO J=JSLAT,JELAT
 DO I=JSLON,JELON
 TPS(I,J)=0.5E0*(CBC(I,J)+CBC(I-1,J))                                 &
      *SQRT(UB(I,J,KBM1)**2+(.25E0*(VB(I,J,KBM1)                      &
      +VB(I,J+1,KBM1)+VB(I-1,J,KBM1)+VB(I-1,J+1,KBM1)))**2)
 UF(I,J,KBM1)=(C(I,J,KBM1)*GG(I,J,KBM2)-UF(I,J,KBM1))/(TPS(I,J)       &
  *DT2/(-DZ(KBM1)*DH(I,J))-1.E0-(EE(I,J,KBM2)-1.E0)*C(I,J,KBM1))
 UF(I,J,KBM1)=UF(I,J,KBM1)*DUM(I,J)
 END DO
 END DO
 
! LOOP 103 
! DO 103 J=2,JMM1
! DO 103 I=2,IMM1
 DO K=2,KBM1
   KI=KB-K
   DO J=JSLAT,JELAT
   DO I=JSLON,JELON
   UF(I,J,KI)=(EE(I,J,KI)*UF(I,J,KI+1)+GG(I,J,KI))*DUM(I,J)
   END DO
   END DO
 END DO
! LOOP 104
! DO 104 J=2,JMM1
! DO 104 I=2,IMM1
 DO J=JSLAT,JELAT
 DO I=JSLON,JELON
 WUBOT(I,J)=-TPS(I,J)*UF(I,J,KBM1)
 END DO
 END DO
 RETURN
 END
