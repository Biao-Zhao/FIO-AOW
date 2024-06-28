 SUBROUTINE DENS(SI,TI,RHOO,DT)
 USE CONTROL
 USE ALL_VAR, ONLY : FSM,ZZ
 USE data_kind_mod
 USE CONST
 USE LIMS
 IMPLICIT NONE
 REAL(kind_r4),DIMENSION(ISLON-LE:IELON+LE,ISLAT-LE:IELAT+LE,KB) :: SI,TI,RHOO
 REAL(kind_r4),DIMENSION(ISLON-LE:IELON+LE,ISLAT-LE:IELAT+LE) :: DT
 
 REAL(kind_r4) TR,SR,TR2,TR3,TR4,RHOR,P,CR
! If using 32 bit precision, it is recommended that
! TR, SR, P, RHOR , CR be made double precision.
! and the E s in the constants should be changed 
! to D s.
!
! THIS SUBROUTINE COMPUTES (DENSITY- 1000.)/RHOREF
! T = POTENTIAL TEMPERATURE
!( See: Mellor, 1991, J. Atmos. Oceanic Tech., 609-611)
!
 DO K=1,KBM1
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
    TR=TI(I,J,K)+TBIAS
    SR=SI(I,J,K)+SBIAS
    TR2=TR*TR
    TR3=TR2*TR
    TR4=TR3*TR
!   Approximate pressure in units of bars
    P=-GRAV*RHOREF*ZZ(K)*DT(I,J)*1.E-5
!
    RHOR = -0.157406  + 6.793952E-2*TR                 &
           - 9.095290E-3*TR2 + 1.001685E-4*TR3         &
           - 1.120083E-6*TR4 + 6.536332E-9*TR4*TR
!
    RHOR = RHOR + (0.824493 - 4.0899E-3*TR             &
           + 7.6438E-5*TR2 - 8.2467E-7*TR3             &
           + 5.3875E-9*TR4) * SR                       &
           + (-5.72466E-3 + 1.0227E-4*TR               &
           - 1.6546E-6*TR2) * ABS(SR)**1.5             &
           + 4.8314E-4 * SR*SR
!
    CR=1449.1+.0821*P+4.55*TR-.045*TR2+1.34*(SR-35.)
    RHOR=RHOR + 1.E5*P/(CR*CR)*(1.-2.*P/(CR*CR))
!
    RHOO(I,J,K)=RHOR/RHOREF*FSM(I,J)
 END DO
 END DO
 END DO
!
 RETURN
 END
