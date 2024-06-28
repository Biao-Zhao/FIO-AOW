 SUBROUTINE BCOND(IDX)
 USE DATA_KIND_MOD
 USE LIMS
 USE CONTROL
 USE CONST, ONLY : PI,SMALL,GRAV,ZERO
 USE ALL_VAR, ONLY : ELF,UAF,EL,VAF,DT,UF,U,VF,V,T,S,W,Q2,Q2L,        &
                     TBE,SBE,TBW,SBW,TBN,SBN,TBS,SBS,UABE,ELE,UABW,   &
                     ELW,ELN,ELS,VABN,VABS,RFE,RFW,RFS,RFN,DUM,DVM,   &
                     DX,DY,ZZ,FSM,H,UBS,VBS,UBN,VBN,UBW,VBW,UBE,VBE
 IMPLICIT NONE
   
!  Closed boundary conditions are automatically enabled through
!  specification of the masks, DUM, DVM and FSM in which case open
!  boundary condition, included below, will be overwritten.
!
!
!*******************************************************************************
!                            POM (C-Grid)   
!                          ================
!
!    The diagram below and some of the text was provided by D.-S. Ko. 
!    It is for the case where U and V are the primary boundary conditions
!    together with T and S (co-located with E). E = EL itself is rather
!    unimportant and is substituted from an adjacent interior point.
!    
!    Inside ....... indicate the interior (non-boundary) grid points.
!    In general only those variables in the interior are computed and
!    variables at open boundary have to be specified.
!    All interpolations are centered in space except those at lateral
!    open boundary where an upstream scheme is usually used.
!
!    Horizontal locations of E(EL), T and S (etc.) are coincident.
!    NU = Not Used is indicated by *. However, for attractive output
!    adjacent interior values may be filled in at these points.   
!
!    I have been asked many times what kind of B.C. along the side wall
!    POM uses from people not acquainted with sigma coordinates. Although
!    the issue is not important as it might be for z-level grids, a direct
!    answer is "half slip" which, of course, is between free slip and
!    non-slip B.C.s.
!
!-------------------------------------------------------------------------------
!     |                               N O R T H
!     |
!     |    1     2     3           I-1   I    I+1         IM-2  IM-1   IM 
!-----+-------------------------------------------------------------------------
!     |   NU BC BC                                                    BC BC
!     |    v  v  v                                                     v  v
!     |
!     |BC> U* E  U  E  U  E  .  .  U  E  U  E  U  E  .  .  U  E  U  E  U  E  <BC
!     |    |     |     |           |     |     |           |     |     |      
!  JM |BC> +--V--+--V--+--V--   .  +--V--+--V--+--V--   .  +--V--+--V--+--V- <BC
!     |    |     | ....|...........|.....|.....|...........|.....|.... |      
!     |    U* E  U :E  U  E  .  .  U  E  U  E  U  E  .  .  U  E  U  E: U  E 
!     |    |     | :   |           |     |     |           |     |   : |      
! JM-1|    +--V--+--V--+--V--   .  +--V--+--V--+--V--   .  +--V--+--V--+--V-
!     |    |     | :   |           |     |     |           |     |   : |
!     |    U* E  U :E  U  E  .  .  U  E  U  E  U  E  .  .  U  E  U  E: U  E
!     |    |     | :   |           |     |     |           |     |   : |
! JM-2|    +--V--+--V--+--V--   .  +--V--+--V--+--V--   .  +--V--+--V--+--V-
!     |            :                                                 :          
! W   |       .  . :.  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .  .: .  .    E
! E   |            :                    Interior                     :         A
! S   |       .    :.     .     .     .     .     .     .     .     .:    .    S
! T   |            :                                                 :         T
!     |    |     | :   |           |     |     |           |     |   : |
!     |    U* E  U :E  U  E  .  .  U  E  U  E  U  E  .  .  U  E  U  E: U  E
!     |    |     | :   |           |     |     |           |     |   : |
!   3 |    +--V--+--V--+--V--   .  +--V--+--V--+--V--   .  +--V--+--V--+--V-
!     |    |     | :   |           |     |     |           |     |   : |      
!     |    U* E  U :E  U  E  .  .  U  E  U  E  U  E  .  .  U  E  U  E: U  E
!     |    |     | ....|...........|.....|.....|...........|.....|.... |      
!   2 |BC> +--V--+--V--+--V--   .  +--V--+--V--+--V--   .  +--V--+--V--+--V- <BC
!     |    |     |     |           |     |     |           |     |     |      
!     |BC> U* E  U  E  U  E  .  .  U  E  U  E  U  E  .  .  U  E  U  E  U  E  <BC
!     |    |     |     |           |     |     |           |     |     |      --
!   1 |NU> +--V*-+--V*-+--V*-   .  +--V*-+--V*-+--V*-   .  +--V*-+--V*-+--V* <NU
!     |
!     |    ^  ^  ^                                                     ^  ^
!     |   NU BC BC                                                    BC BC
!-----+-------------------------------------------------------------------------
!     |    1     2     3           I-1   I     I+1         IM-2  IM-1  IM
!     |
!     |                                S O U T H
!-------------------------------------------------------------------------------

 INTEGER IDX,IMM1,JMM1
 REAL(kind_r4) :: HMAX,GA,U1,WM
!
! GO TO (10,20,30,40,50,60), IDX
!
!-----------------------------------------------------------------------
!                   EXTERNAL B.C.S
!  In this example the governing boundary conditions are a radiation   
!  condition on UAF in the east and in the west and VAF in the north
!  and south. The tangential velocities are set to zero on both boundaries.
!  These are only one set of possibilities and may not represent a choice
!  which yields the most physically realistic result.
!-----------------------------------------------------------------------
!
 IMM1=IM-1
 JMM1=JM-1

 SELECT CASE(IDX)
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 
!---------ELEVATION --------------------
! In this application, elevation is not a primary B.C.     
  CASE(1)
! EAST:  
 IF(RFE)THEN
   DO J=ISLAT-1,IELAT+1
   ELF(IM,J)=ELF(IMM1,J)
   END DO
 END IF
! WEST:
! DO 120 J=1,JM
 IF(RFW)THEN
   DO J=ISLAT-1,IELAT+1
   ELF(1,J)=ELF(2,J)
   END DO
 END IF
 
! SOUTH: 
! DO 130 I=1,IM
 IF(RFS)THEN
   DO I=ISLON-1,IELON+1
   ELF(I,1)=ELF(I,2)
   END DO
 END IF
! NORTH:
 IF(RFN)THEN
   DO I=ISLON-1,IELON+1
   ELF(I,JM)=ELF(I,JMM1)
   END DO
 END IF   
!
! DO 140 J=1,JM
! DO 140 I=1,IM
 DO J=ISLAT-1,IELAT+1
 DO I=ISLON-1,IELON+1
 ELF(I,J)=ELF(I,J)*FSM(I,J)
 END DO
 END DO
 RETURN
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!  External (2-D) velocity:
!---------- VELOCITY --------------
!--- Governing external B.C.s -------------
 CASE(2)
! EAST: 
! DO 210 J=2,JMM1
 IF(RFE)THEN
   DO J=JSLAT,JELAT
   UAF(IM,J)=UABE(J)+SQRT(GRAV/H(IMM1,J))*(EL(IMM1,J)-ELE(J))
                  ! +RFE*SQRT(GRAV/H(IMM1,J))*(EL(IMM1,J)-ELE(J))
   UAF(IM,J)=FACT*UAF(IM,J)
   VAF(IM,J)=ZERO
   END DO
 END IF
! WEST
 IF(RFW)THEN
   DO J=JSLAT,JELAT
   UAF(2,J)=UABW(J)-SQRT(GRAV/H(2,J))*(EL(2,J)-ELW(J))
                  ! +RFE*SQRT(GRAV/H(2,J))*(EL(2,J)-ELW(J))
   UAF(2,J)=FACT*UAF(2,J)
   UAF(1,J)=UAF(2,J)
   VAF(1,J)=ZERO
   END DO
 END IF 
 
! NORTH:
 IF(RFN)THEN
  ! DO 220 I=2,IMM1
   DO I=JSLON,JELON
   VAF(I,JM)=VABN(I)+SQRT(GRAV/H(I,JMM1))*(EL(I,JMM1)-ELN(I))
                  ! +RFN*SQRT(GRAV/H(I,JMM1))*(EL(I,JMM1)-ELN(I))
   VAF(I,JM)=FACT*VAF(I,JM)
   UAF(I,JM)=ZERO
   END DO
 END IF
! SOUTH
 IF(RFS)THEN
   DO I=JSLON,JELON
   VAF(I,2)=VABS(I)-SQRT(GRAV/H(I,2))*(EL(I,2)-ELS(I))
                  !-RFS*SQRT(GRAV/H(I,2))*(EL(I,2)-ELS(I))
   VAF(I,2)=FACT*VAF(I,2)
   VAF(I,1)=VAF(I,2)
   UAF(I,1)=ZERO
   END DO
 END IF
!---------------------------
!
! DO 240 J=1,JM
! DO 240 I=1,IM
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
 UAF(I,J)=UAF(I,J)*DUM(I,J)
 VAF(I,J)=VAF(I,J)*DVM(I,J)
 END DO
 END DO
 RETURN
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!  Internal (3-D) boundary conditions:
!-----------------------------------------------------------------------
!  INTERNAL VEL B.C.S
!-----------------------------------------------------------------------
!  Radiation boundary conditions. Smoothing is used
!  in the direction tangential to the boundaries.
! 
 CASE(3)
 HMAX=5500.
! EAST 
 IF(RFE)THEN
   DO K=1,KBM1
  ! DO 340 J=2,JMM1
   DO J=JSLAT,JELAT
   GA=SQRT(H(IM,J)/HMAX)    
   UF(IM,J,K)                                                          &
     =GA*(.25*U(IMM1,J-1,K)+.5*U(IMM1,J,K)+.25*U(IMM1,J+1,K))          &
      +(1.E0-GA)*(.25*U(IM,J-1,K)+.5*U(IM,J,K)+.25*U(IM,J+1,K))
   VF(IM,J,K)=ZERO  
   END DO
   END DO
 END IF
! WEST
 IF(RFW)THEN
   DO K=1,KBM1
   DO J=JSLAT,JELAT                                      
   GA=SQRT(H(1,J)/HMAX)    
   UF(2,J,K)                                                          &
     =GA*(.25*U(3,J-1,K)+.5*U(3,J,K)+.25*U(3,J+1,K))                  &
      +(1.-GA)*(.25*U(2,J-1,K)+.5*U(2,J,K)+.25*U(2,J+1,K))
   UF(1,J,K)=UF(2,J,K)
   VF(1,J,K)=ZERO
   END DO
   END DO
 END IF
! NORTH
 IF(RFN)THEN
   DO K=1,KBM1
   ! DO 360 I=2,IMM1
   DO I=JSLON,JELON
   GA=SQRT(H(I,JM)/HMAX)    
   VF(I,JM,K)                                                         &
     =GA*(.25*V(I-1,JMM1,K)+.5*V(I,JMM1,K)+.25*V(I+1,JMM1,K))         &
      +(1.-GA)*(.25*V(I-1,JM,K)+.5*V(I,JM,K)+.25*V(I+1,JM,K))
   UF(I,JM,K)=ZERO
   END DO
   END DO
 END IF
!SOUTH
 IF(RFS)THEN
   DO K=1,KBM1
   DO I=JSLON,JELON
   GA=SQRT(H(I,1)/HMAX)    
   VF(I,2,K)                                                          &
     =GA*(.25*V(I-1,3,K)+.5*V(I,3,K)+.25*V(I+1,3,K))                  &
      +(1.-GA)*(.25*V(I-1,2,K)+.5*V(I,2,K)+.25*V(I+1,2,K))
   VF(I,1,K)=VF(I,2,K)
   UF(I,1,K)=0.
   END DO
   END DO
 END IF
!
 DO K=1,KBM1
! DO 380 J=1,JM
! DO 380 I=1,IM
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
    UF(I,J,K)=UF(I,J,K)*DUM(I,J)
    VF(I,J,K)=VF(I,J,K)*DVM(I,J)
 END DO
 END DO
 END DO
!      
 RETURN
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!-----------------------------------------------------------------------
!     Temperature and salinity boundary conditions (using uf and vf,
!     respectively):
!     TEMP(UF) & SAL(VF) B.C.S
!-----------------------------------------------------------------------
!
 CASE(4)
! EAST:
 IF(RFE)THEN
   DO K=1,KBM1
   !DO 420 J=1,JM
   DO J=ISLAT,IELAT
   ! UF(IM,J,K)=TBE(J,K)
   ! VF(IM,J,K)=SBE(J,K)
   U1=2.*U(IM,J,K)*DTI/(DX(IM,J)+DX(IMM1,J))
   IF(U1<=0.)THEN
     UF(IM,J,K)=T(IM,J,K)-U1*(TBE(J,K) -T(IM,J,K)) 
     VF(IM,J,K)=S(IM,J,K)-U1*(SBE(J,K) -S(IM,J,K)) 
   ELSE
     UF(IM,J,K)=T(IM,J,K)-U1*(T(IM,J,K)-T(IMM1,J,K))
     VF(IM,J,K)=S(IM,J,K)-U1*(S(IM,J,K)-S(IMM1,J,K))
     IF(K/=1.AND.K/=KBM1)THEN
       WM=.5*(W(IMM1,J,K)+W(IMM1,J,K+1))*DTI/((ZZ(K-1)-ZZ(K+1))*DT(IMM1,J))
       UF(IM,J,K)=UF(IM,J,K)-WM*(T(IMM1,J,K-1)-T(IMM1,J,K+1))
       VF(IM,J,K)=VF(IM,J,K)-WM*(S(IMM1,J,K-1)-S(IMM1,J,K+1))
     ENDIF 
   ENDIF
   END DO
   END DO
 END IF
! WEST
 IF(RFW)THEN
   DO K=1,KBM1
   DO J=ISLAT,IELAT
   ! UF(1,J,K)=TBW(J,K)
   ! VF(1,J,K)=SBW(J,K)
   U1=2.*U(2,J,K)*DTI/(DX(1,J)+DX(2,J))
   IF(U1>=0.) THEN
     UF(1,J,K)=T(1,J,K)-U1*(T(1,J,K)-TBW(J,K)) 
     VF(1,J,K)=S(1,J,K)-U1*(S(1,J,K)-SBW(J,K)) 
   ELSE
     UF(1,J,K)=T(1,J,K)-U1*(T(2,J,K)-T(1,J,K))
     VF(1,J,K)=S(1,J,K)-U1*(S(2,J,K)-S(1,J,K))
     IF(K/=1.AND.K/=KBM1) THEN
       WM=.5*(W(2,J,K)+W(2,J,K+1))*DTI/((ZZ(K-1)-ZZ(K+1))*DT(2,J))
       UF(1,J,K)=UF(1,J,K)-WM*(T(2,J,K-1)-T(2,J,K+1))
       VF(1,J,K)=VF(1,J,K)-WM*(S(2,J,K-1)-S(2,J,K+1))
     ENDIF
   ENDIF
   END DO
   END DO
 END IF
! NORTH
 IF(RFN)THEN
   DO K=1,KBM1
   ! DO 440 I=1,IM
   DO I=ISLON,IELON
   ! UF(I,JM,K)=TBN(I,K)
   ! VF(I,JM,K)=SBN(I,K)
   U1=2.*V(I,JM,K)*DTI/(DY(I,JM)+DY(I,JMM1))
   IF(U1<=0.)THEN
     UF(I,JM,K)=T(I,JM,K)-U1*(TBN(I,K)-T(I,JM,K)) 
     VF(I,JM,K)=S(I,JM,K)-U1*(SBN(I,K)-S(I,JM,K)) 
   ELSE
     UF(I,JM,K)=T(I,JM,K)-U1*(T(I,JM,K)-T(I,JMM1,K))
     VF(I,JM,K)=S(I,JM,K)-U1*(S(I,JM,K)-S(I,JMM1,K))
     IF(K/=1.AND.K/=KBM1)THEN
       WM=.5*(W(I,JMM1,K)+W(I,JMM1,K+1))*DTI/((ZZ(K-1)-ZZ(K+1))*DT(I,JMM1))
       UF(I,JM,K)=UF(I,JM,K)-WM*(T(I,JMM1,K-1)-T(I,JMM1,K+1))
       VF(I,JM,K)=VF(I,JM,K)-WM*(S(I,JMM1,K-1)-S(I,JMM1,K+1))
     ENDIF
   ENDIF
   END DO
   END DO
 END IF
! SOUTH 
 IF(RFS)THEN
   DO K=1,KBM1
   DO I=ISLON,IELON
   U1=2.*V(I,2,K)*DTI/(DY(I,1)+DY(I,2))
   IF(U1>=0.) THEN
     UF(I,1,K)=T(I,1,K)-U1*(T(I,1,K)-TBS(I,K)) 
     VF(I,1,K)=S(I,1,K)-U1*(S(I,1,K)-SBS(I,K)) 
   ELSE
     UF(I,1,K)=T(I,1,K)-U1*(T(I,2,K)-T(I,1,K))
     VF(I,1,K)=S(I,1,K)-U1*(S(I,2,K)-S(I,1,K))
     IF(K/=1.AND.K/=KBM1)THEN
       WM=.5*(W(I,2,K)+W(I,2,K+1))*DTI/((ZZ(K-1)-ZZ(K+1))*DT(I,2))
       UF(I,1,K)=UF(I,1,K)-WM*(T(I,2,K-1)-T(I,2,K+1))
       VF(I,1,K)=VF(I,1,K)-WM*(S(I,2,K-1)-S(I,2,K+1))
     END IF
   ENDIF
   END DO
   END DO
 END IF
!
 DO K=1,KBM1
! DO 460 J=1,JM
! DO 460 I=1,IM
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
 UF(I,J,K)=UF(I,J,K)*FSM(I,J)
 VF(I,J,K)=VF(I,J,K)*FSM(I,J)
 END DO
 END DO
 END DO
 RETURN
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
! Vertical velocity boundary conditions:
!---------------VERTICAL VEL. B. C.S --------------------------------
 CASE (5)
 DO K=1,KBM1
 !DO 550 J=1,JM
 !DO 550 I=1,IM
 DO J=ISLAT-1,LELAT
 DO I=ISLON-1,LELON
 W(I,J,K)=W(I,J,K)*FSM(I,J)
 END DO
 END DO
 END DO
!    
 RETURN
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!  q2 and q2l boundary conditions
!---------------- Q2 AND Q2L B.C.S -----------------------------------
 CASE (6)
! EAST
 IF(RFE)THEN
   DO K=1,KB
   ! DO 605 J=1,JM
   DO J=ISLAT,IELAT
   U1=2.*U(IM,J,K)*DTI/(DX(IM,J)+DX(IMM1,J))
   IF(U1<=0.) THEN
     UF(IM,J,K)=Q2(IM,J,K)-U1*(SMALL-Q2(IM,J,K)) 
     VF(IM,J,K)=Q2L(IM,J,K)-U1*(SMALL-Q2L(IM,J,K)) 
   ELSE
     UF(IM,J,K)=Q2(IM,J,K)-U1*(Q2(IM,J,K)-Q2(IMM1,J,K))
     VF(IM,J,K)=Q2L(IM,J,K)-U1*(Q2L(IM,J,K)-Q2L(IMM1,J,K))
   ENDIF
   END DO
   END DO
 END IF
! WEST
 IF(RFW)THEN
   DO K=1,KB
   DO J=ISLAT,IELAT
   U1=2.*U(2,J,K)*DTI/(DX(1,J)+DX(2,J))
   IF(U1>=0.) THEN
     UF(1,J,K)=Q2(1,J,K)-U1*(Q2(1,J,K)-SMALL) 
     VF(1,J,K)=Q2L(1,J,K)-U1*(Q2L(1,J,K)-SMALL)       
   ELSE
     UF(1,J,K)=Q2(1,J,K)-U1*(Q2(2,J,K)-Q2(1,J,K))
     VF(1,J,K)=Q2L(1,J,K)-U1*(Q2L(2,J,K)-Q2L(1,J,K))
   ENDIF
   END DO
   END DO
 END IF
! NORTH
 IF(RFN)THEN
   DO K=1,KB
   ! DO 610 I=1,IM
   DO I=ISLON,IELON
   U1=2.*V(I,JM,K)*DTI/(DY(I,JM)+DY(I,JMM1))
   IF(U1<=0.) THEN
     UF(I,JM,K)=Q2(I,JM,K)-U1*(SMALL-Q2(I,JM,K)) 
     VF(I,JM,K)=Q2L(I,JM,K)-U1*(SMALL-Q2L(I,JM,K)) 
   ELSE
     UF(I,JM,K)=Q2(I,JM,K)-U1*(Q2(I,JM,K)-Q2(I,JMM1,K))
     VF(I,JM,K)=Q2L(I,JM,K)-U1*(Q2L(I,JM,K)-Q2L(I,JMM1,K))
   ENDIF
   END DO
   END DO
 END IF
! SOUTH
 IF(RFS)THEN
   DO K=1,KB
   DO I=ISLON,IELON
   U1=2.*V(I,2,K)*DTI/(DY(I,1)+DY(I,2))
   IF(U1>=0.) THEN
     UF(I,1,K)=Q2(I,1,K)-U1*(Q2(I,1,K)-SMALL)    
     VF(I,1,K)=Q2L(I,1,K)-U1*(Q2L(I,1,K)-SMALL)        
   ELSE
     UF(I,1,K)=Q2(I,1,K)-U1*(Q2(I,2,K)-Q2(I,1,K))
     VF(I,1,K)=Q2L(I,1,K)-U1*(Q2L(I,2,K)-Q2L(I,1,K))
   ENDIF
   END DO
   END DO
 END IF

 DO K=1,KB
! DO 615 J=1,JM
! DO 615 I=1,IM
 DO J=ISLAT,IELAT
 DO I=ISLON,IELON
 UF(I,J,K)=UF(I,J,K)*FSM(I,J)+1.E-10
 VF(I,J,K)=VF(I,J,K)*FSM(I,J)+1.E-10
 END DO
 END DO
 END DO
 RETURN
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 CASE DEFAULT
 WRITE(*,"('Unkonwn IDX '),I1") IDX
 STOP
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 END SELECT
 END
