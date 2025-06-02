! numerical_solvers.f90
! Module for SOR solver and iteration control routines

module numerical_solvers
  use common_data
  use solver_module, only: BCEND
  implicit none
  public :: SYOR, SOLVE, RECIRC, REDUB, RESET

contains

  ! Perform one SOR sweep computing residuals and updating P
  subroutine SYOR()
    use common_data, only: P, X, Y, IMIN, IMAX, IUP, IDOWN, ILE, ITE
    use common_data, only: JMIN, JMAX, JUP, JLOW, JTOP, JBOT, J1, J2
    use common_data, only: AK, GAM1, CVERGE, ERROR, IERROR, JERROR
    use common_data, only: CXL, CXC, CXR, CXXL, CXXC, CXXR, C1
    use common_data, only: CYYC, CYYD, CYYU, DIAG, RHS, SUB, SUP
    use common_data, only: CYYBUC, CYYBUU, CYYBLC, CYYBLD, FXUBC, FXLBC
    use common_data, only: PJUMP, FCR, KUTTA, EPS, WI
    implicit none
    integer :: I, J, K, IM2, JA, JB, J1_LOC, J2_LOC
    real :: EPSX, VC_VAL, EMU_VAL, ARHS, DNOM
    real, dimension(100) :: VC, EMU_CUR, EMU_PREV, SAVE_ARR, POLD_CUR

    IM2 = IUP - 1
    if (AK < 0.0) IM2 = IUP - 2

    J1_LOC = JBOT + 1
    J2_LOC = JTOP - JBOT

    do I = IUP, IDOWN
      EPSX = EPS / ((X(I) - X(I-1))**2)

      ! Compute VC = 1 - M**2 and EMU
      do J = JBOT, JTOP
        VC(J) = C1(I) - (CXL(I)*POLD_CUR(J) + CXC(I)*P(J,I) + CXR(I)*P(J,I+1))
        EMU_CUR(J) = 0.0
        POLD_CUR(J) = P(J,I)
        if (VC(J) < 0.0) EMU_CUR(J) = VC(J)
      end do

      if (.not. FCR) then
        EMU_PREV = EMU_CUR
      end if

      ! Compute matrix elements
      do J = JBOT, JTOP
        DIAG(J) = (EMU_CUR(J) - VC(J)) * CXXC(I) * WI + EMU_PREV(J) * CXXR(I-1) - CYYC(J)
        SUP(J) = CYYD(J)
        SUB(J) = CYYU(J)
      end do

      ! Compute residual
      do J = JBOT, JTOP
        RHS(J) = -(VC(J) - EMU_CUR(J)) * &
                 (CXXL(I)*P(J,I-1) - CXXC(I)*P(J,I) + CXXR(I)*P(J,I+1))
        RHS(J) = RHS(J) - (EMU_PREV(J) * &
                 (CXXL(I-1)*P(J,IM2) - CXXC(I-1)*P(J,I-1) + CXXR(I-1)*P(J,I)))
      end do

      ! Interior points
      JA = JBOT + 1
      JB = JTOP - 1
      do J = JA, JB
        RHS(J) = RHS(J) - (CYYD(J)*P(J-1,I) - CYYC(J)*P(J,I) + CYYU(J)*P(J+1,I))
      end do

      ! Bottom boundary
      RHS(JBOT) = RHS(JBOT) - (-CYYC(JBOT)*P(JBOT,I) + CYYU(JBOT)*P(JBOT+1,I))
      if (JBOT /= JMIN) then
        RHS(JBOT) = RHS(JBOT) - CYYD(JBOT)*P(JBOT-1,I)
      end if

      ! Top boundary
      RHS(JTOP) = RHS(JTOP) - (CYYD(JTOP)*P(JTOP-1,I) - CYYC(JTOP)*P(JTOP,I))
      if (JTOP /= JMAX) then
        RHS(JTOP) = RHS(JTOP) - CYYU(JTOP)*P(JTOP+1,I)
      end if

      ! Check for airfoil boundary condition
      if (I >= ILE .and. I <= ITE) then
        ! Upper surface
        J = JUP
        DIAG(J) = DIAG(J) + CYYC(J) - CYYBUC
        SUP(J) = 0.0
        SUB(J) = CYYBUU
        RHS(J) = RHS(J) + CYYD(J)*P(J-1,I) - CYYC(J)*P(J,I) + CYYU(J)*P(J+1,I) &
                - (-CYYBUC*P(J,I) + CYYBUU*P(J+1,I) + FXUBC(I))

        ! Lower surface
        J = JLOW
        DIAG(J) = DIAG(J) + CYYC(J) - CYYBLC
        SUP(J) = CYYBLD
        SUB(J) = 0.0
        RHS(J) = RHS(J) + CYYD(J)*P(J-1,I) - CYYC(J)*P(J,I) + CYYU(J)*P(J+1,I) &
                - (-CYYBLC*P(J,I) + CYYBLD*P(J-1,I) + FXLBC(I))
      else if (I > ITE) then
        ! Kutta slice
        RHS(JLOW) = RHS(JLOW) + CYYU(JLOW)*PJUMP(I)
        RHS(JUP) = RHS(JUP) - CYYD(JUP)*PJUMP(I)
      end if

      ! Apply wall boundary conditions
      call BCEND()

      ! Add artificial dissipation
      do J = JBOT, JTOP
        DIAG(J) = DIAG(J) - EPSX
        RHS(J) = RHS(J) - EPSX*(P(J,I-1) - POLD_CUR(J))
      end do

      ! Solve tridiagonal system
      DNOM = 1.0 / DIAG(JBOT)
      SAVE_ARR(JBOT) = SUB(JBOT) * DNOM
      RHS(JBOT) = RHS(JBOT) * DNOM

      do J = J1_LOC, JTOP
        DNOM = 1.0 / (DIAG(J) - SUP(J)*SAVE_ARR(J-1))
        SAVE_ARR(J) = SUB(J) * DNOM
        RHS(J) = (RHS(J) - SUP(J)*RHS(J-1)) * DNOM
      end do

      do K = 1, J2_LOC
        J = JTOP - K
        RHS(J) = RHS(J) - SAVE_ARR(J) * RHS(J+1)
      end do

      ! Update solution and track maximum error
      do J = JBOT, JTOP
        P(J,I) = P(J,I) + RHS(J)
        ARHS = abs(RHS(J))
        if (ARHS > ERROR) then
          ERROR = ARHS
          IERROR = I
          JERROR = J
        end if
      end do

      EMU_PREV = EMU_CUR
    end do

  end subroutine SYOR

  ! Main iteration loop: solver, convergence, and flow updates
  subroutine SOLVE()
    use common_data, only: MAXIT, ERROR, CVERGE, DVERGE, IPRTER, PRTFLO, ABORT1
    use common_data, only: IREF, WE, EPS, IMIN, IMAX, JMIN, JMAX, IUP, IDOWN
    use common_data, only: ILE, ITE, JUP, JLOW, JTOP, JBOT, J1, J2, KSTEP
    use common_data, only: P, X, Y, AK, ALPHA, DUB, GAM1, RTK
    use common_data, only: EMU, POLD, DCIRC, OUTERR, I1, I2, IERROR, JERROR
    use common_data, only: THETA, BCTYPE, CIRCFF, FHINV, POR, CIRCTE
    use common_data, only: NWDGE, WSLP, XSHK, THAMAX, AM1, ZETA, NVWPRT, NISHK
    use common_data, only: WCONST, REYNLD, WI, C1
    use common_data, only: CLFACT, CMFACT, UNIT_OUTPUT
    use math_module, only: LIFT, PITCH
    use math_module, only: VWEDGE
    implicit none
    
    integer :: ITER, MAXITM, KK, J, I, IK, JK, JINC, N, NN
    real :: WEP, CL_LOCAL, CM_LOCAL, ERCIRC, THA
    logical :: CONVERGED
    integer, parameter :: NDUB = 25
    
    ! Initialize
    ABORT1 = .false.
    CONVERGED = .false.
    
    ! Write header to output files
    write(UNIT_OUTPUT, '(1H1)')
    
    ! Calculate maximum iterations based on refinement level
    if (IREF == 2) MAXITM = MAXIT / 4
    if (IREF == 1) MAXITM = MAXIT / 2
    if (IREF == 0) MAXITM = MAXIT
    
    ! Set relaxation parameter based on refinement level
    KK = 3 - IREF
    WEP = WE(KK)
    WI = 1.0 / WEP
    
    ! Write solver parameters
    write(UNIT_OUTPUT, '(3X,"WE = ",F7.4,5X,"EPS = ",F8.4,5X,"MAXIT FOR THIS MESH = ",I4)') WEP, EPS, MAXITM
    
    ! Write iteration header
    write(UNIT_OUTPUT, '(/,"  ITER",5X,"CL",8X,"CM",4X,"IERR",1X,"JERR",4X,"ERROR",4X,"IRL",2X,"JRL",4X,"BIGRL",8X,"ERCIRC")')
    
    ! Main iteration loop
    do ITER = 1, MAXITM
        ! Initialize EMU array
        I1 = 1
        I2 = 2
        do J = JMIN, JMAX
            POLD(J,I2) = P(J,IUP-1)
            EMU(J,I2) = 0.0
        end do
        
        ! Set EMU for subsonic flow
        if (AK <= 0.0) then
            do J = JMIN, JMAX
                EMU(J,I2) = C1(2)
            end do
        end if
        
        ! Set output flag for this iteration
        OUTERR = .false.
        if (mod(ITER, IPRTER) == 0) OUTERR = .true.
        
        ! Reset error tracking
        ERROR = 0.0
        
        ! Update circulation-jump boundary
        call RECIRC()
        
        ! Perform SOR sweep
        call SYOR()
        
        ! Update circulation for subsonic freestream flow
        if (AK >= 0.0 .and. BCTYPE == 1) then
            IK = IUP - IMIN
            do I = IUP, IDOWN
                IK = IK + KSTEP
                JK = JBOT - JMIN
                do J = JBOT, JTOP
                    JINC = KSTEP
                    if (Y(J) < 0.0 .and. Y(J+1) > 0.0) JINC = 2 * KSTEP - 1
                    JK = JK + JINC
                    P(J,I) = P(J,I) + DCIRC * THETA(JK,IK)
                end do
            end do
        end if
        
        ! Update doublet strength every NDUB iterations
        if (mod(ITER, NDUB) == 0) call REDUB()
        
        ! Reset boundary conditions
        call RESET()
        
        ! Compute viscous wedge if enabled
        if (NWDGE > 0) call VWEDGE()
        
        ! Print iteration results if needed
        if (OUTERR) then

            CL_LOCAL = LIFT(CLFACT)
            CM_LOCAL = PITCH(CMFACT)
            ERCIRC = abs(DCIRC)
            
            write(UNIT_OUTPUT, '(1X,I4,2F10.5,2I5,E13.4,2I4,2E13.4)') ITER, CL_LOCAL, CM_LOCAL, IERROR, JERROR, ERROR, 0, 0, 0.0, ERCIRC  ! IRL, JRL, BIGRL simplified
            
            ! Output viscous wedge quantities if enabled
            if (NWDGE > 0) then
                
              write(UNIT_OUTPUT, '(10X,"COMPUTED VISCOUS WEDGE QUANTITIES")')
                
                ! Upper surface shocks
                NN = NVWPRT(1)
                if (NN > 0) then
                  write(UNIT_OUTPUT, '(" UPPER SHOCK",8X,"X/C",10X,"MACH NO",9X,"THETA",10X,"ZETA")')
                  do N = 1, NN                        
                    if (AM1(1,N) > 1.0) then
                      THA = THAMAX(1,N) * 57.29578  ! Convert to degrees
                      write(UNIT_OUTPUT, '(I9,4F15.5)') N, XSHK(1,N), AM1(1,N), THA, ZETA(1,N)
                    else
                      write(UNIT_OUTPUT, '(I9,5X,"WEAK SHOCK, NO WEDGE INCLUDED")') N
                    end if
                  end do
                end if
                
                ! Lower surface shocks
                NN = NVWPRT(2)
                if (NN > 0) then
                  write(UNIT_OUTPUT, '(" LOWER SHOCK",8X,"X/C",10X,"MACH NO",9X,"THETA",10X,"ZETA")')
                  do N = 1, NN                        
                    if (AM1(2,N) > 1.0) then
                        THA = THAMAX(2,N) * 57.29578  ! Convert to degrees
                        write(UNIT_OUTPUT, '(I9,4F15.5)') N, XSHK(2,N), AM1(2,N), THA, ZETA(2,N)
                      else
                        write(UNIT_OUTPUT, '(I9,5X,"WEAK SHOCK, NO WEDGE INCLUDED")') N
                      end if
                  end do
                end if

                if (NISHK == 0) write(UNIT_OUTPUT, '(5X,"NO VISCOUS WEDGE, SINCE NO SHOCKS EXIST ")')

                write(UNIT_OUTPUT, '(/,"  ITER",5X,"CL",8X,"CM",4X,"IERR",1X,"JERR",4X,"ERROR",4X,"IRL",2X,"JRL",4X,"BIGRL",8X,"ERCIRC")')

            end if
        end if
        
        ! Check convergence
        if (ERROR <= CVERGE) then
            CONVERGED = .true.
            write(UNIT_OUTPUT, '(//20X,"........SOLUTION CONVERGED........")')
            exit
        end if
        
        ! Check divergence
        if (ERROR >= DVERGE) then
            ABORT1 = .true.
            write(UNIT_OUTPUT, '(//20X,"******  SOLUTION DIVERGED  ******")')
            exit
        end if

    end do
    
    ! Handle case where iteration limit is reached
    if (.not. CONVERGED .and. .not. ABORT1) then
      write(UNIT_OUTPUT, '(//20X,"******  ITERATION LIMIT REACHED  ******")')
    end if

    return

  end subroutine SOLVE

  ! Update circulation-jump boundary after Kutta or M divergence
  ! RECIRC computes:
  ! 1.) Jump in P at trailing edge = CIRCTE
  ! 2.) Circulation for farfield boundary = CIRCFF
  ! 3.) Jump in P along slit Y=0, X > 1 by linear interpolation between CIRCTE and CIRCFF
  subroutine RECIRC()
    use common_data, only: P, X, IMIN, IMAX, ITE, JUP, JLOW, CJUP, CJUP1, CJLOW, CJLOW1
    use common_data, only: PJUMP, CIRCFF, CIRCTE, DCIRC, WCIRC, CLSET, CLFACT, KUTTA
    implicit none
    integer :: I
    real :: CTEOLD, PUP, PLOW, CIRCO, FACTOR

    ! Compute jump in potential at trailing edge
    CTEOLD = CIRCTE
    PUP = CJUP*P(JUP,ITE) - CJUP1*P(JUP+1,ITE)
    PLOW = CJLOW*P(JLOW,ITE) - CJLOW1*P(JLOW-1,ITE)
    CIRCTE = PUP - PLOW
    
    ! Compute far field circulation
    CIRCO = CIRCFF
    if (KUTTA) then
      CIRCFF = (1.0 - WCIRC)*CIRCO + CIRCTE*WCIRC
    else
      CIRCFF = 0.5*CLSET/CLFACT
    end if
    
    ! Fix jump in P at airfoil trailing edge if KUTTA=.FALSE.
    ! and lift of airfoil exceeds CLSET
    if (.not. KUTTA) CIRCTE = CIRCFF
    DCIRC = CIRCTE - CTEOLD
    
    ! Set jump in P along Y = 0, X > 1 by linear interpolation
    FACTOR = (CIRCFF - CIRCTE)/(X(IMAX) - 1.0)
    do I = ITE, IMAX
      PJUMP(I) = CIRCTE + (X(I) - 1.0) * FACTOR
    end do

  end subroutine RECIRC

  ! Update doublet strength DUB for nonlinear correction
  ! For lifting free air flows, doublet strength is set equal to model volume.
  ! For other flows, the nonlinear contribution is added.
  subroutine REDUB()
    use common_data, only: P, X, Y, IMIN, IMAX, IUP, IDOWN, ILE, ITE
    use common_data, only: JMIN, JMAX, JUP, JLOW, JTOP, JBOT, J1, J2
    use common_data, only: AK, ALPHA, DUB, GAM1, RTK, XDIFF, YDIFF
    use common_data, only: BCTYPE, CIRCFF, VOL, XI, ARG
    use math_module, only: PX, TRAP
    implicit none
    
    integer :: I, J, K, L, JSTART, JEND
    real :: UPXSQ, SUM, UPSQ, UU
    
    ! For free air with circulation, set DUB = VOL
    if (BCTYPE == 1 .and. abs(CIRCFF) >= 0.0001) then
      DUB = VOL
      return
    end if
    
    ! Compute double integral of U*U over mesh domain for doublet strength
    ! U = PX is centered midway between X mesh points.
    ! First the integral (PX**2)DY is calculated for X = constant lines
    
    L = 0
    do I = IUP, IDOWN
      L = L + 1
      XI(L) = 0.5 * (X(I) + X(I-1))
      
      ! Integrate (PX**2) in Y direction
      K = 0
      JSTART = JBOT
      JEND = JTOP
      
      do J = JSTART, JEND
        K = K + 1
        UU = PX(I,J)
        ARG(K) = UU * UU
        if (K == 1) then
          Y(K) = 0.5 * (Y(J) + Y(J-1))
        else
          Y(K) = 0.5 * (Y(J) + Y(J-1))
        end if
      end do
      
      call TRAP(Y, ARG, K, UPSQ)
      ARG(L) = UPSQ
    end do
    
    ! Now integrate UPSQ in X direction
    call TRAP(XI, ARG, L, UPXSQ)
    
    ! Update doublet strength
    DUB = DUB + UPXSQ

  end subroutine REDUB

  ! Reset far-field boundary values after mesh change or Kutta
  subroutine RESET()
    use common_data, only: P, IMIN, IMAX, JMIN, JMAX, JUP, JLOW, KSTEP
    use common_data, only: DUP, DDOWN, DTOP, DBOT, VUP, VDOWN, VTOP, VBOT
    use common_data, only: CIRCFF, DUB, BCTYPE
    implicit none
    integer :: J, I, K

    ! Set boundary conditions at upstream and downstream ends
    K = JMIN - KSTEP
    do J = JMIN, JMAX
      K = K + KSTEP
      if (J == JUP) K = K + KSTEP - 1
      P(J,IMIN) = CIRCFF*VUP(K) + DUB*DUP(K)
      P(J,IMAX) = CIRCFF*VDOWN(K) + DUB*DDOWN(K)
    end do

    if (BCTYPE == 1) then
      ! Update boundary conditions on top and bottom
      K = IMIN - KSTEP
      do I = IMIN, IMAX
        K = K + KSTEP
        P(JMIN,I) = CIRCFF*VBOT(K) + DUB*DBOT(K)
        P(JMAX,I) = CIRCFF*VTOP(K) + DUB*DTOP(K)
      end do
    end if

  end subroutine RESET

end module numerical_solvers
