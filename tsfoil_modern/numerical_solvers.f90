! numerical_solvers.f90
! Module for SOR solver and iteration control routines

module numerical_solvers
  implicit none
  public :: SYOR, SOLVE, RECIRC, REDUB, RESET

contains

  ! Perform one SOR sweep computing residuals and updating P
  ! SYOR COMPUTES NEW P AT ALL MESH POINTS.
  ! CALLED BY - SOLVE.
  subroutine SYOR(I1, I2, BIGRL, IRL, JRL)
    use common_data, only: P, X, IUP, IDOWN, ILE, ITE
    use common_data, only: JMIN, JMAX, JUP, JLOW, JTOP, JBOT
    use common_data, only: AK, ERROR, IERROR, JERROR
    use common_data, only: CXL, CXC, CXR, CXXL, CXXC, CXXR, C1
    use common_data, only: CYYC, CYYD, CYYU, DIAG, RHS, SUB, SUP, IVAL
    use common_data, only: CYYBUC, CYYBUU, CYYBLC, CYYBLD, FXUBC, FXLBC
    use common_data, only: PJUMP, FCR, EPS, WI
    use common_data, only: EMU, POLD, OUTERR
    use common_data, only: N_MESH_POINTS
    use solver_module, only: BCEND
    implicit none
    integer, intent(inout) :: I1, I2
    real, intent(out) :: BIGRL  ! Maximum residual value
    integer, intent(out) :: IRL, JRL  ! Location indices of maximum residual

    integer :: I, J, K, IM2, JA, JB, ISAVE
    real :: EPSX, ARHS, DNOM, denominator
    real, dimension(N_MESH_POINTS) :: VC, SAVE_VAR
    real, parameter :: TOLERANCE = 1.0E-6   ! Tolerance for division by zero protection (more reasonable value)
    
    BIGRL = 0.0

    IM2 = IUP - 1
    if (AK < 0.0) IM2 = IUP - 2
        
    do I = IUP, IDOWN
        EPSX = EPS / ((X(I) - X(I-1))**2)
        
        ! Compute VC = 1 - M**2
        do J = JBOT, JTOP
            VC(J) = C1(I) - (CXL(I)*POLD(J,I2) + CXC(I)*P(J,I) + CXR(I)*P(J,I+1))
            EMU(J,I1) = 0.0
            POLD(J,I1) = P(J,I)
        end do
        
        do J = JBOT, JTOP
            if (VC(J) < 0.0) EMU(J,I1) = VC(J)
        end do
        
        if (.not. FCR) then
            do J = JBOT, JTOP
                EMU(J,I2) = EMU(J,I1)
            end do
        end if
        
        ! Compute elements of matrix
        do J = JBOT, JTOP
            DIAG(J) = (EMU(J,I1) - VC(J)) * CXXC(I) * WI + EMU(J,I2) * CXXR(I-1) - CYYC(J)
            SUP(J) = CYYD(J)
            SUB(J) = CYYU(J)
        end do
        
        ! Compute residual
        do J = JBOT, JTOP
            RHS(J) = -(VC(J) - EMU(J,I1)) * (CXXL(I)*P(J,I-1) - CXXC(I)*P(J,I) + CXXR(I)*P(J,I+1))
        end do
        
        do J = JBOT, JTOP
            RHS(J) = RHS(J) - (EMU(J,I2) * (CXXL(I-1)*P(J,IM2) - CXXC(I-1)*P(J,I-1) + CXXR(I-1)*P(J,I)))
        end do
        
        JA = JBOT + 1
        JB = JTOP - 1
        do J = JA, JB
            RHS(J) = RHS(J) - (CYYD(J)*P(J-1,I) - CYYC(J)*P(J,I) + CYYU(J)*P(J+1,I))
        end do
        
        RHS(JBOT) = RHS(JBOT) - (-CYYC(JBOT)*P(JBOT,I) + CYYU(JBOT)*P(JBOT+1,I))
        if (JBOT /= JMIN) then
            RHS(JBOT) = RHS(JBOT) - CYYD(JBOT)*P(JBOT-1,I)
        end if
        
        RHS(JTOP) = RHS(JTOP) - (CYYD(JTOP)*P(JTOP-1,I) - CYYC(JTOP)*P(JTOP,I))
        if (JTOP /= JMAX) then
            RHS(JTOP) = RHS(JTOP) - CYYU(JTOP)*P(JTOP+1,I)
        end if
        
        ! Check for airfoil B.C. and Kutta slice
        if (I < ILE) then
            ! Before airfoil - do nothing
        else if (I <= ITE) then
            ! Airfoil B.C.
            J = JUP
            DIAG(J) = DIAG(J) + CYYC(J) - CYYBUC
            SUP(J) = 0.0
            SUB(J) = CYYBUU
            RHS(J) = RHS(J) + CYYD(J)*P(J-1,I) - CYYC(J)*P(J,I) + CYYU(J)*P(J+1,I) &
                    - (-CYYBUC*P(J,I) + CYYBUU*P(J+1,I) + FXUBC(I))
            
            J = JLOW
            DIAG(J) = DIAG(J) + CYYC(J) - CYYBLC
            SUP(J) = CYYBLD
            SUB(J) = 0.0
            RHS(J) = RHS(J) + CYYD(J)*P(J-1,I) - CYYC(J)*P(J,I) + CYYU(J)*P(J+1,I) &
                    - (-CYYBLC*P(J,I) + CYYBLD*P(J-1,I) + FXLBC(I))
        else
            ! Kutta slice change
            RHS(JLOW) = RHS(JLOW) + CYYU(JLOW)*PJUMP(I)
            RHS(JUP) = RHS(JUP) - CYYD(JUP)*PJUMP(I)
        end if
        
        ! Insert wall B.C.
        IVAL = I
        call BCEND()
        
        ! Compute max residual
        if (OUTERR) then
            do J = JBOT, JTOP
                ARHS = abs(RHS(J))
                if (ARHS > BIGRL) then
                    BIGRL = ARHS
                    IRL = I
                    JRL = J
                end if
            end do
        end if
        
        ! Add PXT (artificial dissipation)
        do J = JBOT, JTOP
            DIAG(J) = DIAG(J) - EPSX
            RHS(J) = RHS(J) - EPSX*(P(J,I-1) - POLD(J,I2))
        end do
        
        ! Solve tridiagonal matrix equation
        DNOM = 1.0 / DIAG(JBOT)
        SAVE_VAR(JBOT) = SUB(JBOT) * DNOM
        RHS(JBOT) = RHS(JBOT) * DNOM

        do J = JBOT + 1, JTOP
            denominator = DIAG(J) - SUP(J)*SAVE_VAR(J-1)
            DNOM = 1.0 / denominator
            SAVE_VAR(J) = SUB(J) * DNOM
            RHS(J) = (RHS(J) - SUP(J)*RHS(J-1)) * DNOM
            if (abs(RHS(J)) < 1.0E-30) RHS(J) = 0.0
        end do
        
        ! Back-substitution with floating-point protection
        do K = 1, JTOP - JBOT
            J = JTOP - K
            RHS(J) = RHS(J) - SAVE_VAR(J) * RHS(J+1)
            if (abs(RHS(J)) < 1.0E-30) RHS(J) = 0.0
        end do
        
        ! Compute new P
        do J = JBOT, JTOP
            P(J,I) = P(J,I) + RHS(J)
        end do
        
        ! Compute max error (always needed for convergence checking)
        do J = JBOT, JTOP
            ARHS = abs(RHS(J))
            if (ARHS > ERROR) then
                ERROR = ARHS
                IERROR = I
                JERROR = J
            end if
        end do
        
        ! Supersonic freestream flow condition
        if (AK <= 0.0 .and. I == IDOWN-1) then
            ! Set P(IDOWN+1) = P(IDOWN-1) to obtain centered velocity at IDOWN for supersonic freestream flow
            do J = JMIN, JMAX
                P(J,IDOWN+1) = P(J,IDOWN-1)
            end do
        end if
        
        ! Swap I1 and I2 indices
        ISAVE = I2
        I2 = I1
        I1 = ISAVE
        IM2 = I - 1
    end do

  end subroutine SYOR

  ! Main iteration loop: solver, convergence, and flow updates
  subroutine SOLVE()
    use common_data, only: MAXIT, ERROR, CVERGE, DVERGE, IPRTER, ABORT1
    use common_data, only: WE, EPS, IMIN, JMIN, JMAX, IUP, IDOWN
    use common_data, only: JTOP, JBOT, KSTEP
    use common_data, only: P, Y, AK
    use common_data, only: EMU, POLD, DCIRC, OUTERR, IERROR, JERROR
    use common_data, only: THETA, BCTYPE
    use common_data, only: NWDGE, XSHK, THAMAX, AM1, ZETA, NVWPRT, NISHK
    use common_data, only: WI, C1
    use common_data, only: CLFACT, CMFACT, UNIT_OUTPUT
    use math_module, only: LIFT, PITCH, VWEDGE
    use solver_module, only: SETBC
    implicit none
    
    integer :: ITER, MAXITM, KK, J, I, IK, JK, JINC, N, NN, I1, I2
    integer :: IRL, JRL  ! Location indices of maximum residual

    real :: BIGRL  ! Maximum residual value
    real :: WEP, CL_LOCAL, CM_LOCAL, ERCIRC, THA
    logical :: CONVERGED
    integer, parameter :: NDUB = 25
    
    ! Initialize
    ABORT1 = .false.
    CONVERGED = .false.
    
    ! Write header to output files
    write(UNIT_OUTPUT, '(1H1)')
    
    ! Calculate maximum iterations based on refinement level
    MAXITM = MAXIT
    
    ! Set relaxation parameter based on refinement level
    KK = 3
    WEP = WE(KK)
    WI = 1.0 / WEP
    
    ! Write solver parameters
    write(UNIT_OUTPUT, '(3X,"WE = ",F7.4,5X,"EPS = ",F8.4,5X,"MAXIT FOR THIS MESH = ",I4)') WEP, EPS, MAXITM
    
    ! Write iteration header
    write(UNIT_OUTPUT, '(/,"  ITER",5X,"CL",8X,"CM",4X,"IERR",1X,"JERR",4X,"ERROR",4X,"IRL",2X,"JRL",4X,"BIGRL",8X,"ERCIRC")')
    write(*, '(/,"  ITER",5X,"CL",8X,"CM",4X,"IERR",1X,"JERR",4X,"ERROR",4X,"IRL",2X,"JRL",4X,"BIGRL",8X,"ERCIRC")')

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
        if (ITER == 1) OUTERR = .true.

        ! Reset error tracking for this iteration
        ERROR = 0.0
        if (OUTERR) BIGRL = 0.0
        
        ! Update circulation-jump boundary
        call RECIRC()
        
        ! Perform SOR sweep
        call SYOR(I1, I2, BIGRL, IRL, JRL)
        
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
        if (NWDGE > 0) then
          call VWEDGE()
          call SETBC(1)
        end if
        
        ! Print iteration results if needed
        if (OUTERR) then

            CL_LOCAL = LIFT(CLFACT)
            CM_LOCAL = PITCH(CMFACT)
            ERCIRC = abs(DCIRC)
            
            write(UNIT_OUTPUT, '(1X,I4,2F10.5,2I5,E13.4,2I4,2E13.4)') &
                  ITER, CL_LOCAL, CM_LOCAL, IERROR, JERROR, ERROR, IRL, JRL, BIGRL, ERCIRC
            write(*, '(1X,I4,2F10.5,2I5,E13.4,2I4,2E13.4)') &
                  ITER, CL_LOCAL, CM_LOCAL, IERROR, JERROR, ERROR, IRL, JRL, BIGRL, ERCIRC

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

                if (ITER == 1 .or. mod(ITER, IPRTER) == 0) then
                    
                    if (NISHK == 0) write(UNIT_OUTPUT, '(5X,"NO VISCOUS WEDGE, SINCE NO SHOCKS EXIST ")')

                    write(UNIT_OUTPUT, '(/,"  ITER",5X,"CL",8X,"CM",4X,"IERR",1X,"JERR",4X, &
                          &"ERROR",4X,"IRL",2X,"JRL",4X,"BIGRL",8X,"ERCIRC")')

                end if
            end if
        end if
        
        ! Check convergence
        if (ERROR <= CVERGE) then
            CONVERGED = .true.
            write(UNIT_OUTPUT, '(//20X,"........SOLUTION CONVERGED........")')
            write(*,*) 'Solution converged after', ITER, 'iterations.'
            exit
        end if
        
        ! Check for floating-point exceptions during iteration
        ! call check_iteration_fp_exceptions(ITER)
        
        ! Check divergence
        if (ERROR >= DVERGE) then
            ABORT1 = .true.
            write(UNIT_OUTPUT, '(//20X,"******  SOLUTION DIVERGED  ******")')
            write(*,*) 'Solution diverged after', ITER, 'iterations.'
            exit
        end if

    end do
    
    ! Handle case where iteration limit is reached
    if (.not. CONVERGED .and. .not. ABORT1) then
      write(UNIT_OUTPUT, '(//20X,"******  ITERATION LIMIT REACHED  ******")')
      write(*,*) 'Iteration limit reached after', MAXITM, 'iterations.'
    end if

    return

  end subroutine SOLVE

  ! Update circulation-jump boundary after Kutta or M divergence
  ! RECIRC computes:
  ! 1.) Jump in P at trailing edge = CIRCTE
  ! 2.) Circulation for farfield boundary = CIRCFF
  ! 3.) Jump in P along slit Y=0, X > 1 by linear interpolation between CIRCTE and CIRCFF
  subroutine RECIRC()
    use common_data, only: P, X, IMAX, ITE, JUP, JLOW, CJUP, CJUP1, CJLOW, CJLOW1
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
    use common_data, only: P, Y, IMIN, IMAX, JMIN, JMAX, N_MESH_POINTS
    use common_data, only: DUB, GAM1, XDIFF, BCTYPE, CIRCFF, VOL
    use math_module, only: TRAP
    implicit none
    
    ! Local variables
    integer :: I, J, IEND, NARG
    real :: DBLSUM, SUM, TEMP
    real :: XI(N_MESH_POINTS), ARG(N_MESH_POINTS)
    
    ! For lifting free air flows with circulation, set doublet strength equal to model volume
    if (BCTYPE == 1 .and. abs(CIRCFF) >= 0.0001) then
      DUB = VOL
      return
    end if
    
    ! Compute double integral of U*U over mesh domain for doublet strength
    ! U = (∂P/∂x) is centered midway between X mesh points.
    ! First the integral (∂P/∂x)²dy is calculated for X held constant.
    ! Thus 1/(X(I+1)-X(I))² may be pulled out of the integral which is
    ! calculated by the trapezoidal rule. The X integration corresponds
    ! to summing these integrals, which lie midway between X mesh points,
    ! using a modified trapezoidal rule.
    
    IEND = IMAX - 1
    DBLSUM = 0.0
    
    do I = IMIN, IEND
      NARG = 0
      
      ! Build arrays for Y-direction integration at constant X
      do J = JMIN, JMAX
        NARG = NARG + 1
        TEMP = P(J, I+1) - P(J, I)  ! Finite difference approximation of ∂P/∂x
        ARG(NARG) = TEMP * TEMP     ! Square of velocity component
        XI(NARG) = Y(J)             ! Y-coordinates for integration
      end do
      
      ! Integrate (∂P/∂x)² in Y-direction using trapezoidal rule
      call TRAP(XI, ARG, NARG, SUM)
      
      ! Add contribution to double sum with X-direction weighting
      DBLSUM = DBLSUM + SUM * XDIFF(I+1)
    end do
    
    ! Apply scaling factor and update doublet strength
    DBLSUM = GAM1 * 0.25 * DBLSUM
    DUB = VOL + DBLSUM

  end subroutine REDUB

  ! Reset far-field boundary values after mesh change or Kutta
  ! Updates far field boundary conditions for subsonic freestream flows.
  ! CALLED BY - SOLVE.
  subroutine RESET()
    use common_data, only: P, IMIN, IMAX, JMIN, JMAX, JUP, KSTEP
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

    ! Update boundary conditions on top and bottom
    if (BCTYPE == 1) then
      K = IMIN - KSTEP
      do I = IMIN, IMAX
        K = K + KSTEP
        P(JMIN,I) = CIRCFF*VBOT(K) + DUB*DBOT(K)
        P(JMAX,I) = CIRCFF*VTOP(K) + DUB*DTOP(K)
      end do
    end if

  end subroutine RESET

end module numerical_solvers
