! main_iteration.f90
! Module for SOR solver and iteration control routines

module main_iteration
    implicit none
    private

    public :: SYOR, RECIRC, REDUB, RESET

contains

    ! Perform one SOR sweep computing residuals and updating P
    ! SYOR COMPUTES NEW P AT ALL MESH POINTS.
    ! CALLED BY - SOLVE.
    subroutine SYOR(I1, I2, OUTERR, BIGRL, IRL, JRL, IERROR, JERROR, ERROR)
        use common_data, only: X, IUP, IDOWN, ILE, ITE, JMIN, JMAX, JUP, JLOW, JTOP, JBOT
        use common_data, only: AK, FCR, EPS, N_MESH_POINTS
        use solver_data, only: P, PJUMP, DIAG, RHS, FXUBC, FXLBC, EMU, POLD, WI
        use solver_data, only: CXL, CXC, CXR, CXXL, CXXC, CXXR, C1, CYYC, CYYD, CYYU
        use solver_data, only: CYYBUC, CYYBUU, CYYBLC, CYYBLD
        implicit none
        integer, intent(inout) :: I1, I2  ! Indices for potential values
        logical, intent(inout) :: OUTERR  ! outer iteration error (logical)
        real, intent(out) :: BIGRL        ! Maximum residual value
        integer, intent(out) :: IRL, JRL  ! Location indices of maximum residual
        integer, intent(out) :: IERROR, JERROR  ! Location indices of maximum error
        real, intent(out) :: ERROR              ! Maximum error

        integer :: I=0, J=0, K=0, IM2=0, JA=0, JB=0, ISAVE=0
        real :: EPSX=0.0, ARHS=0.0, DNOM=0.0, denominator=0.0
        real :: VC(N_MESH_POINTS)=0.0, SAVE_VAR(N_MESH_POINTS)=0.0
        real :: SUB(N_MESH_POINTS)=0.0, SUP(N_MESH_POINTS)=0.0

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
            call BCEND(I)
            
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

    ! Update circulation-jump boundary after Kutta or M divergence
    ! RECIRC computes:
    ! 1.) Jump in P at trailing edge = CIRCTE
    ! 2.) Circulation for farfield boundary = CIRCFF
    ! 3.) Jump in P along slit Y=0, X > 1 by linear interpolation between CIRCTE and CIRCFF
    subroutine RECIRC(DCIRC)
        use common_data, only: X, IMAX, ITE, JUP, JLOW
        use common_data, only: CLSET, KUTTA, WCIRC
        use solver_data, only: P, PJUMP, CLFACT, CIRCFF
        use solver_data, only: CIRCTE, CJUP, CJUP1, CJLOW, CJLOW1
        implicit none
        real, intent(out) :: DCIRC  ! circulation change
        
        integer :: I=0
        real :: CTEOLD=0.0, PUP=0.0, PLOW=0.0, CIRCO=0.0, FACTOR=0.0

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
        use common_data, only: Y, IMIN, IMAX, JMIN, JMAX, N_MESH_POINTS
        use common_data, only: GAM1, XDIFF, BCTYPE, VOL
        use solver_data, only: P, CIRCFF, DUB
        implicit none
        
        ! Local variables
        integer :: I=0, J=0, IEND=0, NARG=0
        real :: DBLSUM=0.0, SUM=0.0, TEMP=0.0
        real :: XI(N_MESH_POINTS)=0.0, ARG(N_MESH_POINTS)=0.0
        
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
        use common_data, only: IMIN, IMAX, JMIN, JMAX, JUP, BCTYPE
        use solver_data, only: P, CIRCFF, DUB, KSTEP
        use solver_data, only: DUP, DDOWN, DTOP, DBOT, VUP, VDOWN, VTOP, VBOT
        implicit none
        integer :: J=0, I=0, K=0

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

    ! Apply boundary conditions on each i-line (upper/lower boundaries),
    ! which modifies the DIAG and RHS vectors on each I line in the
    ! appropriate way to include the boundary conditions at JBOT and JTOP.
    ! Called by - SYOR.
    subroutine BCEND(IVAL)    
        use common_data, only: X, Y, IUP, IDOWN, JMIN, JMAX, JTOP, JBOT, AK, XDIFF, BCTYPE, POR
        use solver_data, only: P, CYYD, CYYU, CIRCFF, FHINV, DIAG, RHS
        implicit none
        integer, intent(in) :: IVAL
        
        integer :: I=0, II=0
        real :: DFACL=0.0, DFACU=0.0, RFACL=0.0, RFACU=0.0, PJMIN=0.0, PJMAX=0.0, TERM=0.0, RTK=0.0
        logical :: apply_dirichlet=.false., apply_neumann=.false.
        
        I = IVAL
        apply_dirichlet = .false.
        apply_neumann = .false.
        
        ! Branch to appropriate address for BCTYPE
        select case (BCTYPE)
        
        case (1)  
            ! BCTYPE = 1, FREE AIR
            ! Dirichlet boundary condition for subsonic freestream
            if (AK > 0.0) return

            ! Neumann boundary condition for supersonic freestream
            RTK = sqrt(abs(AK))
            DFACL = -CYYD(JBOT) * RTK * XDIFF(I)
            DFACU = -CYYU(JTOP) * RTK * XDIFF(I)
            RFACL = DFACL * (P(JMIN,I) - P(JMIN,I-1))
            RFACU = DFACU * (P(JMAX,I) - P(JMAX,I-1))
            apply_neumann = .true.
            
        case (2)  
            ! BCTYPE = 2, SOLID WALL
            ! Neumann boundary condition = 0.
            ! No modification necessary to DIAG or RHS
            return
            
        case (3)  
            ! BCTYPE = 3, FREE JET
            ! Dirichlet boundary condition
            if (AK < 0.0) then
                PJMIN = 0.0
                PJMAX = 0.0
            else
                PJMIN = -0.75 * CIRCFF
                PJMAX = -0.25 * CIRCFF
            end if
            apply_dirichlet = .true.
            
        case (4)  
            ! BCTYPE = 4, IDEAL SLOTTED WALL
            ! Neumann boundary condition
            DFACL = -FHINV * CYYD(JBOT)
            DFACU = -FHINV * CYYU(JTOP)
            if (AK < 0.0) then
                RFACL = DFACL * P(JBOT,I)
                RFACU = DFACU * P(JTOP,I)
            else
                RFACL = DFACL * (0.75 * CIRCFF + P(JBOT,I))
                RFACU = DFACU * (0.25 * CIRCFF + P(JTOP,I))
            end if
            apply_neumann = .true.
            
        case (5)  
            ! BCTYPE = 5, POROUS/PERFORATED WALL
            if (POR > 1.5) then
                ! Dirichlet boundary condition for POR > 1.5
                if (I /= IUP) return
                ! Set values of P on boundary by integrating PX using
                ! old values of potential
                PJMIN = P(JMIN,IUP)
                TERM = -0.5 / (POR * (Y(JMIN) - Y(JMIN+1)))
                do II = IUP, IDOWN
                    P(JMIN,II) = P(JMIN,II-1) - TERM * (X(II)-X(II-1)) * &
                                (P(JMIN,II)+P(JMIN,II-1)-P(JMIN+1,II)-P(JMIN+1,II-1))
                end do
                PJMAX = P(JMAX,IUP)
                TERM = 0.5 / (POR * (Y(JMAX) - Y(JMAX-1)))
                do II = IUP, IDOWN
                    P(JMAX,II) = P(JMAX,II-1) - TERM * (X(II) - X(II-1)) * &
                                (P(JMAX,II)+P(JMAX,II-1)-P(JMAX-1,II)-P(JMAX-1,II-1))
                end do
                RHS(JBOT) = RHS(JBOT) - (CYYD(JBOT)*(P(JBOT-1,I)-PJMIN))
                RHS(JTOP) = RHS(JTOP) - (CYYU(JTOP)*(P(JTOP+1,I)-PJMAX))
                return
            else
                ! Neumann boundary condition for POR < 1.5
                DFACL = -CYYD(JBOT) * POR * XDIFF(I)
                DFACU = -CYYU(JTOP) * POR * XDIFF(I)
                RFACL = DFACL * (P(JMIN,I) - P(JMIN,I-1))
                RFACU = DFACU * (P(JMAX,I) - P(JMAX,I-1))
                apply_neumann = .true.
            end if
            
        case (6)  
            ! BCTYPE = 6, GENERAL WALL BOUNDARY CONDITION
            ! Difference equations for this boundary condition
            ! have not yet been worked out. User must insert
            ! information needed for calculation
            write(*, '(A, /, A)') '1ABNORMAL STOP IN SUBROUTINE BCEND', &
                                            'BCTYPE=6 IS NOT USEABLE'
            stop
                
        case default
            write(*, *) 'ERROR: Invalid BCTYPE = ', BCTYPE
            stop
                
        end select
        
        ! Apply Dirichlet boundary conditions
        if (apply_dirichlet) then
            RHS(JBOT) = RHS(JBOT) - (CYYD(JBOT)*(PJMIN-P(JBOT-1,I)))
            RHS(JTOP) = RHS(JTOP) - (CYYU(JTOP)*(PJMAX-P(JTOP+1,I)))
            return
        end if
        
        ! Apply Neumann boundary conditions
        if (apply_neumann) then
            DIAG(JBOT) = DIAG(JBOT) + DFACL
            DIAG(JTOP) = DIAG(JTOP) + DFACU
            RHS(JBOT) = RHS(JBOT) - RFACL + CYYD(JBOT)*P(JBOT-1,I)
            RHS(JTOP) = RHS(JTOP) - RFACU + CYYU(JTOP)*P(JTOP+1,I)
        end if
        
    end subroutine BCEND

    ! Integrates Y DX by trapezoidal rule
    subroutine TRAP(X_arr, Y_arr, N, SUM)
        implicit none
        integer, intent(in) :: N
        real, intent(in) :: X_arr(N), Y_arr(N)
        real, intent(out) :: SUM
        integer :: I_loop=0, NM1=0
        real :: Z=0.0, W=0.0
        
        SUM = 0.0
        NM1 = N - 1
        do I_loop = 1, NM1
            Z = X_arr(I_loop+1) - X_arr(I_loop)
            W = Y_arr(I_loop+1) + Y_arr(I_loop)
            SUM = SUM + Z*W
        end do
        SUM = 0.5*SUM

    end subroutine TRAP

end module main_iteration
