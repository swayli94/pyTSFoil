! solver_functions.f90
! Key functions for the solver

module solver_functions
    implicit none
    private

    public :: SETBC, BCEND, FARFLD, SCALE, EMACH1, VWEDGE
    
contains

  
    ! Scale physical variables to transonic similarity variables
    ! Define scaling factors for physical variables:
    !   CPFACT, CLFACT, CMFACT, CDFACT, VFACT, YFACT
    ! IF PHYS = .TRUE., ALL INPUT/OUTPUT QUANTITIES ARE IN PHYSICAL UNITS NORMALIZED 
    ! BY FREESTREAM VALUES AND AIRFOIL CHORD. 
    ! THIS SUBROUTINE THEN SCALES THE QUANTITIES TO TRANSONIC VARIABLES BY THE FOLLOWING CONVENTION
    !   SIMDEF = 1  COLE SCALING
    !   SIMDEF = 2  SPREITER SCALING
    !   SIMDEF = 3  KRUPP SCALING
    ! IF PHYS = .FALSE., INPUT IS ALREADY IN SCALED VARIABLES AND NO FURTHER SCALING IS DONE.
    ! CALLED BY - TSFOIL.
    subroutine SCALE()
        use common_data, only: PHYS, EMACH, SIMDEF
        use common_data, only: AK, ALPHA, GAM1
        use common_data, only: YIN, JMIN, JMAX, DELTA, H, POR
        use common_data, only: INPERR, UNIT_OUTPUT, FLAG_OUTPUT_SOLVE
        use solver_data, only: CPSTAR, CPFACT, CLFACT, CDFACT, CMFACT, YFACT, VFACT, SONVEL
        implicit none
        real :: EMACH2, BETA, DELRT1, DELRT2
        real :: YFACIV, EMROOT
        integer :: J
        
        if (.not. PHYS) then
            ! PHYS = .FALSE.  NO SCALING
            CPFACT = 1.0
            CDFACT = 1.0
            CLFACT = 1.0
            CMFACT = 1.0
            YFACT = 1.0
            VFACT = 1.0

        else
            ! PHYS = .TRUE.  COMPUTE CONSTANTS
            EMACH2 = EMACH*EMACH
            BETA = 1.0 - EMACH2
            DELRT1 = DELTA**(1.0/3.0)
            DELRT2 = DELTA**(2.0/3.0)

            ! Branch to appropriate scaling
            select case (SIMDEF)
            case (1)
                ! SIMDEF = 1
                ! COLE SCALING
                AK = BETA / DELRT2
                YFACT = 1.0 / DELRT1
                CPFACT = DELRT2
                CLFACT = DELRT2
                CDFACT = DELRT2 * DELTA
                CMFACT = DELRT2
                VFACT = DELTA * 57.295779
                
            case (2)
                ! SIMDEF = 2
                ! SPREITER SCALING
                EMROOT = EMACH**(2.0/3.0)
                AK = BETA / (DELRT2 * EMROOT * EMROOT)
                YFACT = 1.0 / (DELRT1 * EMROOT)
                CPFACT = DELRT2 / EMROOT
                CLFACT = CPFACT
                CMFACT = CPFACT
                CDFACT = CPFACT * DELTA
                VFACT = DELTA * 57.295779
                
            case (3)
                ! SIMDEF = 3
                ! KRUPP SCALING
                AK = BETA / (DELRT2 * EMACH)
                YFACT = 1.0 / (DELRT1 * EMACH**0.5)
                CPFACT = DELRT2 / (EMACH**0.75)
                CLFACT = CPFACT
                CMFACT = CPFACT
                CDFACT = CPFACT * DELTA
                VFACT = DELTA * 57.295779
                
            case default
                write(UNIT_OUTPUT, '(A, /, A)') '1ABNORMAL STOP IN SUBROUTINE SCALE', ' INVALID SIMDEF VALUE'
                stop

            end select

            ! Scale Y mesh
            YFACIV = 1.0 / YFACT
            do J = JMIN, JMAX
                YIN(J) = YIN(J) * YFACIV
            end do

            ! Scale tunnel parameters (wall height)
            H = H / YFACT
            POR = POR * YFACT
            if (FLAG_OUTPUT_SOLVE == 1) then
                write(UNIT_OUTPUT,'(//10X,11HSCALED POR=,F10.5)') POR
            end if

            ! Scale angle of attack
            ALPHA = ALPHA / VFACT
        end if

        ! CHECK VALUE OF AK FOR DEFAULT.
        if (AK == 0.0) call INPERR(7)

        ! COMPUTE SONIC VELOCITY
        if (abs(GAM1) <= 0.0001) then
            SONVEL = 1.0
            CPSTAR = 0.0
            return
        end if
        
        SONVEL = AK / GAM1
        CPSTAR = -2.0 * SONVEL * CPFACT

    end subroutine SCALE  

    ! Define solution limits and apply body slope boundary conditions
    ! SUBROUTINE SETBC sets the limits on range of I and J
    ! for solution of the difference equations.
    ! The body slope boundary condition at the current
    ! X mesh points on the body are multiplied by mesh
    ! spacing constants and entered into arrays FXUBC and
    ! FXLBC for use in subroutine SYOR.
    subroutine SETBC(IJUMP)
        use common_data, only: IMIN, IMAX, IUP, IDOWN, JMIN, JMAX, JTOP, JBOT
        use common_data, only: ILE, ITE, FXL, FXU, POR
        use common_data, only: AK, ALPHA, BCTYPE
        use solver_data, only: CYYBLU, CYYBUD, FXLBC, FXUBC, WSLP
        implicit none
        integer, intent(in) :: IJUMP
        integer, parameter :: KSTEP = 1 ! Step size for circulation-jump boundary update
        integer :: I, IF1, N, NFOIL, INT, JINT

        ! Set limits on I and J indices
        if (IJUMP <= 0) then
            ! IJUMP <= 0, use full range of I and J
            INT = 0
            if (AK < 0.0) INT = 1
            IUP = IMIN + 1 + INT
            IDOWN = IMAX - 1 + INT
            
            JINT = 0
            if (BCTYPE == 1 .and. AK > 0.0) JINT = 1
            if (BCTYPE == 3) JINT = 1
            if (BCTYPE == 5 .and. POR > 1.5) JINT = 1
            JBOT = JMIN + JINT
            JTOP = JMAX - JINT
        end if

        ! Airfoil body boundary condition
        ! Zero elements in arrays for upper and lower body boundary conditions
        do I = IMIN, IMAX
            FXLBC(I) = 0.0
            FXUBC(I) = 0.0
        end do
        
        ! Enter body slopes at mesh points on airfoil
        ! into arrays for body boundary conditions
        NFOIL = ITE - ILE + 1
        IF1 = NFOIL + KSTEP
        I = ITE + 1

        do N = 1, NFOIL
            I = I - 1
            IF1 = IF1 - KSTEP
            FXLBC(I) = CYYBLU * (FXL(IF1) - ALPHA + WSLP(I,2))
            FXUBC(I) = CYYBUD * (FXU(IF1) - ALPHA + WSLP(I,1))
        end do

    end subroutine SETBC

    ! Apply boundary conditions on each i-line (upper/lower boundaries),
    ! which modifies the DIAG and RHS vectors on each I line in the
    ! appropriate way to include the boundary conditions at JBOT and JTOP.
    ! Called by - SYOR.
    subroutine BCEND(IVAL)    
        use common_data, only: X, Y, IUP, IDOWN, JMIN, JMAX, JTOP, JBOT, AK, XDIFF, BCTYPE, UNIT_OUTPUT, POR
        use solver_data, only: P, CYYD, CYYU, CIRCFF, FHINV, DIAG, RHS
        implicit none
        integer, intent(in) :: IVAL
        
        integer :: I, II
        real :: DFACL, DFACU, RFACL, RFACU, PJMIN, PJMAX, TERM, RTK
        logical :: apply_dirichlet, apply_neumann
        
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
            write(UNIT_OUTPUT, '(A, /, A)') '1ABNORMAL STOP IN SUBROUTINE BCEND', &
                                            'BCTYPE=6 IS NOT USEABLE'
            stop
                
        case default
            write(UNIT_OUTPUT, *) 'ERROR: Invalid BCTYPE = ', BCTYPE
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

    ! Compute far-field boundary conditions for outer boundaries
    subroutine FARFLD()
        use common_data, only: AK, X, Y, IMIN, IMAX, JMIN, JMAX, BCTYPE, PI, TWOPI, HALFPI, UNIT_OUTPUT
        use common_data, only: F, H, POR
        use solver_data, only: XSING, FHINV
        use solver_data, only: B_COEF, OMEGA0, OMEGA1, OMEGA2, JET, PSI0, PSI1, PSI2
        use solver_data, only: DTOP, DBOT, VTOP, VBOT, DUP, DDOWN, VUP, VDOWN
        use solver_data, only: RTKPOR
        use solver_data, only: ALPHA0, ALPHA1, ALPHA2, BETA0, BETA1, BETA2
        use solver_base, only: ANGLE
        implicit none
        integer :: I, J
        real :: YT, YB, XU_BC, XD_BC, YT2, YB2, XU2, XD2, COEF1, COEF2
        real :: XP, XP2, YJ, YJ2, Q, ARG0, ARG1, ARG2
        real :: EXARG0, EXARG1, EXARG2, TERM
        real :: RTK

        ! Test for supersonic or subsonic freestream
        if (AK <= 0.0) then
            ! Supersonic freestream
            if (F /= 0.0 .and. H /= 0.0) then
                FHINV = 1.0 / (F * H)
            else
                FHINV = 1.0
            end if
            ! For supersonic case, upstream boundary conditions correspond to uniform
            ! undisturbed flow. Downstream boundary required to be supersonic.
            ! Top and bottom boundaries use simple wave solution.
            return
        end if

        RTK = sqrt(abs(AK))

        ! Subsonic freestream
        ! Functional form of the potential on outer boundaries is prescribed.
        ! Equations represent asymptotic form for doublet and vortex in free air
        ! and wind tunnel environment. Doublet and vortex are located at X=XSING, Y=0.
        ! Actual boundary values are set in subroutines RECIRC and REDUB where the 
        ! functional forms are multiplied by the vortex and doublet strengths.
        ! The boundary conditions are calculated herein for the input X and Y mesh 
        ! and values are deleted for the coarse mesh in subroutine SETBC.

        ! Set default values for tunnel wall parameters
        B_COEF = 0.0
        OMEGA0 = 1.0
        OMEGA1 = 1.0
        OMEGA2 = 1.0
        JET = 0.0
        PSI0 = 1.0
        PSI1 = 1.0
        PSI2 = 1.0

        ! Branch to appropriate formulas depending on BCTYPE
        select case (BCTYPE)
        case (1)
            ! BCTYPE = 1: FREE AIR BOUNDARY CONDITION
            ! Set boundary ordinates
            YT = Y(JMAX) * RTK
            YB = Y(JMIN) * RTK
            XU_BC = X(IMIN) - XSING
            XD_BC = X(IMAX) - XSING
            YT2 = YT * YT
            YB2 = YB * YB
            XU2 = XU_BC * XU_BC
            XD2 = XD_BC * XD_BC
            COEF1 = 1.0 / TWOPI
            COEF2 = 1.0 / (TWOPI * RTK)

            ! Compute doublet and vortex terms on top and bottom boundaries
            do I = IMIN, IMAX
                XP = X(I) - XSING
                XP2 = XP * XP
                DTOP(I) = XP / (XP2 + YT2) * COEF2
                DBOT(I) = XP / (XP2 + YB2) * COEF2
                VTOP(I) = -atan2(YT, XP) * COEF1
                VBOT(I) = -(atan2(YB, XP) + TWOPI) * COEF1
            end do

            ! Compute doublet and vortex terms on upstream and downstream boundaries
            do J = JMIN, JMAX
                YJ = Y(J) * RTK
                YJ2 = YJ * YJ
                DUP(J) = XU_BC / (XU2 + YJ2) * COEF2
                DDOWN(J) = XD_BC / (XD2 + YJ2) * COEF2
                Q = PI - sign(PI, YJ)
                VUP(J) = -(atan2(YJ, XU_BC) + Q) * COEF1
                VDOWN(J) = -(atan2(YJ, XD_BC) + Q) * COEF1
            end do
            
            if (AK > 0.0) then
                call ANGLE()
            end if
            return

        case (2)
            ! BCTYPE = 2: SOLID WALL TUNNEL
            POR = 0.0
            ! Set constants for doublet solution
            B_COEF = 0.5
            ALPHA0 = PI
            ALPHA1 = PI
            ALPHA2 = PI
            ! Set constants for vortex solution
            BETA0 = HALFPI
            BETA1 = HALFPI
            BETA2 = HALFPI

        case (3)
            ! BCTYPE = 3: FREE JET
            F = 0.0
            RTKPOR = 0.0
            ! Set constants for doublet solution
            ALPHA0 = HALFPI
            ALPHA1 = HALFPI
            ALPHA2 = HALFPI
            ! Set constants for vortex solution
            JET = 0.5
            BETA0 = 0.0
            BETA1 = 0.0
            BETA2 = 0.0

        case (4)
            ! BCTYPE = 4: IDEAL SLOTTED WALL
            RTKPOR = 0.0
            FHINV = 1.0 / (F * H)
            ! Set constants for doublet solution
            call DROOTS()
            ! Set constants for vortex solution
            JET = 0.5
            call VROOTS()

        case (5)
            ! BCTYPE = 5: IDEAL PERFORATED/POROUS WALL
            F = 0.0
            RTKPOR = RTK / POR
            ! Set constants for doublet solution
            ALPHA0 = HALFPI - atan(-RTKPOR)
            ALPHA1 = ALPHA0
            ALPHA2 = ALPHA0
            ! Set constants for vortex solution
            BETA0 = atan(RTKPOR)
            BETA1 = BETA0
            BETA2 = BETA1

        case (6)
            ! BCTYPE = 6: GENERAL HOMOGENEOUS WALL BOUNDARY CONDITION
            ! Boundary condition is not operable yet in finite difference subroutines.
            ! Far field solution has been derived and is included here for future use
            RTKPOR = RTK / POR
            call DROOTS()
            call VROOTS()
            write(UNIT_OUTPUT, '(A)') '1ABNORMAL STOP IN SUBROUTINE FARFLD'
            write(UNIT_OUTPUT, '(A)') ' BCTYPE=6 IS NOT USEABLE'
            stop

        case default
            write(UNIT_OUTPUT, '(A,I0)') 'FARFLD: Invalid BCTYPE = ', BCTYPE
            stop
        
        end select

        ! Compute functional forms for upstream and downstream boundary conditions
        ! for doublet and vortex (for tunnel wall cases only - BCTYPE 2,3,4,5,6)
        if (BCTYPE /= 1) then

            XU_BC = (X(IMIN) - XSING) / (RTK * H)
            XD_BC = (X(IMAX) - XSING) / (RTK * H)

            ! Doublet terms
            COEF1 = 0.5 / AK / H
            ARG0 = ALPHA0
            ARG1 = PI - ALPHA1
            ARG2 = TWOPI - ALPHA2
            EXARG0 = exp(-ARG0 * XD_BC)
            EXARG1 = exp(ARG1 * XU_BC)
            EXARG2 = exp(ARG2 * XU_BC)

            do J = JMIN, JMAX
                YJ = Y(J) / H
                DDOWN(J) = COEF1 * (B_COEF + OMEGA0 * cos(YJ * ARG0) * EXARG0)
                DUP(J) = -COEF1 * ((1.0 - B_COEF) * OMEGA1 * cos(YJ * ARG1) * EXARG1 + &
                                    OMEGA2 * cos(YJ * ARG2) * EXARG2)
            end do

            ! Vortex terms
            ARG0 = BETA0
            ARG1 = PI + BETA1
            ARG2 = PI - BETA2
            EXARG0 = exp(-ARG0 * XD_BC)
            EXARG1 = exp(-ARG1 * XD_BC)
            EXARG2 = exp(ARG2 * XU_BC)

            do J = JMIN, JMAX
                YJ = Y(J) / H
                TERM = YJ
                if (JET == 0.0) TERM = sin(YJ * ARG0) / ARG0
                VDOWN(J) = -0.5 * (1.0 - sign(1.0, YJ) + (1.0 - JET) * PSI0 * TERM * EXARG0 + &
                                PSI1 * sin(YJ * ARG1) * EXARG1 / ARG1)
                TERM = 0.0
                if (JET /= 0.0) TERM = JET * YJ / (1.0 + F)
                VUP(J) = -0.5 * (1.0 - TERM - PSI2 * sin(YJ * ARG2) * EXARG2 / ARG2)
            end do
        end if

    end subroutine FARFLD

    ! Computes local similarity parameter or local Mach number
    ! Called by - VWEDGE, PRINT_SHOCK, OUTPUT_FIELD
    function EMACH1(U, DELTA) result(result_emach)
        use common_data, only: AK, GAM1, PHYS, EMACH, UNIT_OUTPUT, SIMDEF
        implicit none
        real, intent(in) :: U         ! Local velocity
        real, intent(in) :: DELTA     ! Maximum thickness of airfoil
        real :: result_emach
        real :: AK1, ARG, DELRT2
        
        ! Compute similarity parameter based on local velocity
        AK1 = AK - GAM1*U
        
        if (.not. PHYS) then
            ! Return value of local similarity parameter
            result_emach = AK1
            
        else
            ! Compute value of local Mach number and return
            DELRT2 = DELTA**(2.0/3.0)
        
            if (SIMDEF == 1) then ! Cole scaling
                ARG = 1.0 - DELRT2*AK1
            else if (SIMDEF == 2) then ! Spreiter scaling
                ARG = 1.0 - DELRT2*AK1*EMACH**(4.0/3.0)
            else if (SIMDEF == 3) then ! Krupp scaling
                ARG = 1.0 - DELRT2*AK1*EMACH
            else
                write(UNIT_OUTPUT, '(A, /, A, I3)') '1ABNORMAL STOP IN SUBROUTINE EMACH1', ' SIMDEF not supported', SIMDEF
                stop
            end if
        
            result_emach = 0.0
            if (ARG > 0.0) result_emach = sqrt(ARG)
        
        end if

    end function EMACH1

    ! Computes Murman or Yoshihara viscous wedge and modifies slope conditions
    ! to account for jump in displacement thickness due to shock/boundary layer interaction
    subroutine VWEDGE(AM1, XSHK, THAMAX, ZETA, NVWPRT, NISHK)
        use common_data, only: X, ILE, ITE, JUP, JLOW, GAM1, XDIFF, DELTA, NWDGE, REYNLD, WCONST
        use solver_data, only: WSLP, SONVEL
        use solver_base, only: PX, FINDSK
        implicit none

        real , intent(out) :: AM1(2,3)      ! Mach numbers upstream of shocks
        real , intent(out) :: XSHK(2,3)     ! Shock x-locations
        real , intent(out) :: THAMAX(2,3)   ! Maximum wedge angles
        real , intent(out) :: ZETA(2,3)     ! Wedge length scales
        integer , intent(out) :: NVWPRT(2)  ! Number of shocks on upper and lower surfaces
        integer , intent(out) :: NISHK      ! Number of shocks

        integer :: I, J, N, M, ISK, ISK3, ISK1, ISTART, JMP
        real :: SIGN, U, V1, AM1SQ, REYX, CF, DSTAR1, DXS, AETA, XEND

        ! intialization
        AM1 = 0.0
        XSHK = 0.0
        THAMAX = 0.0
        ZETA = 0.0
        NVWPRT = 0
        NISHK = 0

        ! Zero out previous wedge slopes
        do J = 1, 2
            do I = ILE, ITE
                WSLP(I,J) = 0.0
            end do
        end do
        
        SIGN = 1.0
        N = 1
        ISTART = ILE
        JMP = 0
        
        ! Locate shock on upper surface and compute wedge if shock exists
        M = 1
        
        do while (M <= 2)

            call FINDSK(ISTART, ITE, merge(JUP, JLOW, M==1), SONVEL, ISK)

            if (ISK < 0) then
                if (M == 1) then
                    ! Move to lower surface
                    N = 1
                    ISTART = ILE
                    SIGN = -SIGN
                    M = 2
                    cycle
                else
                    exit  ! No more shocks
                end if
            end if
            
            NISHK = NISHK + 1
            NVWPRT(M) = NVWPRT(M) + 1
            
            ! Compute X position of shock by interpolation
            V1 = PX(ISK-1, merge(JUP, JLOW, M==1))
            XSHK(M,N) = X(ISK-1) + (SONVEL - V1) / ((PX(ISK, merge(JUP, JLOW, M==1)) - V1) * XDIFF(ISK))
            
            ! Compute flow properties 3 points upstream
            ISK3 = ISK - 3
            U = PX(ISK3, merge(JUP, JLOW, M==1))
            AM1(M,N) = EMACH1(U, DELTA)
            AM1SQ = AM1(M,N) * AM1(M,N)
            
            if (AM1SQ <= 1.0) then
                JMP = 1

            else
                THAMAX(M,N) = WANGLE(AM1SQ, NWDGE, GAM1) * SIGN
                
                ! NWDGE = 1, Murman wedge
                if (NWDGE == 1) then
                    ! Murman wedge
                    REYX = REYNLD * XSHK(M,N)
                    CF = 0.02666 / (REYX**0.139)
                    DSTAR1 = 0.01738 * REYX**0.861 / REYNLD
                    
                    if (N > 1 .and. JMP == 0) then
                        DXS = XSHK(M,N) - XSHK(M,N-1)
                        if (DXS < ZETA(M,N-1)) then
                            AETA = DXS / ZETA(M,N-1)
                            DSTAR1 = DXS * THAMAX(M,N-1) * (1.0 + AETA * (AETA/3.0 - 1.0))
                        else
                            DSTAR1 = ZETA(M,N-1) * THAMAX(M,N-1) / 3.0
                        end if
                    end if
                    
                    JMP = 0
                    ZETA(M,N) = WCONST * sqrt((AM1SQ - 1.0) / CF) * DSTAR1
                    
                    ! Compute wedge slopes
                    XEND = XSHK(M,N) + ZETA(M,N)
                    do I = ISK, ITE
                        if (X(I) >= XEND) exit
                        AETA = (X(I) - XSHK(M,N)) / ZETA(M,N)
                        WSLP(I,M) = THAMAX(M,N) * (1.0 - AETA)**2 / DELTA
                    end do

                ! NWDGE = 2, Yoshihara wedge
                else if (NWDGE == 2) then
                    ! Yoshihara wedge
                    ISK1 = ISK - 1
                    do I = ISK1, ISK
                        WSLP(I,M) = THAMAX(M,N) / DELTA
                    end do
                
                end if
                
            end if
            
            ! Check for additional shock on surface
            N = N + 1
            if (N >= 4) then
                if (M == 1) then
                    ! Move to lower surface
                    N = 1
                    ISTART = ILE
                    SIGN = -SIGN
                    M = 2
                else
                    exit
                end if
            else
                ISTART = ISK + 2
            end if
        end do

    end subroutine VWEDGE

    ! Compute wedge angle for viscous correction
    function WANGLE(AM2, NW, G) result(wedge_angle)
        implicit none
        real, intent(in) :: AM2, G
        integer, intent(in) :: NW
        real :: wedge_angle ! Wedge angle
        real :: AM3, AM4, AM7, RM, RS, S2TM, S2TS, TM, TS, TTM, TTS, TDM, TDS
        
        if (NW == 1) then
            ! Murman wedge
            wedge_angle = 4.0 * ((AM2 - 1.0) / 3.0)**1.5 / G
        else
            ! Yoshihara wedge
            AM3 = 3.0 * AM2
            AM4 = 4.0 * AM2
            AM7 = 7.0 * AM2
            RM = sqrt(3.0 * (AM3 * AM2 + AM4 + 20.0))
            RS = sqrt(3.0 * (AM3 * AM2 - AM4 + 13.0))
            S2TM = (AM3 - 5.0 + RM) / AM7
            S2TS = (AM3 - 2.0 + RS) / AM7
            TM = asin(sqrt(S2TM))
            TS = asin(sqrt(S2TS))
            TTM = tan(TM)
            TTS = tan(TS)
            TDM = 5.0 * (AM2 * S2TM - 1.0) / (TTM * (5.0 + AM2 * (6.0 - 5.0 * S2TM)))
            TDS = 5.0 * (AM2 * S2TS - 1.0) / (TTS * (5.0 + AM2 * (6.0 - 5.0 * S2TS)))
            wedge_angle = 0.5 * (atan(TDM) + atan(TDS))
        end if
    end function WANGLE

    ! Compute constants ALPHA0, ALPHA1, ALPHA2, OMEGA0, OMEGA1, OMEGA2
    ! Used in formula for doublet in slotted wind tunnel with subsonic freestream
    subroutine DROOTS
        use common_data, only: HALFPI, PI, TWOPI, report_convergence_error, F
        use solver_data, only: RTKPOR
        use solver_data, only: ALPHA0, ALPHA1, ALPHA2, OMEGA0, OMEGA1, OMEGA2
        implicit none
        real :: ERROR_LOCAL, TEMP, Q, DALPHA
        integer :: I
        logical :: converged
        integer :: MAX_ITERATIONS = 100
        
        ERROR_LOCAL = 0.00001
        
        ! Compute ALPHA0
        ALPHA0 = 0.0
        converged = .false.
        do I = 1, MAX_ITERATIONS
            TEMP = ALPHA0
            Q = F*TEMP - RTKPOR
            ALPHA0 = HALFPI - atan(Q)
            DALPHA = abs(ALPHA0 - TEMP)
            if (DALPHA < ERROR_LOCAL) then
                converged = .true.
                exit
            end if
        end do
        if (.not. converged) then
            call report_convergence_error('DROOTS', 'ALPHA', 0)
        end if
        
        ! Compute ALPHA1
        ALPHA1 = 0.0
        converged = .false.
        do I = 1, MAX_ITERATIONS
            TEMP = ALPHA1
            Q = F*(TEMP - PI) - RTKPOR
            ALPHA1 = HALFPI - atan(Q)
            DALPHA = abs(ALPHA1 - TEMP)
            if (DALPHA < ERROR_LOCAL) then
                converged = .true.
                exit
            end if
        end do
        if (.not. converged) then
            call report_convergence_error('DROOTS', 'ALPHA', 1)
        end if
        
        ! Compute ALPHA2
        ALPHA2 = 0.0
        converged = .false.
        do I = 1, MAX_ITERATIONS
            TEMP = ALPHA2
            Q = F*(TEMP - TWOPI) - RTKPOR
            ALPHA2 = HALFPI - atan(Q)
            DALPHA = abs(ALPHA2 - TEMP)
            if (DALPHA < ERROR_LOCAL) then
                converged = .true.
                exit
            end if
        end do
        if (.not. converged) then
            call report_convergence_error('DROOTS', 'ALPHA', 2)
        end if
        
        ! Compute OMEGA0, OMEGA1, OMEGA2
        TEMP = 1.0 / tan(ALPHA0)
        OMEGA0 = 1.0 / (1.0 + F/(1.0 + TEMP*TEMP))
        TEMP = 1.0 / tan(ALPHA1)
        OMEGA1 = 1.0 / (1.0 + F/(1.0 + TEMP*TEMP))
        TEMP = 1.0 / tan(ALPHA2)
        OMEGA2 = 1.0 / (1.0 + F/(1.0 + TEMP*TEMP))
        
    end subroutine DROOTS

    ! Compute constants BETA0, BETA1, BETA2, PSI0, PSI1, PSI2
    ! Used in formula for vortex in slotted wind tunnel with subsonic freestream
    subroutine VROOTS
        use common_data, only: PI, report_convergence_error, F
        use solver_data, only: PSI0, PSI1, PSI2, RTKPOR
        use solver_data, only: BETA0, BETA1, BETA2
        implicit none
        real :: ERROR_LOCAL, TEMP, Q, DBETA
        integer :: I
        logical :: converged
        integer :: MAX_ITERATIONS = 100
        
        ERROR_LOCAL = 0.00001
        
        ! Calculate BETA0
        BETA0 = 0.0
        converged = .false.
        do I = 1, MAX_ITERATIONS
            TEMP = BETA0
            Q = -F*TEMP + RTKPOR
            BETA0 = atan(Q)
            DBETA = abs(TEMP - BETA0)
            if (DBETA < ERROR_LOCAL) then
                converged = .true.
                exit
            end if
        end do
        if (.not. converged) then
            call report_convergence_error('VROOTS', 'BETA', 0)
        end if
        
        ! Calculate BETA1  
        BETA1 = 0.0
        converged = .false.
        do I = 1, MAX_ITERATIONS
            TEMP = BETA1
            Q = -F*(TEMP + PI) + RTKPOR
            BETA1 = atan(Q)
            DBETA = abs(BETA1 - TEMP)
            if (DBETA < ERROR_LOCAL) then
                converged = .true.
                exit
            end if
        end do
        if (.not. converged) then
            call report_convergence_error('VROOTS', 'BETA', 1)
        end if
        
        ! Calculate BETA2
        BETA2 = 0.0
        converged = .false.
        do I = 1, MAX_ITERATIONS
            TEMP = BETA2
            Q = -F*(TEMP - PI) + RTKPOR
            BETA2 = atan(Q)
            DBETA = abs(BETA2 - TEMP)
            if (DBETA < ERROR_LOCAL) then
                converged = .true.
                exit
            end if
        end do
        if (.not. converged) then
            call report_convergence_error('VROOTS', 'BETA', 2)
        end if
        
        ! Compute PSI0, PSI1, PSI2
        TEMP = tan(BETA0)
        PSI0 = 1.0 / (1.0 + F/(1.0 + TEMP*TEMP))
        TEMP = tan(BETA1)
        PSI1 = 1.0 / (1.0 + F/(1.0 + TEMP*TEMP))
        TEMP = tan(BETA2)
        PSI2 = 1.0 / (1.0 + F/(1.0 + TEMP*TEMP))
        
    end subroutine VROOTS

end module solver_functions
