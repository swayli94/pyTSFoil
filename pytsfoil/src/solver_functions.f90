! solver_functions.f90
! Key functions for the solver

module solver_functions
    implicit none
    private

    public :: SETBC, BCEND, EMACH1, VWEDGE
    
contains


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
        integer :: I=0, IF1=0, N=0, NFOIL=0, INT=0, JINT=0

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
        use common_data, only: X, Y, IUP, IDOWN, JMIN, JMAX, JTOP, JBOT, AK, XDIFF, BCTYPE, UNIT_OUTPUT, POR, FLAG_OUTPUT
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
            if (FLAG_OUTPUT == 1) then
                write(UNIT_OUTPUT, '(A, /, A)') '1ABNORMAL STOP IN SUBROUTINE BCEND', &
                                            'BCTYPE=6 IS NOT USEABLE'
            end if
            stop
                
        case default
            if (FLAG_OUTPUT == 1) then
                write(UNIT_OUTPUT, *) 'ERROR: Invalid BCTYPE = ', BCTYPE
            end if
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

    ! Computes local similarity parameter or local Mach number
    ! Called by - VWEDGE, PRINT_SHOCK, OUTPUT_FIELD
    function EMACH1(U, DELTA) result(result_emach)
        use common_data, only: AK, GAM1, PHYS, EMACH, UNIT_OUTPUT, SIMDEF, FLAG_OUTPUT
        implicit none
        real, intent(in) :: U         ! Local velocity
        real, intent(in) :: DELTA     ! Maximum thickness of airfoil
        real :: result_emach
        real :: AK1=0.0, ARG=0.0, DELRT2=0.0
        
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
                if (FLAG_OUTPUT == 1) then
                    write(UNIT_OUTPUT, '(A, /, A, I3)') '1ABNORMAL STOP IN SUBROUTINE EMACH1', ' SIMDEF not supported', SIMDEF
                end if
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

        integer :: I=0, J=0, N=0, M=0, ISK=0, ISK3=0, ISK1=0, ISTART=0, JMP=0
        real :: SIGN=0.0, U=0.0, V1=0.0, AM1SQ=0.0, REYX=0.0, CF=0.0, DSTAR1=0.0, DXS=0.0, AETA=0.0, XEND=0.0

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
        real :: AM3=0.0, AM4=0.0, AM7=0.0, RM=0.0, RS=0.0
        real :: S2TM=0.0, S2TS=0.0, TM=0.0, TS=0.0, TTM=0.0, TTS=0.0, TDM=0.0, TDS=0.0
        
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

end module solver_functions
