! solver_base.f90
! Base functions (that are not affected by methods of scaling, correction, etc.):
!   - Finite difference functions
!   - Post-processing functions

module solver_base
    implicit none
    private

    ! Public functions
    public :: PX, PY, DIFCOE, ANGLE
    public :: LIFT, PITCH, FINDSK, CDCOLE

contains

    ! Computes U = DP/DX at point I,J
    function PX(I, J) result(result_px)
        use common_data, only: IMIN, IMAX, XDIFF
        use solver_data, only: P
        implicit none
        integer, intent(in) :: I, J
        real :: result_px
        real :: PJI=0.0
        
        ! Test to locate end points
        if (I == IMIN) then
            ! Upstream boundary
            result_px = 1.5*XDIFF(I+1)*(P(J,I+1)-P(J,I)) - &
                        0.5*XDIFF(I+2)*(P(J,I+2)-P(J,I+1))
        else if (I == IMAX) then
            ! Downstream boundary  
            result_px = 1.5*XDIFF(I)*(P(J,I)-P(J,I-1)) - &
                        0.5*XDIFF(I-1)*(P(J,I-1)-P(J,I-2))
        else
            ! Interior mesh point
            PJI = P(J,I)
            result_px = 0.5*(XDIFF(I+1)*(P(J,I+1)-PJI) + XDIFF(I)*(PJI-P(J,I-1)))
        end if

    end function PX
      
    ! Computes V = DP/DY at point I,J
    function PY(I, J) result(result_py)
        use common_data, only: JMIN, JMAX, JUP, JLOW, ILE, ITE
        use common_data, only: YDIFF, ALPHA, FXU, FXL
        use solver_data, only: P, PJUMP
        implicit none
        integer, intent(in) :: I, J
        real :: result_py
        real :: PJI=0.0, VMINUS=0.0, VPLUS=0.0
        integer :: IC=0
        
        ! Test for end points or points near airfoil slit
        if (J == JMIN) then
            ! I,J is on lower boundary. Use one sided derivative
            result_py = 1.5*YDIFF(J+1)*(P(J+1,I) - P(J,I)) - &
                        0.5*YDIFF(J+2)*(P(J+2,I) - P(J+1,I))
            return

        else if (J == JLOW) then
            ! I,J is on row of mesh points below airfoil
            VMINUS = YDIFF(J)*(P(J,I) - P(J-1,I))
            
            ! Test to see if I,J is ahead, under, or behind slit
            if (I < ILE) then
                ! I,J is ahead of airfoil
                result_py = 0.5*((P(JUP,I) - P(JLOW,I)) * YDIFF(JUP) + VMINUS)
            else if (I > ITE) then
                ! I,J is behind airfoil
                result_py = 0.5*((P(JUP,I) - PJUMP(I) - P(JLOW,I)) * YDIFF(JUP) + VMINUS)
            else
                ! I,J is under airfoil. Use derivative boundary condition
                IC = I - ILE + 1
                result_py = 0.5 * (FXL(IC) - ALPHA + VMINUS)
            end if
            return

        else if (J == JUP) then
            ! I,J is on row of mesh points above airfoil
            VPLUS = YDIFF(J+1)*(P(J+1,I) - P(J,I))
            
            ! Test to see if I is ahead of, over, or behind airfoil slit
            if (I < ILE) then
                ! I,J is ahead of airfoil
                result_py = 0.5*((P(JUP,I) - P(JLOW,I)) * YDIFF(JUP) + VPLUS)
            else if (I > ITE) then
                ! I,J is behind airfoil
                result_py = 0.5*((P(JUP,I) - PJUMP(I) - P(JLOW,I)) * YDIFF(JUP) + VPLUS)
            else
                IC = I - ILE + 1
                result_py = 0.5 * (VPLUS + FXU(IC) - ALPHA)
            end if
            return
            
        else if (J == JMAX) then
            ! I,J is on top row of mesh points. Use one sided formula
            result_py = 1.5*YDIFF(J)*(P(J,I) - P(J-1,I)) - &
                        0.5*YDIFF(J-1)*(P(J-1,I) - P(J-2,I))
            return

        else
            ! I,J is an interior point
            PJI = P(J,I)
            result_py = 0.5*(YDIFF(J+1)*(P(J+1,I)-PJI) + YDIFF(J)*(PJI-P(J-1,I)))

        end if

    end function PY
    
    ! Compute finite-difference coefficients in x and y directions
    subroutine DIFCOE()
        use common_data, only: IMIN, IMAX, JMIN, JMAX, X, Y, GAM1, AK
        use common_data, only: XDIFF, YDIFF
        use common_data, only: JLOW, JUP
        use solver_data, only: CJUP, CJUP1, CJLOW, CJLOW1
        use solver_data, only: CXXL, CXXR, CXXC, CXL, CXR, CXC
        use solver_data, only: CYYD, CYYU, CYYC, CYYBUD, CYYBUC, CYYBUU, CYYBLC, CYYBLD, CYYBLU
        use solver_data, only: C1
        implicit none
        integer :: I=0, J=0, ISTART=0, IEND=0, JSTART=0, JEND=0
        real :: DXL=0.0, DXR=0.0, DXC=0.0, DYD=0.0, DYU=0.0, DYC=0.0, DX=0.0, DYU_MIN=0.0, C2=0.0, Q=0.0

        ! Coefficients for (P)X and (P)XX at IMIN
        CXXL(IMIN) = 0.0
        CXXR(IMIN) = 0.0
        CXXC(IMIN) = 0.0
        CXL(IMIN) = 0.0
        CXR(IMIN) = 0.0
        CXC(IMIN) = 0.0

        ! Coefficients for (P)X and (P)XX from I=IMIN+1 to I=IMAX-1
        C2 = GAM1 * 0.5
        ISTART = IMIN + 1
        IEND = IMAX - 1
        do I = ISTART, IEND
            DXL = X(I) - X(I-1)
            DXR = X(I+1) - X(I)
            DXC = 0.5 * (X(I+1) - X(I-1))
            
            ! For VC
            C1(I) = AK / DXC
            
            ! For (P)X
            CXL(I) = -C2 / (DXL * DXC)
            CXR(I) = C2 / (DXR * DXC)
            CXC(I) = -CXL(I) - CXR(I)
            
            ! For (P)XX
            CXXL(I) = 1.0 / DXL
            CXXR(I) = 1.0 / DXR
            CXXC(I) = CXXL(I) + CXXR(I)
        end do

        ! Coefficients for (P)X and (P)XX at IMAX
        DX = X(IMAX) - X(IMAX-1)
        Q = 1.0 / (DX * DX)
        C1(IMAX) = AK / DX
        CXL(IMAX) = -C2 * Q
        CXR(IMAX) = C2 * Q
        CXC(IMAX) = 0.0
        CXXL(IMAX) = 1.0 / DX
        CXXR(IMAX) = 1.0 / DX
        CXXC(IMAX) = CXXL(IMAX) + CXXR(IMAX)

        ! Coefficients for (P)YY at JMIN
        DYU_MIN = Y(JMIN+1) - Y(JMIN)
        CYYD(JMIN) = 2.0 / DYU_MIN
        CYYU(JMIN) = 2.0 / (DYU_MIN * DYU_MIN)
        CYYC(JMIN) = CYYU(JMIN)

        ! Coefficients for (P)YY from J=JMIN+1 to J=JMAX-1
        JSTART = JMIN + 1
        JEND = JMAX - 1
        do J = JSTART, JEND
            DYD = Y(J) - Y(J-1)
            DYU = Y(J+1) - Y(J)
            DYC = Y(J+1) - Y(J-1)
            CYYD(J) = 2.0 / (DYD * DYC)
            CYYU(J) = 2.0 / (DYU * DYC)
            CYYC(J) = CYYD(J) + CYYU(J)
        end do

        ! Coefficients for (P)YY at JMAX
        DYD = Y(JMAX) - Y(JMAX-1)
        CYYD(JMAX) = 2.0 / (DYD * DYD)
        CYYU(JMAX) = 2.0 / DYD
        CYYC(JMAX) = CYYD(JMAX)

        ! Coefficients for velocity formulas
        ISTART = IMIN + 1
        do I = ISTART, IMAX
            XDIFF(I) = 1.0 / (X(I) - X(I-1))
        end do
        
        JSTART = JMIN + 1
        do J = JSTART, JMAX
            YDIFF(J) = 1.0 / (Y(J) - Y(J-1))
        end do

        ! Coefficients for extrapolation formulas for airfoil surface properties
        CJLOW = -Y(JLOW-1) / (Y(JLOW) - Y(JLOW-1))
        CJLOW1 = -Y(JLOW) / (Y(JLOW) - Y(JLOW-1))
        CJUP = Y(JUP+1) / (Y(JUP+1) - Y(JUP))
        CJUP1 = Y(JUP) / (Y(JUP+1) - Y(JUP))

        ! Special difference coefficients for PYY for airfoil boundary condition
        ! Upper surface
        CYYBUD = -2.0 / (Y(JUP+1) + Y(JUP))
        CYYBUC = -CYYBUD / (Y(JUP+1) - Y(JUP))
        CYYBUU = CYYBUC
        
        ! Lower surface
        CYYBLU = -2.0 / (Y(JLOW) + Y(JLOW-1))
        CYYBLC = CYYBLU / (Y(JLOW) - Y(JLOW-1))
        CYYBLD = CYYBLC

    end subroutine DIFCOE

    ! Compute the angle THETA at each mesh point
    subroutine ANGLE()
        use common_data, only: IMIN, IMAX, JMIN, JMAX, X, Y
        use common_data, only: PI, TWOPI, AK
        use solver_data, only: THETA, XSING
        implicit none
        integer :: I, J
        real :: XX=0.0, YY=0.0, R=0.0, ATN=0.0, Q=0.0, R2PI=0.0
        real :: RTK=0.0
        
        R2PI = 1.0 / TWOPI
        RTK = sqrt(abs(AK))
        
        do I = IMIN, IMAX
            XX = X(I) - XSING
            do J = JMIN, JMAX
                YY = Y(J) * RTK
                R = sqrt(Y(J)**2 + XX*XX)
                ATN = atan2(YY, XX)
                Q = PI - sign(PI, YY)
                THETA(J,I) = -(ATN + Q) * R2PI
                if (R <= 1.0) THETA(J,I) = THETA(J,I) * R
            end do
        end do

    end subroutine ANGLE

    ! Computes pressure drag coefficient by integrating
    ! U*V around airfoil using trapezoidal rule.
    function DRAG(CDFACT_in) result(result_drag)
        use common_data, only: X, ILE, ITE, JUP, JLOW
        use common_data, only: FXU, FXL, N_MESH_POINTS
        use solver_data, only: CJUP, CJUP1, CJLOW, CJLOW1
        use math_module, only: TRAP
        implicit none
        real, intent(in) :: CDFACT_in
        real :: result_drag
        real :: PXUP=0.0, PXLOW=0.0, SUM=0.0
        real :: XI(N_MESH_POINTS)=0.0, ARG(N_MESH_POINTS)=0.0
        integer :: K=0, I=0
        
        K = 1
        ARG(1) = 0.0
        XI(1) = X(ILE-1)
        do I = ILE, ITE
            K = K + 1
            PXUP = CJUP*PX(I,JUP) - CJUP1*PX(I,JUP+1)
            PXLOW = CJLOW*PX(I,JLOW) - CJLOW1*PX(I,JLOW-1)
            ARG(K) = FXU(K-1)*PXUP - FXL(K-1)*PXLOW
            XI(K) = X(I)
        end do

        K = K + 1
        ARG(K) = 0.0
        XI(K) = X(ITE+1)
        call TRAP(XI, ARG, K, SUM)
        result_drag = -SUM*CDFACT_in*2.0

    end function DRAG
    
    ! Computes lift coefficient from jump in P at trailing edge
    function LIFT(CLFACT_in) result(result_lift)
        use common_data, only: JUP, ITE, JLOW
        use solver_data, only: CJUP, CJUP1, CJLOW, CJLOW1, P
        implicit none
        real, intent(in) :: CLFACT_in
        real :: result_lift
        real :: PTOP=0.0, PBOT=0.0
        
        PTOP = CJUP*P(JUP,ITE) - CJUP1*P(JUP+1,ITE)
        PBOT = CJLOW*P(JLOW,ITE) - CJLOW1*P(JLOW-1,ITE)
        result_lift = 2.0*CLFACT_in*(PTOP-PBOT)

    end function LIFT
      
    ! Computes airfoil pitching moment about X = XM, Y = 0
    function PITCH(CMFACT_in) result(result_pitch)
        use common_data, only: X, ILE, ITE, JUP, JLOW, N_MESH_POINTS
        use solver_data, only: CJUP, CJUP1, CJLOW, CJLOW1, P
        use math_module, only: TRAP
        implicit none
        real, intent(in) :: CMFACT_in
        real :: result_pitch
        real :: XM=0.0, PTOP=0.0, PBOT=0.0, SUM=0.0
        real :: XI(N_MESH_POINTS)=0.0, ARG(N_MESH_POINTS)=0.0
        integer :: K=0, I_loop=0
        
        ! Set XM to quarter chord
        XM = 0.25
        K = 0
        do I_loop = ILE, ITE
            K = K + 1
            PTOP = CJUP*P(JUP,I_loop) - CJUP1*P(JUP+1,I_loop)
            PBOT = CJLOW*P(JLOW,I_loop) - CJLOW1*P(JLOW-1,I_loop)
            ARG(K) = PTOP - PBOT
            XI(K) = X(I_loop)
        end do

        call TRAP(XI, ARG, K, SUM)
        result_pitch = CMFACT_in*((1.0-XM)*ARG(K) - SUM) * (-2.0)

    end function PITCH
      
    ! Compute drag coefficient by momentum integral method
    ! Integrates around a contour enclosing the body and along all shocks inside the contour
    ! Called by - PRINT
    subroutine CDCOLE(SONVEL, YFACT, DELTA)
        use common_data, only: X, Y, IMIN, IMAX, IUP, ILE, ITE, N_MESH_POINTS
        use common_data, only: JMIN, JMAX, JUP, JLOW
        use common_data, only: AK, GAM1
        use common_data, only: FXL, FXU
        use common_data, only: UNIT_OUTPUT, UNIT_SUMMARY
        use solver_data, only: CJUP, CJUP1, CJLOW, CJLOW1
        use solver_data, only: CDFACT
        use math_module, only: TRAP
        implicit none
    
        real, intent(in) :: SONVEL  ! Speed of sound
        real, intent(in) :: YFACT   ! Scaling factor for Y-coordinate
        real, intent(in) :: DELTA   ! Maximum thickness of airfoil
        
        ! Local variables
        integer :: IU=0, ID=0, JT=0, JB=0, ISTOP=0, IBOW=0, ISK=0, JSTART=0, J=0, JJ=0, JSK=0, ISKOLD=0
        integer :: ILIM=0, IB=0, I=0, L=0, NSHOCK=0, LPRT1=0, LPRT2=0, ISTART=0
        real :: GAM123=0.0, U=0.0, V=0.0, UU=0.0, UL=0.0, SUM=0.0, CDSK=0.0, CDWAVE=0.0, CDC=0.0, CD=0.0
        real :: CDUP=0.0, CDTOP=0.0, CDBOT=0.0, CDDOWN=0.0, CDBODY=0.0
        real :: XU_LOC=0.0, XD_LOC=0.0, YT_LOC=0.0, YB_LOC=0.0, ULE=0.0
        real :: XI(N_MESH_POINTS)=0.0, ARG(N_MESH_POINTS)=0.0
        
        GAM123 = GAM1 * 2.0 / 3.0
        ISKOLD = 0
        
        ! Set locations of contour boundaries
        
        ! Upstream boundary
        ! If AK = 0.0 CDCOLE will not be called. AMACH may not be = 1.0
        if (AK > 0.0) then
            IU = (ILE + IMIN) / 2
        else
            IU = IUP
        end if
        
        ! Top and bottom boundaries
        ! Subsonic freestream
        ! Set JB,JT to include as much of shocks as possible
        JT = JMAX - 1
        JB = JMIN + 1
        
        if (AK <= 0.0) then
            ! Supersonic freestream
            ! Set JB,JT to include only subsonic part of detached bow wave
            
            ! Find bow shock wave
            ISTOP = ILE - 3
            call FINDSK(IUP, ISTOP, JUP, SONVEL, IBOW)
            if (IBOW < 0) then
                ! Shock is too close to body to do contour integral.
                ! Write message and return
                ULE = PX(ILE, JUP)
                
                if (ULE > SONVEL) then
                    write(UNIT_OUTPUT, '("31H1SHOCK WAVE IS ATTACHED TO BODY/", &
                        & "33H MOMENTUM INTEGRAL CANNOT BE DONE/", &
                        & "45H DRAG OBTAINED FROM SURFACE PRESSURE INTEGRAL/")')
                else
                    write(UNIT_OUTPUT, '("41H1DETACHED SHOCK WAVE IS TOO CLOSE TO BODY/", &
                        & "33H MOMENTUM INTEGRAL CANNOT BE DONE/", &
                        & "45H DRAG OBTAINED FROM SURFACE PRESSURE INTEGRAL/")')
                end if
                
                CD = DRAG(CDFACT)
                write(UNIT_OUTPUT, '("4H0CD=", F12.6)') CD
                return
            end if
            
            ! Search up shock to find tip of subsonic region
            ISK = IBOW
            JSTART = JUP + 1
            JT = JUP - 1
            do J = JSTART, JMAX
                JT = JT + 1
                ISKOLD = ISK
                call NEWISK(ISKOLD, J, SONVEL, ISK)
                if (ISK < 0) exit
            end do
            
            ! Search down shock to find tip of subsonic region
            ISK = IBOW
            JB = JLOW + 2
            do J = JMIN, JLOW
                JJ = JLOW - J + JMIN
                JB = JB - 1
                ISKOLD = ISK
                call NEWISK(ISKOLD, JJ, SONVEL, ISK)
                if (ISK < 0) exit
            end do
            
            ! Save I location of bow shock wave on lower boundary
            IBOW = ISKOLD
        end if
        
        ! Downstream boundary
        ID = (ITE + IMAX) / 2
        if (PX(ITE+1, JUP) >= SONVEL) then
            ! Trailing edge is supersonic. Place downstream
            ! boundary ahead of trailing edge to avoid tail shock
            I = ITE
            do while (X(I) > 0.75)
                I = I - 1
            end do
            ID = I
        end if
        
        ! All boundaries are fixed
        ! Compute integrals along boundaries
        ! Integral on upstream boundary
        CDUP = 0.0
        if (AK >= 0.0) then
            L = 0
            do J = JB, JT
                L = L + 1
                XI(L) = Y(J)
                U = PX(IU, J)
                V = PY(IU, J)
                ARG(L) = ((AK - GAM123*U)*U*U - V*V) * 0.5
            end do
            call TRAP(XI, ARG, L, SUM)
            CDUP = 2.0 * CDFACT * SUM
        end if
        
        ! Integral on top boundary
        L = 0
        do I = IU, ID
            L = L + 1
            XI(L) = X(I)
            ARG(L) = -PX(I, JT) * PY(I, JT)
        end do
        call TRAP(XI, ARG, L, SUM)
        CDTOP = 2.0 * CDFACT * SUM
        
        ! Integral on bottom boundary
        L = 0
        do I = IU, ID
            L = L + 1
            ARG(L) = PX(I, JB) * PY(I, JB)
        end do
        call TRAP(XI, ARG, L, SUM)
        CDBOT = 2.0 * CDFACT * SUM
        
        ! Integral on downstream boundary
        L = 0
        do J = JB, JT
            L = L + 1
            XI(L) = Y(J)
            U = PX(ID, J)
            ! If flow supersonic, use backward difference formula
            if (U > SONVEL) U = PX(ID-1, J)
            V = PY(ID, J)
            ARG(L) = ((GAM123*U - AK)*U*U + V*V) * 0.5
        end do
    
        call TRAP(XI, ARG, L, SUM)
        CDDOWN = 2.0 * CDFACT * SUM
          
        ! Integral on body boundary
        CDBODY = 0.0
        if (ID <= ITE) then
            ILIM = ITE + 1
            L = 0
            do I = ID, ILIM
                IB = I - ILE + 1
                L = L + 1
                XI(L) = X(I)
                UU = CJUP*PX(I, JUP) - CJUP1*PX(I, JUP+1)
                UL = CJLOW*PX(I, JLOW) - CJLOW1*PX(I, JLOW-1)
                ARG(L) = -UU*FXU(IB) + UL*FXL(IB)
            end do
            call TRAP(XI, ARG, L, SUM)
            CDBODY = 2.0 * CDFACT * SUM
        end if
          
        ! Integration along shock waves
        CDWAVE = 0.0
        LPRT1 = 0
        LPRT2 = 0
        NSHOCK = 0
        
        if (AK <= 0.0) then
            ! Integrate along detached bow wave
            NSHOCK = NSHOCK + 1
            LPRT1 = 1
            LPRT2 = 1
            L = 0
            ISK = IBOW
            do J = JB, JT
                L = L + 1
                ISKOLD = ISK
                call NEWISK(ISKOLD, J, SONVEL, ISK)
                XI(L) = Y(J)
                ARG(L) = (PX(ISK+1, J) - PX(ISK-2, J))**3
            end do
            call TRAP(XI, ARG, L, SUM)
            CDSK = -GAM1/6.0 * CDFACT * SUM
            CDWAVE = CDWAVE + CDSK
            call PRTSK(XI, ARG, L, NSHOCK, CDSK, LPRT1, YFACT, DELTA)
        end if
          
        ! Integrate along shocks above airfoil
        ISTART = ILE
        
        ! Loop to find and process all shocks above airfoil
        do
            call FINDSK(ISTART, ITE, JUP, SONVEL, ISK)
            if (ISK < 0) exit  ! No more shocks found
            
            ! Shock wave found
            ISTART = ISK + 1
            NSHOCK = NSHOCK + 1
            LPRT1 = 0
            L = 1
            XI(L) = 0.0
            ARG(L) = (CJUP*(PX(ISK+1, JUP) - PX(ISK-2, JUP)) - &
                        CJUP1*(PX(ISK+1, JUP+1) - PX(ISK-2, JUP+1)))**3
            
            do J = JUP, JT
                L = L + 1
                XI(L) = Y(J)
                ARG(L) = (PX(ISK+1, J) - PX(ISK-2, J))**3
                ISKOLD = ISK
                JSK = J + 1
                call NEWISK(ISKOLD, JSK, SONVEL, ISK)
                if (ISK < 0) exit
                if (ISK > ID) then
                LPRT1 = 1
                exit
                end if
            end do
            
            if (ISK < 0) LPRT1 = 1
            
            call TRAP(XI, ARG, L, SUM)
            CDSK = -GAM1/6.0 * CDFACT * SUM
            CDWAVE = CDWAVE + CDSK
            call PRTSK(XI, ARG, L, NSHOCK, CDSK, LPRT1, YFACT, DELTA)
            if (LPRT1 == 1) LPRT2 = 1
        end do
          
        ! Integrate along shocks below airfoil
        ISTART = ILE
        
        ! Loop to find and process all shocks below airfoil  
        do
            call FINDSK(ISTART, ITE, JLOW, SONVEL, ISK)
            if (ISK < 0) exit  ! No more shocks found
            
            ! Shock wave found
            ISTART = ISK + 1
            NSHOCK = NSHOCK + 1
            LPRT1 = 0
            L = 1
            XI(L) = 0.0
            ARG(L) = (CJLOW*(PX(ISK+1, JLOW) - PX(ISK-2, JLOW)) - &
                        CJLOW1*(PX(ISK+1, JLOW-1) - PX(ISK-2, JLOW-1)))**3
            
            do JJ = JB, JLOW
                J = JLOW + JB - JJ
                L = L + 1
                XI(L) = Y(J)
                ARG(L) = (PX(ISK+1, J) - PX(ISK-2, J))**3
                ISKOLD = ISK
                JSK = J - 1
                call NEWISK(ISKOLD, JSK, SONVEL, ISK)
                if (ISK < 0) exit
                if (ISK > ID) then
                LPRT1 = 1
                exit
                end if
            end do
            
            if (ISK < 0) LPRT1 = 1
            
            call TRAP(XI, ARG, L, SUM)
            CDSK = -GAM1/6.0 * (-SUM)
            CDWAVE = CDWAVE + CDSK
            call PRTSK(XI, ARG, L, NSHOCK, CDSK, LPRT1, YFACT, DELTA)
            if (LPRT1 == 1) LPRT2 = 1
        end do
        
        ! Integration along shocks is complete
        ! Printout CD information
        XU_LOC = X(IU)
        XD_LOC = X(ID)
        YT_LOC = Y(JT) * YFACT
        YB_LOC = Y(JB) * YFACT
        CDC = CDUP + CDTOP + CDBOT + CDDOWN + CDBODY
        CD = CDC + CDWAVE
        
        ! Write drag coefficient breakdown
        write(UNIT_OUTPUT, '(A)') '1'
        write(UNIT_OUTPUT, '(A)') ' CALCULATION OF DRAG COEFFICIENT BY MOMENTUM INTEGRAL METHOD'
        write(UNIT_OUTPUT, '(A)') ''
        write(UNIT_OUTPUT, '(A)') ' BOUNDARIES OF CONTOUR USED CONTRIBUTION TO CD'
        write(UNIT_OUTPUT, '(A,F12.6,A,F12.6)') ' UPSTREAM    X =', XU_LOC, '  CDUP   =', CDUP
        write(UNIT_OUTPUT, '(A,F12.6,A,F12.6)') ' DOWNSTREAM  X =', XD_LOC, '  CDDOWN =', CDDOWN  
        write(UNIT_OUTPUT, '(A,F12.6,A,F12.6)') ' TOP         Y =', YT_LOC, '  CDTOP  =', CDTOP
        write(UNIT_OUTPUT, '(A,F12.6,A,F12.6)') ' BOTTOM      Y =', YB_LOC, '  CDBOT  =', CDBOT
        write(UNIT_OUTPUT, '(A)') ''
        write(UNIT_OUTPUT, '(A,I3)')    'Number of shock inside contour, N =      ', NSHOCK
        write(UNIT_OUTPUT, '(A,F15.9)') 'Body aft location,              X =      ', XD_LOC
        write(UNIT_OUTPUT, '(A,F15.9)') 'Drag due to body,               CD_body =', CDBODY
        write(UNIT_OUTPUT, '(A,F15.9)') 'Drag due to shock,              CD_wave =', CDWAVE
        write(UNIT_OUTPUT, '(A,F15.9)') 'Drag by momentum integral,      CD_int = ', CDC
        write(UNIT_OUTPUT, '(A,F15.9)') 'Total drag (CD_int + CD_wave),  CD =     ', CD
        write(UNIT_OUTPUT, '(A)') ''
    
        if (NSHOCK > 0 .and. LPRT2 == 0) then
            write(UNIT_OUTPUT, '("NOTE - All shocks contained within contour, CD_wave equals total wave drag")')
        end if
        
        if (NSHOCK > 0 .and. LPRT2 == 1) then
            write(UNIT_OUTPUT, '("NOTE - One or more shocks extend outside of contour, CD_wave does not equal total wave drag")')
        end if
    
        write(UNIT_SUMMARY, '(A,I3)')    'Number of shock inside contour, N =      ', NSHOCK
        write(UNIT_SUMMARY, '(A,F15.9)') 'Body aft location,              X =      ', XD_LOC
        write(UNIT_SUMMARY, '(A,F15.9)') 'Drag due to body,               CD_body =', CDBODY
        write(UNIT_SUMMARY, '(A,F15.9)') 'Drag due to shock,              CD_wave =', CDWAVE
        write(UNIT_SUMMARY, '(A,F15.9)') 'Drag by momentum integral,      CD_int = ', CDC
        write(UNIT_SUMMARY, '(A,F15.9)') 'Total drag (CD_int + CD_wave),  CD =     ', CD
    
    end subroutine CDCOLE

    ! Finds shock location on line J between ISTART and IEND
    subroutine FINDSK(ISTART, IEND, J, SONVEL, ISK)
        implicit none
        integer, intent(in) :: ISTART, IEND, J
        real, intent(in) :: SONVEL
        integer, intent(out) :: ISK
        real :: U1=0.0, U2=0.0
        
        ISK = ISTART - 1
        U2 = PX(ISK, J)
        
        do
            ISK = ISK + 1
            U1 = U2
            U2 = PX(ISK, J)
            if (U1 > SONVEL .and. U2 <= SONVEL) exit
            if (ISK >= IEND) then
                ISK = -IEND
                exit
            end if
        end do
    end subroutine FINDSK
    
    ! Find new location of shockwave (ISKNEW) on line J
    ! given an initial guess for location (ISKOLD).
    ! Shock location is defined as location of shock point.
    ! If no shock is found, ISKNEW is set negative.
    ! Called by - CDCOLE.
    subroutine NEWISK(ISKOLD, J, SONVEL, ISKNEW)
        implicit none
        integer, intent(in) :: ISKOLD, J
        real, intent(in) :: SONVEL
        integer, intent(out) :: ISKNEW
        integer :: I2=0
        real :: U1=0.0, U2=0.0
        
        I2 = ISKOLD + 2
        ISKNEW = ISKOLD - 3
        U2 = PX(ISKNEW, J)
        
        do
            ISKNEW = ISKNEW + 1
            U1 = U2
            U2 = PX(ISKNEW, J)
            if (U1 > SONVEL .and. U2 <= SONVEL) exit
            if (ISKNEW >= I2) then
                ! No shock point found, tip of shock reached
                ISKNEW = -ISKNEW
                exit
            end if
        end do
    end subroutine NEWISK
    
    ! Print shock wave drag contributions and total pressure loss along shock wave
    ! Called by - CDCOLE
    subroutine PRTSK(XI,ARG,L,NSHOCK,CDSK,LPRT1,YFACT,DELTA)
        use common_data, only: GAM1, UNIT_OUTPUT, N_MESH_POINTS
        use solver_data, only: CDFACT
        implicit none
        real, intent(in) :: XI(N_MESH_POINTS), ARG(N_MESH_POINTS)
        integer, intent(in) :: L, NSHOCK, LPRT1
        real, intent(in) :: CDSK
        real, intent(in) :: YFACT
        real, intent(in) :: DELTA   ! Maximum thickness of airfoil
        real :: CDYCOF=0.0, POYCOF=0.0, YY=0.0, CDY=0.0, POY=0.0
        integer :: K=0
    
        CDYCOF = -CDFACT * GAM1 / (6.0 * YFACT)
        POYCOF = DELTA**2 * GAM1 * (GAM1 - 1.0) / 12.0
        
        ! Write header for first shock wave only (format 1001 equivalent)
        if (NSHOCK == 1) then
            write(UNIT_OUTPUT, '(A)') '0'
            write(UNIT_OUTPUT,'(A)') ' INVISCID WAKE PROFILES FOR INDIVIDUAL SHOCK WAVES WITHIN MOMENTUM CONTOUR'
        end if
        
        ! Write shock information (format 1002 equivalent)
        write(UNIT_OUTPUT,'(A)') ''  ! blank line for 0 carriage control
        write(UNIT_OUTPUT,'(A,I3)') 'SHOCK', NSHOCK
        write(UNIT_OUTPUT,'(A,F12.6)') ' WAVE DRAG FOR THIS SHOCK=', CDSK
        write(UNIT_OUTPUT,'(A,A,A,A,A)') '      Y', '         ', 'CD(Y)', '        ', 'PO/POINF'
        
        ! Write shock profile data (format 1003 equivalent)
        do K = 1, L
            YY = XI(K) * YFACT
            CDY = CDYCOF * ARG(K)
            POY = 1.0 + POYCOF * ARG(K)
            write(UNIT_OUTPUT,'(1X,3F12.8)') YY, CDY, POY
        end do
        
        ! Write footer if shock extends outside contour (format 1004 equivalent)
        if (LPRT1 == 1) then
            write(UNIT_OUTPUT,'(A)') ''  ! blank line for 0 carriage control
            write(UNIT_OUTPUT,'(A)') ' SHOCK WAVE EXTENDS OUTSIDE CONTOUR'
            write(UNIT_OUTPUT,'(A)') ' PRINTOUT OF SHOCK LOSSES ARE NOT AVAILABLE FOR REST OF SHOCK'
        end if
    end subroutine PRTSK
    
end module solver_base
