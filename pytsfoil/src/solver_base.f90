! solver_base.f90
! Base functions (that are not affected by methods of scaling, correction, etc.):
!   - Finite difference functions
!   - Post-processing functions

module solver_base
    implicit none
    private

    ! Public functions
    public :: TRAP, PX, PY, DIFCOE, ANGLE
    public :: LIFT, PITCH, FINDSK

contains

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
    
end module solver_base
