! solver_module.f90
! Module for finite-difference setup and boundary condition routines

module solver_module
  use common_data
  implicit none
  public :: DIFCOE, SETBC, BCEND, FARFLD

contains

  ! Compute finite-difference coefficients in x and y directions
  subroutine DIFCOE()
    use common_data, only: IMIN, IMAX, JMIN, JMAX, X, Y, GAM1, AK
    use common_data, only: CXC, CXL, CXR, CXXC, CXXL, CXXR, C1
    use common_data, only: CYYC, CYYD, CYYU, XDIFF, YDIFF
    use common_data, only: JLOW, JUP, CJUP, CJUP1, CJLOW, CJLOW1
    use common_data, only: CYYBUD, CYYBUC, CYYBUU, CYYBLU, CYYBLC, CYYBLD
    implicit none
    integer :: I, J, ISTART, IEND, JSTART, JEND
    real :: DXL, DXR, DXC, DYD, DYU, DYC, DX, DYU_MIN, C2, Q

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

  ! Define solution limits and apply body slope boundary conditions
  subroutine SETBC(IJUMP)
    use common_data, only: IMIN, IMAX, IUP, IDOWN, JMIN, JMAX, JTOP, JBOT
    use common_data, only: ILE, ITE, JUP, JLOW, FXLBC, FXUBC, FXL, FXU
    implicit none
    integer, intent(in) :: IJUMP
    integer :: I, IC

    ! Set solution domain limits
    if (IJUMP == 0) then
      IUP = IMIN + 1
      IDOWN = IMAX - 1
      JTOP = JMAX - 1
      JBOT = JMIN + 1
    end if

    ! Apply body slope boundary conditions
    IC = 0
    do I = ILE, ITE
      IC = IC + 1
      FXUBC(I) = FXU(IC)
      FXLBC(I) = FXL(IC)
    end do

  end subroutine SETBC

  ! Apply boundary conditions on each i-line (upper/lower boundaries)
  subroutine BCEND()
    use common_data, only: P, IMIN, IMAX, JMIN, JMAX, JTOP, JBOT
    use common_data, only: DTOP, DBOT, VTOP, VBOT
    implicit none
    integer :: I

    ! Apply top boundary conditions
    do I = IMIN, IMAX
      P(JMAX, I) = DTOP(I) + VTOP(I) * P(JTOP, I)
    end do

    ! Apply bottom boundary conditions  
    do I = IMIN, IMAX
      P(JMIN, I) = DBOT(I) + VBOT(I) * P(JBOT, I)
    end do

  end subroutine BCEND

  ! Compute far-field boundary conditions for outer boundaries
  subroutine FARFLD()
    use common_data, only: AK, RTK, XIN, YIN, IMIN, IMAX, JMIN, JMAX
    use common_data, only: DTOP, DBOT, DUP, DDOWN, VTOP, VBOT, VUP, VDOWN
    use common_data, only: BCTYPE, F, H, POR, PI, TWOPI, HALFPI
    use common_data, only: B, ALPHA0, ALPHA1, ALPHA2, BETA0, BETA1, BETA2
    use common_data, only: PSI0, PSI1, PSI2, OMEGA0, OMEGA1, OMEGA2, JET
    use common_data, only: XSING, FHINV, RTKPOR    
    implicit none
    integer :: I, J
    real :: YT, YB, XUP, XDN, YT2, YB2, XUP2, XDN2, COEF1, COEF2
    real :: XU2, XD2  ! Additional variables for far-field calculation
    real :: XP, XP2, YJ, YJ2, Q, ARG0, ARG1, ARG2
    real :: EXARG0, EXARG1, EXARG2, TERM

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

    ! Subsonic freestream
    ! Functional form of the potential on outer boundaries is prescribed.
    ! Equations represent asymptotic form for doublet and vortex in free air
    ! and wind tunnel environment. Doublet and vortex are located at X=XSING, Y=0.

    ! Set location of singular vortex and doublet
    XSING = 0.5

    ! Set default values for tunnel wall parameters
    B = 0.0
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
      YT = YIN(JMAX) * RTK
      YB = YIN(JMIN) * RTK
      XUP = XIN(IMIN) - XSING
      XDN = XIN(IMAX) - XSING
      YT2 = YT * YT
      YB2 = YB * YB
      XU2 = XUP * XUP
      XD2 = XDN * XDN
      COEF1 = 1.0 / TWOPI
      COEF2 = 1.0 / (TWOPI * RTK)

      ! Compute doublet and vortex terms on top and bottom boundaries
      do I = IMIN, IMAX
        XP = XIN(I) - XSING
        XP2 = XP * XP
        DTOP(I) = XP / (XP2 + YT2) * COEF2
        DBOT(I) = XP / (XP2 + YB2) * COEF2
        VTOP(I) = -atan2(YT, XP) * COEF1
        VBOT(I) = -(atan2(YB, XP) + TWOPI) * COEF1
      end do

      ! Compute doublet and vortex terms on upstream and downstream boundaries
      do J = JMIN, JMAX
        YJ = YIN(J) * RTK
        YJ2 = YJ * YJ
        DUP(J) = XUP / (XU2 + YJ2) * COEF2
        DDOWN(J) = XDN / (XD2 + YJ2) * COEF2
        Q = PI - sign(PI, YJ)
        VUP(J) = -(atan2(YJ, XUP) + Q) * COEF1
        VDOWN(J) = -(atan2(YJ, XDN) + Q) * COEF1
      end do

      if (AK > 0.0) then
        ! call ANGLE() ! This may need to be implemented if needed
      end if

    case (2)
      ! BCTYPE = 2: SOLID WALL TUNNEL
      POR = 0.0
      B = 0.5
      ALPHA0 = PI
      ALPHA1 = PI
      ALPHA2 = PI
      BETA0 = HALFPI
      BETA1 = HALFPI
      BETA2 = HALFPI
      call compute_tunnel_conditions()

    case (3)
      ! BCTYPE = 3: FREE JET
      F = 0.0
      RTKPOR = 0.0
      ALPHA0 = HALFPI
      ALPHA1 = HALFPI
      ALPHA2 = HALFPI
      JET = 0.5
      BETA0 = 0.0
      BETA1 = 0.0
      BETA2 = 0.0
      call compute_tunnel_conditions()

    case (4)
      ! BCTYPE = 4: IDEAL SLOTTED WALL
      RTKPOR = 0.0
      FHINV = 1.0 / (F * H)
      ! SET CONSTANTS FOR DOUBLET SOLUTION
      ! call DROOTS() ! Would need to implement
      ! SET CONSTANTS FOR VORTEX SOLUTION
      JET = 0.5
      ! call VROOTS() ! Would need to implement
      call compute_tunnel_conditions()

    case (5)
      ! BCTYPE = 5: IDEAL PERFORATED/POROUS WALL
      F = 0.0
      RTKPOR = RTK / POR
      ALPHA0 = HALFPI - atan(-RTKPOR)
      ALPHA1 = ALPHA0
      ALPHA2 = ALPHA0
      BETA0 = atan(RTKPOR)
      BETA1 = BETA0
      BETA2 = BETA1
      call compute_tunnel_conditions()

    case (6)
      ! BCTYPE = 6: GENERAL HOMOGENEOUS WALL BOUNDARY CONDITION
      ! This boundary condition is not operable yet in finite difference subroutines
      write(15, '(A)') 'ABNORMAL STOP IN SUBROUTINE FARFLD'
      write(15, '(A)') 'BCTYPE=6 IS NOT USEABLE'
      stop 'FARFLD: BCTYPE=6 is not implemented'

    case default
      write(15, '(A,I0)') 'FARFLD: Invalid BCTYPE = ', BCTYPE
      stop 'FARFLD: Invalid BCTYPE'
    end select

  contains

    subroutine compute_tunnel_conditions()
      ! Compute functional forms for upstream and downstream boundary conditions
      ! for doublet and vortex
      XUP = (XIN(IMIN) - XSING) / (RTK * H)
      XDN = (XIN(IMAX) - XSING) / (RTK * H)

      ! Doublet terms
      COEF1 = 0.5 / AK / H
      ARG0 = ALPHA0
      ARG1 = PI - ALPHA1
      ARG2 = TWOPI - ALPHA2
      EXARG0 = exp(-ARG0 * XDN)
      EXARG1 = exp(ARG1 * XUP)
      EXARG2 = exp(ARG2 * XUP)

      do J = JMIN, JMAX
        YJ = YIN(J) / H
        DDOWN(J) = COEF1 * (B + OMEGA0 * cos(YJ * ARG0) * EXARG0)
        DUP(J) = -COEF1 * ((1.0 - B) * OMEGA1 * cos(YJ * ARG1) * EXARG1 + &
                            OMEGA2 * cos(YJ * ARG2) * EXARG2)
      end do

      ! Vortex terms
      ARG0 = BETA0
      ARG1 = PI + BETA1
      ARG2 = PI - BETA2
      EXARG0 = exp(-ARG0 * XDN)
      EXARG1 = exp(-ARG1 * XDN)
      EXARG2 = exp(ARG2 * XUP)

      do J = JMIN, JMAX
        YJ = YIN(J) / H
        TERM = YJ
        if (JET == 0.0) TERM = sin(YJ * ARG0) / ARG0
        VDOWN(J) = -0.5 * (1.0 - sign(1.0, YJ) + (1.0 - JET) * PSI0 * TERM * EXARG0 + &
                           PSI1 * sin(YJ * ARG1) * EXARG1 / ARG1)
        TERM = 0.0
        if (JET /= 0.0) TERM = JET * YJ / (1.0 + F)
        VUP(J) = -0.5 * (1.0 - TERM - PSI2 * sin(YJ * ARG2) * EXARG2 / ARG2)
      end do
    end subroutine compute_tunnel_conditions

  end subroutine FARFLD

end module solver_module
