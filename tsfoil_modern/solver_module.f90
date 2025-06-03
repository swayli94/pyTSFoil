! solver_module.f90
! Module for finite-difference setup and boundary condition routines

module solver_module
  use common_data
  implicit none
  public :: DIFCOE, SETBC, BCEND, FARFLD, ANGLE, EXTRAP

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
    ! SUBROUTINE SETBC sets the limits on range of I and J
    ! for solution of the difference equations.
    ! The body slope boundary condition at the current
    ! X mesh points on the body are multiplied by mesh
    ! spacing constants and entered into arrays FXUBC and
    ! FXLBC for use in subroutine SYOR.
    use common_data, only: IMIN, IMAX, IUP, IDOWN, JMIN, JMAX, JTOP, JBOT, J1, J2
    use common_data, only: ILE, ITE, JUP, JLOW, FXLBC, FXUBC, FXL, FXU
    use common_data, only: AK, ALPHA, BCTYPE, POR, IREF, KSTEP, IFOIL
    use common_data, only: CYYBLU, CYYBUD, WSLP
    implicit none
    integer, intent(in) :: IJUMP
    integer :: I, IF, N, NFOIL, INT, JINT

    if (IJUMP > 0) goto 20
    
    ! Set limits on I and J indices
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
    J1 = JBOT + 1
    J2 = JTOP - 1

    ! Airfoil body boundary condition
    ! Zero elements in arrays for upper and lower body boundary conditions
20  do I = IMIN, IMAX
      FXLBC(I) = 0.0
      FXUBC(I) = 0.0
    end do
    
    ! Enter body slopes at mesh points on airfoil
    ! into arrays for body boundary conditions
    if (IREF <= 0) KSTEP = 1
    if (IREF == 1) KSTEP = 2
    if (IREF == 2) KSTEP = 4
    
    NFOIL = ITE - ILE + 1
    IF = IFOIL + KSTEP
    I = ITE + 1
    
    do N = 1, NFOIL
      I = I - 1
      IF = IF - KSTEP
      FXLBC(I) = CYYBLU * (FXL(IF) - ALPHA + WSLP(I,2))
      FXUBC(I) = CYYBUD * (FXU(IF) - ALPHA + WSLP(I,1))
    end do

  end subroutine SETBC

  ! Apply boundary conditions on each i-line (upper/lower boundaries)
  subroutine BCEND()
    ! SUBROUTINE BCEND modifies the DIAG and RHS vectors
    ! on each I line in the appropriate way to include the
    ! boundary conditions at JBOT and JTOP.
    ! Called by - SYOR.
    
    use common_data, only: P, X, Y, IMIN, IMAX, IUP, IDOWN, ILE, ITE, &
                          JMIN, JMAX, JUP, JLOW, JTOP, JBOT, J1, J2, &
                          AK, ALPHA, DUB, GAM1, RTK, &
                          XDIFF, YDIFF, &
                          DIAG, RHS, SUB, SUP, &
                          CYYC, CYYD, CYYU, IVAL, &
                          BCTYPE, CIRCFF, FHINV, POR, CIRCTE, &
                          UNIT_OUTPUT
    implicit none
    
    integer :: I, II
    real :: DFACL, DFACU, RFACL, RFACU, PJMIN, PJMAX, TERM
    
    I = IVAL
    
    ! Branch to appropriate address for BCTYPE
    select case (BCTYPE)
    
    case (1)  
      ! BCTYPE = 1, FREE AIR
      ! Dirichlet boundary condition for subsonic freestream
      if (AK > 0.0) return
      ! Neumann boundary condition for supersonic freestream
      DFACL = -CYYD(JBOT) * RTK * XDIFF(I)
      DFACU = -CYYU(JTOP) * RTK * XDIFF(I)
      RFACL = DFACL * (P(JMIN,I) - P(JMIN,I-1))
      RFACU = DFACU * (P(JMAX,I) - P(JMAX,I-1))
      goto 95
        
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
      goto 90
        
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
      goto 95
        
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
        goto 95
      end if
        
    case (6)  
      ! BCTYPE = 6, GENERAL WALL BOUNDARY CONDITION
      ! Difference equations for this boundary condition
      ! have not yet been worked out. User must insert
      ! information needed for calculation
      write(UNIT_OUTPUT, 1000)
1000  format('1ABNORMAL STOP IN SUBROUTINE BCEND', /, &
               'BCTYPE=6 IS NOT USEABLE')
      stop
        
    case default
      write(UNIT_OUTPUT, *) 'ERROR: Invalid BCTYPE = ', BCTYPE
      stop
        
    end select
    
    ! Dirichlet boundary conditions
90  continue
    RHS(JBOT) = RHS(JBOT) - (CYYD(JBOT)*(PJMIN-P(JBOT-1,I)))
    RHS(JTOP) = RHS(JTOP) - (CYYU(JTOP)*(PJMAX-P(JTOP+1,I)))
    return
    
    ! Neumann boundary conditions
95  continue
    DIAG(JBOT) = DIAG(JBOT) + DFACL
    DIAG(JTOP) = DIAG(JTOP) + DFACU
    RHS(JBOT) = RHS(JBOT) - RFACL + CYYD(JBOT)*P(JBOT-1,I)
    RHS(JTOP) = RHS(JTOP) - RFACU + CYYU(JTOP)*P(JTOP+1,I)
    
  end subroutine BCEND

  ! Compute far-field boundary conditions for outer boundaries
  subroutine FARFLD()
    use common_data, only: AK, RTK, XIN, YIN, IMIN, IMAX, JMIN, JMAX
    use common_data, only: DTOP, DBOT, DUP, DDOWN, VTOP, VBOT, VUP, VDOWN
    use common_data, only: BCTYPE, F, H, POR, PI, TWOPI, HALFPI
    use common_data, only: B, ALPHA0, ALPHA1, ALPHA2, BETA0, BETA1, BETA2
    use common_data, only: PSI0, PSI1, PSI2, OMEGA0, OMEGA1, OMEGA2, JET
    use common_data, only: XSING, FHINV, RTKPOR
    use math_module, only: DROOTS, VROOTS
    implicit none
    integer :: I, J
    real :: YT, YB, XU_BC, XD_BC, YT2, YB2, XU2, XD2, COEF1, COEF2
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
    ! Actual boundary values are set in subroutines RECIRC and REDUB where the 
    ! functional forms are multiplied by the vortex and doublet strengths.
    ! The boundary conditions are calculated herein for the input X and Y mesh 
    ! and values are deleted for the coarse mesh in subroutine SETBC.

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
      XU_BC = XIN(IMIN) - XSING
      XD_BC = XIN(IMAX) - XSING
      YT2 = YT * YT
      YB2 = YB * YB
      XU2 = XU_BC * XU_BC
      XD2 = XD_BC * XD_BC
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
      B = 0.5
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

      XU_BC = (XIN(IMIN) - XSING) / (RTK * H)
      XD_BC = (XIN(IMAX) - XSING) / (RTK * H)

      ! Doublet terms
      COEF1 = 0.5 / AK / H
      ARG0 = ALPHA0
      ARG1 = PI - ALPHA1
      ARG2 = TWOPI - ALPHA2
      EXARG0 = exp(-ARG0 * XD_BC)
      EXARG1 = exp(ARG1 * XU_BC)
      EXARG2 = exp(ARG2 * XU_BC)

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
      EXARG0 = exp(-ARG0 * XD_BC)
      EXARG1 = exp(-ARG1 * XD_BC)
      EXARG2 = exp(ARG2 * XU_BC)

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
    end if

  end subroutine FARFLD

  ! Compute the angle THETA at each mesh point
  subroutine ANGLE()
    use common_data, only: IMIN, IMAX, JMIN, JMAX, XIN, YIN, RTK
    use common_data, only: XSING, THETA, PI, TWOPI
    implicit none
    integer :: I, J
    real :: XX, YY, R, ATN, Q, R2PI
    
    R2PI = 1.0 / TWOPI
    
    do I = IMIN, IMAX
      XX = XIN(I) - XSING
      do J = JMIN, JMAX
        YY = YIN(J) * RTK
        R = sqrt(YIN(J)**2 + XX*XX)
        ATN = atan2(YY, XX)
        Q = PI - sign(PI, YY)
        THETA(J,I) = -(ATN + Q) * R2PI
        if (R <= 1.0) THETA(J,I) = THETA(J,I) * R
      end do
    end do

  end subroutine ANGLE

  ! Compute P at point (XP,YP) using far-field solution for subsonic flow
  ! This subroutine extrapolates the potential P at coordinates (XP,YP) using
  ! far-field solutions when new mesh points are outside the range of old mesh points
  ! during restart interpolation. Called by GUESSP during restart operations.
  subroutine EXTRAP(XP, YP, PNEW)
    use common_data, only: AK, DUB, GAM1, RTK, BCTYPE, CIRCFF, FHINV, POR, CIRCTE
    use common_data, only: F, H, HALFPI, PI, RTKPOR, TWOPI
    use common_data, only: B, BETA0, BETA1, BETA2, PSI0, PSI1, PSI2
    use common_data, only: ALPHA0, ALPHA1, ALPHA2, XSING, OMEGA0, OMEGA1, OMEGA2, JET
    implicit none
    real, intent(in) :: XP, YP    ! Coordinates where P is to be computed
    real, intent(out) :: PNEW     ! Computed potential value
      ! Local variables
    real :: XI_LOC, ETA_LOC, TERM, ARG1, ARG2, YP_local
    
    ! Handle the three boundary condition cases
    select case (BCTYPE)
    
    case (1)
      ! BCTYPE = 1: FREE AIR BOUNDARY CONDITION
      YP_local = YP
      if (abs(YP_local) < 1.0E-6) YP_local = -1.0E-6
      
      XI_LOC = XP - XSING
      ETA_LOC = YP_local * RTK
      
      ! Far-field solution for free air
      PNEW = -CIRCFF / TWOPI * (atan2(ETA_LOC, XI_LOC) + PI - sign(PI, ETA_LOC)) + &
             DUB / TWOPI / RTK * (XI_LOC / (XI_LOC*XI_LOC + ETA_LOC*ETA_LOC))
      
    case (2, 4, 5)
      ! BCTYPE = 2, 4, 5: TUNNEL WALL BOUNDARY CONDITIONS
      ETA_LOC = YP / H
      XI_LOC = (XP - XSING) / (H * RTK)
      
      if (XI_LOC >= 0.0) then
        ! XP is downstream of airfoil
        TERM = ETA_LOC
        if (BCTYPE /= 3) TERM = sin(ETA_LOC * BETA0) / BETA0
        
        PNEW = -0.5 * CIRCFF * (1.0 - sign(1.0, ETA_LOC) + &
               (1.0 - JET) * PSI0 * TERM * exp(-BETA0 * XI_LOC)) + &
               DUB * 0.5 / (AK * H) * (B + OMEGA0 * cos(ETA_LOC * ALPHA0) * &
               exp(-ALPHA0 * XI_LOC))
      else
        ! XP is upstream of airfoil
        TERM = 0.0
        if (JET /= 0.0) TERM = JET * ETA_LOC / (1.0 + F)
        
        ARG1 = PI - ALPHA1
        ARG2 = PI - BETA2
        
        PNEW = -0.5 * CIRCFF * (1.0 - TERM - PSI2 * sin(ETA_LOC * ARG2) / ARG2 * &
               exp(ARG2 * XI_LOC)) - 0.5 * DUB / (AK * H) * &
               ((1.0 - B) * OMEGA1 * cos(ETA_LOC * ARG1) * exp(XI_LOC * ARG1))
      end if
      
    case (3)
      ! BCTYPE = 3: FREE JET
      ETA_LOC = YP / H
      XI_LOC = (XP - XSING) / (H * RTK)
      
      if (XI_LOC >= 0.0) then
        ! XP is downstream of airfoil - for free jet, TERM = ETA_LOC (no sine)
        TERM = ETA_LOC
        
        PNEW = -0.5 * CIRCFF * (1.0 - sign(1.0, ETA_LOC) + &
               (1.0 - JET) * PSI0 * TERM * exp(-BETA0 * XI_LOC)) + &
               DUB * 0.5 / (AK * H) * (B + OMEGA0 * cos(ETA_LOC * ALPHA0) * &
               exp(-ALPHA0 * XI_LOC))
      else
        ! XP is upstream of airfoil
        TERM = 0.0
        if (JET /= 0.0) TERM = JET * ETA_LOC / (1.0 + F)
        
        ARG1 = PI - ALPHA1
        ARG2 = PI - BETA2
        
        PNEW = -0.5 * CIRCFF * (1.0 - TERM - PSI2 * sin(ETA_LOC * ARG2) / ARG2 * &
               exp(ARG2 * XI_LOC)) - 0.5 * DUB / (AK * H) * &
               ((1.0 - B) * OMEGA1 * cos(ETA_LOC * ARG1) * exp(XI_LOC * ARG1))
      end if
      
    case default
      ! For other boundary types, set to zero (supersonic behavior)
      PNEW = 0.0
      
    end select
    
  end subroutine EXTRAP

end module solver_module
