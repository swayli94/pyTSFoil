! solver_module.f90
! Module for finite-difference setup and boundary condition routines

module solver_module
  use common_data
  implicit none
  public :: DIFCOE, SETBC, BCEND

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

end module solver_module
