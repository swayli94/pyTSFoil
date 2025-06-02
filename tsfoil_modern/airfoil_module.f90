! airfoil_module.f90
! Module for airfoil geometry routines

module airfoil_module
  use common_data
  implicit none
  public :: BODY, PRBODY

contains

  ! Print airfoil geometry summary: thickness, camber, volume
  ! Prints out body geometry
  ! If PHYS = .TRUE.  all dimensions are normalized by airfoil chord
  ! If PHYS = .FALSE. all dimensions except X are normalized by chord length and thickness ratio
  ! Called by - BODYil geometry: thickness, camber, volume
  subroutine BODY()
    use common_data, only: IMIN, IMAX, ILE, ITE, XIN, AMESH
    use common_data, only: FL, FXL, FU, FXU, CAMBER, THICK, VOL, XFOIL, IFOIL
    use common_data, only: BCFOIL, NL, NU, XL, XU, YL, YU, RIGF, IFLAP, DELFLP, FLPLOC
    use common_data, only: PHYS, DELTA, FSYM, UNIT_INPUT
    use math_module, only: SIMP
    use spline_module, only: SPLN1, SPLN1X
    implicit none
    integer :: I, IC, IFP, IERR, ISYM, N
    real :: Z, RTZ, Z2, Z3, Z4
    real :: DELINV, DY1, DY2, XP, YP, DYP
    real :: VOLU, VOLL, DFLAP, SDFLAP, DELY
    real :: FNU, FNL

    ! Number of points on airfoil
    IFOIL = ITE - ILE + 1

    ! Zero previous arrays
    do I = IMIN, IMAX
      FU(I) = 0.0
      FL(I) = 0.0
      FXU(I) = 0.0
      FXL(I) = 0.0
    end do

    select case (BCFOIL)

    case (1)
      ! NACA 00XX symmetric airfoil
      IC = 0
      do I = ILE, ITE
        IC = IC + 1
        Z = XIN(I)
        XFOIL(IC) = Z
        RTZ = sqrt(Z)
        Z2 = Z*Z; Z3 = Z2*Z; Z4 = Z3*Z
        FU(IC) = 1.4845*RTZ - 0.63*Z - 1.758*Z2 + 1.4215*Z3 - 0.5075*Z4
        FL(IC) = -FU(IC)
        FXU(IC) = 0.74225/RTZ - 0.63 - 3.516*Z + 4.2645*Z2 - 2.03*Z3
        FXL(IC) = -FXU(IC)
      end do

    case (2)
      ! Parabolic arc bi-convex
      IC = 0
      do I = ILE, ITE
        IC = IC + 1
        Z = XIN(I)
        XFOIL(IC) = Z
        Z2 = Z*Z
        FU(IC) = 2.0*(Z - Z2)
        FL(IC) = -FU(IC)
        FXU(IC) = 2.0 - 4.0*Z
        FXL(IC) = -FXU(IC)
      end do

    case (5)
      ! Diamond airfoil
      IC = 0
      do I = ILE, ITE
        IC = IC + 1
        Z = XIN(I)
        XFOIL(IC) = Z
        if (Z <= 0.5) then
          FU(IC) = 0.5*Z
          FXU(IC) = 1.0
        else
          FU(IC) = 1.0 - Z
          FXU(IC) = -1.0
        end if
        FL(IC) = -FU(IC)
        FXL(IC) = -FXU(IC)
      end do

    case (3)
      ! Airfoil ordinates read in: cubic spline interpolation
      DELINV = 1.0
      if (PHYS) DELINV = 1.0/DELTA
      ! Upper surface
      DY1 = (YU(2) - YU(1))/(XU(2) - XU(1))
      DY2 = (YU(NU) - YU(NU-1))/(XU(NU) - XU(NU-1))      
      call SPLN1(XU, YU, NU)
      IC = 0
      do I = ILE, ITE
        IC = IC + 1
        XP = XIN(I)
        XFOIL(IC) = XP
        call SPLN1X(XU, YU, NU, XP, YP, DYP)
        FU(IC) = YP*DELINV
        FXU(IC) = DYP*DELINV
      end do
      ! Lower surface
      DY1 = (YL(2) - YL(1))/(XL(2) - XL(1))
      DY2 = (YL(NL) - YL(NL-1))/(XL(NL) - XL(NL-1))
      call SPLN1(XL, YL, NL)
      IC = 0
      do I = ILE, ITE
        IC = IC + 1
        XP = XIN(I)
        call SPLN1X(XL, YL, NL, XP, YP, DYP)
        FL(IC) = YP*DELINV
        FXL(IC) = DYP*DELINV
      end do

    case (4)      ! Jameson's airfoil input format
      DELINV = 1.0

      if (PHYS) DELINV = 1.0/DELTA

      read(UNIT_INPUT,'(1X)')
      read(UNIT_INPUT,'(3F10.0)') FSYM, FNU, FNL

      ISYM = FSYM
      NU = FNU
      NL = FNL

      read(UNIT_INPUT,'(1X)')
      read(UNIT_INPUT,'(2F10.0)') (XU(N), YU(N), N=1, NU)

      DY1 = (YU(2) - YU(1))/(XU(2) - XU(1))
      DY2 = (YU(NU) - YU(NU-1))/(XU(NU) - XU(NU-1))

      call SPLN1(XU, YU, NU)

      IC = 0
      do I = ILE, ITE
        IC = IC + 1
        XP = XIN(I)
        XFOIL(IC) = XP
        call SPLN1X(XU, YU, NU, XP, YP, DYP)
        FU(IC) = YP*DELINV
        FXU(IC) = DYP*DELINV
      end do

      if (ISYM == 0) then
        NL = NU
        IC = 0
        do I = ILE, ITE
          IC = IC + 1
          FL(IC) = -FU(IC)
          FXL(IC) = -FXU(IC)
        end do
        do N = 1, NL
          XL(N) = XU(N)
          YL(N) = -YU(N)
        end do

      else
        read(UNIT_INPUT,'(1X)')
        read(UNIT_INPUT,'(2F10.0)') (XL(N), YL(N), N=1, NL)

        DY1 = (YL(2) - YL(1))/(XL(2) - XL(1))
        DY2 = (YL(NL) - YL(NL-1))/(XL(NL) - XL(NL-1))

        call SPLN1(XL, YL, NL)

        IC = 0
        do I = ILE, ITE
          IC = IC + 1
          XP = XIN(I)
          call SPLN1X(XL, YL, NL, XP, YP, DYP)
          FL(IC) = YP*DELINV
          FXL(IC) = DYP*DELINV
        end do

      end if

    end select

    ! Compute volume by Simpson's rule
    call SIMP(VOLU, XFOIL, FU, IFOIL, IERR)
    call SIMP(VOLL, XFOIL, FL, IFOIL, IERR)
    VOL = VOLU - VOLL

    ! Add flap deflection if any
    if (IFLAP /= 0) then
      DFLAP = DELFLP/57.29578
      SDFLAP = sin(DFLAP)
      do I = 1, IFOIL
        if (XFOIL(I) >= FLPLOC) exit
      end do

      IFP = I
      do I = IFP, IFOIL
        DELY = (XFOIL(I)-FLPLOC)*SDFLAP*DELINV
        FU(I) = FU(I) - DELY
        FL(I) = FL(I) - DELY
        FXU(I) = FXU(I) - DFLAP*DELINV
        FXL(I) = FXL(I) - DFLAP*DELINV
      end do

    end if

    ! Compute camber and thickness
    do I = 1, IFOIL
      CAMBER(I) = 0.5*(FU(I)+FL(I))
      THICK(I) = 0.5*(FU(I)-FL(I))
      FXU(I) = FXU(I)/sqrt(1.0+RIGF*(DELTA*FXU(I))**2)
      FXL(I) = FXL(I)/sqrt(1.0+RIGF*(DELTA*FXL(I))**2)
    end do

    ! Print or log geometry
    call PRBODY()

  end subroutine BODY

  ! Print airfoil geometry summary: thickness, camber, volume
  ! Prints out body geometry
  ! If PHYS = .TRUE.  all dimensions are normalized by airfoil chord
  ! If PHYS = .FALSE. all dimensions except X are normalized by chord length and thickness ratio
  ! Called by - BODY
  subroutine PRBODY()
    use common_data, only: FL, FXL, FU, FXU, CAMBER, THICK, VOL, XFOIL, IFOIL, PHYS, DELTA, UNIT_OUTPUT
    implicit none
    real :: THMAX, CAMAX, VOLUME, YUP, YXUP, YLO, YXLO, TH, CA
    integer :: II    
    
    ! Header message
    write(UNIT_OUTPUT, '(1x,"AIRFOIL GEOMETRY INFORMATION")')

    ! Find maximum thickness and camber
    THMAX = 0.0
    CAMAX = 0.0

    do II = 1, IFOIL
      THMAX = amax1(THMAX, THICK(II))
      CAMAX = amax1(CAMAX, CAMBER(II))
    end do

    THMAX = 2.0*THMAX
    
    if (.not. PHYS) then
      ! Printout in similarity variables
      write(UNIT_OUTPUT, '(1x,"PRINTOUT IN SIMILARITY VARIABLES",28X,"MAX THICKNESS =",F12.8)') THMAX
      write(UNIT_OUTPUT, '(1x,"AIRFOIL VOLUME =",F12.8,32X,"MAX CAMBER    =",F10.6//20X,"UPPER  SURFACE",14X,"LOWER  SURFACE")') VOL, CAMAX
      write(UNIT_OUTPUT, '(8X,"X",1X,2(12X,"F",9X,"DF/DX"),8X,"THICKNESS",4X,"CAMBER")')

      do II = 1, IFOIL
        write(UNIT_OUTPUT, '(1X,F12.8,2X,2F12.8,2(3X,2F12.8))') XFOIL(II), FU(II), FXU(II), FL(II), FXL(II), THICK(II), CAMBER(II)
      end do

    else
      ! Printout in physical variables
      THMAX = DELTA*THMAX
      CAMAX = DELTA*CAMAX
      VOLUME = VOL*DELTA
      write(UNIT_OUTPUT, '(1x,"PRINTOUT IN PHYSICAL VARIABLES NORMALIZED BY CHORD LENGTH",3X,"MAX THICKNESS =",F10.6)') THMAX
      write(UNIT_OUTPUT, '(1x,"AIRFOIL VOLUME =",F12.8,32X,"MAX CAMBER    =",F10.6//20X,"UPPER  SURFACE",14X,"LOWER  SURFACE")') VOLUME, CAMAX
      write(UNIT_OUTPUT, '(8X,"X",1X,2(12X,"Y",9X,"DY/DX"),8X,"THICKNESS",4X,"CAMBER")')

      do II = 1, IFOIL
        YUP = DELTA*FU(II)
        YXUP = DELTA*FXU(II)
        YLO = DELTA*FL(II)
        YXLO = DELTA*FXL(II)
        TH = DELTA*THICK(II)
        CA = DELTA*CAMBER(II)
        write(UNIT_OUTPUT, '(1X,F12.8,2X,2F12.8,2(3X,2F12.8))') XFOIL(II), YUP, YXUP, YLO, YXLO, TH, CA
      end do

    end if

  end subroutine PRBODY

end module airfoil_module
