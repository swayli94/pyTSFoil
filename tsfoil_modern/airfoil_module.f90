! airfoil_module.f90
! Module for airfoil geometry routines

module airfoil_module
  implicit none
  public :: BODY, PRBODY

contains

  ! Print airfoil geometry summary: thickness, camber, volume
  ! Prints out body geometry
  ! If PHYS = .TRUE.  all dimensions are normalized by airfoil chord
  ! If PHYS = .FALSE. all dimensions except X are normalized by chord length and thickness ratio
  ! Called by - BODYil geometry: thickness, camber, volume
  subroutine BODY()
    use common_data, only: IMIN, IMAX, ILE, ITE, XIN
    use common_data, only: FL, FXL, FU, FXU, CAMBER, THICK, VOL, XFOIL, IFOIL
    use common_data, only: NL, NU, XL, XU, YL, YU, RIGF, IFLAP, DELFLP, FLPLOC
    use common_data, only: PHYS, DELTA
    use math_module, only: SIMP
    use spline_module, only: SPLN1, SPLN1X, set_boundary_conditions
    implicit none
    integer :: I, IC, IFP, IERR
    real :: DELINV, DY1, DY2, XP, YP, DYP
    real :: VOLU, VOLL, DFLAP, SDFLAP, DELY

    DELINV = 1.0
    if (PHYS) DELINV = 1.0/DELTA

    ! Number of points on airfoil
    IFOIL = ITE - ILE + 1

    ! Zero previous arrays
    do I = IMIN, IMAX
      FU(I) = 0.0
      FL(I) = 0.0
      FXU(I) = 0.0
      FXL(I) = 0.0
    end do

    ! BCFOIL = 3: read airfoil coordinates from file with cubic spline interpolation
    ! Upper surface
    DY1 = (YU(2) - YU(1))/(XU(2) - XU(1))
    DY2 = (YU(NU) - YU(NU-1))/(XU(NU) - XU(NU-1))
    ! Set boundary conditions: K1=1, K2=1 for first derivative specified at both ends
    call set_boundary_conditions(1, 1, DY1, DY2)
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
    ! Set boundary conditions: K1=1, K2=1 for first derivative specified at both ends
    call set_boundary_conditions(1, 1, DY1, DY2)
    call SPLN1(XL, YL, NL)
    IC = 0
    do I = ILE, ITE
      IC = IC + 1
      XP = XIN(I)
      call SPLN1X(XL, YL, NL, XP, YP, DYP)
      FL(IC) = YP*DELINV
      FXL(IC) = DYP*DELINV
    end do


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
