! io_module.f90
! Module for input/output routines

module io_module
  use common_data
  implicit none

  ! Complete namelist matching the original /INP/ namelist exactly
  namelist /INP/ AK, ALPHA, BCTYPE, CLSET, &
                  CVERGE, DELTA, DVERGE, EMACH, EPS, F, &
                  FCR, GAM, H, IMAXI, IMIN, &
                  IPRTER, JMAXI, JMIN, KUTTA, MAXIT, NL, &
                  NU, PHYS, POR, &
                  RIGF, SIMDEF, WCIRC, WE, &
                  XIN, YIN, XL, YL, XU, YU, &
                  NWDGE, REYNLD, WCONST, IFLAP, DELFLP, &
                  FLPLOC
  
  ! Declare public procedures
  public :: READIN, SCALE, PRINT
  public :: open_output_files, close_output_files

contains
  
  ! Open all output files with unique file units
  subroutine open_output_files()
    use common_data, only: UNIT_OUTPUT, UNIT_SUMMARY
    implicit none
    
    open(unit=UNIT_OUTPUT, file='tsfoil2.out', status='replace', action='write')   ! Unit 15
    open(unit=UNIT_SUMMARY, file='smry.out', status='replace', action='write')     ! Unit 16
        
  end subroutine open_output_files

  ! Close all output files
  subroutine close_output_files()
    use common_data, only: UNIT_OUTPUT, UNIT_SUMMARY
    implicit none

    close(UNIT_OUTPUT)   ! tsfoil2.out
    close(UNIT_SUMMARY)  ! smry.out

  end subroutine close_output_files
  
  ! Main input reading routine - reads one case at a time and returns for processing
  ! Reads title card, namelist input, and manages restart data for current case  
  ! The original READIN is designed to be called once per case from main program
  subroutine READIN()
    use common_data
    use mesh_module, only: setup_mesh
    implicit none    
    character(len=100) :: IN_FILENAME, TITLE
    integer :: ios
        
    ! Handle command line argument for input file
    call get_command_argument(1, IN_FILENAME)

    if (IN_FILENAME == '') then
      IN_FILENAME = 'tsfoil.inp'  ! Default input file name
    else
      write(*, '(A)') 'Using input file: ' // trim(IN_FILENAME)
    end if
    
    open(unit=UNIT_INPUT, file=trim(IN_FILENAME), status='old')
    
    ! Open output files
    call open_output_files()
    
    ! Read title card
    read(UNIT_INPUT, '(A)', iostat=ios) TITLE
    if (ios /= 0) then
        write(UNIT_OUTPUT, '(A,I0)') 'Error reading title card. IOSTAT = ', ios
        write(UNIT_OUTPUT, '(A)') 'IOSTAT < 0: End of file reached.'
        write(UNIT_OUTPUT, '(A)') 'IOSTAT > 0: Read error.'
        stop
    end if
    write(UNIT_OUTPUT, '(1H1,4X,A)') trim(TITLE)

    ! Read namelist input
    read(UNIT_INPUT, INP, iostat=ios)
    if (ios /= 0) then
        write(UNIT_OUTPUT, '(A)') 'Error reading namelist input. Please check the input file.'
        stop
    end if
    close(UNIT_INPUT)
    
    ! Set AK=0 for physical coordinates
    if (PHYS) AK = 0.0

    ! Set derived constants
    GAM1 = GAM + 1.0

    ! Setup and output mesh
    call setup_mesh()
    ! call OUTPUT_MESH()

    ! Output parameters to output file
    call OUTPUT_PARAMETERS()

  end subroutine READIN
  
  ! Scale physical variables to transonic similarity variables
  subroutine SCALE()
    ! IF PHYS = .TRUE., ALL INPUT/OUTPUT QUANTITIES ARE IN PHYSICAL UNITS NORMALIZED 
    ! BY FREESTREAM VALUES AND AIRFOIL CHORD. 
    ! THIS SUBROUTINE THEN SCALES THE QUANTITIES TO TRANSONIC VARIABLES BY THE FOLLOWING CONVENTION
    !   SIMDEF = 1  COLE SCALING
    !   SIMDEF = 2  SPREITER SCALING
    !   SIMDEF = 3  KRUPP SCALING
    ! IF PHYS = .FALSE., INPUT IS ALREADY IN SCALED VARIABLES AND NO FURTHER SCALING IS DONE.
    ! CALLED BY - TSFOIL.
    use common_data, only: PHYS, DELTA, EMACH, SIMDEF
    use common_data, only: AK, ALPHA, GAM1, RTK, YFACT, CPFACT, CLFACT, CDFACT, CMFACT, VFACT
    use common_data, only: YIN, JMIN, JMAX
    use common_data, only: H, POR, SONVEL, CPSTAR, DELRT2, EMROOT, INPERR
    implicit none
    real :: EMACH2, BETA, DELRT1
    real :: YFACIV
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

      ! SCALE Y MESH
      YFACIV = 1.0 / YFACT
      do J = JMIN, JMAX
        YIN(J) = YIN(J) * YFACIV
      end do

      ! SCALE TUNNEL PARAMETERS
      H = H / YFACT
      POR = POR * YFACT
      write(UNIT_OUTPUT,'(//10X,11HSCALED POR=,F10.5)') POR

      ! SCALE ANGLE OF ATTACK
      ALPHA = ALPHA / VFACT
    end if

    ! CHECK VALUE OF AK FOR DEFAULT.
    if (AK == 0.0) call INPERR(7)

    ! COMPUTE SQUARE ROOT OF AK
    RTK = sqrt(abs(AK))

    ! COMPUTE SONIC VELOCITY
    if (abs(GAM1) <= 0.0001) then
      SONVEL = 1.0
      CPSTAR = 0.0
      return
    end if
    
    SONVEL = AK / GAM1
    CPSTAR = -2.0 * SONVEL * CPFACT
    return

  end subroutine SCALE  
    
  ! Main print driver: prints configuration parameters and calls specialized subroutines
  ! Subroutine for main output print control. Prints relative parameters and calls
  ! specialized print/plot subroutines as required.
  ! Matches original PRINT subroutine functionality exactly
  subroutine PRINT()
    use common_data
    use math_module, only: PITCH, LIFT
    implicit none

    ! Write page break
    write(UNIT_OUTPUT, '(1H1)')
    
    ! Print similarity/physical variables information
    if (PHYS) then
        write(UNIT_SUMMARY, '(A)') '0 PRINTOUT IN PHYSICAL VARIABLES.'
    else
        write(UNIT_SUMMARY, '(A)') '0 PRINTOUT IN SIMILARITY VARIABLES.'
    end if
    
    ! Print similarity parameter definition
    if (SIMDEF == 1) then
        write(UNIT_SUMMARY, '(A)') '0 DEFINITION OF SIMILARITY PARAMETERS BY COLE'
    else if (SIMDEF == 2) then
        write(UNIT_SUMMARY, '(A)') '0 DEFINITION OF SIMILARITY PARAMETERS BY SPREITER'
    else if (SIMDEF == 3) then
        write(UNIT_SUMMARY, '(A)') '0 DEFINITION OF SIMILARITY PARAMETERS BY KRUPP'
    end if
    
    ! Print boundary condition information
    select case (BCTYPE)
        case (1)
            write(UNIT_SUMMARY, '(A)') '0 BOUNDARY CONDITION FOR FREE AIR'
        case (2)
            write(UNIT_SUMMARY, '(A)') '0 BOUNDARY CONDITION FOR SOLID WALL'
        case (3)
            write(UNIT_SUMMARY, '(A)') '0 BOUNDARY CONDITION FOR FREE JET'
    end select
    
    ! Print difference equation information
    if (FCR) then
        write(UNIT_SUMMARY, '(A)') '0 DIFFERENCE EQUATIONS ARE FULLY CONSERVATIVE.'
    else
        write(UNIT_SUMMARY, '(A)') '0 DIFFERENCE EQUATIONS ARE NOT CONSERVATIVE AT SHOCK.'
    end if
    
    ! Print Kutta condition information
    if (KUTTA) then
      write(UNIT_SUMMARY, '(30H0 KUTTA CONDITION IS ENFORCED.)')
    else
      write(UNIT_SUMMARY, '(37H0 LIFT COEFFICIENT SPECIFIED BY USER.)')
    end if
    
    ! Print flow parameters
    write(UNIT_SUMMARY, '(1H0)')
    
    if (PHYS) then
      write(UNIT_SUMMARY, '(14X,6HMACH =,F12.7/13X,7HDELTA =,F12.7)') EMACH, DELTA
    end if
    
    write(UNIT_SUMMARY, '(13X,7HALPHA =,F12.7/17X,3HK =,F12.7)') ALPHA * VFACT, AK
    
    if (AK > 0.0) then
      write(UNIT_SUMMARY, '(2X,18HDOUBLET STRENGTH =,F12.7)') DUB
    end if
    
    if (PHYS) then
      write(UNIT_SUMMARY, '(12X,A,F12.7)') 'CPFACT =', CPFACT
      write(UNIT_SUMMARY, '(12X,A,F12.7)') 'CDFACT =', CDFACT  
      write(UNIT_SUMMARY, '(12X,A,F12.7)') 'CMFACT =', CMFACT
      write(UNIT_SUMMARY, '(12X,A,F12.7)') 'CLFACT =', CLFACT
      write(UNIT_SUMMARY, '(13X,A,F12.7)') 'YFACT = ', YFACT
      write(UNIT_SUMMARY, '(13X,A,F12.7)') 'VFACT = ', VFACT
    end if
    
    ! Print shock and mach number on Y=0 line
    call PRINT_SHOCK()

    ! Output field data
    call OUTPUT_FIELD()
    
    if (ABORT1) return
    
    ! Call PRTWAL for boundary conditions other than 1 and 3 (matches original logic)
    if (BCTYPE /= 1 .and. BCTYPE /= 3) then
      call PRTWAL()
    end if
    
    call CDCOLE()  ! Momentum integral drag calculation
    
  end subroutine PRINT
  
  ! Print Cp and Mach along body and build plot arrays
  ! Prints pressure coefficient and Mach number on Y=0 line, and plots CP along side of print
  subroutine PRINT_SHOCK()
    use common_data
    use math_module, only: PX, EMACH1, LIFT, PITCH
    implicit none
    
    ! Local variables exactly matching original - renamed to avoid conflicts
    integer :: I_P1, IEM
    real :: CL_val, CM
    real :: UL_P1, UU_P1, CJ01, CJ02
    real :: EM1L(N_MESH_POINTS), EM1U(N_MESH_POINTS)
    
    ! Compute coefficients exactly like original
    CL_val = LIFT(CLFACT)
    CM = PITCH(CMFACT)

    IEM = 0
    CJ01 = -Y(JLOW)/(Y(JUP)-Y(JLOW))
    CJ02 = Y(JUP)/(Y(JUP)-Y(JLOW))
    
    ! Main computation loop exactly matching original logic
    do I_P1 = IMIN, IMAX
      UL_P1 = CJLOW*PX(I_P1,JLOW) - CJLOW1*PX(I_P1,JLOW-1)
      if (I_P1 > ITE) UL_P1 = CJ01*PX(I_P1,JUP) + CJ02*PX(I_P1,JLOW)
      if (I_P1 < ILE) UL_P1 = CJ01*PX(I_P1,JUP) + CJ02*PX(I_P1,JLOW)
      CPL(I_P1) = -2.0 * UL_P1 * CPFACT
      EM1L(I_P1) = EMACH1(UL_P1)
      if (EM1L(I_P1) > 1.3) IEM = 1
      
      UU_P1 = CJUP*PX(I_P1,JUP) - CJUP1*PX(I_P1,JUP+1)
      if (I_P1 > ITE) UU_P1 = UL_P1
      if (I_P1 < ILE) UU_P1 = UL_P1
      CPU(I_P1) = -2.0 * UU_P1 * CPFACT
      EM1U(I_P1) = EMACH1(UU_P1)
      if (EM1U(I_P1) > 1.3) IEM = 1
    end do

    write(UNIT_SUMMARY, '(A, F16.12)') 'CL =', CL_val
    write(UNIT_SUMMARY, '(A, F16.12)') 'CM =', CM  
    write(UNIT_SUMMARY, '(A, F16.12)') 'CP* =', CPSTAR
    
    ! Check for detached shock - exactly like original with GO TO 70 logic
    if (CPL(IMIN) < CPSTAR .and. CPL(IMIN+1) > CPSTAR) then
      write(UNIT_SUMMARY, '(A)') '0 ***** CAUTION *****'
      write(UNIT_SUMMARY, '(A)') ' DETACHED SHOCK WAVE UPSTREAM OF X-MESH,SOLUTION TERMINATED.'
      return
    end if
    
    ! Mach number warning
    if (IEM == 1 .and. PHYS) then
      write(UNIT_SUMMARY, '(A)') '0 ***** CAUTION *****'
      write(UNIT_SUMMARY, '(A)') ' MAXIMUM MACH NUMBER EXCEEDS 1.3'
      write(UNIT_SUMMARY, '(A)') ' SHOCK JUMPS IN ERROR IF UPSTREAM NORMAL MACH NUMBER GREATER THAN 1.3'
    end if
      
    ! Output airfoil surface data
    call OUTPUT_CP_MACH_XLINE(CL_val, CM, EM1L, EM1U)

  end subroutine PRINT_SHOCK

  ! Output settings and parameters to output file
  subroutine OUTPUT_PARAMETERS()
    use common_data
    implicit none
    integer :: IDX, JDX
    
    ! Echo input parameters to output file with exact original format strings
    write(UNIT_OUTPUT, '(1H0,4X,7HEMACH =,F9.5,5X,5HPOR =,F9.5,3X,6HIMIN =,I4,3X,8HBCTYPE =,I3)') &
      EMACH, POR, IMIN, BCTYPE
    write(UNIT_OUTPUT, '(1H0,4X,7HDELTA =,F9.5,3X,7HCLSET =,F9.5,3X,6HIMAX =,I4,6X,7HPHYS = ,L1)') &
      DELTA, CLSET, IMAX, PHYS
    write(UNIT_OUTPUT, '(1H0,4X,7HALPHA =,F9.5,5X,5HEPS =,F9.5,3X,6HJMIN =,I4,7X,6HFCR = ,L1)') &
      ALPHA, EPS, JMIN, FCR
    write(UNIT_OUTPUT, '(1H0,7X,4HAK =,F9.5,4X,6HRIGF =,F9.5,3X,6HJMAX =,I4,5X,8HKUTTA = ,L1)') &
      AK, RIGF, JMAX, KUTTA
    write(UNIT_OUTPUT, '(1H0,6X,5HGAM =,F9.5,3X,7HWCIRC =,F9.5,2X,7HMAXIT =,I4,3X,8HIPRTER =,I3)') &
      GAM, WCIRC, MAXIT, IPRTER
    write(UNIT_OUTPUT, '(1H0,8X,3HF =,F9.5,2X,8HCVERGE =,F9.5,5X,4HNU =,I4,3X,8HSIMDEF =,I3)') &
      F, CVERGE, NU, SIMDEF
    write(UNIT_OUTPUT, '(1H0,8X,3HH =,F9.5,2X,8HDVERGE =,F9.1,5X,4HNL =,I4,4X,7HNWDGE =,I3)') &
      H, DVERGE, NL, NWDGE
    write(UNIT_OUTPUT, '(1H0,7X,5HWE = ,F4.2,2(1H,,F4.2))') WE
    
    if (NWDGE == 1) then
      write(UNIT_OUTPUT, '(1H0,15X,12HMURMAN WEDGE,5X,8HREYNLD =,E10.3,5X,8HWCONST =,F9.5)') REYNLD, WCONST
    elseif (NWDGE == 2) then
      write(UNIT_OUTPUT, '(1H0,15X,15HYOSHIHARA WEDGE)')
    else
      write(UNIT_OUTPUT, '(1H0,15X,8HNO WEDGE)')
    end if
    
    if (IFLAP /= 0) then
      write(UNIT_OUTPUT, '(1H0,15X,17HFLAP IS DEFLECTED,F5.2,20H DEGREES FROM H.L. =,F6.3,8H TO T.E.)') DELFLP, FLPLOC
    end if

    ! Output mesh coordinates
    write(UNIT_OUTPUT, '(1H0,4X,3HXIN)')
    write(UNIT_OUTPUT, '(4X,6F11.6)') (XIN(IDX), IDX=IMIN, IMAX)
    write(UNIT_OUTPUT, '(1H0,4X,3HYIN)')
    write(UNIT_OUTPUT, '(4X,6F11.6)') (YIN(JDX), JDX=JMIN, JMAX)

    ! Print airfoil coordinates
    write(UNIT_OUTPUT, '(1H0,15X,2HXU)')
    write(UNIT_OUTPUT, '(4X,6F11.6)') (XU(IDX), IDX=1, NU)
    write(UNIT_OUTPUT, '(1H0,15X,2HYU)')
    write(UNIT_OUTPUT, '(4X,6F11.6)') (YU(IDX), IDX=1, NU)
    write(UNIT_OUTPUT, '(1H0,15X,2HXL)')
    write(UNIT_OUTPUT, '(4X,6F11.6)') (XL(IDX), IDX=1, NL)
    write(UNIT_OUTPUT, '(1H0,15X,2HYL)')
    write(UNIT_OUTPUT, '(4X,6F11.6)') (YL(IDX), IDX=1, NL)

  end subroutine OUTPUT_PARAMETERS

  ! Output mesh data to file in Tecplot format
  subroutine OUTPUT_MESH(OPTIONAL_FILE_NAME)
    use common_data, only: X, Y, JMIN, JMAX, UNIT_MESH, IMIN, IMAX
    implicit none
    integer :: I_P1, J_P1
    character(len=*), optional :: OPTIONAL_FILE_NAME

    if (present(OPTIONAL_FILE_NAME)) then
      open(unit=UNIT_MESH, file=trim(OPTIONAL_FILE_NAME), status='replace', action='write')
    else
      open(unit=UNIT_MESH, file='mesh.dat', status='replace', action='write')
    end if

    write(UNIT_MESH, '(A)') 'VARIABLES = "X", "Y"'
    write(UNIT_MESH, '(A,I5,A,I5,A)') 'ZONE I= ', IMAX-IMIN+1, ' J= ', JMAX-JMIN+1, ' F= POINT'
    do J_P1 = JMIN, JMAX
      do I_P1 = IMIN, IMAX
        write(UNIT_MESH, '(2F12.6)') X(I_P1), Y(J_P1)
      end do
    end do

    close(UNIT_MESH)

    write(*, '(A)') 'Output to mesh.dat: Mesh data'
    
  end subroutine OUTPUT_MESH

  ! Output Cp, Mach distribution on a x-line (Y=0) in Tecplot format
  subroutine OUTPUT_CP_MACH_XLINE(CL_val, CM, EM1U, EM1L)
    use common_data, only: IMIN, IMAX, UNIT_CPXS, VFACT
    use common_data, only: X, EMACH, CPSTAR, ALPHA
    use common_data, only: CPU, CPL
    implicit none
    real, intent(in) :: CL_val, CM
    real, intent(in) :: EM1U(:), EM1L(:)
    
    integer :: I_P1

    open(unit=UNIT_CPXS, file='cpxs.dat', status='replace', action='write')

    ! Write coefficients
    write(UNIT_CPXS, '(A, F10.6)') '# Mach = ', EMACH
    write(UNIT_CPXS, '(A, F10.6)') '# Alpha = ', ALPHA * VFACT
    write(UNIT_CPXS, '(A, F10.6)') '# CL = ', CL_val
    write(UNIT_CPXS, '(A, F10.6)') '# CM = ', CM
    write(UNIT_CPXS, '(A, F10.6)') '# Cp* = ', CPSTAR

    write(UNIT_CPXS, '(A)') 'VARIABLES = "X", "Cp-up", "M-up", "Cp-low", "M-low"'

    do I_P1 = IMIN, IMAX
      write(UNIT_CPXS, '(2x,f10.5,2x,f10.5,2x,f10.5,2x,f10.5,2x,f10.5)') &
            X(I_P1), CPU(I_P1), EM1U(I_P1), CPL(I_P1), EM1L(I_P1)
    end do

    close(UNIT_CPXS)

    write(*, '(A)') 'Output to cpx.dat: Cp, Mach distribution on a x-line (Y=0)'
    
  end subroutine OUTPUT_CP_MACH_XLINE

  ! Output field in Tecplot format
  subroutine OUTPUT_FIELD()
    use common_data, only: UNIT_FIELD, X, Y, JMIN, JMAX, IMIN, IMAX, CPFACT
    use common_data, only: EMACH, ALPHA, VFACT, N_MESH_POINTS
    use common_data, only: P, IUP, IDOWN, C1, CXL, CXC, CXR
    use math_module, only: PX, EMACH1
    implicit none
    integer :: I, J
    real :: U, EM, CP_VAL, FLOW_TYPE_NUM
    real :: VT(N_MESH_POINTS,2)            ! Velocity time history

    open(unit=UNIT_FIELD, file='field.dat', status='replace', action='write')

    ! Write Tecplot header
    write(UNIT_FIELD, '(A)') '# Flow types: -1=Outside domain, 0=Elliptic, 1=Parabolic, 2=Hyperbolic, 3=Shock'
    write(UNIT_FIELD, '(A, F10.6)') '# Mach = ', EMACH
    write(UNIT_FIELD, '(A, F10.6)') '# Alpha = ', ALPHA * VFACT
    write(UNIT_FIELD, '(A,F10.6)') '# CPFACT = ', CPFACT
    
    write(UNIT_FIELD, '(A)') 'VARIABLES = "X", "Y", "Mach", "Cp", "P", "FlowType"'
    write(UNIT_FIELD, '(A,I5,A,I5,A)') 'ZONE I= ', IMAX-IMIN+1, ' J= ', JMAX-JMIN+1, ' F= POINT'

    ! Initialize VT array for flow type calculation
    do J = JMIN, JMAX
      VT(J,1) = C1(2)
    end do

    ! Write field data in point format
    do J = JMIN, JMAX
      do I = IMIN, IMAX

        ! Calculate flow variables
        U = PX(I, J)    ! Function PX computes U = DP/DX at point I,J
        EM = EMACH1(U)  ! Function EMACH1 computes Mach number from U
        CP_VAL = -2.0 * U * CPFACT  ! CPFACT is a scaling factor for pressure coefficient
        
        ! Calculate flow type for points within the computational domain
        if (I >= IUP .and. I <= IDOWN) then
          ! Flow type classification using PRTMC logic
          VT(J,2) = VT(J,1)
          VT(J,1) = C1(I) - (CXL(I)*P(J,I-1) + CXC(I)*P(J,I) + CXR(I)*P(J,I+1))
          
          if (VT(J,1) > 0.0) then
            if (VT(J,2) < 0.0) then
              ! Shock point
              FLOW_TYPE_NUM = 3.0
            else
              ! Elliptic point (subsonic)
              FLOW_TYPE_NUM = 0.0
            end if
          else
            if (VT(J,2) < 0.0) then
              ! Hyperbolic point (supersonic)
              FLOW_TYPE_NUM = 2.0
            else
              ! Parabolic point (sonic)
              FLOW_TYPE_NUM = 1.0
            end if
          end if
        else
          ! Outside computational domain
          FLOW_TYPE_NUM = -1.0
        end if
        
        write(UNIT_FIELD, '(6F16.12)') X(I), Y(J), EM, CP_VAL, P(J,I), FLOW_TYPE_NUM
      end do
    end do

    close(UNIT_FIELD)

    write(*, '(A)') 'Output to field.dat: Cp, Mach, Potential field data'

  end subroutine OUTPUT_FIELD

  ! Print Cp and flow angles on tunnel walls
  ! Prints pressure coefficient and flow angle on Y=-H and Y=+H, 
  ! and plots CP along side of tabulation. 
  subroutine PRTWAL()
    use common_data, only: P, X, Y, CPFACT, VFACT, YFACT, JMIN, JMAX, &
                          IUP, IDOWN, JBOT, JTOP, JTOP, JBOT, &
                          BCTYPE, CIRCFF, FHINV, POR, F, H, CPSTAR, &
                          XDIFF, UNIT_OUTPUT
    use math_module, only: PX, PY
    implicit none
    
    ! Local variables
    integer :: I, K, NCOL, NCOLS, NCOLU, NCOLL, I2_LOCAL, I1_LOCAL
    ! Character variables for plotting symbols (matching original DATA statements)
    character(len=1), parameter :: IB = ' '    ! Blank space
    character(len=1), parameter :: IL = 'L'    ! Lower surface
    character(len=1), parameter :: IU = 'U'    ! Upper surface  
    character(len=1), parameter :: IS = '*'    ! CP* reference line
    character(len=1), parameter :: IBB = 'B'   ! Both surfaces same CP
    real :: THH, PORF, CPMIN, CPMAX, CPT
    real :: CPLW(N_MESH_POINTS), CPUW(N_MESH_POINTS), VLW(N_MESH_POINTS), VUW(N_MESH_POINTS)
    real :: COL, CPLARG, UNPCOL
    character(len=1) :: LINE1(60)
    character(len=4) :: BCT(15)
    
    ! Data statements
    character(len=1), parameter :: BLANK = ' ', DOT = '.', STAR = '*', DASH = '-'
    
    ! Print single variables
    I2_LOCAL = 3 * BCTYPE
    I1_LOCAL = I2_LOCAL - 2
    write(UNIT_OUTPUT, '(2H1 ,3A4,20H BOUNDARY CONDITION.)') (BCT(I),I=I1_LOCAL,I2_LOCAL)
    
    THH = H * YFACT
    write(UNIT_OUTPUT, '(1H0,10X,24HH (TUNNEL HALF HEIGHT) =,F9.6)') THH
    
    if (BCTYPE >= 5) then
      PORF = POR / YFACT
      write(UNIT_OUTPUT, '(1H0,11X,23HPOR (POROSITY FACTOR) =,F9.6)') PORF
    end if
    
    if (BCTYPE == 4 .or. BCTYPE == 6) then
      write(UNIT_OUTPUT, '(1H0,14X,20HF (SLOT PARAMETER) =,F9.6)') F
    end if
    
    write(UNIT_OUTPUT, '(1H0,29X,5HCP* =,F9.6)') CPSTAR
    
    CPMIN = 1.0E37
    CPMAX = -CPMIN
    
    ! Compute CP values using proper PX function calls
    CPT = -2.0 * CPFACT
    do I = IUP, IDOWN
      CPLW(I) = CPT * PX(I,JMIN)
      CPUW(I) = CPT * PX(I,JMAX)
      CPMAX = max(CPMAX, CPUW(I), CPLW(I))
      CPMIN = min(CPMIN, CPUW(I), CPLW(I))
    end do
    
    ! Compute flow angles based on boundary condition type
    do I = IUP, IDOWN
      select case (BCTYPE)
      case (2)
        ! Solid wall
        VLW(I) = 0.0
        VUW(I) = 0.0
        
      case (3)
        ! Free jet
        VLW(I) = VFACT * PY(I,JMIN)
        VUW(I) = VFACT * PY(I,JMAX)
        
      case (4)
        ! Slotted wall
        VLW(I) =  VFACT * FHINV * (P(JBOT,I) + 0.75 * CIRCFF)
        VUW(I) = -VFACT * FHINV * (P(JTOP,I) - 0.25 * CIRCFF)
        
      case (5, 6)
        ! Porous wall
        if (POR <= 1.5) then
          VLW(I) =  VFACT * POR * XDIFF(I)*(P(JMIN,I)-P(JMIN,I-1))
          VUW(I) = -VFACT * POR * XDIFF(I)*(P(JMAX,I)-P(JMAX,I-1))
        else
          VLW(I) = VFACT * 0.25*(P(JMIN+1,I+1)+2.*P(JMIN+1,I)+P(JMIN+1,I-1) &
                  - P(JMIN  ,I+1)-2.*P(JMIN  ,I)-P(JMIN  ,I-1)) &
                  / (Y(JMIN+1)-Y(JMIN))
          VUW(I) = VFACT * 0.25*(P(JMAX,I+1)  +2.*P(JMAX,I)  +P(JMAX,I-1) &
                  - P(JMAX-1,I+1)-2.*P(JMAX-1,I)-P(JMAX-1,I-1)) &
                  / (Y(JMAX)-Y(JMAX-1))
        end if
        
      case default
        ! Default case
        VLW(I) = 0.0
        VUW(I) = 0.0
      end select
    end do
    
    ! Set up plotting scale
    CPLARG = max(CPMAX, abs(CPMIN))
    UNPCOL = CPLARG / 29.0
    
    ! Locate CP* for printer plot
    COL = -CPSTAR / UNPCOL
    NCOL = sign(int(abs(COL) + 0.5), nint(COL))
    NCOLS = NCOL + 30
    
    ! Print column headers
    write(UNIT_OUTPUT,'(1H0,27X,5HLOWER,23X,5HUPPER/28X,4HY=-H,24X,4HY=+H)')
    write(UNIT_OUTPUT,'(3X,1HI,8X,1HX,10X,2HCP,9X,5HTHETA,12X,2HCP,9X,5HTHETA/)')
    
    ! Print data with character-based plot
    do I = IUP, IDOWN
      ! Initialize line
      do K = 1, 60
        LINE1(K) = IB
      end do
      
      ! Plot upper wall CP
      COL = -CPUW(I) / UNPCOL
      NCOL = sign(int(abs(COL) + 0.5), nint(COL))
      NCOLU = NCOL + 30
      if (NCOLU >= 1 .and. NCOLU <= 60) LINE1(NCOLU) = IU
      
      ! Plot lower wall CP
      COL = -CPLW(I) / UNPCOL
      NCOL = sign(int(abs(COL) + 0.5), nint(COL))
      NCOLL = NCOL + 30
      if (NCOLL >= 1 .and. NCOLL <= 60) LINE1(NCOLL) = IL
      if (NCOLL == NCOLU .and. NCOLL >= 1 .and. NCOLL <= 60) LINE1(NCOLL) = IBB
      
      ! Plot CP* reference line
      if (abs(NCOLS) <= 60 .and. NCOLS >= 1) LINE1(NCOLS) = IS
      
      ! Write formatted output
      write(UNIT_OUTPUT,'(1H ,I3,3F12.6,4X,2F12.6,2X,60A1)') I, X(I), CPLW(I), VLW(I), CPUW(I), VUW(I), LINE1
    end do
    
  end subroutine PRTWAL
    
  ! Compute drag coefficient by momentum integral method
  ! Integrates around a contour enclosing the body and along all shocks inside the contour
  ! CALLED BY - PRINT.
  subroutine CDCOLE()
    use common_data, only: X, Y, IMIN, IMAX, IUP, ILE, ITE, N_MESH_POINTS
    use common_data, only: JMIN, JMAX, JUP, JLOW
    use common_data, only: AK, GAM1, CJUP, CJUP1, CJLOW, CJLOW1
    use common_data, only: CDFACT, YFACT
    use common_data, only: SONVEL, FXL, FXU
    use common_data, only: UNIT_OUTPUT, UNIT_SUMMARY
    use math_module, only: PX, PY, TRAP, FINDSK, NEWISK, DRAG
    implicit none
    
    ! Local variables
    integer :: IU, ID, JT, JB, ISTOP, IBOW, ISK, JSTART, J, JJ, JSK, ISKOLD
    integer :: ILIM, IB, I, L, NSHOCK, LPRT1, LPRT2, ISTART
    real :: GAM123, U, V, UU, UL, SUM, CDSK, CDWAVE, CDC, CD
    real :: CDUP, CDTOP, CDBOT, CDDOWN, CDBODY
    real :: XU_LOC, XD_LOC, YT_LOC, YB_LOC, ULE
    real :: XI(N_MESH_POINTS), ARG(N_MESH_POINTS)
    
    GAM123 = GAM1 * 2.0 / 3.0
    
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
      call FINDSK(IUP, ISTOP, JUP, IBOW)
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
        call NEWISK(ISKOLD, J, ISK)
        if (ISK < 0) exit
      end do
      
      ! Search down shock to find tip of subsonic region
      ISK = IBOW
      JB = JLOW + 2
      do J = JMIN, JLOW
        JJ = JLOW - J + JMIN
        JB = JB - 1
        ISKOLD = ISK
        call NEWISK(ISKOLD, JJ, ISK)
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
        call NEWISK(ISKOLD, J, ISK)
        XI(L) = Y(J)
        ARG(L) = (PX(ISK+1, J) - PX(ISK-2, J))**3
      end do
      call TRAP(XI, ARG, L, SUM)
      CDSK = -GAM1/6.0 * CDFACT * SUM
      CDWAVE = CDWAVE + CDSK
      call PRTSK(XI, ARG, L, NSHOCK, CDSK, LPRT1)
    end if
      
    ! Integrate along shocks above airfoil
    ISTART = ILE
    
    ! Loop to find and process all shocks above airfoil
    do
      call FINDSK(ISTART, ITE, JUP, ISK)
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
        call NEWISK(ISKOLD, JSK, ISK)
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
      call PRTSK(XI, ARG, L, NSHOCK, CDSK, LPRT1)
      if (LPRT1 == 1) LPRT2 = 1
    end do
      
    ! Integrate along shocks below airfoil
    ISTART = ILE
    
    ! Loop to find and process all shocks below airfoil  
    do
      call FINDSK(ISTART, ITE, JLOW, ISK)
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
        call NEWISK(ISKOLD, JSK, ISK)
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
      call PRTSK(XI, ARG, L, NSHOCK, CDSK, LPRT1)
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

  ! Print shock wave drag contributions and total pressure loss along shock wave
  ! PRINTOUT WAVE DRAG CONTRIBUTION AND TOTAL PRESSURE
  ! LOSS ALONG SHOCK WAVE
  ! CALLED BY - CDCOLE.
  subroutine PRTSK(Z,ARG_PARAM,L,NSHOCK,CDSK,LPRT1)
    use common_data, only: CDFACT, GAM1, DELTA, YFACT, UNIT_OUTPUT
    implicit none
    real, intent(in) :: Z(:), ARG_PARAM(:)
    integer, intent(in) :: L, NSHOCK, LPRT1
    real, intent(in) :: CDSK
    real :: CDYCOF, POYCOF, YY, CDY, POY
    integer :: K

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
      YY = Z(K) * YFACT
      CDY = CDYCOF * ARG_PARAM(K)
      POY = 1.0 + POYCOF * ARG_PARAM(K)
      write(UNIT_OUTPUT,'(1X,3F12.8)') YY, CDY, POY
    end do
    
    ! Write footer if shock extends outside contour (format 1004 equivalent)
    if (LPRT1 == 1) then
      write(UNIT_OUTPUT,'(A)') ''  ! blank line for 0 carriage control
      write(UNIT_OUTPUT,'(A)') ' SHOCK WAVE EXTENDS OUTSIDE CONTOUR'
      write(UNIT_OUTPUT,'(A)') ' PRINTOUT OF SHOCK LOSSES ARE NOT AVAILABLE FOR REST OF SHOCK'
    end if
  end subroutine PRTSK

end module io_module
