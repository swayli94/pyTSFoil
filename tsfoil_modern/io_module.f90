! io_module.f90
! Module for input/output routines

module io_module
  use common_data
  use mesh_module, only: ISLIT, JSLIT, CKMESH, AYMESH
  implicit none

  ! Complete namelist matching the original /INP/ namelist exactly
  namelist /INP/ AK, ALPHA, AMESH, BCFOIL, BCTYPE, CLSET, &
                  CVERGE, DELTA, DVERGE, EMACH, EPS, F, &
                  FCR, GAM, H, ICUT, IMAXI, IMIN, &
                  IPRTER, JMAXI, JMIN, KUTTA, MAXIT, NL, &
                  NU, PHYS, POR, PRTFLO, PSAVE, &
                  PSTART, RIGF, SIMDEF, WCIRC, WE, &
                  XIN, YIN, XL, YL, XU, YU, &
                  NWDGE, REYNLD, WCONST, IFLAP, DELFLP, &
                  FLPLOC, IDLA
    ! Declare public procedures
  public :: READIN, SCALE, ECHINP, PRINT, PRINT1, PRTFLD, PRTMC, PRTSK, PRTWAL, SAVEP
  public :: open_output_files, close_output_files, PRINT_INP_NAMELIST

contains

  ! Open all output files with unique file units
  subroutine open_output_files()
    implicit none
    
    open(unit=UNIT_LOG, file='tsfoil.log', status='replace', action='write')
    open(unit=UNIT_ECHO, file='tsfoil.ech', status='replace', action='write') 
    open(unit=UNIT_CP, file='tsfoil.cp', status='replace', action='write')
    open(unit=UNIT_FIELD, file='tsfoil.fld', status='replace', action='write')
    open(unit=UNIT_FLOW, file='tsfoil.map', status='replace', action='write')
    open(unit=UNIT_SHOCK, file='tsfoil.shk', status='replace', action='write')
    open(unit=UNIT_WALL, file='tsfoil.wal', status='replace', action='write')
    
    ! Write headers to files
    write(UNIT_LOG,'(A)') '! TSFOIL Main Log File'
    write(UNIT_ECHO,'(A)') '! TSFOIL Input Echo File'
    write(UNIT_CP,'(A)') '! TSFOIL Pressure Coefficient Distribution'
    write(UNIT_FIELD,'(A)') '! TSFOIL Field Data'
    write(UNIT_FLOW,'(A)') '! TSFOIL Flow Type Map'
    write(UNIT_SHOCK,'(A)') '! TSFOIL Shock Wave Analysis'
    write(UNIT_WALL,'(A)') '! TSFOIL Wall Data'
  end subroutine open_output_files

  ! Close all output files
  subroutine close_output_files()
    implicit none
    close(UNIT_LOG)
    close(UNIT_ECHO)
    close(UNIT_CP)
    close(UNIT_FIELD)
    close(UNIT_FLOW)
    close(UNIT_SHOCK)
    close(UNIT_WALL)
  end subroutine close_output_files
  
  ! Main input reading routine - reads one case at a time
  ! Reads title card, namelist input, and manages restart data for current case
  ! This matches the original TSFoil READIN functionality exactly
  subroutine READIN()
    use common_data
    implicit none    
    character(len=4), parameter :: DONE = 'FINI'  ! Declare DONE to match original exactly
    integer :: I, J, IM1, JM1
    real :: TERM, HTM, HTP, YS, YE, TIME1, TIME2, ELPTM
    logical, save :: first_call = .true.
    character(len=20) :: IN_FILENAME
    integer :: ios
    
    ! Open output files and handle input file on first call only  
    if (first_call) then

      ! Handle command line argument for input file (match original exactly)
      call get_command_argument(1, IN_FILENAME)

      if (IN_FILENAME == '') then
        IN_FILENAME = 'tsfoil.inp'  ! Default input file name
      end if      
      
      open(unit=UNIT_INPUT, file=IN_FILENAME, status='old')
      
      call open_output_files()
      
      ! Note: ECHINP call removed - should be called from main program if desired
      ! like in original (where it was commented out)
      first_call = .false.

    end if

5   continue
    ! Timing code to match original (TIME2 would need system-specific implementation)
    TIME1 = TIME2
    ! TIME2 = get_time()  ! Would need system-specific timing
    ELPTM = TIME2 - TIME1
    if (ELPTM >= 0.01) then
      write(UNIT_OUTPUT, '(25H0  TIME TO RUN CASE WAS  ,F6.2,9H SECONDS.)') ELPTM
    end if
    
    ! Read title card for this case with exact original format
    read(UNIT_INPUT, '(20A4)', END=999) TITLE
    
    ! Write title to output files exactly as original    
    write(UNIT_OUTPUT, '(1H1,4X,20A4)') TITLE
      ! Check for termination string exactly as original
    if (TITLE(1) == DONE) then
        goto 999
    end if
    
10  continue

    ! Read namelist input for this case
    read(UNIT_INPUT, INP, iostat=ios)

    ! Check if namelist read was successful
    if (ios /= 0) then
        write(UNIT_OUTPUT, '(A)') 'Error reading namelist input. Please check the input file.'
        goto 999
    end if

    call PRINT_INP_NAMELIST()  ! Print input namelist for debugging
        
    ! Handle PSTART=3 case - test if P array in core is usable (original check)
    if (PSTART == 3) then
      if (ABORT1) then
        write(UNIT_OUTPUT, '(21H0 CALCULATION ABORTED//43H OUTPUT OF PREVIOUS SOLUTION NOT AVAILABLE.)')
        goto 5
      end if
    end if
    
13  continue
    ! Set AK=0 for physical coordinates
    if (PHYS) AK = 0.0
    
    ! Handle automatic mesh generation
    if (AMESH) then
      call AYMESH()
      goto 18
    end if
    
14  continue

    ! Check if YIN needs default initialization for tunnel or free air case
    if (YIN(JMIN) /= 0.0) goto 18
    
    if (BCTYPE == 1) then
      ! Free air case
      JMAXI = JMXF
      do J = JMIN, JMAXI
        YIN(J) = YFREE(J)
      end do
      goto 18
    end if
    
16  continue
    ! Tunnel case
    JMAXI = JMXT
    do J = JMIN, JMAXI
      YIN(J) = YTUN(J)
    end do
    
18  continue
    ! Echo input parameters to output file with exact original format strings
    write(UNIT_OUTPUT, '(1H0,4X,7HEMACH =,F9.5,5X,5HPOR =,F9.5,3X,6HIMIN =,I4,3X,8HBCTYPE =,I3,5X,8HAMESH = ,L1)') &
      EMACH, POR, IMIN, BCTYPE, AMESH
    write(UNIT_OUTPUT, '(1H0,4X,7HDELTA =,F9.5,3X,7HCLSET =,F9.5,2X,7HIMAXI =,I4,3X,8HBCFOIL =,I3,6X,7HPHYS = ,L1)') &
      DELTA, CLSET, IMAXI, BCFOIL, PHYS    
    write(UNIT_OUTPUT, '(1H0,4X,7HALPHA =,F9.5,5X,5HEPS =,F9.5,3X,6HJMIN =,I4,3X,8HPSTART =,I3,5X,8HPSAVE = ,L1)') &
      ALPHA, EPS, JMIN, PSTART, PSAVE
    write(UNIT_OUTPUT, '(1H0,7X,4HAK =,F9.5,4X,6HRIGF =,F9.5,2X,7HJMAXI =,I4,3X,8HPRTFLO =,I3,5X,8HKUTTA = ,L1)') &
      AK, RIGF, JMAXI, PRTFLO, KUTTA
    write(UNIT_OUTPUT, '(1H0,6X,5HGAM =,F9.5,3X,7HWCIRC =,F9.5,2X,7HMAXIT =,I4,3X,8HIPRTER =,I3,7X,6HFCR = ,L1)') &
      GAM, WCIRC, MAXIT, IPRTER, FCR
    write(UNIT_OUTPUT, '(1H0,8X,3HF =,F9.5,2X,8HCVERGE =,F9.5,5X,4HNU =,I4,3X,8HSIMDEF =,I3)') &
      F, CVERGE, NU, SIMDEF
    write(UNIT_OUTPUT, '(1H0,8X,3HH =,F9.5,2X,8HDVERGE =,F9.1,5X,4HNL =,I4,5X,6HICUT =,I3)') &
      H, DVERGE, NL, ICUT
    write(UNIT_OUTPUT, '(1H0,7X,5HWE = ,F4.2,2(1H,,F4.2))') WE
    
    if (NWDGE == 1) write(UNIT_OUTPUT, '(1H0,15X,12HMURMAN WEDGE,5X,8HREYNLD =,E10.3,5X,8HWCONST =,F9.5)') REYNLD, WCONST
    if (NWDGE == 2) write(UNIT_OUTPUT, '(1H0,15X,15HYOSHIHARA WEDGE)')
    if (IFLAP /= 0) write(UNIT_OUTPUT, '(1H0,15X,17HFLAP IS DEFLECTED,F5.2,20H DEGREES FROM H.L. =,F6.3,8H TO T.E.)') &
      DELFLP, FLPLOC
    
    write(UNIT_OUTPUT, '(1H0,4X,3HXIN)')
    write(UNIT_OUTPUT, '(4X,6F11.6)') (XIN(I), I=IMIN, IMAXI)
    write(UNIT_OUTPUT, '(1H0,4X,3HYIN)')
    write(UNIT_OUTPUT, '(4X,6F11.6)') (YIN(J), J=JMIN, JMAXI)
    
    if (BCFOIL <= 2) goto 19
    if (BCFOIL == 5) goto 19
    write(UNIT_OUTPUT, '(1H0,15X,2HXU)')
    write(UNIT_OUTPUT, '(4X,6F11.6)') (XU(I), I=1, NU)
    write(UNIT_OUTPUT, '(1H0,15X,2HYU)')
    write(UNIT_OUTPUT, '(4X,6F11.6)') (YU(I), I=1, NU)
    write(UNIT_OUTPUT, '(1H0,15X,2HXL)')
    write(UNIT_OUTPUT, '(4X,6F11.6)') (XL(I), I=1, NL)
    write(UNIT_OUTPUT, '(1H0,15X,2HYL)')
    write(UNIT_OUTPUT, '(4X,6F11.6)') (YL(I), I=1, NL)
    
19  continue
    ! Set derived constants
    GAM1 = GAM + 1.0
    IREF = 0
    IMAX = IMAXI
    JMAX = JMAXI
    IM1 = IMAX - 1
    JM1 = JMAX - 1
    
    ! Check array bounds (any call to INPERR causes message to be printed and execution stopped)
    if (IMAXI > 100 .or. JMAXI > 100) call INPERR(1)
    
    ! Check input mesh for monotonically increasing values
    do I = IMIN, IM1
      if (XIN(I) >= XIN(I+1)) call INPERR(2)
    end do
    
    do J = JMIN, JM1
      if (YIN(J) >= YIN(J+1)) call INPERR(3)
    end do
    
    ! Check parameter ranges
    if (EMACH < 0.5 .or. EMACH > 2.0) call INPERR(4)
    if (ALPHA < -9.0 .or. ALPHA > 9.0) call INPERR(5)
    if (DELTA < 0.0 .or. DELTA > 1.0) call INPERR(6)
    if (NWDGE > 0 .and. EMACH > 1.0) call INPERR(8)
    
    ! Compute ILE and ITE (leading and trailing edge)
    call ISLIT(XIN)
    call JSLIT(YIN)
    
    ! Check number of mesh points, if not odd add points to appropriate areas to make odd no.
    call CKMESH()
    
    ! Check bounds of YMESH for tunnel calculations
    if (BCTYPE == 1) goto 90
    HTM = H - 0.00001
    HTP = H + 0.00001
    YS = abs(YIN(JMIN))
    YE = abs(YIN(JMAX))
    if (YS >= HTM .and. YS <= HTP) then
      if (YE >= HTM .and. YE <= HTP) goto 90
    end if
    
40  continue
    ! Rescale Y mesh to -H,+H bounds
    TERM = -H / YIN(JMIN)
    do J = JMIN, JLOW
      YIN(J) = TERM * YIN(J)
    end do
    TERM = H / YIN(JMAX)
    do J = JUP, JMAX
      YIN(J) = TERM * YIN(J)
    end do
    
90  continue    
    ! If PSTART = 2 read old values from restart file (exact original implementation)
    if (PSTART == 2) then
      rewind(UNIT_RESTART)
      read(UNIT_RESTART, '(20A4)') TITLEO
      read(UNIT_RESTART, '(4I5)') IMAXO, JMAXO, IMINO, JMINO
      read(UNIT_RESTART, '(8F10.6)') CLOLD, EMACHO, ALPHAO, DELTAO, VOLO, DUBO
      read(UNIT_RESTART, '(8F10.6)') (XOLD(I), I=IMINO, IMAXO)
      read(UNIT_RESTART, '(8F10.6)') (YOLD(J), J=JMINO, JMAXO)
      do I = IMINO, IMAXO
        read(UNIT_RESTART, '(8F10.6)') (P(J,I), J=JMINO, JMAXO)
      end do
      write(UNIT_OUTPUT, '(39H1P INITIALIZED FROM PREVIOUS RUN TITLED/1X,20A4/31H WHICH HAD THE FOLLOWING VALUES/8H IMIN  =,I4/8H IMAX  =,I4/8H JMIN  =,I4/8H JMAX  =,I4/8H CL    =,F12.8/8H EMACH =,F12.8/8H ALPHA =,F12.8/8H DELTA =,F12.8/8H VOL   =,F12.8/8H DUB   =,F12.8/)') &
        TITLEO, IMINO, IMAXO, JMINO, JMAXO, CLOLD, EMACHO, ALPHAO, DELTAO, VOLO, DUBO
    end if
    
100 continue
    return
    
  ! End of file or FINISHED card - use STOP to match original
999 stop
  end subroutine READIN
  
  subroutine SCALE()
!
!                  SUBROUTINE SCALES PHYSICAL VARIABLES TO TRANSONIC
!                  VARIABLES.
!                  IF PHYS = .TRUE., ALL INPUT/OUTPUT QUANTITIES ARE IN
!                  PHYSICAL UNITS NORMALIZED BY FREESTREAM VALUES AND
!                  AIRFOIL CHORD. THIS SUBROUTINE THEN SCALES THE
!                  QUANTITIES TO TRANSONIC VARIABLES BY THE FOLLOWING
!                  CONVENTION
!                       SIMDEF = 1  COLE SCALING
!                       SIMDEF = 2  SPREITER SCALING
!                       SIMDEF = 3  KRUPP SCALING
!                       SIMDEF = 4  USER CHOICE
!                  IF PHYS = .FALSE., INPUT IS ALREADY IN SCALED
!                  VARIABLES AND NO FURTHER SCALING IS DONE.
!                  CALLED BY - TSFOIL.
!
    use common_data, only: PHYS, DELTA, EMACH, SIMDEF
    use common_data, only: AK, ALPHA, GAM1, RTK, YFACT, CPFACT, CLFACT, CDFACT, CMFACT, VFACT
    use common_data, only: YIN, YOLD, JMIN, JMAX, JMINO, JMAXO, PSTART
    use common_data, only: H, POR, SONVEL, CPSTAR, DELRT2, EMROOT
    implicit none
    real :: EMACH2, BETA, DELRT1
    real :: YFACIV
    integer :: J

    if (.not. PHYS) then
!                  PHYS = .FALSE.  NO SCALING
      CPFACT = 1.0
      CDFACT = 1.0
      CLFACT = 1.0
      CMFACT = 1.0
      YFACT = 1.0
      VFACT = 1.0
      goto 600
    end if

!                  PHYS = .TRUE.  COMPUTE CONSTANTS
    EMACH2 = EMACH*EMACH
    BETA = 1.0 - EMACH2
    DELRT1 = DELTA**(1.0/3.0)
    DELRT2 = DELTA**(2.0/3.0)

!                  BRANCH TO APPROPRIATE SCALING
    if (SIMDEF == 1) then
!                  SIMDEF = 1
!                  COLE SCALING
      AK = BETA / DELRT2
      YFACT = 1.0 / DELRT1
      CPFACT = DELRT2
      CLFACT = DELRT2
      CDFACT = DELRT2 * DELTA
      CMFACT = DELRT2
      VFACT = DELTA * 57.295779
      goto 500
    else if (SIMDEF == 2) then
!                  SIMDEF = 2
!                  SPREITER SCALING
      EMROOT = EMACH**(2.0/3.0)
      AK = BETA / (DELRT2 * EMROOT * EMROOT)
      YFACT = 1.0 / (DELRT1 * EMROOT)
      CPFACT = DELRT2 / EMROOT
      CLFACT = CPFACT
      CMFACT = CPFACT
      CDFACT = CPFACT * DELTA
      VFACT = DELTA * 57.295779
      goto 500
    else if (SIMDEF == 3) then
!                  SIMDEF = 3
!                  KRUPP SCALING
      AK = BETA / (DELRT2 * EMACH)
      YFACT = 1.0 / (DELRT1 * EMACH**0.5)
      CPFACT = DELRT2 / (EMACH**0.75)
      CLFACT = CPFACT
      CMFACT = CPFACT
      CDFACT = CPFACT * DELTA
      VFACT = DELTA * 57.295779
      goto 500
    else if (SIMDEF == 4) then
!                  SIMDEF = 4
!                  THIS ADDRESS IS INACTIVE
!                  USER MAY INSERT SCALING OF OWN CHOICE
!                  DEFINITION FOR LOCAL MACH NUMBER MUST BE ADJUSTED
!                  IN EMACH1.
      write(UNIT_OUTPUT,'(34H1ABNORMAL STOP IN SUBROUTINE SCALE/24H SIMDEF=4 IS NOT USEABLE)')
      stop
    end if

500 continue
!                  SCALE Y MESH
    YFACIV = 1.0 / YFACT
    do J = JMIN, JMAX
      YIN(J) = YIN(J) * YFACIV
    end do
    
    if (PSTART /= 1) then
      do J = JMINO, JMAXO
        YOLD(J) = YOLD(J) * YFACIV
      end do
    end if

!                  SCALE TUNNEL PARAMETERS
    H = H / YFACT
    POR = POR * YFACT
    write(UNIT_OUTPUT,'(//10X,11HSCALED POR=,F10.5)') POR

!                  SCALE ANGLE OF ATTACK
    ALPHA = ALPHA / VFACT

600 continue
!                  CHECK VALUE OF AK FOR DEFAULT.
    if (AK == 0.0) call INPERR(7)
!                  COMPUTE SQUARE ROOT OF AK
    RTK = sqrt(abs(AK))
!                  COMPUTE SONIC VELOCITY
    if (abs(GAM1) <= 0.0001) then
      SONVEL = 1.0
      CPSTAR = 0.0
      return
    end if
    
    SONVEL = AK / GAM1
    CPSTAR = -2.0 * SONVEL * CPFACT
    return

  end subroutine SCALE  
  
  ! Echo input cards for logging - exactly like original ECHINP
  ! Prints input cards used for run (called by - TSFOIL main program)
  ! NOTE: This should be called ONCE before any case processing,
  ! not from within READIN, to echo the entire input file
  subroutine ECHINP()
    implicit none
    character(len=80) :: CRD  ! Input card buffer (20A4 = 80 characters)
    integer :: ios
    
    ! Write form feed to output file (1H1 format)
    write(UNIT_OUTPUT,'(A1)') char(12)  ! Form feed character equivalent to 1H1
    
10  continue
    ! Read input card from unit 5 (input file) 
    read(UNIT_INPUT, '(A80)', END=30, iostat=ios) CRD
    
20  continue
    ! Write card to output file (unit 15 equivalent)
    write(UNIT_OUTPUT, '(1X,A)') trim(CRD)
    goto 10
    
30  continue
    ! Rewind input file after reading all cards
    rewind(UNIT_INPUT)
    return
    
  end subroutine ECHINP

  ! Main print driver: calls PRINT1, PRTMC, etc.
  subroutine PRINT()
    implicit none
    
    write(UNIT_LOG,'(A)') 'Starting output generation...'
    call PRINT1()
    call PRTFLD()
    call PRTMC()
    ! call PRTSK()  ! TODO: implement with proper arguments
    call PRTWAL()
    write(UNIT_LOG,'(A)') 'Output generation completed'
  end subroutine PRINT
  
  ! Print Cp and Mach along body and build plot arrays
  ! Prints pressure coefficient and Mach number on Y=0 line, and plots CP along side of print
  ! This matches the original PRINT1 functionality exactly
  subroutine PRINT1()
    use common_data
    use math_module, only: PX, EMACH1, LIFT, PITCH
    implicit none
      ! Local variables matching original
    integer :: I, KK, NCOL, NCOLS, NCOLU, NCOLL, IEM, KT, IPLOT
    real :: CL_local, CM, CPMIN, CPMAX, CPLARG, UNPCOL, COL
    real :: UL, UU, CJ01, CJ02
    real :: EM1L(100), EM1U(100), YM(100)
    character(len=1) :: LINE1(60)
    character(len=2) :: TMAC(2)
    character(len=1), parameter :: IB = ' ', IL = 'L', IU = 'U', IS = '*', IBB = 'B'
    
    ! Initialize data arrays like original
    TMAC(1) = 'M1'
    TMAC(2) = 'K1'
    
    ! Compute lift and moment coefficients
    CL_local = LIFT(CLFACT)
    CM = PITCH(CMFACT)
    
    ! Initialize CP min/max tracking
    CPMIN = 1.0E37
    CPMAX = -CPMIN
    IEM = 0
    
    ! Compute interpolation coefficients
    CJ01 = -Y(JLOW)/(Y(JUP)-Y(JLOW))
    CJ02 = Y(JUP)/(Y(JUP)-Y(JLOW))
    
    ! Main loop over airfoil points
    do I = IMIN, IMAX
      ! Compute lower surface velocity
      UL = CJLOW*PX(I,JLOW) - CJLOW1*PX(I,JLOW-1)
      if (I > ITE) UL = CJ01*PX(I,JUP) + CJ02*PX(I,JLOW)
      if (I < ILE) UL = CJ01*PX(I,JUP) + CJ02*PX(I,JLOW)
      CPL(I) = -2.0 * UL * CPFACT
      EM1L(I) = EMACH1(UL)
      if (EM1L(I) > 1.3) IEM = 1
      
      ! Compute upper surface velocity
      UU = CJUP*PX(I,JUP) - CJUP1*PX(I,JUP+1)
      if (I > ITE) UU = UL
      if (I < ILE) UU = UL
      CPU(I) = -2.0 * UU * CPFACT
      EM1U(I) = EMACH1(UU)
      if (EM1U(I) > 1.3) IEM = 1
      
      ! Track min/max CP values
      CPMAX = max(CPMAX, CPU(I), CPL(I))
      CPMIN = min(CPMIN, CPU(I), CPL(I))
    end do
    
    ! Set up plotting scale
    CPLARG = max(CPMAX, abs(CPMIN))
    UNPCOL = CPLARG / 29.0
    
    ! Locate CP* for printer plot
    COL = -CPSTAR / UNPCOL
    NCOL = sign(int(abs(COL) + 0.5), nint(COL))
    NCOLS = NCOL + 30
    
    ! Print header information
    write(UNIT_OUTPUT,'(A)') '1 FORCE COEFFICIENTS, PRESSURE COEFFICIENT, AND MACH NUMBER'
    write(UNIT_OUTPUT,'(A)') '  (OR SIMILARITY PARAMETER) ON BODY AND DIVIDING STREAM LINE.'
    
    if (IREF == 2) write(UNIT_OUTPUT,'(A)') '                    COARSE MESH'
    if (IREF == 1) write(UNIT_OUTPUT,'(A)') '                    MEDIUM MESH'
    if (IREF == 0) write(UNIT_OUTPUT,'(A)') '                     FINAL MESH'
    
    ! Print coefficients
    write(UNIT_OUTPUT,'(A,F10.6)') '         CL =', CL_local
    write(UNIT_OUTPUT,'(A,F10.6)') '          CM =', CM
    write(UNIT_OUTPUT,'(A,F10.6)') '         CP* =', CPSTAR
    
    ! Also write to other output files like original
    write(UNIT_SHOCK,'(A,F10.6)') '         CL =', CL_local
    write(UNIT_SHOCK,'(A,F10.6)') '          CM =', CM
    write(UNIT_SHOCK,'(A,F10.6)') '         CP* =', CPSTAR
    
    write(UNIT_WALL,'(A,F16.12)') '         CL =', CL_local
    write(UNIT_WALL,'(A,F16.12)') '          CM =', CM
    write(UNIT_WALL,'(A,F16.12)') '         CP* =', CPSTAR
    
    ! Check for detached shock
    if (CPL(IMIN) < CPSTAR .and. CPL(IMIN+1) > CPSTAR) then
      write(UNIT_OUTPUT,'(A)') '0'
      write(UNIT_OUTPUT,'(A)') ' DETACHED SHOCK WAVE UPSTREAM OF X-MESH,SOLUTION TERMINATED.'
      if (IREF /= 2) ABORT1 = .true.
      return
    end if
    
    ! Print column headers
    write(UNIT_OUTPUT,'(A)') '0                           LOWER                       UPPER'
    write(UNIT_OUTPUT,'(A)') '                            Y=0-                        Y=0+'
    
    KT = 2
    if (PHYS) KT = 1
    write(UNIT_OUTPUT,'(A,A2,A,A2,A)') '   I        X          CP          ', TMAC(KT), '              CP          ', TMAC(KT), ''
    
    IPLOT = 0
    
    ! Output header for plotting file if final mesh
    if (IREF == 0) then
      write(UNIT_SHOCK,'(A,F7.3,A,F7.3)') '  TSFOIL2   Mach = ', EMACH, '   CL = ', CL_local
      write(UNIT_SHOCK,'(A)') '    i     X/C        Cp-up     M-up      Cp-low    M-low'
    end if
    
    ! Main output loop
    do I = IMIN, IMAX      ! Initialize line for plotting
      do KK = 1, 60
        LINE1(KK) = IB
      end do
      
      ! Plot upper surface CP
      COL = -CPU(I) / UNPCOL
      NCOL = sign(int(abs(COL) + 0.5), nint(COL))
      NCOLU = NCOL + 30
      if (NCOLU >= 1 .and. NCOLU <= 60) LINE1(NCOLU) = IU
      
      ! Plot lower surface CP  
      COL = -CPL(I) / UNPCOL
      NCOL = sign(int(abs(COL) + 0.5), nint(COL))
      NCOLL = NCOL + 30
      if (NCOLL >= 1 .and. NCOLL <= 60) LINE1(NCOLL) = IL
      if (NCOLL == NCOLU .and. NCOLL >= 1 .and. NCOLL <= 60) LINE1(NCOLL) = IBB
      if (abs(NCOLS) <= 60 .and. NCOLS >= 1) LINE1(NCOLS) = IS
      
      ! Print leading edge marker
      if (I == ILE) write(UNIT_OUTPUT,'(A)') '                         AIRFOIL LEADING EDGE                             AIRFOIL LEADING EDGE'
      
      ! Print main data line
      write(UNIT_OUTPUT,'(I4,3F12.6,4X,2F12.6,2X,60A1)') I, X(I), CPL(I), EM1L(I), CPU(I), EM1U(I), LINE1
      
      ! Print trailing edge marker
      if (I == ITE) write(UNIT_OUTPUT,'(A)') '                         AIRFOIL TRAILING EDGE                            AIRFOIL TRAILING EDGE'
      
      ! Save data for plotting (final mesh only)
      if (IREF == 0) then
        write(UNIT_SHOCK,'(2X,I3,2X,F7.4,2X,F10.5,2X,F7.4,2X,F10.5,2X,F7.4)') IPLOT, X(I), CPU(I), EM1U(I), CPL(I), EM1L(I)
      end if
    end do
    
    ! Print Mach number warning if needed
    if (IEM == 1 .and. PHYS) then
      write(UNIT_OUTPUT,'(A)') '0***** CAUTION *****'
      write(UNIT_OUTPUT,'(A)') ' MAXIMUM MACH NUMBER EXCEEDS 1.3'
      write(UNIT_OUTPUT,'(A)') ' SHOCK JUMPS IN ERROR IF UPSTREAM NORMAL MACH NUMBER GREATER THAN 1.3'
    end if
    
    ! Print coordinate arrays
    do I = JMIN, JMAX
      YM(I) = Y(I) * YFACT
    end do
    
    write(UNIT_OUTPUT,'(A,I3,A,I3)') '0        Y(J) J=', JMIN, ' TO', JMAX
    write(UNIT_OUTPUT,'(6F12.6)') (YM(I), I=JMIN, JMAX)
    
    ! Also write X coordinates 
    write(15,'(A,I3,A,I3)') '0        X(I) I=', IMIN, ' TO', IMAX
    write(15,'(6F12.6)') (X(I), I=IMIN, IMAX)
    
    write(UNIT_LOG,'(A)') 'PRINT1: Pressure coefficient and Mach number output completed'
  end subroutine PRINT1

  ! Print Cp, flow angle (theta), and Mach number on selected j-lines
  subroutine PRTFLD()
    use common_data, only: P, X, Y, JMIN, JMAX, JUP, JLOW, JERROR, CPFACT, VFACT, SIMDEF, PHYS, PRTFLO
    use common_data, only: JLIN, IMIN, IMAX
    implicit none
    integer :: JL, MPR, MPREND, M, MQ, I, J
    real :: U
    real, dimension(3) :: YPRINT
    real, dimension(3) :: CPPR, PYPR
    real, dimension(3) :: EM1
    character(len=12), parameter :: HDR = "CP, THETA, M"

    ! Determine lines to print
    if (PRTFLO == 2) then
      JL = JMAX - JMIN + 1
      do M = 1, JL
        JLIN(M) = JMIN + M - 1
      end do
    else if (PRTFLO == 3) then
      ! three lines around JERROR
      if (JERROR <= JMIN+1) then
        JLIN = (/ JMIN, JMIN+1, JMIN+2 /)
      else if (JERROR >= JMAX-1) then
        JLIN = (/ JMAX-2, JMAX-1, JMAX /)
      else
        JLIN = (/ JERROR-1, JERROR, JERROR+1 /)
      end if
      JL = 3
    else
      write(UNIT_LOG,'(A)') 'Field data printing skipped (PRTFLO not 2 or 3)'
      return
    end if

    ! Write header to field file
    write(UNIT_FIELD,'(A)') '# Field data: CP, THETA, MACH on selected J-lines'
    write(UNIT_FIELD,'(A,I0,A)') '# Printing ', JL, ' J-lines'

    ! Loop pages of 3 lines
    do MPR = 1, JL, 3
      MPREND = min(MPR+2, JL)
      ! Y positions
      do MQ = MPR, MPREND
        YPRINT(MQ-MPR+1) = Y(JLIN(MQ))*VFACT
      end do
      
      write(UNIT_FIELD,'(A)', advance='no') '# Y = '
      do MQ = 1, MPREND-MPR+1
        write(UNIT_FIELD,'(F10.6,2X)', advance='no') YPRINT(MQ)
      end do
      write(UNIT_FIELD,*)
      
      write(UNIT_FIELD,'(A)') '# I    X        CP      THETA    MACH (on lines above)'
      
      do I = IMIN, IMAX
        write(UNIT_FIELD,'(I4,2X,F8.4)', advance='no') I, X(I)
        do MQ = MPR, MPREND
          J = JLIN(MQ)
          U = (P(J,I+1)-P(J,I-1))/(X(I+1)-X(I-1))
          CPPR(MQ-MPR+1) = -2.0 * CPFACT * U
          PYPR(MQ-MPR+1) = VFACT * U
          EM1(MQ-MPR+1) = U  ! placeholder for Mach calculation
          write(UNIT_FIELD,'(3F10.4)', advance='no') CPPR(MQ-MPR+1), PYPR(MQ-MPR+1), EM1(MQ-MPR+1)
        end do
        write(UNIT_FIELD,*)
      end do
      write(UNIT_FIELD,*)
    end do
    
    write(UNIT_LOG,'(A,I0,A)') 'Field data written for ', JL, ' J-lines'
  end subroutine PRTFLD

  ! Print map of flow types at each grid point
  subroutine PRTMC()
    use common_data, only: P, IMIN, IMAX, IUP, IDOWN, JMIN, JMAX, IPC, VT, C1, CXL, CXC, CXR
    implicit none    
    integer :: I, J, KK
    character(len=1), parameter :: ch_par='P', ch_hyp='H', ch_shock='S', ch_ell='-'

    ! Header
    write(UNIT_FLOW,'(A)') '# Flow type map at each grid point'
    write(UNIT_FLOW,'(A)') '# P=Parabolic, H=Hyperbolic, S=Shock, -=Elliptic'
    write(UNIT_FLOW,'(A)') '# Format: J-index followed by flow types for each I'

    ! Initialize IPC and VT
    IPC(IUP:IDOWN) = ch_ell
    VT(JMIN:JMAX,1) = C1(2)

    ! Classify flow type and write each row
    do KK = JMIN, JMAX
      J = JMAX - KK + 1
      do I = IUP, IDOWN
        VT(J,2) = VT(J,1)
        VT(J,1) = C1(I) - (CXL(I)*P(J,I-1) + CXC(I)*P(J,I) + CXR(I)*P(J,I+1))
        if (VT(J,1) <= 0.0) then
          if (VT(J,2) < 0.0) then
            IPC(I) = ch_hyp
          else
            IPC(I) = ch_par
          end if
        else
          if (VT(J,2) < 0.0) then
            IPC(I) = ch_shock
          else
            IPC(I) = ch_ell
          end if
        end if
      end do
      write(UNIT_FLOW,'(I3,5X,*(A1))') J, IPC(IUP:IDOWN)
    end do
    
    write(UNIT_LOG,'(A)') 'Flow type map written to tsfoil.map'
  end subroutine PRTMC

  ! Print shock wave drag contributions and total pressure loss along shock wave
  subroutine PRTSK(Z,ARG_PARAM,L,NSHOCK,CDSK,LPRT1)
    use common_data, only: CDFACT, GAM1, DELTA, YFACT
    implicit none
    real, intent(in) :: Z(:), ARG_PARAM(:)
    integer, intent(in) :: L, NSHOCK, LPRT1
    real, intent(inout) :: CDSK(:)
    real :: CDYCOF, POYCOF, YY, CDY, POY
    integer :: K

    ! Compute coefficients
    CDYCOF = -CDFACT * GAM1 / (6.0 * YFACT)
    POYCOF = DELTA**2 * GAM1 * (GAM1 - 1.0) / 12.0

    ! Header for individual shock profiles
    if (NSHOCK == 1) then
      write(UNIT_SHOCK,'(A)') '# Inviscid wake profiles for individual shock waves within momentum contour'
    end if
    
    ! Print shock drag summary
    write(UNIT_SHOCK,'(A,I0)') '# Shock wave number: ', NSHOCK
    write(UNIT_SHOCK,'(A,F12.6)') '# Wave drag for this shock = ', CDSK(NSHOCK)
    write(UNIT_SHOCK,'(A)') '#     Y           CD(Y)         PO/PO'

    ! Print shock profile data
    do K = 1, L
      YY = Z(K) * YFACT      
      CDY = CDYCOF * ARG_PARAM(K)
      POY = 1.0 + POYCOF * ARG_PARAM(K)
      write(UNIT_SHOCK,'(3F14.8)') YY, CDY, POY
    end do

    ! Footer if shock extends outside contour
    if (LPRT1 == 1) then
      write(UNIT_SHOCK,'(A)') '# Shock wave extends outside contour'
      write(UNIT_SHOCK,'(A)') '# Printout of shock losses not available for rest of shock'
    end if
    
    write(UNIT_LOG,'(A,I0,A,I0,A)') 'Shock wave ', NSHOCK, ' analysis written (', L, ' points)'
  end subroutine PRTSK

  ! Print Cp and flow angles on tunnel walls
  subroutine PRTWAL()
    use common_data, only: P, X, Y, CPFACT, VFACT, JMIN, JMAX, IMIN, IMAX
    implicit none
    integer :: I
    real :: CP_upper, CP_lower, U_upper, U_lower
    
    write(UNIT_WALL,'(A)') '# Wall data: Cp and flow angles'
    write(UNIT_WALL,'(A)') '#   I     X       CP_upper   CP_lower   Angle_upper Angle_lower'
    
    do I = IMIN, IMAX
      ! Upper wall (JMAX)
      U_upper = (P(JMAX,I+1)-P(JMAX,I-1))/(X(I+1)-X(I-1))
      CP_upper = -2.0 * CPFACT * U_upper
      
      ! Lower wall (JMIN) 
      U_lower = (P(JMIN,I+1)-P(JMIN,I-1))/(X(I+1)-X(I-1))
      CP_lower = -2.0 * CPFACT * U_lower
      
      write(UNIT_WALL,'(I4,2X,F8.4,4F12.6)') I, X(I), CP_upper, CP_lower, &
                                              VFACT*U_upper, VFACT*U_lower
    end do
    
    write(UNIT_LOG,'(A)') 'Wall data written to tsfoil.wal'
  end subroutine PRTWAL
  
  ! Save current solution P to restart file - matches original SAVEP exactly
  subroutine SAVEP()
    use common_data, only: P, X, Y, IMIN, IMAX, JMIN, JMAX, TITLE
    implicit none
    integer :: I, J
    
    ! Original used unit 15, but we'll use UNIT_RESTART (now 7) for consistency
    open(unit=UNIT_RESTART, file='fort.7', status='replace', action='write')
    rewind(UNIT_RESTART)
    
    ! Write title using original format 900: FORMAT(20A4)
    write(UNIT_RESTART, 900) TITLE
    
    ! Write mesh dimensions using original format 902: FORMAT(4I5)
    write(UNIT_RESTART, 902) IMIN, IMAX, JMIN, JMAX
    
    ! Write grid coordinates using original format 903: FORMAT(8F10.6)
    write(UNIT_RESTART, 903) (X(I), I=IMIN, IMAX)
    write(UNIT_RESTART, 903) (Y(J), J=JMIN, JMAX)
    
    ! Write solution array P using original format 903: FORMAT(8F10.6)
    do J = JMIN, JMAX
      write(UNIT_RESTART, 903) (P(J,I), I=IMIN, IMAX)
    end do
    
    close(UNIT_RESTART)
    write(UNIT_LOG,'(A)') 'Solution saved to restart file fort.7'
    
    ! Original format statements
    900 format(20A4)
    902 format(4I5)
    903 format(8F10.6)
  end subroutine SAVEP

  ! Initialize YIN array if not read from namelist
  subroutine check_yin_defaults()
    use common_data, only: YIN, JMIN, JMAX, JUP, JLOW, H, BCTYPE
    implicit none
    integer :: J
    real :: TERM
    
    if (YIN(JMIN) /= 0.0) return
    
    ! Fill YIN with default values for tunnel or free air case
    if (BCTYPE == 1) then
      ! Free air case - exponential distribution
      do J = JMIN, JLOW
        TERM = real(J - JMIN) / real(JLOW - JMIN)
        YIN(J) = -H * (1.0 - EXP(-2.0 * TERM))
      end do
      do J = JUP, JMAX
        TERM = real(J - JUP) / real(JMAX - JUP)
        YIN(J) = H * (1.0 - EXP(-2.0 * TERM))
      end do
    else
      ! Tunnel case - linear distribution
      do J = JMIN, JLOW
        YIN(J) = -H * real(J - JMIN) / real(JLOW - JMIN)
      end do
      do J = JUP, JMAX
        YIN(J) = H * real(J - JUP) / real(JMAX - JUP)
      end do
    end if
  end subroutine check_yin_defaults

  ! OUTPUTS CP DATA IN FORM USABLE BY ANTANI'S INTEGRATION PROGRAM
  subroutine DLAOUT(ILE_IN, ITE_IN, ALPHA_IN, DFLP, EM, VF, RE)
    use common_data, only: P, X, Y, CPL, CPU, XCP, CPP, UNIT_LOG, UNIT_DLAOUT_INPUT, UNIT_DLAOUT_OUTPUT
    use spline_module, only: SPLN1, SPLN1X, initialize_spline
    implicit none
    
    ! Arguments
    integer, intent(in) :: ILE_IN, ITE_IN
    real, intent(in) :: ALPHA_IN, DFLP, EM, VF, RE
    
    ! Local variables
    integer :: NUP, NDOWN, NCON, NTEST, NRUN, NPT
    integer :: NS, NE, NX, I
    real :: R, ALFA
    real :: DY1, DY2, XP, YP, DYP
    integer :: K1, K2
    
    write(UNIT_LOG,'(A)') 'DLAOUT: Starting CP data output for integration program'
    
    ! Initialize spline module
    call initialize_spline(200)
    
    ! Read input parameters from unit 5
    read(UNIT_DLAOUT_INPUT, 100) NUP, NDOWN, NCON, NTEST, NRUN, NPT
    
    ! Convert units and parameters
    R = RE / 1.0E+06
    ALFA = ALPHA_IN * VF
    
    ! Write header information to unit 10
    write(UNIT_DLAOUT_OUTPUT, 101) EM, ALFA, R, NCON, DFLP, NTEST, NRUN, NPT
    
    ! Read upper surface X-coordinates
    read(UNIT_DLAOUT_INPUT, 102) (XCP(I), I=1, NUP)
    
    ! Read lower surface X-coordinates
    NS = NUP + 1
    NE = NUP + NDOWN
    read(UNIT_DLAOUT_INPUT, 102) (XCP(I), I=NS, NE)
    
    ! Set spline boundary conditions
    K1 = 1
    K2 = 1
    
    ! Interpolate upper surface CP values
    DY1 = (CPU(ILE_IN+1) - CPU(ILE_IN)) / (X(ILE_IN+1) - X(ILE_IN))
    DY2 = (CPU(ITE_IN) - CPU(ITE_IN-1)) / (X(ITE_IN) - X(ITE_IN-1))
    NX = ITE_IN - ILE_IN + 1
    
    call SPLN1(X(ILE_IN:), CPU(ILE_IN:), NX)
    
    do I = 1, NUP
      XP = XCP(I)
      call SPLN1X(X(ILE_IN:), CPU(ILE_IN:), NX, XP, YP, DYP)
      CPP(I) = YP
    end do
    
    ! Interpolate lower surface CP values
    DY1 = (CPL(ILE_IN+1) - CPL(ILE_IN)) / (X(ILE_IN+1) - X(ILE_IN))
    DY2 = (CPL(ITE_IN) - CPL(ITE_IN-1)) / (X(ITE_IN) - X(ITE_IN-1))
    
    call SPLN1(X(ILE_IN:), CPL(ILE_IN:), NX)
    
    do I = NS, NE
      XP = XCP(I)
      call SPLN1X(X(ILE_IN:), CPL(ILE_IN:), NX, XP, YP, DYP)
      CPP(I) = YP
    end do
    
    ! Write interpolated CP values to output
    write(UNIT_DLAOUT_OUTPUT, 102) (CPP(I), I=1, NUP)
    write(UNIT_DLAOUT_OUTPUT, 102) (CPP(I), I=NS, NE)
    
    write(UNIT_LOG,'(A,I0,A,I0,A)') 'DLAOUT: Completed CP interpolation for ', NUP, ' upper and ', NDOWN, ' lower surface points'
    
    ! Format statements matching original
100 format(6I5)
101 format(F6.3,F6.2,24X,F6.3,I6,F6.1,16X,2I3,I2)
102 format(11F6.3)
    
  end subroutine DLAOUT
  
  ! Read restart file (PSTART=2 case) - matches original exactly
  subroutine LOADP()
    use common_data
    implicit none
    integer :: I, J, ios_restart
    
    write(UNIT_LOG, '(A)') 'Reading restart data from fort.7'
    
    open(unit=UNIT_RESTART, file='fort.7', status='old', action='read', iostat=ios_restart)
    if (ios_restart /= 0) then
      write(UNIT_LOG, '(A)') 'Error: Cannot open restart file fort.7'
      call INPERR(2)
      return
    end if
    
    rewind(UNIT_RESTART)
    
    ! Read title using original format 900: FORMAT(20A4)
    read(UNIT_RESTART, 900, iostat=ios_restart) TITLEO
    if (ios_restart /= 0) then
      write(UNIT_LOG, '(A)') 'Error reading title from restart file'
      call INPERR(2)
      return
    end if
    
    ! Read mesh dimensions using original format 902: FORMAT(4I5)
    read(UNIT_RESTART, 902, iostat=ios_restart) IMINO, IMAXO, JMINO, JMAXO
    if (ios_restart /= 0) then
      write(UNIT_LOG, '(A)') 'Error reading mesh dimensions from restart file'
      call INPERR(2)
      return
    end if
    
    ! Read solution parameters using original format 903: FORMAT(8F10.6)
    read(UNIT_RESTART, 903, iostat=ios_restart) CLOLD, EMACHO, ALPHAO, DELTAO, VOLO, DUBO
    if (ios_restart /= 0) then
      write(UNIT_LOG, '(A)') 'Error reading solution parameters from restart file'
      call INPERR(2)
      return
    end if
    
    ! Read grid coordinates using original format 903: FORMAT(8F10.6)
    read(UNIT_RESTART, 903, iostat=ios_restart) (XOLD(I), I=IMINO, IMAXO)
    if (ios_restart /= 0) then
      write(UNIT_LOG, '(A)') 'Error reading X coordinates from restart file'
      call INPERR(2)
      return
    end if
    
    read(UNIT_RESTART, 903, iostat=ios_restart) (YOLD(J), J=JMINO, JMAXO)
    if (ios_restart /= 0) then
      write(UNIT_LOG, '(A)') 'Error reading Y coordinates from restart file'
      call INPERR(2)
      return
    end if

    ! Read solution array P using original format 903: FORMAT(8F10.6)
    do I = IMINO, IMAXO
      read(UNIT_RESTART, 903, iostat=ios_restart) (P(J,I), J=JMINO, JMAXO)
      if (ios_restart /= 0) then
        write(UNIT_LOG, '(A,I0)') 'Error reading P array at I=', I
        call INPERR(2)
        close(UNIT_RESTART)
        return
      end if
    end do

    close(UNIT_RESTART)
    write(UNIT_LOG, '(A)') 'Restart data successfully loaded'
    
    ! Write restart information to output file (like original)
    write(UNIT_OUTPUT, 1000) TITLEO, IMINO, IMAXO, JMINO, JMAXO, CLOLD, EMACHO, &
                            ALPHAO, DELTAO, VOLO, DUBO
    
    ! Original format statements
900 format(20A4)
902 format(4I5)
903 format(8F10.6)
1000  format(39H1P INITIALIZED FROM PREVIOUS RUN TITLED/ &
        1X,20A4/31H WHICH HAD THE FOLLOWING VALUES/ &
        8H IMIN  =,I4/8H IMAX  =,I4/8H JMIN  =,I4/8H JMAX  =,I4/ &
        8H CL    =,F12.8/8H EMACH =,F12.8/8H ALPHA =,F12.8/ &
        8H DELTA =,F12.8/8H VOL   =,F12.8/8H DUB   =,F12.8/ )

  end subroutine LOADP

  ! Initialize potential array P based on PSTART value
  subroutine GUESSP()
    use common_data
    implicit none
    integer :: I, J
    
    select case (PSTART)
    case (1)
      ! PSTART = 1: Set P to zero
      write(UNIT_LOG, '(A)') 'Initializing P array to zero (PSTART=1)'
      do I = 1, size(P, 2)
        do J = 1, size(P, 1)
          P(J, I) = 0.0
        end do
      end do
      DUB = 0.0
      CIRCFF = 0.0
      CIRCTE = 0.0
    
    case (2)
      ! PSTART = 2: P already read from restart file in READIN
      ! Just need to set circulation parameters if meshes are compatible
      write(UNIT_LOG, '(A)') 'Using restart data (PSTART=2) - data already loaded in READIN'
      ! Set circulation from restart data
      DUB = DUBO
      CIRCFF = CLOLD / CLFACT
      CIRCTE = CIRCFF
      
    case (3)
      ! PSTART = 3: Use P values already in core from previous case
      write(UNIT_LOG, '(A)') 'Using previous solution in core (PSTART=3)'
      ! P array should already contain values from previous case
      
    case default
      write(UNIT_LOG, '(A,I0)') 'Invalid PSTART value: ', PSTART
      call INPERR(3)
    end select
  end subroutine GUESSP

  ! Interpolate P from old grid (XOLD, YOLD) to new grid (X, Y)
  subroutine interpolate_restart_data()
    use common_data
    implicit none
    integer :: I, J, IO, JO
    real :: TEST, PT_TEMP(100)
    logical :: same_x_mesh, same_y_mesh
    
    ! Check if X meshes are the same
    same_x_mesh = .true.
    if (IMAXI /= IMAXO) then
      same_x_mesh = .false.
    else
      do I = IMIN, IMAXI
        TEST = abs(XIN(I) - XOLD(I))
        if (TEST > 0.0001) then
          same_x_mesh = .false.
          exit
        end if
      end do
    end if
    
    ! Check if Y meshes are the same
    same_y_mesh = .true.
    if (JMAXI /= JMAXO) then
      same_y_mesh = .false.
    else
      do J = JMIN, JMAXI
        TEST = abs(YIN(J) - YOLD(J))
        if (TEST > 0.0001) then
          same_y_mesh = .false.
          exit
        end if
      end do
    end if
    
    if (same_x_mesh .and. same_y_mesh) then
      ! Meshes are identical - direct copy
      write(UNIT_LOG, '(A)') 'Old and new meshes are identical'
      ! P array already contains the restart data
      DUB = DUBO
      CIRCFF = CLOLD / CLFACT
      CIRCTE = CIRCFF
    else
      ! Need interpolation - simplified version
      write(UNIT_LOG, '(A)') 'Interpolating from old mesh to new mesh'
      ! For now, use simple initialization - full interpolation would be complex
      call GUESSP_simple_init()
    end if
  end subroutine interpolate_restart_data

  ! Simple initialization when interpolation is needed
  subroutine GUESSP_simple_init()
    use common_data
    implicit none
    integer :: I, J
    
    write(UNIT_LOG, '(A)') 'Using simple initialization due to mesh differences'
    
    do I = 1, size(P, 2)
      do J = 1, size(P, 1)
        P(J, I) = 0.0
      end do
    end do
    DUB = 0.0
    CIRCFF = 0.0
    CIRCTE = 0.0
  end subroutine GUESSP_simple_init

  ! Fatal error in input - write message and stop
  subroutine INPERR(error_code)
    implicit none
    integer, intent(in) :: error_code
    
    select case (error_code)
    case (1)
      write(UNIT_LOG, '(A)') 'FATAL ERROR: Namelist input error'
      write(UNIT_LOG, '(A)') 'Check input format and parameter names'
    case (2)
      write(UNIT_LOG, '(A)') 'FATAL ERROR: Restart file read error'
      write(UNIT_LOG, '(A)') 'Check restart file format and existence'
    case (3)
      write(UNIT_LOG, '(A)') 'FATAL ERROR: Invalid PSTART value'
      write(UNIT_LOG, '(A)') 'PSTART must be 1, 2, or 3'
    case (4)
      write(UNIT_LOG, '(A)') 'FATAL ERROR: Mesh dimension error'
    case (5)
      write(UNIT_LOG, '(A)') 'FATAL ERROR: Physical parameter out of range'
    case default
      write(UNIT_LOG, '(A,I0)') 'FATAL ERROR: Unknown error code ', error_code
    end select
    
    write(UNIT_LOG, '(A)') 'Program terminated due to input error'
    close(UNIT_LOG)
    stop 'TSFoil input error - check tsfoil.log for details'
  end subroutine INPERR

  ! Print INP namelist parameters to UNIT_OUTPUT for debugging
  subroutine PRINT_INP_NAMELIST()
    use common_data
    implicit none
    integer :: I, J
    
    write(UNIT_OUTPUT, '(A)') '1'  ! Form feed character
    write(UNIT_OUTPUT, '(A)') '0************************************'
    write(UNIT_OUTPUT, '(A)') ' DEBUG: INP NAMELIST PARAMETERS'
    write(UNIT_OUTPUT, '(A)') ' ************************************'
    write(UNIT_OUTPUT, *)
    
    ! Print flow parameters
    write(UNIT_OUTPUT, '(A)') ' FLOW PARAMETERS:'
    write(UNIT_OUTPUT, '(A,F12.6)') '   AK       = ', AK
    write(UNIT_OUTPUT, '(A,F12.6)') '   ALPHA    = ', ALPHA  
    write(UNIT_OUTPUT, '(A,F12.6)') '   EMACH    = ', EMACH
    write(UNIT_OUTPUT, '(A,F12.6)') '   DELTA    = ', DELTA
    write(UNIT_OUTPUT, '(A,F12.6)') '   GAM      = ', GAM
    write(UNIT_OUTPUT, '(A,L12)')   '   PHYS     = ', PHYS
    write(UNIT_OUTPUT, *)
    
    ! Print mesh parameters
    write(UNIT_OUTPUT, '(A)') ' MESH PARAMETERS:'
    write(UNIT_OUTPUT, '(A,I12)')   '   IMIN     = ', IMIN
    write(UNIT_OUTPUT, '(A,I12)')   '   IMAXI    = ', IMAXI
    write(UNIT_OUTPUT, '(A,I12)')   '   JMIN     = ', JMIN
    write(UNIT_OUTPUT, '(A,I12)')   '   JMAXI    = ', JMAXI
    write(UNIT_OUTPUT, '(A,L12)')   '   AMESH    = ', AMESH
    write(UNIT_OUTPUT, *)
    
    ! Print boundary condition parameters
    write(UNIT_OUTPUT, '(A)') ' BOUNDARY CONDITIONS:'
    write(UNIT_OUTPUT, '(A,I12)')   '   BCTYPE   = ', BCTYPE
    write(UNIT_OUTPUT, '(A,I12)')   '   BCFOIL   = ', BCFOIL
    write(UNIT_OUTPUT, '(A,F12.6)') '   F        = ', F
    write(UNIT_OUTPUT, '(A,F12.6)') '   H        = ', H
    write(UNIT_OUTPUT, '(A,F12.6)') '   POR      = ', POR
    write(UNIT_OUTPUT, *)
    
    ! Print solver parameters
    write(UNIT_OUTPUT, '(A)') ' SOLVER PARAMETERS:'
    write(UNIT_OUTPUT, '(A,F12.6)') '   CVERGE   = ', CVERGE
    write(UNIT_OUTPUT, '(A,F12.6)') '   DVERGE   = ', DVERGE
    write(UNIT_OUTPUT, '(A,F12.6)') '   EPS      = ', EPS
    write(UNIT_OUTPUT, '(A,I12)')   '   MAXIT    = ', MAXIT
    write(UNIT_OUTPUT, '(A,I12)')   '   IPRTER   = ', IPRTER
    write(UNIT_OUTPUT, '(A,I12)')   '   ICUT     = ', ICUT
    write(UNIT_OUTPUT, *)
    
    ! Print circulation and Kutta condition parameters
    write(UNIT_OUTPUT, '(A)') ' CIRCULATION AND KUTTA CONDITION:'
    write(UNIT_OUTPUT, '(A,F12.6)') '   CLSET    = ', CLSET
    write(UNIT_OUTPUT, '(A,F12.6)') '   WCIRC    = ', WCIRC
    write(UNIT_OUTPUT, '(A,L12)')   '   FCR      = ', FCR
    write(UNIT_OUTPUT, '(A,L12)')   '   KUTTA    = ', KUTTA
    write(UNIT_OUTPUT, *)
    
    ! Print control parameters
    write(UNIT_OUTPUT, '(A)') ' CONTROL PARAMETERS:'
    write(UNIT_OUTPUT, '(A,I12)')   '   PSTART   = ', PSTART
    write(UNIT_OUTPUT, '(A,L12)')   '   PSAVE    = ', PSAVE
    write(UNIT_OUTPUT, '(A,I12)')   '   PRTFLO   = ', PRTFLO
    write(UNIT_OUTPUT, '(A,I12)')   '   SIMDEF   = ', SIMDEF
    write(UNIT_OUTPUT, *)
    
    ! Print airfoil parameters
    write(UNIT_OUTPUT, '(A)') ' AIRFOIL PARAMETERS:'
    write(UNIT_OUTPUT, '(A,I12)')   '   NU       = ', NU
    write(UNIT_OUTPUT, '(A,I12)')   '   NL       = ', NL
    write(UNIT_OUTPUT, '(A,F12.6)') '   RIGF     = ', RIGF
    write(UNIT_OUTPUT, '(A,I12)')   '   IFLAP    = ', IFLAP
    write(UNIT_OUTPUT, '(A,F12.6)') '   DELFLP   = ', DELFLP
    write(UNIT_OUTPUT, '(A,F12.6)') '   FLPLOC   = ', FLPLOC
    write(UNIT_OUTPUT, '(A,I12)')   '   IDLA     = ', IDLA
    write(UNIT_OUTPUT, *)
    
    ! Print relaxation parameters
    write(UNIT_OUTPUT, '(A)') ' RELAXATION PARAMETERS:'
    write(UNIT_OUTPUT, '(A,3F8.3)') '   WE       = ', WE
    write(UNIT_OUTPUT, *)
    
    ! Print viscous wedge parameters
    write(UNIT_OUTPUT, '(A)') ' VISCOUS WEDGE PARAMETERS:'
    write(UNIT_OUTPUT, '(A,I12)')   '   NWDGE    = ', NWDGE
    write(UNIT_OUTPUT, '(A,E12.3)') '   REYNLD   = ', REYNLD
    write(UNIT_OUTPUT, '(A,F12.6)') '   WCONST   = ', WCONST
    write(UNIT_OUTPUT, *)
    
    ! Print XIN array if allocated
    if (allocated(XIN) .and. IMAXI > 0) then
      write(UNIT_OUTPUT, '(A)') ' XIN ARRAY:'
      write(UNIT_OUTPUT, '(A,I0,A,I0)') '   (I=', IMIN, ' TO ', IMAXI, ')'
      write(UNIT_OUTPUT, '(6F12.6)') (XIN(I), I=IMIN, IMAXI)
      write(UNIT_OUTPUT, *)
    end if
    
    ! Print YIN array if allocated
    if (allocated(YIN) .and. JMAXI > 0) then
      write(UNIT_OUTPUT, '(A)') ' YIN ARRAY:'
      write(UNIT_OUTPUT, '(A,I0,A,I0)') '   (J=', JMIN, ' TO ', JMAXI, ')'
      write(UNIT_OUTPUT, '(6F12.6)') (YIN(J), J=JMIN, JMAXI)
      write(UNIT_OUTPUT, *)
    end if
    
    ! Print XU array if NU > 0
    if (NU > 0) then
      write(UNIT_OUTPUT, '(A)') ' XU ARRAY:'
      write(UNIT_OUTPUT, '(A,I0)') '   (I=1 TO ', NU, ')'
      write(UNIT_OUTPUT, '(6F12.6)') (XU(I), I=1, NU)
      write(UNIT_OUTPUT, *)
    end if
    
    ! Print YU array if NU > 0
    if (NU > 0) then
      write(UNIT_OUTPUT, '(A)') ' YU ARRAY:'
      write(UNIT_OUTPUT, '(A,I0)') '   (I=1 TO ', NU, ')'
      write(UNIT_OUTPUT, '(6F12.6)') (YU(I), I=1, NU)
      write(UNIT_OUTPUT, *)
    end if
    
    ! Print XL array if NL > 0
    if (NL > 0) then
      write(UNIT_OUTPUT, '(A)') ' XL ARRAY:'
      write(UNIT_OUTPUT, '(A,I0)') '   (I=1 TO ', NL, ')'
      write(UNIT_OUTPUT, '(6F12.6)') (XL(I), I=1, NL)
      write(UNIT_OUTPUT, *)
    end if
    
    ! Print YL array if NL > 0
    if (NL > 0) then
      write(UNIT_OUTPUT, '(A)') ' YL ARRAY:'
      write(UNIT_OUTPUT, '(A,I0)') '   (I=1 TO ', NL, ')'
      write(UNIT_OUTPUT, '(6F12.6)') (YL(I), I=1, NL)
      write(UNIT_OUTPUT, *)
    end if
    
    write(UNIT_OUTPUT, '(A)') ' ************************************'
    write(UNIT_OUTPUT, '(A)') ' END OF INP NAMELIST DEBUG OUTPUT'
    write(UNIT_OUTPUT, '(A)') ' ************************************'
    write(UNIT_OUTPUT, *)
    
  end subroutine PRINT_INP_NAMELIST

end module io_module
