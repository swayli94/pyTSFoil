! io_module.f90
! Module for input/output routines

module io_module
  use common_data
  implicit none
  
  ! File unit numbers for different output files
  integer, parameter :: UNIT_INPUT = 5          ! Standard input
  integer, parameter :: UNIT_OUTPUT = 6         ! Standard output  
  integer, parameter :: UNIT_LOG = 10           ! Main log file (tsfoil.log)
  integer, parameter :: UNIT_ECHO = 11          ! Input echo file (tsfoil.ech)
  integer, parameter :: UNIT_CP = 12            ! Cp distribution file (tsfoil.cp)
  integer, parameter :: UNIT_FIELD = 13         ! Field data file (tsfoil.fld)
  integer, parameter :: UNIT_FLOW = 14          ! Flow map file (tsfoil.map)
  integer, parameter :: UNIT_SHOCK = 15         ! Shock analysis file (tsfoil.shk)
  integer, parameter :: UNIT_WALL = 16          ! Wall data file (tsfoil.wal)
  integer, parameter :: UNIT_RESTART = 17       ! Restart file (tsfoil.rst)
  
  ! Define a simplified namelist for the variables actually available in common_data
  namelist /TSFOIL_INP/ AK, ALPHA, DUB, PHYS, &
                        ICUT, AMESH, &
                        DELTA, EMACH, PRTFLO, SIMDEF, &
                        F, H, &
                        XIN, YIN, &
                        BCFOIL, NL, NU, PSTART
  
  public :: READIN, SCALE, ECHINP, PRINT, PRINT1, PRTFLD, PRTMC, PRTSK, PRTWAL, SAVEP
  public :: open_output_files, close_output_files

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

  ! Main input reading routine - handles multiple cases
  ! Reads title cards, namelist input, and manages restart data
  subroutine READIN()
    use common_data
    use mesh_module, only: AYMESH, CKMESH, CUTOUT
    implicit none
    
    character(len=80) :: title_card
    character(len=8), parameter :: FINISHED = 'FINISHED'
    logical :: case_finished
    integer :: ios
    
    ! Open output files first
    call open_output_files()
    
    ! Echo input parameters
    call ECHINP()
    
    ! Main case processing loop
    case_loop: do
      ! Read title card for this case
      read(UNIT_INPUT, '(A)', iostat=ios, end=999) title_card
      if (ios /= 0) exit case_loop
      
      ! Store title in TITLE array (convert to 20*4-byte format)
      TITLE = title_card
      
      ! Write title to output
      write(UNIT_LOG, '(A)') title_card
      
      ! Check for termination string
      if (title_card(1:8) == FINISHED) then
        write(UNIT_LOG, '(A)') 'End of cases detected'
        exit case_loop
      end if      ! Read namelist input for this case
      read(UNIT_INPUT, nml=TSFOIL_INP, iostat=ios)
      if (ios /= 0) then
        write(UNIT_LOG, '(A,I0)') 'Error reading namelist: ', ios
        call INPERR(1)
      end if
      
      ! Handle PSTART=3 case - check if previous solution is usable
      if (PSTART == 3) then
        if (ABORT1) then
          write(UNIT_LOG, '(A)') 'Previous solution not usable, setting PSTART=1'
          PSTART = 1
        end if
      end if
      
      ! Set AK=0 for physical coordinates
      if (PHYS) AK = 0.0
      
      ! Handle mesh generation
      if (AMESH) then
        call AYMESH()
      else
        ! Check if YIN needs default initialization
        call check_yin_defaults()
      end if
      
      ! Process mesh
      call CKMESH()
      
      ! Handle mesh refinement
      if (ICUT > 0) then
        IREF = -1
        call CUTOUT()
      end if
      
      ! Handle restart data if PSTART=2
      if (PSTART == 2) then
        call LOADP()
      end if
      
      ! Scale physical variables
      call SCALE()
      
      ! Initialize solution based on PSTART
      call GUESSP()
      
      ! Continue with solution process...
      ! (This would be handled by the main program loop)
      write(UNIT_LOG, '(A)') 'Case setup completed'
      exit case_loop  ! For now, handle one case at a time
      
    end do case_loop
    
999 continue
    return
  end subroutine READIN

  ! Convert physical variables to similarity variables
  subroutine SCALE()
    use common_data, only: PHYS, DELTA, EMACH, SIMDEF
    use common_data, only: AK, YFACT, CPFACT, CLFACT, CDFACT, CMFACT, VFACT
    implicit none
    real :: BETA, DELRT1, DELRT2, EMROOT

    if (.not. PHYS) then
      CPFACT = 1.0; CLFACT = 1.0; CDFACT = 1.0; CMFACT = 1.0; YFACT = 1.0; VFACT = 1.0
      AK = 0.0
      write(UNIT_LOG,'(A)') 'Using non-physical (similarity) variables'
      return
    end if

    ! Compute scaling parameters
    BETA = 1.0 - EMACH**2
    DELRT1 = DELTA**(1.0/3.0)
    DELRT2 = DELTA**(2.0/3.0)

    select case (SIMDEF)
    case (1)
      ! Cole scaling
      AK = BETA / DELRT2
      YFACT = 1.0 / DELRT1
      CPFACT = DELRT2
      CLFACT = DELRT2
      CMFACT = DELRT2
      CDFACT = DELRT2 * DELTA
      VFACT = DELTA * 57.295779
      write(UNIT_LOG,'(A)') 'Using Cole scaling'

    case (2)
      ! Spreiter scaling
      EMROOT = EMACH**(2.0/3.0)
      AK = BETA / (DELRT2 * EMROOT * EMROOT)
      YFACT = 1.0 / (DELRT1 * EMROOT)
      CPFACT = DELRT2 / EMROOT
      CLFACT = CPFACT
      CMFACT = CPFACT
      CDFACT = CPFACT * DELTA
      VFACT = DELTA * 57.295779
      write(UNIT_LOG,'(A)') 'Using Spreiter scaling'

    case (3)
      ! Krupp scaling
      AK = BETA / (DELRT2 * EMACH)
      YFACT = 1.0 / (DELRT1 * sqrt(EMACH))
      CPFACT = DELRT2 / (EMACH**0.75)
      CLFACT = CPFACT
      CMFACT = CPFACT
      CDFACT = CPFACT * DELTA
      VFACT = DELTA * 57.295779
      write(UNIT_LOG,'(A)') 'Using Krupp scaling'

    case default
      write(UNIT_LOG,'(A,I0)') 'ERROR: Unsupported SIMDEF = ', SIMDEF
      stop 'Unsupported SIMDEF'
    end select

    ! Log scaling parameters to main log file
    write(UNIT_LOG,'(A)') 'Scaling parameters computed:'
    write(UNIT_LOG,'(A,F10.6)') '  AK = ', AK
    write(UNIT_LOG,'(A,F10.6)') '  YFACT = ', YFACT
    write(UNIT_LOG,'(A,F10.6)') '  CPFACT = ', CPFACT
    write(UNIT_LOG,'(A,F10.6)') '  CLFACT = ', CLFACT
    write(UNIT_LOG,'(A,F10.6)') '  CMFACT = ', CMFACT
    write(UNIT_LOG,'(A,F10.6)') '  CDFACT = ', CDFACT
    write(UNIT_LOG,'(A,F10.6)') '  VFACT = ', VFACT

  end subroutine SCALE

  ! Echo input cards for logging to input echo file
  subroutine ECHINP()
    use common_data, only: AMESH, SIMDEF, EMACH, DELTA, BCFOIL, BCTYPE
    implicit none
    
    write(UNIT_ECHO,'(A)') 'Input parameters:'
    write(UNIT_ECHO,'(A,L1)') '  AMESH = ', AMESH
    write(UNIT_ECHO,'(A,I0)') '  SIMDEF = ', SIMDEF
    write(UNIT_ECHO,'(A,F7.3)') '  EMACH = ', EMACH
    write(UNIT_ECHO,'(A,F7.3)') '  DELTA = ', DELTA
    write(UNIT_ECHO,'(A,I4)') '  BCFOIL = ', BCFOIL
    write(UNIT_ECHO,'(A,I4)') '  BCTYPE = ', BCTYPE
    
    ! Also write to main log
    write(UNIT_LOG,'(A)') 'Input parameters echoed to tsfoil.ech'
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
  subroutine PRINT1()
    use common_data, only: CPFACT, XIN, ILE, ITE, P, CPL, CPU, JUP, JLOW
    implicit none
    integer :: I
    real :: CP
    
    write(UNIT_CP,'(A)') '# X/C     CP'
    write(UNIT_CP,'(A)') '# Pressure coefficient distribution along airfoil'
    
    do I = ILE, ITE
      CP = -2.0*(P(JUP,I)-P(JLOW,I)) / (XIN(I+1)-XIN(I-1))
      CPL(I-ILE+1) = CP; CPU(I-ILE+1) = CP
      write(UNIT_CP,'(F8.5,2X,F10.6)') XIN(I), CP
    end do
    
    write(UNIT_LOG,'(A,I0,A)') 'Cp distribution written with ', ITE-ILE+1, ' points'
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
    integer :: I, J, K
    character(len=1), parameter :: ch_par='P', ch_hyp='H', ch_shock='S', ch_ell='-'

    ! Header
    write(UNIT_FLOW,'(A)') '# Flow type map at each grid point'
    write(UNIT_FLOW,'(A)') '# P=Parabolic, H=Hyperbolic, S=Shock, -=Elliptic'
    write(UNIT_FLOW,'(A)') '# Format: J-index followed by flow types for each I'

    ! Initialize IPC and VT
    IPC(IUP:IDOWN) = ch_ell
    VT(JMIN:JMAX,1) = C1(2)

    ! Classify flow type and write each row
    do K = JMIN, JMAX
      J = JMAX - K + 1
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
  subroutine PRTSK(Z,ARG,L,NSHOCK,CDSK,LPRT1)
    use common_data, only: CDFACT, GAM1, DELTA, YFACT
    implicit none
    real, intent(in) :: Z(:), ARG(:)
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
      CDY = CDYCOF * ARG(K)
      POY = 1.0 + POYCOF * ARG(K)
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

  ! Save current solution P to restart file
  subroutine SAVEP()
    use common_data, only: P, X, Y, IMIN, IMAX, JMIN, JMAX
    implicit none
    integer :: I, J
    
    open(unit=UNIT_RESTART, file='tsfoil.rst', status='replace', action='write')
    
    write(UNIT_RESTART,'(A)') '# TSFOIL Restart File'
    write(UNIT_RESTART,'(4I6)') IMIN, IMAX, JMIN, JMAX
    
    ! Write grid coordinates
    write(UNIT_RESTART,'(A)') '# Grid X coordinates'
    write(UNIT_RESTART,'(*(F12.8))') (X(I), I=IMIN, IMAX)
    
    write(UNIT_RESTART,'(A)') '# Grid Y coordinates' 
    write(UNIT_RESTART,'(*(F12.8))') (Y(J), J=JMIN, JMAX)
    
    ! Write solution array P
    write(UNIT_RESTART,'(A)') '# Solution array P(J,I)'
    do J = JMIN, JMAX
      write(UNIT_RESTART,'(*(F16.12))') (P(J,I), I=IMIN, IMAX)
    end do
    
    close(UNIT_RESTART)
    write(UNIT_LOG,'(A)') 'Solution saved to restart file tsfoil.rst'
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

  ! Read restart file (PSTART=2 case)
  subroutine LOADP()
    use common_data
    implicit none
    integer :: I, J, ios_restart
      write(UNIT_LOG, '(A)') 'Reading restart data from tsfoil.rst'
    
    open(unit=UNIT_RESTART, file='tsfoil.rst', status='old', action='read', iostat=ios_restart)
    if (ios_restart /= 0) then
      write(UNIT_LOG, '(A)') 'Error: Cannot open restart file tsfoil.rst'
      call INPERR(2)
      return
    end if
    
    ! Skip comment lines
    read(UNIT_RESTART, '(A)')  ! Skip header
    
    ! Read old title
    read(UNIT_RESTART, '(A)', iostat=ios_restart) TITLEO
    if (ios_restart /= 0) then
      write(UNIT_LOG, '(A)') 'Error reading title from restart file'
      call INPERR(2)
    end if
    
    ! Read old mesh dimensions
    read(UNIT_RESTART, *, iostat=ios_restart) IMAXO, JMAXO, IMINO, JMINO
    if (ios_restart /= 0) then
      write(UNIT_LOG, '(A)') 'Error reading mesh dimensions from restart file'
      call INPERR(2)
    end if
    
    ! Read old solution parameters
    read(UNIT_RESTART, *, iostat=ios_restart) CLOLD, EMACHO, ALPHAO, DELTAO, VOLO, DUBO
    if (ios_restart /= 0) then
      write(UNIT_LOG, '(A)') 'Error reading solution parameters from restart file'
      call INPERR(2)
    end if
    
    ! Read old grid coordinates
    read(UNIT_RESTART, *)  ! Skip X coordinate header
    read(UNIT_RESTART, *, iostat=ios_restart) (XOLD(I), I=IMINO, IMAXO)
    if (ios_restart /= 0) then
      write(UNIT_LOG, '(A)') 'Error reading X coordinates from restart file'
      call INPERR(2)
    end if
    
    read(UNIT_RESTART, *)  ! Skip Y coordinate header
    read(UNIT_RESTART, *, iostat=ios_restart) (YOLD(J), J=JMINO, JMAXO)
    if (ios_restart /= 0) then
      write(UNIT_LOG, '(A)') 'Error reading Y coordinates from restart file'
      call INPERR(2)
    end if
    
    ! Read old solution array P
    read(UNIT_RESTART, *)  ! Skip P array header
    do J = JMINO, JMAXO
      read(UNIT_RESTART, *, iostat=ios_restart) (P(J,I), I=IMINO, IMAXO)
      if (ios_restart /= 0) then
        write(UNIT_LOG, '(A,I0)') 'Error reading P array at J=', J
        call INPERR(2)
        exit
      end if
    end do
    
    close(UNIT_RESTART)
    write(UNIT_LOG, '(A)') 'Restart data successfully loaded'
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
      ! PSTART = 2: P read from restart file in LOADP
      ! Need to interpolate from old grid to new grid if different
      write(UNIT_LOG, '(A)') 'Using restart data (PSTART=2)'
      call interpolate_restart_data()
      
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

end module io_module
