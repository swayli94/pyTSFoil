! common_data.f90
! Module replacing legacy COMMON blocks for shared data

module common_data
  implicit none
  private
  public :: IMIN, IMAX, IUP, IDOWN, ILE, ITE, JMIN, JMAX, JUP, JLOW, JTOP, JBOT, J1, J2
  public :: AK, ALPHA, DUB, GAM1, RTK, PHYS
  public :: IREF, ICUT, KSTEP, ABORT1
  public :: XIN, YIN, AMESH
  public :: P, X, Y  ! Main solution and coordinate arrays
  public :: FL, FXL, FU, FXU, CAMBER, THICK, XFOIL, VOL, IFOIL
  public :: BCFOIL, NL, NU, XL, XU, YL, YU, PERCENT, CHORD
  public :: RIGF, IFLAP, DELFLP, FLPLOC, FSYM
  public :: SIMDEF, DELTA, EMACH, PRTFLO, DELRT2, EMROOT, CL
  public :: CDFACT, CLFACT, CMFACT, CPFACT, CPSTAR, YFACT, VFACT, SONVEL
  public :: F, H, HALFPI, PI, RTKPOR, TWOPI
  public :: ALPHAO, CLOLD, DELTAO, DUBO, EMACHO, VOLO
  public :: IMINO, IMAXO, IMAXI, JMINO, JMAXO, JMAXI
  public :: PSAVE, TITLE, TITLEO, XOLD, YOLD
  public :: XMID, YMID  ! Additional public declarations for io_module and other modules
  public :: JERROR, BCTYPE, CPL, CPU, C1, CXL, CXC, CXR
  public :: CXXC, CXXL, CXXR, CYYC, CYYD, CYYU, XDIFF, YDIFF
  public :: CJUP, CJUP1, CJLOW, CJLOW1, CYYBUD, CYYBUC, CYYBUU
  public :: CYYBLU, CYYBLC, CYYBLD, FXLBC, FXUBC    
  public :: DTOP, DBOT, VTOP, VBOT, DUP, DDOWN, VUP, VDOWN
  public :: DIAG, RHS, SUB, SUP
  public :: JLIN, IPC, VT, PSTART, CIRCFF, CIRCTE
  public :: PJUMP, FCR, KUTTA, CVERGE, ERROR, IERROR, MAXIT, IPRTER
  public :: CLSET, IDLA
  public :: EPS, WE, NWDGE, REYNLD, WCONST, WSLP, WI
  public :: DVERGE, GAM, POR, FHINV, WCIRC
  public :: YFREE, YTUN, JMXF, JMXT
  public :: B, BETA0, BETA1, BETA2, PSI0, PSI1, PSI2
  public :: ALPHA0, ALPHA1, ALPHA2, XSING, OMEGA0, OMEGA1, OMEGA2, JET
  public :: initialize_common
  public :: UNIT_INPUT, UNIT_LOG, UNIT_ECHO, UNIT_CP, UNIT_FIELD
  public :: UNIT_FLOW, UNIT_OUTPUT, UNIT_WALL, UNIT_RESTART, UNIT_SHOCK
  
  ! Mesh indices (from COMMON /COM1/)
  integer :: IMIN, IMAX       ! grid i-range
  integer :: IUP, IDOWN       ! upstream/downstream indices
  integer :: ILE, ITE         ! leading/trailing edge i-indices
  integer :: JMIN, JMAX       ! grid j-range
  integer :: JUP, JLOW        ! upper/lower surface j-indices
  integer :: JTOP, JBOT       ! far-field top/bottom j-indices
  integer :: J1, J2           ! auxiliary indices

  ! Main solution arrays
  real, allocatable :: P(:,:)    ! Potential solution array
  real, allocatable :: X(:), Y(:) ! Coordinate arrays

  ! Flow parameters (from COMMON /COM2/ and logical PHYS)
  real :: AK       ! freestream similarity parameter
  real :: ALPHA    ! angle of attack
  real :: DUB      ! doublet strength
  real :: GAM1     ! gamma - 1
  real :: RTK      ! sqrt(gamma)
  logical :: PHYS  ! physical (True) vs similarity (False)

  ! Control flags and refinement (from COMMON /COM3/)
  integer :: IREF   ! mesh refinement flag
  integer :: ICUT   ! number of coarse refinements
  integer :: KSTEP  ! SOR sweep step size
  logical :: ABORT1 ! input abort flag

  ! Analytical mesh arrays (from COMMON /COM4/)
  real, allocatable :: XIN(:) ! mesh x-coordinates
  real, allocatable :: YIN(:) ! mesh y-coordinates
  logical :: AMESH           ! use analytical mesh

  ! Shared data replacing COMMON / COM5/ - / COM30/
  ! COM5: mesh derivative arrays
  real :: XDIFF(100), YDIFF(100)
  
  ! COM6: surface and flow arrays
  real :: FL(100), FXL(100), FU(100), FXU(100), CAMBER(100), THICK(100), XFOIL(100), VOL
  integer :: IFOIL
  
  ! COM7: boundary extrapolation/coefficient flags
  real :: CJUP, CJUP1, CJLOW, CJLOW1

  ! COM8: solver control parameters
  real :: CVERGE, DVERGE, TOL, RSAVE
  real :: EPS = 1.0e-6              ! Convergence tolerance
  real, parameter :: WI = 1.05      ! SOR relaxation factor
  real :: WE(3)
  integer :: IPRTER, MAXIT, NEX, N_O, NPRINT, NPT
  
  ! COM9: airfoil definition flags
  integer :: BCFOIL, NL, NU
  real :: XL(100), XU(100), YL(100), YU(100), PERCENT, CHORD
  
  ! Airfoil control parameters
  real :: RIGF        ! rigidity factor for transonic effects
  integer :: IFLAP    ! flap flag
  real :: DELFLP      ! flap deflection angle  
  real :: FLPLOC      ! flap location
  integer :: FSYM     ! symmetry flag
  
  ! COM10: free-stream/tunnel arrays
  real :: YFREE(100), YTUN(100), GAM
  integer :: JMXF, JMXT
  
  ! COM11: restart and case storage
  real :: ALPHAO, CLOLD, DELTAO, DUBO, EMACHO, VOLO
  integer :: IMINO, IMAXO, IMAXI, JMINO, JMAXO, JMAXI
  logical :: PSAVE
  integer :: PSTART
  character(len=4) :: TITLE(20), TITLEO(20)
  real :: XOLD(100), YOLD(100)
  
  ! COM12: wall/tunnel constants  
  real :: F, H, HALFPI, PI, RTKPOR, TWOPI
  
  ! COM13: coefficient scaling factors
  real :: CDFACT, CLFACT, CMFACT, CPFACT, CPSTAR
  
  ! COM14: Kutta and circulation flags
  real :: CLSET, WCIRC
  logical :: FCR, KUTTA
  
  ! COM15: vortex/doublet parameters
  real :: B, BETA0, BETA1, BETA2, PSI0, PSI1, PSI2
  
  ! COM16: far-field root parameters
  real :: ALPHA0, ALPHA1, ALPHA2, XSING, OMEGA0, OMEGA1, OMEGA2, JET
  
  ! COM17: special boundary coefficient arrays
  real :: CYYBLC, CYYBLD, CYYBLU, CYYBUC, CYYBUD, CYYBUU
  real :: FXLBC(100), FXUBC(100)
  integer :: ITEMP1, ITEMP2
    
  ! COM18: error tracking and diagnostics
  real :: ERROR
  integer :: I1, I2, IERROR, JERROR, IPRINT, LERR, NVAR, STATUS, NEXT
  
  ! COM19: jump arrays and pressure jump
  real :: PJUMP(100)
  
  ! COM19: tridiagonal solver arrays
  real :: DIAG(100), RHS(100), SUB(100), SUP(100)
  
  ! COM20: mesh midpoint storage
  real :: XMID(100), YMID(100)
  
  ! COM22: central differencing coefficients
  real :: CXC(100), CXL(100), CXR(100), CXXC(100), CXXL(100), CXXR(100), C1(100)
  
  ! COM23: boundary differencing coefficients
  real :: CYYC(100), CYYD(100), CYYU(100)
  integer :: IVAL
  
  ! COM24: far-field boundary arrays
  real :: DTOP(100), DBOT(100), DUP(100), DDOWN(100)
  real :: VTOP(100), VBOT(100), VUP(100), VDOWN(100)
  
  ! COM25: plot arrays
  real :: CPL(100), CPU(100)
  integer :: IDLA
  
  ! COM27: transonic similarity state
  real :: CL, DELTA, DELRT2, EMACH, EMROOT
  integer :: PRTFLO, SIMDEF
  real :: SONVEL, VFACT, YFACT
  
  ! COM28: boundary condition identifiers
  integer :: BCTYPE
  real :: CIRCFF, FHINV, POR, CIRCTE
  
  ! COM34: viscous wedge parameters  
  integer :: NWDGE
  real :: REYNLD, WCONST
  real :: WSLP(100,2)  ! Viscous wedge slopes
  
  ! COM30: general workspace arrays  
  real :: XI(100), ARG(100), REST(204)  ! Additional variables needed by other modules
  integer :: JLIN(3)           ! Line indices for printing
  character(len=1) :: IPC(100) ! Flow regime indicators
  real :: VT(100,2)            ! Velocity time history

  ! File unit numbers for different output files
  integer, parameter :: UNIT_INPUT = 2          ! Input file (like original iread=2)
  integer, parameter :: UNIT_LOG = 10           ! Main log file (tsfoil.log)
  integer, parameter :: UNIT_ECHO = 11          ! Input echo file (tsfoil.ech)
  integer, parameter :: UNIT_CP = 12            ! Cp distribution file (tsfoil.cp)
  integer, parameter :: UNIT_FIELD = 13         ! Field data file (tsfoil.fld)
  integer, parameter :: UNIT_FLOW = 14          ! Flow map file (tsfoil.map)
  integer, parameter :: UNIT_OUTPUT = 15        ! Standard output (tsfoil.out)
  integer, parameter :: UNIT_WALL = 16          ! Wall data file (tsfoil.wal)
  integer, parameter :: UNIT_RESTART = 7        ! Restart file (fort.7)
  integer, parameter :: UNIT_SHOCK = 18         ! Shock analysis file (tsfoil.shk)

contains

  subroutine initialize_common(nx, ny)
    ! Allocates and initializes mesh arrays to match original TSFOIL dimensions
    ! Original: P(102,101), X(100), Y(100), XIN(100), YIN(100)
    integer, intent(in) :: nx, ny
    if (allocated(XIN)) deallocate(XIN)
    if (allocated(YIN)) deallocate(YIN)
    if (allocated(P)) deallocate(P)
    if (allocated(X)) deallocate(X)
    if (allocated(Y)) deallocate(Y)    ! Allocate arrays with original TSFOIL dimensions plus room for extra points
    allocate(XIN(102))    ! Original XIN(100) + room for 2 extra points in CKMESH
    allocate(YIN(102))    ! Original YIN(100) + room for 2 extra points in CKMESH
    allocate(P(102,101))  ! Original P(102,101)
    allocate(X(102))      ! Original X(100) + room for extra points
    allocate(Y(102))      ! Original Y(100) + room for extra points
    ! Initialize arrays to zero
    P = 0.0
    X = 0.0
    Y = 0.0    
    XIN = 0.0
    YIN = 0.0
    ! Default initial values (will be overridden by READIN with IMAXI/JMAXI from input)
    IMIN = 1
    IMAX = 100    ! Temporary default - will be set to IMAXI in READIN
    JMIN = 1
    JMAX = 100    ! Temporary default - will be set to JMAXI in READIN
    
    ! Initialize mesh indices to safe defaults (will be recalculated later)
    IUP = 2
    IDOWN = IMAX - 1
    ILE = IMIN + 5  ! Safe default
    ITE = IMAX - 5  ! Safe default
    JUP = (JMAX + JMIN) / 2 + 1  ! Safe default above center
    JLOW = (JMAX + JMIN) / 2 - 1  ! Safe default below center
    JTOP = JMAX - 1
    JBOT = JMIN + 1
    J1 = JBOT + 1
    J2 = JTOP - 1
      AMESH = .false.
    IREF = 0
    ICUT = 0
    KSTEP = 1
    ABORT1 = .false.
    PHYS = .true.
    SIMDEF = 1
    BCFOIL = 1
    BCTYPE = 1  ! Default to free air boundary condition
    DELTA = 0.1
    EMACH = 0.8
    PRTFLO = 1
    PSTART = 1  ! Default to fresh start
    
    ! Initialize airfoil control parameters
    RIGF = 1.0
    IFLAP = 0
    DELFLP = 0.0
    FLPLOC = 0.8
    FSYM = 0
    
    ! Initialize viscous wedge parameters
    NWDGE = 0
    REYNLD = 0.0
    WCONST = 4.0
    WSLP = 0.0
    
    ! Initialize constants    
    PI = 3.1415926535897932384626
    HALFPI = PI * 0.5
    TWOPI = PI * 2.0
    RTKPOR = 0.0
    F = 0.0
    H = 0.0

  end subroutine initialize_common

end module common_data
