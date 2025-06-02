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
  public :: CXXC, CXXL, CXXR, CYYC, CYYD, CYYU, IVAL, XDIFF, YDIFF
  public :: CJUP, CJUP1, CJLOW, CJLOW1, CYYBUD, CYYBUC, CYYBUU
  public :: CYYBLU, CYYBLC, CYYBLD, FXLBC, FXUBC    
  public :: DTOP, DBOT, VTOP, VBOT, DUP, DDOWN, VUP, VDOWN
  public :: DIAG, RHS, SUB, SUP
  public :: JLIN, IPC, VT, PSTART, CIRCFF, CIRCTE
  public :: PJUMP, FCR, KUTTA, CVERGE, ERROR, IERROR, MAXIT, IPRTER
  public :: CLSET, IDLA
  public :: EPS, WE, NWDGE, REYNLD, WCONST, WSLP, WI
  public :: XCP, CPP  ! For DLAOUT functionality
  public :: XSHK, THAMAX, AM1, ZETA, NVWPRT, NISHK
  public :: DVERGE, GAM, POR, FHINV, WCIRC
  public :: YFREE, YTUN, JMXF, JMXT
  public :: B, BETA0, BETA1, BETA2, PSI0, PSI1, PSI2
  public :: ALPHA0, ALPHA1, ALPHA2, XSING, OMEGA0, OMEGA1, OMEGA2, JET
  public :: XI, ARG, REST  ! Working arrays for DRAG function
  public :: THETA  ! COM33: angle array for each mesh point
  public :: initialize_common
  public :: UNIT_INPUT, UNIT_LOG, UNIT_ECHO, UNIT_CP, UNIT_FIELD
  public :: UNIT_FLOW, UNIT_OUTPUT, UNIT_WALL, UNIT_RESTART, UNIT_SHOCK
  public :: UNIT_DLAOUT_INPUT, UNIT_DLAOUT_OUTPUT  ! DLAOUT input and output files
  
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
  
  ! COM33: angle array for each mesh point
  real :: THETA(100,100)
  
  ! COM34: viscous wedge parameters  
  integer :: NWDGE
  real :: REYNLD, WCONST
  real :: WSLP(100,2)  ! Viscous wedge slopes
  real :: XSHK(2,3)    ! Shock x-locations
  real :: THAMAX(2,3)  ! Maximum wedge angles  
  real :: AM1(2,3)     ! Mach numbers upstream of shocks
  real :: ZETA(2,3)    ! Wedge length scales
  integer :: NVWPRT(2) ! Number of viscous wedge prints
  integer :: NISHK     ! Number of shocks
  
  ! COM30: general workspace arrays  
  real :: XI(100), ARG(100), REST(204)  ! Additional variables needed by other modules
  real :: XCP(100), CPP(304)  ! Arrays for DLAOUT subroutine - CP interpolation points and values
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
  integer, parameter :: UNIT_DLAOUT_INPUT = 5   ! DLAOUT input file
  integer, parameter :: UNIT_DLAOUT_OUTPUT = 10 ! DLAOUT output file

contains

  ! Initialize common data arrays and parameters
  subroutine initialize_common()
    implicit none

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
    ICUT = 2   ! Updated to match BLOCK DATA
    KSTEP = 1
    ABORT1 = .true.   ! Updated to match BLOCK DATA  
    PHYS = .true.
    SIMDEF = 3   ! Updated to match BLOCK DATA
    BCFOIL = 3   ! Updated to match BLOCK DATA
    BCTYPE = 1  ! Default to free air boundary condition
    DELTA = 0.115  ! Updated to match BLOCK DATA
    EMACH = 0.75   ! Updated to match BLOCK DATA
    PRTFLO = 1
    PSTART = 1  ! Default to fresh start
    
    ! Initialize airfoil control parameters
    RIGF = 0.0     ! Updated to match BLOCK DATA
    IFLAP = 0
    DELFLP = 5.0   ! Updated to match BLOCK DATA
    FLPLOC = 0.77  ! Updated to match BLOCK DATA
    FSYM = 0
    
    ! Initialize viscous wedge parameters
    NWDGE = 0
    REYNLD = 4.0E+06  ! Updated to match BLOCK DATA
    WCONST = 4.0
    WSLP = 0.0
    XSHK = 0.0
    THAMAX = 0.0
    AM1 = 0.0
    ZETA = 0.0
    NVWPRT = 0
    NISHK = 0
      
    ! Initialize constants (from BLOCK DATA)    
    PI = 3.14159265
    HALFPI = 1.570796325
    TWOPI = 6.28318531
    RTKPOR = 0.0
    F = 0.0
    H = 0.0
    
    ! Flow parameters (from BLOCK DATA)
    EMACH = 0.75
    DELTA = 0.115
    ALPHA = 0.12
    AK = 0.0
    GAM = 1.4
    RIGF = 0.0
    EPS = 0.2
    
    ! Solver parameters (from BLOCK DATA)
    CLSET = 0.0
    CVERGE = 0.00001
    DVERGE = 10.0
    WCIRC = 1.0
    WE(1) = 1.8
    WE(2) = 1.9
    WE(3) = 1.95
    
    ! Grid parameters (from BLOCK DATA)
    IMAXI = 77
    JMXF = 56
    MAXIT = 500
    NL = 75
    NU = 100
    IPRTER = 10
    JMAXI = 64
    JMXT = 48
    
    ! Logical flags (from BLOCK DATA)
    PHYS = .true.
    PSAVE = .false.
    FCR = .true.
    KUTTA = .true.
    ABORT1 = .true.
    AMESH = .false.
    
    ! Boundary condition parameters (from BLOCK DATA)
    BCFOIL = 3
    BCTYPE = 1
    PSTART = 1
    PRTFLO = 1
    SIMDEF = 3
    
    ! Flap parameters (from BLOCK DATA)
    IFLAP = 0
    DELFLP = 5.0
    FLPLOC = 0.77
    
    ! Viscous parameters (from BLOCK DATA)
    REYNLD = 4.0E+06
    WCONST = 4.0
    NWDGE = 0
    IDLA = 0
    
    ! Initialize YIN array to zero (from BLOCK DATA)
    YIN = 0.0
    
    ! Initialize XIN array with default mesh distribution (from BLOCK DATA)
    XIN(1:77) = (/ -1.075, -0.950, -0.825, -0.7, -0.575, -0.45, -0.35, &
                   -0.25, -0.175, -0.125, -0.075, -0.0525, -0.035, -0.0225, -0.015, &
                   -0.0075, -0.0025, 0.0025, 0.0075, 0.0125, 0.0175, 0.0225, &
                   0.0275, 0.0325, 0.0375, 0.045, 0.055, 0.065, 0.075, 0.085, &
                   0.0975, 0.115, 0.140625, 0.171875, 0.203125, 0.234375, 0.265625, &
                   0.296875, 0.328125, 0.359375, 0.390625, 0.421875, 0.453125, &
                   0.484375, 0.515625, 0.546875, 0.578125, 0.609375, 0.640625, &
                   0.671875, 0.703125, 0.734375, 0.765625, 0.796875, 0.828125, &
                   0.859375, 0.885, 0.9, 0.915, 0.93, 0.945, 0.96, 0.975, 0.99, &
                   1.0, 1.01, 1.025, 1.05, 1.09, 1.15, 1.225, 1.3, 1.4, 1.5, &
                   1.625, 1.75, 1.875 /)
    if (size(XIN) > 77) XIN(78:) = 0.0
    
    ! Initialize YFREE array with default free-air distribution (from BLOCK DATA)
    YFREE(1:56) = (/ -5.2, -4.4, -3.6, -3.0, -2.4, -1.95, -1.6, -1.35, -1.15, -0.95, &
                     -0.80, -0.65, -0.55, -0.45, -0.39, -0.34, -0.30, -0.27, -0.24, -0.21, &
                     -0.18, -0.15, -0.125, -0.1, -0.075, -0.05, -0.03, -0.01, 0.01, 0.03, &
                     0.05, 0.075, 0.1, 0.125, 0.15, 0.18, 0.21, 0.24, 0.27, 0.30, &
                     0.34, 0.39, 0.45, 0.55, 0.65, 0.8, 0.95, 1.15, 1.35, 1.60, &
                     1.95, 2.4, 3.0, 3.6, 4.4, 5.2 /)
    YFREE(57:) = 0.0
    
    ! Initialize YTUN array with default tunnel distribution (from BLOCK DATA)
    YTUN(1:48) = (/ -2.0, -1.8, -1.6, -1.4, -1.2, -1.0, -0.8, -0.65, -0.55, -0.45, &
                    -0.39, -0.34, -0.30, -0.27, -0.24, -0.21, -0.18, -0.15, -0.125, -0.1, &
                    -0.075, -0.05, -0.03, -0.01, 0.01, 0.03, 0.05, 0.075, 0.1, 0.125, &
                    0.15, 0.18, 0.21, 0.24, 0.27, 0.3, 0.34, 0.39, 0.45, 0.55, &
                    0.65, 0.8, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0 /)
    YTUN(49:) = 0.0
    
    ! Initialize default airfoil upper surface coordinates (from BLOCK DATA)
    XU(1:100) = (/ 0.000008, 0.000167, 0.000391, 0.000799, 0.001407, 0.002153, 0.003331, 0.005336, 0.008648, 0.014583, &
                   0.023481, 0.033891, 0.040887, 0.053973, 0.056921, 0.058456, 0.059966, 0.061445, 0.062909, 0.065925, &
                   0.068785, 0.071482, 0.074007, 0.075322, 0.076603, 0.077862, 0.079112, 0.080445, 0.081819, 0.083269, &
                   0.084841, 0.086702, 0.088848, 0.091378, 0.094413, 0.098308, 0.103104, 0.109010, 0.116244, 0.125452, &
                   0.136635, 0.150037, 0.165853, 0.184699, 0.195177, 0.206361, 0.218244, 0.230813, 0.244047, 0.257917, &
                   0.272371, 0.287410, 0.302990, 0.319057, 0.335555, 0.352421, 0.369591, 0.386995, 0.404133, 0.421391, &
                   0.438708, 0.456013, 0.473246, 0.490343, 0.507242, 0.523881, 0.539536, 0.554867, 0.569823, 0.584351, &
                   0.598405, 0.611936, 0.624904, 0.637273, 0.648435, 0.659016, 0.668987, 0.678321, 0.687012, 0.695090, &
                   0.706936, 0.728406, 0.738649, 0.761390, 0.777010, 0.792241, 0.809068, 0.824992, 0.836953, 0.857188, &
                   0.875621, 0.898268, 0.913686, 0.927686, 0.939804, 0.952002, 0.971789, 0.989100, 0.997860, 1.000000 /)
    
    YU(1:100) = (/ 0.000787, 0.003092, 0.004538, 0.006137, 0.007683, 0.009056, 0.010675, 0.012803, 0.015607, 0.019624, &
                   0.024441, 0.029035, 0.031698, 0.035966, 0.036837, 0.037277, 0.037700, 0.038103, 0.038497, 0.039276, &
                   0.039986, 0.040625, 0.041195, 0.041483, 0.041756, 0.042019, 0.042274, 0.042539, 0.042804, 0.043079, &
                   0.043368, 0.043700, 0.044072, 0.044497, 0.044989, 0.045595, 0.046312, 0.047154, 0.048132, 0.049301, &
                   0.050626, 0.052089, 0.053663, 0.055351, 0.056210, 0.057068, 0.057918, 0.058751, 0.059559, 0.060335, &
                   0.061068, 0.061751, 0.062381, 0.062947, 0.063445, 0.063867, 0.064213, 0.064473, 0.064646, 0.064733, &
                   0.064735, 0.064651, 0.064477, 0.064218, 0.063871, 0.063438, 0.062945, 0.062376, 0.061731, 0.061014, &
                   0.060232, 0.059389, 0.058496, 0.057562, 0.056650, 0.055721, 0.054791, 0.053867, 0.052965, 0.052086, &
                   0.050722, 0.048045, 0.046680, 0.043441, 0.041053, 0.038606, 0.035768, 0.032958, 0.030775, 0.026954, &
                   0.023361, 0.018848, 0.015750, 0.012954, 0.010567, 0.008213, 0.004559, 0.001620, 0.000293, 0.000000 /)
    
    ! Initialize default airfoil lower surface coordinates (from BLOCK DATA)
    XL(1:75) = (/ 0.000000, 0.000012, 0.000043, 0.000183, 0.000249, 0.000348, 0.000455, 0.000680, 0.001011, 0.001481, &
                  0.001875, 0.002316, 0.003055, 0.004201, 0.004747, 0.005779, 0.007035, 0.008265, 0.009969, 0.012286, &
                  0.015346, 0.019276, 0.025335, 0.029379, 0.039095, 0.052516, 0.062469, 0.073329, 0.085290, 0.099822, &
                  0.118563, 0.140987, 0.167184, 0.202933, 0.228511, 0.247895, 0.263995, 0.282047, 0.297045, 0.310147, &
                  0.324075, 0.344872, 0.363502, 0.387644, 0.404492, 0.426308, 0.450016, 0.475378, 0.521837, 0.549843, &
                  0.578612, 0.605305, 0.623479, 0.642152, 0.657543, 0.671212, 0.690340, 0.708891, 0.726684, 0.746683, &
                  0.768502, 0.784892, 0.801149, 0.819187, 0.838548, 0.858817, 0.879431, 0.903723, 0.926504, 0.943652, &
                  0.958668, 0.973623, 0.986187, 0.996582, 1.000000 /)
    XL(76:) = 0.0
    
    YL(1:75) = (/ 0.000000, -0.000700, -0.001385, -0.002868, -0.003330, -0.003880, -0.004379, -0.005199, -0.006133, -0.007183, &
                  -0.007933, -0.008676, -0.009776, -0.011204, -0.011815, -0.012861, -0.013983, -0.014962, -0.016175, -0.017636, &
                  -0.019336, -0.021258, -0.023836, -0.025373, -0.028634, -0.032423, -0.034840, -0.037182, -0.039456, -0.041862, &
                  -0.044483, -0.047017, -0.049298, -0.051443, -0.052406, -0.052859, -0.053062, -0.053117, -0.053027, -0.052849, &
                  -0.052562, -0.051951, -0.051218, -0.050013, -0.049004, -0.047495, -0.045601, -0.043288, -0.038336, -0.034916, &
                  -0.031104, -0.027333, -0.024661, -0.021854, -0.019517, -0.017429, -0.014527, -0.011771, -0.009228, -0.006537, &
                  -0.003868, -0.002086, -0.000524, 0.000950, 0.002227, 0.003224, 0.003885, 0.004212, 0.004067, 0.003657, &
                  0.003067, 0.002242, 0.001329, 0.000376, 0.000000 /)
    YL(76:) = 0.0
    
    ! Initialize other arrays to zero (from BLOCK DATA)
    WSLP = 0.0
    ZETA = 0.0

  end subroutine initialize_common

end module common_data
