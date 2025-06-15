! common_data.f90
! Module replacing legacy COMMON blocks for shared data

module common_data
  implicit none
  private
  
  ! Mesh size parameter - change this to adjust mesh dimensions
  integer, parameter :: N_MESH_POINTS = 1000
  integer, parameter :: NMP_plus2 = N_MESH_POINTS + 2  ! Number of mesh points + 2
  integer, parameter :: NMP_plus1 = N_MESH_POINTS + 1  ! Number of mesh points + 1
  
  public :: N_MESH_POINTS, NMP_plus2, NMP_plus1
  public :: IMIN, IMAX, IUP, IDOWN, ILE, ITE, JMIN, JMAX, JUP, JLOW, JTOP, JBOT
  public :: AK, ALPHA, DUB, GAM1, RTK, PHYS
  public :: KSTEP, ABORT1
  public :: XIN, YIN
  public :: P, X, Y  ! Main solution and coordinate arrays
  public :: FL, FXL, FU, FXU, CAMBER, THICK, XFOIL, VOL, IFOIL
  public :: NL, NU, XL, XU, YL, YU, PERCENT, CHORD
  public :: RIGF, IFLAP, DELFLP, FLPLOC, FSYM
  public :: SIMDEF, DELTA, EMACH, DELRT2, EMROOT, CL
  public :: CDFACT, CLFACT, CMFACT, CPFACT, CPSTAR, YFACT, VFACT, SONVEL
  public :: F, H, HALFPI, PI, RTKPOR, TWOPI
  public :: IMAXI, JMAXI
  public :: XMID, YMID  ! Additional public declarations for io_module and other modules
  public :: BCTYPE, CPL, CPU, C1, CXL, CXC, CXR
  public :: CXXC, CXXL, CXXR, CYYC, CYYD, CYYU, IVAL, XDIFF, YDIFF
  public :: CJUP, CJUP1, CJLOW, CJLOW1, CYYBUD, CYYBUC, CYYBUU
  public :: CYYBLU, CYYBLC, CYYBLD, FXLBC, FXUBC    
  public :: DTOP, DBOT, VTOP, VBOT, DUP, DDOWN, VUP, VDOWN
  public :: DIAG, RHS, SUB, SUP
  public :: CIRCFF, CIRCTE
  public :: PJUMP, FCR, KUTTA, CVERGE, ERROR, IERROR, JERROR, MAXIT, IPRTER
  public :: CLSET
  public :: EPS, WE, NWDGE, REYNLD, WCONST, WSLP, WI
  public :: XSHK, THAMAX, AM1, ZETA, NVWPRT, NISHK
  public :: DVERGE, GAM, POR, FHINV, WCIRC
  public :: YFREE, YTUN, JMXF, JMXT
  public :: B, BETA0, BETA1, BETA2, PSI0, PSI1, PSI2
  public :: ALPHA0, ALPHA1, ALPHA2, XSING, OMEGA0, OMEGA1, OMEGA2, JET
  public :: THETA  ! COM33: angle array for each mesh point
  public :: EMU, POLD, DCIRC, OUTERR  ! Missing variables from COM18
  public :: initialize_common, INPERR
  public :: UNIT_INPUT, UNIT_OUTPUT
  public :: UNIT_SUMMARY, UNIT_CPXS, UNIT_MESH, UNIT_FIELD
  
  ! Mesh indices (from COMMON /COM1/)

  integer :: IUP, IDOWN       ! upstream/downstream indices
  integer :: ILE, ITE         ! leading/trailing edge i-indices
  
  integer :: JUP              ! upper surface j-indices, index of first point where Y > 0.0 (calculated by JSLIT)
  integer :: JLOW             ! lower surface j-indices, JLOW = JUP - 1 (calculated by JSLIT)
  integer :: JTOP, JBOT       ! far-field top/bottom j-indices

  ! Main solution arrays
  real :: P(NMP_plus2, NMP_plus1)    ! Potential solution array


  ! Flow parameters (from /COM2/)
  real :: AK       ! freestream similarity parameter
  real :: ALPHA    ! angle of attack
  real :: DUB      ! doublet strength
  real :: GAM1     ! gamma - 1
  real :: RTK      ! sqrt(gamma)
  logical :: PHYS  ! physical (True) vs similarity (False)

  ! Control flags and refinement (from /COM3/)
  integer :: KSTEP  ! SOR sweep step size
  logical :: ABORT1 ! input abort flag

  ! User-input mesh coordinate arrays (from /COM4/)
  real :: XIN(NMP_plus2), YIN(NMP_plus2)  ! room for 2 extra points in CKMESH
  
  ! Mesh coordinate arrays
  real :: X(NMP_plus2), Y(NMP_plus2) ! room for extra points

  ! Coarse mesh coordinate arrays: midpoint (from /COM20/)
  real :: XMID(N_MESH_POINTS), YMID(N_MESH_POINTS)

  integer :: IMIN, IMAX   ! maximum number of grid points in i-direction used in code
  integer :: JMIN, JMAX   ! maximum number of grid points in j-direction used in code
  integer :: IMAXI, JMAXI ! User-input maximum number of streamwise (X-direction) and spanwise (Y-direction) grid points

  ! COM5: mesh derivative arrays
  real :: XDIFF(N_MESH_POINTS), YDIFF(N_MESH_POINTS)
  
  ! COM6: surface and flow arrays
  real :: FU(N_MESH_POINTS), FL(N_MESH_POINTS), FXU(N_MESH_POINTS), FXL(N_MESH_POINTS)
  real :: CAMBER(N_MESH_POINTS), THICK(N_MESH_POINTS), XFOIL(N_MESH_POINTS)
  real :: VOL
  integer :: IFOIL
  
  ! COM7: boundary extrapolation/coefficient flags
  real :: CJUP, CJUP1, CJLOW, CJLOW1

  ! COM8: solver control parameters
  real :: CVERGE, DVERGE
  real :: WI = 1.05                 ! SOR relaxation factor (from COM18)
  real :: WE(3)
  integer :: IPRTER, MAXIT
  
  ! COM9: airfoil definition flags
  integer :: NL, NU
  real :: XL(N_MESH_POINTS), XU(N_MESH_POINTS), YL(N_MESH_POINTS), YU(N_MESH_POINTS)
  real :: PERCENT, CHORD
  
  ! Airfoil control parameters
  real :: RIGF        ! rigidity factor for transonic effects
  integer :: IFLAP    ! flap flag
  real :: DELFLP      ! flap deflection angle  
  real :: FLPLOC      ! flap location
  integer :: FSYM     ! symmetry flag
  
  ! COM10: free-stream/tunnel arrays
  real :: YFREE(N_MESH_POINTS), YTUN(N_MESH_POINTS), GAM
  integer :: JMXF, JMXT
  
  ! COM12: wall/tunnel constants  
  real :: F, H, HALFPI, PI, RTKPOR, TWOPI
  
  ! COM13: coefficient scaling factors
  real :: CDFACT, CLFACT, CMFACT, CPFACT, CPSTAR
  
  ! COM14: Kutta and circulation flags
  real :: CLSET     ! Lift coefficient setpoint
  real :: WCIRC     ! Weight for circulation jump at trailing edge (0.0-1.0)
  logical :: FCR    ! Whether difference equations are fully conservative
  logical :: KUTTA  ! Whether Kutta condition is enforced
  
  ! COM15: vortex/doublet parameters
  real :: B, BETA0, BETA1, BETA2, PSI0, PSI1, PSI2
  
  ! COM16: far-field root parameters
  real :: ALPHA0, ALPHA1, ALPHA2, XSING, OMEGA0, OMEGA1, OMEGA2, JET
  
  ! COM17: special boundary coefficient arrays
  real :: CYYBLC, CYYBLD, CYYBLU, CYYBUC, CYYBUD, CYYBUU
  real :: FXLBC(N_MESH_POINTS), FXUBC(N_MESH_POINTS)
    
  ! COM18: error tracking and diagnostics
  real :: ERROR
  integer :: IERROR, JERROR
  real :: EMU(N_MESH_POINTS,2)   ! circulation factors
  real :: POLD(N_MESH_POINTS,2)  ! old potential values  
  real :: DCIRC        ! circulation change
  logical :: OUTERR    ! outer iteration error (logical)
  
  ! COM19: jump arrays and pressure jump
  real :: PJUMP(N_MESH_POINTS)
  
  ! COM19: tridiagonal solver arrays
  real :: DIAG(N_MESH_POINTS), RHS(N_MESH_POINTS), SUB(N_MESH_POINTS), SUP(N_MESH_POINTS)
    
  ! COM22: central differencing coefficients
  real :: CXC(N_MESH_POINTS), CXL(N_MESH_POINTS), CXR(N_MESH_POINTS)
  real :: CXXC(N_MESH_POINTS), CXXL(N_MESH_POINTS), CXXR(N_MESH_POINTS)
  real :: C1(N_MESH_POINTS)
  
  ! COM23: boundary differencing coefficients
  real :: CYYC(N_MESH_POINTS), CYYD(N_MESH_POINTS), CYYU(N_MESH_POINTS)
  integer :: IVAL
  
  ! COM24: far-field boundary arrays
  real :: DTOP(N_MESH_POINTS), DBOT(N_MESH_POINTS), DUP(N_MESH_POINTS), DDOWN(N_MESH_POINTS)
  real :: VTOP(N_MESH_POINTS), VBOT(N_MESH_POINTS), VUP(N_MESH_POINTS), VDOWN(N_MESH_POINTS)
  
  ! COM25: pressure coefficient arrays on X-line (Y=0)
  real :: CPU(N_MESH_POINTS), CPL(N_MESH_POINTS)
  
  ! COM27: transonic similarity state
  real :: CL, DELTA, DELRT2, EMACH, EMROOT, EPS
  integer :: SIMDEF
  real :: SONVEL, VFACT, YFACT
  
  ! COM28: boundary condition identifiers
  integer :: BCTYPE
  real :: CIRCFF, FHINV, POR, CIRCTE
  
  ! COM33: angle array for each mesh point
  real :: THETA(N_MESH_POINTS,N_MESH_POINTS)
  
  ! COM34: viscous wedge parameters  
  integer :: NWDGE
  real :: REYNLD, WCONST
  real :: WSLP(N_MESH_POINTS,2)  ! Viscous wedge slopes
  real :: XSHK(2,3)    ! Shock x-locations
  real :: THAMAX(2,3)  ! Maximum wedge angles  
  real :: AM1(2,3)     ! Mach numbers upstream of shocks
  real :: ZETA(2,3)    ! Wedge length scales
  integer :: NVWPRT(2) ! Number of viscous wedge prints
  integer :: NISHK     ! Number of shocks
  
  ! File unit numbers for different output files  
  integer, parameter :: UNIT_INPUT = 2          ! Input file
  integer, parameter :: UNIT_OUTPUT = 15        ! tsfoil2.out (Main output file with comprehensive results)
  integer, parameter :: UNIT_SUMMARY = 16       ! smry.out (Summary file with key results)
  integer, parameter :: UNIT_CPXS = 17          ! cpxs.out (Pressure coefficient vs. X-coordinate data)
  integer, parameter :: UNIT_MESH = 20          ! mesh.dat (Mesh coordinate data)
  integer, parameter :: UNIT_FIELD = 11         ! field.dat (Pressure coefficient and Mach number field data)

contains

  ! Initialize common data arrays and parameters
  subroutine initialize_common()
    implicit none

    ! Initialize mesh coordinate arrays
    X = 0.0
    Y = 0.0    
    XIN = 0.0
    YIN = 0.0

    ! Initialize potential array P
    P = 0.0
    DUB = 0.0
    CIRCFF = 0.0
    CIRCTE = 0.0

    ! Default initial values (will be overridden by READIN with IMAXI/JMAXI from input)
    IMIN = 1
    IMAX = N_MESH_POINTS
    JMIN = 1
    JMAX = N_MESH_POINTS
    
    ! Initialize mesh indices to safe defaults (will be recalculated later)
    IUP = 2
    IDOWN = IMAX - 1
    ILE = IMIN + 5  ! Safe default
    ITE = IMAX - 5  ! Safe default
    JUP = (JMAX + JMIN) / 2 + 1   ! Safe default above center
    JLOW = (JMAX + JMIN) / 2 - 1  ! Safe default below center
    JTOP = JMAX - 1
    JBOT = JMIN + 1

    KSTEP = 1
    PHYS = .true.
    BCTYPE = 1  ! Default to free air boundary condition
    DELTA = 0.115  ! Updated to match BLOCK DATA
    
    ! Initialize airfoil control parameters
    FSYM = 0
    
    ! Initialize viscous wedge parameters
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
    EPS = 0.2     ! Default epsilon for convergence checks
    
    ! Solver parameters (from BLOCK DATA)
    CLSET = 0.0
    CVERGE = 0.00001
    DVERGE = 10.0
    WCIRC = 1.0
    WE(1) = 1.8
    WE(2) = 1.9
    WE(3) = 1.95
    
    ! Grid parameters (from BLOCK DATA)
    IMAXI = 77  ! User-input maximum number of streamwise (X-direction) grid points (match default XIN)
    JMAXI = N_MESH_POINTS  ! User-input maximum number of spanwise (Y-direction) grid points
    NU = 100    ! Number of lower surface grid points (match default XU, YU)
    NL = 75     ! Number of upper surface grid points (match default XL, YL)

    MAXIT = 1000  ! Maximum number of iterations
    IPRTER = 100  ! Print interval for convergence history

    ! Logical flags (from BLOCK DATA)
    PHYS = .true.
    FCR = .true.
    KUTTA = .true.
    ABORT1 = .false.
    
    ! Boundary condition parameters (from BLOCK DATA)
    BCTYPE = 1
    SIMDEF = 3
    
    ! Flap parameters (from BLOCK DATA)
    IFLAP = 0
    DELFLP = 5.0
    FLPLOC = 0.77
    
    ! Viscous parameters (from BLOCK DATA)
    REYNLD = 4.0E+06
    WCONST = 4.0
    NWDGE = 0

    ! Initialize boundary condition identifiers
    POR = 0.0
        
    ! Initialize airfoil coordinate arrays to zero to prevent namelist floating-point exceptions
    XU = 0.0
    YU = 0.0
    XL = 0.0
    YL = 0.0
    
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
    
    JMXF = 56  ! Maximum number of grid points for free-air distribution (YFREE)
    JMXT = 48  ! Maximum number of grid points for tunnel distribution (YTUN)

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

    YL(1:75) = (/ 0.000000, -0.000700, -0.001385, -0.002868, -0.003330, -0.003880, -0.004379, -0.005199, -0.006133, -0.007183, &
                  -0.007933, -0.008676, -0.009776, -0.011204, -0.011815, -0.012861, -0.013983, -0.014962, -0.016175, -0.017636, &
                  -0.019336, -0.021258, -0.023836, -0.025373, -0.028634, -0.032423, -0.034840, -0.037182, -0.039456, -0.041862, &
                  -0.044483, -0.047017, -0.049298, -0.051443, -0.052406, -0.052859, -0.053062, -0.053117, -0.053027, -0.052849, &
                  -0.052562, -0.051951, -0.051218, -0.050013, -0.049004, -0.047495, -0.045601, -0.043288, -0.038336, -0.034916, &
                  -0.031104, -0.027333, -0.024661, -0.021854, -0.019517, -0.017429, -0.014527, -0.011771, -0.009228, -0.006537, &
                  -0.003868, -0.002086, -0.000524, 0.000950, 0.002227, 0.003224, 0.003885, 0.004212, 0.004067, 0.003657, &
                  0.003067, 0.002242, 0.001329, 0.000376, 0.000000 /)

    ! Initialize other arrays to zero (from BLOCK DATA)
    WSLP = 0.0
    ZETA = 0.0
    
    ! Initialize missing COM18 variables
    EMU = 0.0
    POLD = 0.0
    DCIRC = 0.0
    OUTERR = .false.
    IERROR = 0
    JERROR = 0
    ERROR = 0.0
    
    ! Initialize COM32 variables
    THETA = 0.0

  end subroutine initialize_common


  ! Fatal error - write message and stop
  subroutine INPERR(I_ERROR_CODE)
    implicit none
    integer, intent(in) :: I_ERROR_CODE
    
    select case (I_ERROR_CODE)
    case (1)
      write(UNIT_OUTPUT, '(A)') ' '
      write(UNIT_OUTPUT, '(5X,A)') 'IMAX OR JMAX IS GREATER THAN N_MESH_POINTS, NOT ALLOWED.'
    case (2)
      write(UNIT_OUTPUT, '(A)') ' '
      write(UNIT_OUTPUT, '(5X,A)') 'X MESH POINTS NOT MONOTONIC INCREASING.'
    case (3)
      write(UNIT_OUTPUT, '(A)') ' '
      write(UNIT_OUTPUT, '(5X,A)') 'Y MESH POINTS NOT MONOTONIC INCREASING.'
    case (4)
      write(UNIT_OUTPUT, '(A)') ' '
      write(UNIT_OUTPUT, '(5X,A)') 'MACH NUMBER NOT IN PERMITTED RANGE. (.5,2.0)'
    case (5)
      write(UNIT_OUTPUT, '(A)') ' '
      write(UNIT_OUTPUT, '(5X,A)') 'ALPHA NOT IN PERMITTED RANGE. (-9.0, 9.0)'
    case (6)
      write(UNIT_OUTPUT, '(A)') ' '
      write(UNIT_OUTPUT, '(5X,A)') 'DELTA NOT IN PERMITTED RANGE. ( 0.0, 1.0)'
    case (7)
      write(UNIT_OUTPUT, '(A)') ' '
      write(UNIT_OUTPUT, '(5X,A)') 'AK=0. VALUE OF AK MUST BE INPUT SINCE PHYS=F.'
    case (8)
      write(UNIT_OUTPUT, '(A)') ' '
      write(UNIT_OUTPUT, '(5X,A)') 'MACH NUMBER IS NOT LESS THAN 1.0 FOR VISCOUS WEDGE INCLUSION'
    case default
      write(UNIT_OUTPUT, '(A)') ' '
      write(UNIT_OUTPUT, '(5X,A)') 'UNKNOWN ERROR CODE.'
    end select
    
    stop
  end subroutine INPERR


end module common_data
