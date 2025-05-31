! common_data.f90
! Module replacing legacy COMMON blocks for shared data

module common_data
  implicit none
  private
  
  public :: IMIN, IMAX, IUP, IDOWN, ILE, ITE, JMIN, JMAX, JUP, JLOW, JTOP, JBOT, J1, J2
  public :: AK, ALPHA, DUB, GAM1, RTK, PHYS
  public :: IREF, ICUT, KSTEP, ABORT1
  public :: XIN, YIN, AMESH
  
  ! Mesh indices (from COMMON /COM1/)
  integer :: IMIN, IMAX       ! grid i-range
  integer :: IUP, IDOWN       ! upstream/downstream indices
  integer :: ILE, ITE         ! leading/trailing edge i-indices
  integer :: JMIN, JMAX       ! grid j-range
  integer :: JUP, JLOW        ! upper/lower surface j-indices
  integer :: JTOP, JBOT       ! far-field top/bottom j-indices
  integer :: J1, J2           ! auxiliary indices

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
  integer :: IPRTER, MAXIT, NEX, N_O, NPRINT, NPT
  logical :: WCIRC
  
  ! COM9: airfoil definition flags
  integer :: BCFOIL, NL, NU
  real :: XL(100), XU(100), PERCENT, CHORD
  
  ! COM10: free-stream/tunnel arrays
  real :: YFREE(100), YTUN(100), GAM
  integer :: JMXF, JMXT
  
  ! COM11: restart and case storage
  real :: ALPHAO, CLOLD, DELTAO, DUBO, EMACHO, VOLO
  integer :: IMINO, IMAXO, IMAXI, JMINO, JMAXO, JMAXI
  logical :: PSAVE
  integer :: PSTART
  character(len=20) :: TITLE, TITLEO
  real :: XOLD(100), YOLD(100)
  
  ! COM12: wall/tunnel constants
  real :: F, H, HALFPI, PI, RTKPOR, TWOPI
  
  ! COM13: coefficient scaling factors
  real :: CDFACT, CLFACT, CMFACT, CPFACT, CPSTAR
  
  ! COM14: Kutta and circulation flags
  real :: CLSET, FCR
  integer :: KUTTA
  
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
  
  ! COM30: general workspace arrays
  real :: XI(100), ARG(100), REST(204)

contains

  subroutine initialize_common(nx, ny)
    ! Allocates and initializes mesh arrays
    integer, intent(in) :: nx, ny
    if (allocated(XIN)) deallocate(XIN)
    if (allocated(YIN)) deallocate(YIN)
    allocate(XIN(nx))
    allocate(YIN(ny))
    ! default initial values
    IMIN = 1
    IMAX = nx
    JMIN = 1
    JMAX = ny
    AMESH = .false.
    IREF = 0
    ICUT = 0
    KSTEP = 1
    ABORT1 = .false.
  end subroutine initialize_common

end module common_data
