! common_data.f90
! Global data:
!   - Constants
!   - User-input parameters
!   - Mesh and geometry parameters and arrays

module common_data
    implicit none
    
    ! ------------------------------------------------
    ! Constants
    ! ------------------------------------------------
    
    integer, parameter :: N_MESH_POINTS = 1000            ! Mesh size parameter - change this to adjust mesh dimensions
    integer, parameter :: NMP_plus2 = N_MESH_POINTS + 2   ! Number of mesh points + 2
    integer, parameter :: NMP_plus1 = N_MESH_POINTS + 1   ! Number of mesh points + 1

    integer, parameter :: IMIN = 1
    integer, parameter :: JMIN = 1

    real, parameter :: GAM = 1.4            ! Specific heat ratio
    real, parameter :: GAM1 = GAM + 1.0     ! gamma + 1
    real, parameter :: PI = 3.14159265      ! pi
    real, parameter :: HALFPI = 1.570796325 ! 1/2 pi
    real, parameter :: TWOPI = 6.28318531   ! 2 pi

    ! ------------------------------------------------
    ! User-input parameters
    ! ------------------------------------------------

    real :: EMACH = 0.75    ! Mach number
    real :: ALPHA = 0.0     ! Angle of attack

    real :: DELTA = 0.0       ! Maximum thickness of airfoil (set to zero to raise error)

    integer :: IMAXI = 0, JMAXI = 0 ! User-input maximum number of X/Y-direction grid points

    integer :: NWDGE = 0    ! Viscous wedge parameters (0 = no wedge, 1 = Murman wedge, 2 = Yoshihara wedge)
    integer :: SIMDEF = 3   ! Similarity scaling (1 = Cole, 2 = Spreiter, 3 = Krupp)

    real :: AK = 0.0        ! Free stream similarity parameter

    real :: REYNLD = 4.0E6  ! Reynolds number
    real :: WCONST = 4.0    ! Wall constant

    real :: WCIRC = 1.0         ! Weight for circulation jump at trailing edge (0.0-1.0)
    integer :: IPRTER = 100     ! Print interval for convergence history
    integer :: MAXIT = 1000     ! Maximum number of iterations

    real :: EPS = 0.2           ! Convergence tolerance 
    real :: WE(3)               ! SOR relaxation factors
    data WE /1.8, 1.9, 1.95/
    real :: CVERGE = 0.00001    ! Error criterion for convergence
    real :: DVERGE = 10.0       ! Error criterion for divergence

    ! Correction of Full-Supersonic (CFS) / divergence recovery
    logical :: FLAG_CFS = .false. ! Whether to apply CFS correction (set to true to activate Mode B)
    logical :: CFS_TRIGGERED = .false. ! Whether CFS correction has been triggered in current run
    real    :: BETA_SONIC = 100.0 ! Sonic penalty strength multiplier (applied as EPS*BETA_SONIC)
    real    :: EPS_AMPL = 200.0   ! EPS amplification factor at trailing-edge columns in Mode B
    integer :: ITER_START_CFS = 100   ! Iteration when CFS can start being triggered
    real    :: DXTE_CFS = 0.05        ! Monitor supersonic appearance in [1-DX, 1+DX] for CFS triggering

    ! ------------------------------------------------
    ! Mesh and geometry parameters and arrays
    ! ------------------------------------------------
    ! Mesh indices
    integer :: IMAX, JMAX   ! maximum number of grid points in X/Y-direction used in code
    integer :: IUP, IDOWN   ! upstream/downstream indices
    integer :: ILE, ITE     ! leading/trailing edge i-indices
    integer :: JUP          ! upper surface j-indices, index of first point where Y > 0.0 (calculated by JSLIT)
    integer :: JLOW         ! lower surface j-indices, JLOW = JUP - 1 (calculated by JSLIT)
    integer :: JTOP, JBOT   ! far-field top/bottom j-indices

    ! Mesh coordinate arrays
    real :: X(NMP_plus2) = 0.0, Y(NMP_plus2) = 0.0  ! Mesh coordinate arrays
    real :: XDIFF(N_MESH_POINTS) = 0.0, YDIFF(N_MESH_POINTS) = 0.0 ! mesh derivative arrays

    ! Airfoil arrays
    real :: VOL = 0.0
    integer :: NFOIL = 0  ! Number of points on airfoil
    real :: FXU(N_MESH_POINTS) = 0.0 ! Derivative of upper surface to X-coordinate
    real :: FXL(N_MESH_POINTS) = 0.0 ! Derivative of lower surface to X-coordinate

    ! ------------------------------------------------
    ! Output control
    ! ------------------------------------------------

    integer :: FLAG_OUTPUT = 1  ! Flag to print iteration info to screen

contains

    ! Initialize common data arrays and parameters
    subroutine initialize_common()
        implicit none

        ! Default initial values (will be overridden by READIN with IMAXI/JMAXI from input)
        IMAX = N_MESH_POINTS
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

        ! Grid parameters (from BLOCK DATA)
        IMAXI = 77  ! User-input maximum number of streamwise (X-direction) grid points
        JMAXI = N_MESH_POINTS  ! User-input maximum number of spanwise (Y-direction) grid points

        ! ------------------------------------------------
        ! Initialize all common data
        ! ------------------------------------------------
        
        EMACH = 0.75
        ALPHA = 0.0

        DELTA = 0.0
    
        IMAXI = 0
        JMAXI = 0

        NWDGE = 0
        SIMDEF = 3

        AK = 0.0

        REYNLD = 4.0E6
        WCONST = 4.0
    
        WCIRC = 1.0
        IPRTER = 100
        MAXIT = 1000

        EPS = 0.2
        WE = [1.8, 1.9, 1.95]
        CVERGE = 0.00001
        DVERGE = 10.0

        FLAG_CFS = .false.
        CFS_TRIGGERED = .false.
        BETA_SONIC = 100.0
        EPS_AMPL = 200.0
        ITER_START_CFS = 100
        DXTE_CFS = 0.05

        X = 0.0
        Y = 0.0
        XDIFF = 0.0
        YDIFF = 0.0
    
        VOL = 0.0
        NFOIL = 0

    end subroutine initialize_common

    ! Helper subroutine for convergence error reporting
    subroutine report_convergence_error(subroutine_name, variable_name, iteration_number)
        implicit none
        character(len=*), intent(in) :: subroutine_name, variable_name
        integer, intent(in) :: iteration_number

        write(*,'(A,A)') 'ABNORMAL STOP IN SUBROUTINE ', subroutine_name
        write(*,'(A,A,I0)') 'NON-CONVERGENCE OF ITERATION FOR ', variable_name, iteration_number
        stop

    end subroutine report_convergence_error

end module common_data
