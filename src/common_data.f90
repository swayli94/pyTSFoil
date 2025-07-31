! common_data.f90
! Global data:
!   - Constants
!   - User-input parameters
!   - Mesh and geometry parameters and arrays
!   - File unit numbers

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
    real :: CLSET = 0.0     ! Lift coefficient set-point

    integer :: BCTYPE = 1   ! Boundary condition identifiers (1 = free air, 2 = tunnel)

    integer :: NU = 0, NL = 0 ! Number of lower/upper surface data points
    real :: XL(N_MESH_POINTS) = 0.0, XU(N_MESH_POINTS) = 0.0 ! Airfoil X-coordinates
    real :: YL(N_MESH_POINTS) = 0.0, YU(N_MESH_POINTS) = 0.0 ! Airfoil Y-coordinates
    real :: DELTA = 0.0       ! Maximum thickness of airfoil (set to zero to raise error)

    integer :: IMAXI = 0, JMAXI = 0 ! User-input maximum number of X/Y-direction grid points
    real :: XIN(NMP_plus2) = 0.0, YIN(NMP_plus2) = 0.0  ! User-input mesh coordinate arrays
    
    logical :: PHYS = .true.  ! Physical (True) vs similarity (False)
    logical :: KUTTA = .true. ! Whether Kutta condition is enforced
    logical :: FCR = .true.   ! Whether difference equations are fully conservative

    integer :: NWDGE = 0    ! Viscous wedge parameters (0 = no wedge, 1 = Murman wedge, 2 = Yoshihara wedge)
    integer :: SIMDEF = 3   ! Similarity scaling (1 = Cole, 2 = Spreiter, 3 = Krupp)

    real :: AK = 0.0        ! Free stream similarity parameter
    real :: RIGF = 0.0      ! Rigidity factor for transonic effects
    real :: POR = 0.0       ! Porosity

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

    ! Wall/tunnel constants (Optional)
    real :: F = 0.0
    real :: H = 0.0

    ! Flap parameters (Optional)
    integer :: IFLAP = 0      ! Flap flag
    real :: DELFLP = 0.0      ! Flap deflection angle  
    real :: FLPLOC = 0.77     ! Flap location


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
    real :: CAMBER(N_MESH_POINTS) = 0.0 ! Airfoil camber
    real :: THICK(N_MESH_POINTS) = 0.0 ! Airfoil thickness
    real :: XFOIL(N_MESH_POINTS) = 0.0 ! Airfoil X-coordinate
    real :: FU(N_MESH_POINTS) = 0.0 ! Upper surface interpolation
    real :: FL(N_MESH_POINTS) = 0.0 ! Lower surface interpolation
    real :: FXU(N_MESH_POINTS) = 0.0 ! Derivative of upper surface to X-coordinate
    real :: FXL(N_MESH_POINTS) = 0.0 ! Derivative of lower surface to X-coordinate
    
    
    ! ------------------------------------------------
    ! File unit numbers
    ! ------------------------------------------------

    integer :: FLAG_OUTPUT_SOLVE = 1 ! Flag to output SOLVE function process to tsfoil2.out
    
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
        IMAXI = 77  ! User-input maximum number of streamwise (X-direction) grid points (match default XIN)
        JMAXI = N_MESH_POINTS  ! User-input maximum number of spanwise (Y-direction) grid points

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

    ! Helper subroutine for convergence error reporting
    subroutine report_convergence_error(subroutine_name, variable_name, iteration_number)
        implicit none
        character(len=*), intent(in) :: subroutine_name, variable_name
        integer, intent(in) :: iteration_number
        
        write(*,'(A,A)') 'ABNORMAL STOP IN SUBROUTINE ', subroutine_name
        write(*,'(A,A,I0)') 'NON-CONVERGENCE OF ITERATION FOR ', variable_name, iteration_number
        write(UNIT_OUTPUT,'(A,A)') 'ABNORMAL STOP IN SUBROUTINE ', subroutine_name  
        write(UNIT_OUTPUT,'(A,A,I0)') 'NON-CONVERGENCE OF ITERATION FOR ', variable_name, iteration_number
        stop
        
    end subroutine report_convergence_error

end module common_data
