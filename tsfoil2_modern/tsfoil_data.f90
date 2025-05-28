! Module containing all shared data structures for the TSFOIL program
! Replaces the COMMON blocks in the original code

module tsfoil_data
  implicit none

  ! Global parameters
  real, parameter :: PI = 3.1415926535897932384626
  real, parameter :: TWOPI = 2.0 * PI
  real, parameter :: HALFPI = 0.5 * PI

  ! Main grid and solution arrays
  type :: grid_type
    real, allocatable :: p(:,:)      ! Potential array
    real, allocatable :: x(:), y(:)  ! Grid coordinates
  end type grid_type

  ! Grid indices
  type :: grid_indices
    integer :: imin, imax, iup, idown, ile, ite
    integer :: jmin, jmax, jup, jlow, jtop, jbot, j1, j2
  end type grid_indices

  ! Flow parameters
  type :: flow_params
    real :: ak         ! Transonic similarity parameter
    real :: alpha      ! Angle of attack in degrees
    real :: dub        ! Parameter for farfield correction
    real :: gam1       ! (gamma+1)/2
    real :: rtk        ! sqrt(K)
    real :: emach      ! Free stream Mach number
    real :: sonvel     ! Speed of sound
    real :: delta      ! Thickness parameter
    real :: delrt2     ! Delta/sqrt(2)
    real :: emroot     ! sqrt(M∞^2 - 1)
    real :: vfact      ! Velocity normalization factor
    real :: yfact      ! Y-coordinate normalization factor
    real :: cl         ! Lift coefficient
    logical :: phys    ! Physical plane solution flag
  end type flow_params
  ! Airfoil geometry
  type :: airfoil_geom
    real, allocatable :: fl(:), fxl(:)      ! Lower surface and derivatives
    real, allocatable :: fu(:), fxu(:)      ! Upper surface and derivatives
    real, allocatable :: camber(:), thick(:) ! Camber and thickness
    real, allocatable :: xfoil(:)           ! Airfoil x-coordinates
    real, allocatable :: cpu(:), cpl(:)     ! Upper and lower pressure coefficients
    integer :: ifoil                        ! Number of airfoil points
    real :: vol                             ! Volume (area) of airfoil
  end type airfoil_geom

  ! Boundary conditions
  type :: boundary_type
    integer :: bcfoil                        ! Boundary condition type
    integer :: nl, nu                        ! Number of lower/upper points
    real, allocatable :: xl(:), xu(:)        ! X-coordinates
    real, allocatable :: yl(:), yu(:)        ! Y-coordinates
    real :: rigf                             ! Parameter for rigid flap
    integer :: iflap                         ! Flap flag
    real :: delflp                           ! Flap deflection
    real :: flploc                           ! Flap hinge location
  end type boundary_type

  ! Solver parameters
  type :: solver_params
    real :: cverge                           ! Convergence criterion
    real :: dverge                           ! Divergence criterion
    integer :: iprter                        ! Error print control
    integer :: maxit                         ! Maximum iterations
    real :: we(3)                            ! Relaxation parameters
    real :: eps                              ! Small parameter
  end type solver_params

  ! Solution control
  type :: solution_control
    integer :: iref                          ! Mesh refinement count
    logical :: abort1                        ! Abort flag
    integer :: icut                          ! Cut type
    integer :: kstep                         ! Step counter
    logical :: amesh                         ! Automatic mesh flag
    integer :: prtflo                        ! Print flow field flag
    integer :: simdef                        ! Similarity definition flag
    logical :: outerr                        ! Output error flag
  end type solution_control

  ! Computational mesh
  type :: mesh_type
    real, allocatable :: xin(:), yin(:)      ! Initial mesh coordinates
    real, allocatable :: xdiff(:), ydiff(:)  ! Mesh differences
    real :: cjup, cjup1                      ! Upper mesh parameters
    real :: cjlow, cjlow1                    ! Lower mesh parameters
  end type mesh_type

  ! Circulation parameters
  type :: circulation_type
    real :: clset                            ! Target lift coefficient
    logical :: fcr                           ! Fixed circulation flag
    logical :: kutta                         ! Kutta condition flag
    real :: wcirc                            ! Circulation parameter
    real :: dcirc                            ! Circulation change
    real :: circff                           ! Far-field circulation
    real :: circte                           ! Trailing edge circulation
  end type circulation_type

  ! Free Air/Wind Tunnel parameters
  type :: tunnel_type
    real, allocatable :: yfree(:)            ! Free-air y-coordinates
    real, allocatable :: ytun(:)             ! Wind tunnel coordinates
    real :: gam                              ! Specific heat ratio
    integer :: jmxf                          ! Grid point count (free air)
    integer :: jmxt                          ! Grid point count (tunnel)
  end type tunnel_type

  ! Old solution storage
  type :: old_solution
    real :: alphao                           ! Old angle of attack
    real :: clold                            ! Old lift coefficient
    real :: deltao                           ! Old thickness
    real :: dubo                             ! Old far field parameter
    real :: emacho                           ! Old Mach number
    integer :: imino, imaxo                  ! Old i-grid limits
    integer :: imaxi                         ! Initial imax
    integer :: jmino, jmaxo                  ! Old j-grid limits
    integer :: jmaxi                         ! Initial jmax
    logical :: psave                         ! Save potential flag
    integer :: pstart                        ! Restart flag
    character(len=80) :: title               ! Case title
    character(len=80) :: titleo              ! Old case title
    real :: volo                             ! Old volume
    real, allocatable :: xold(:), yold(:)    ! Old coordinates
  end type old_solution

  ! Scaling factors
  type :: scaling_factors
    real :: cdfact                           ! Drag coefficient factor
    real :: clfact                           ! Lift coefficient factor
    real :: cmfact                           ! Moment coefficient factor
    real :: cpfact                           ! Pressure coefficient factor
    real :: cpstar                           ! Critical pressure coefficient
  end type scaling_factors

  ! Boundary parameters
  type :: boundary_params
    real :: b                                ! Base parameter
    real :: beta0, beta1, beta2              ! Beta parameters
    real :: psi0, psi1, psi2                 ! Psi parameters
    real :: alpha0, alpha1, alpha2           ! Alpha parameters
    real :: xsing                            ! Singularity location
    real :: omega0, omega1, omega2           ! Omega parameters
    real :: jet                              ! Jet parameter
  end type boundary_params

  ! Coefficient arrays
  type :: coefficient_arrays
    real, allocatable :: cyylc, cyyld, cyylu ! Lower coefficients
    real, allocatable :: cyyuc, cyyud, cyyuu ! Upper coefficients
    real, allocatable :: cyyblc, cyybld, cyyblu ! BC Lower coefficients
    real, allocatable :: cyybuc, cyybud, cyybuu ! BC Upper coefficients
    real, allocatable :: fxlbc(:), fxubc(:)  ! BC derivatives
    integer :: itemp1, itemp2                ! Temporary indices
    real, allocatable :: cyyc(:), cyyd(:), cyyu(:) ! Y-coefficients
    integer :: ival                          ! Value index
    real, allocatable :: cxc(:), cxl(:), cxr(:) ! X-coefficients
    real, allocatable :: cxxc(:), cxxl(:), cxxr(:) ! X-second derivatives
    real, allocatable :: c1(:)               ! Additional coefficients
  end type coefficient_arrays

  ! Error handling
  type :: error_type
    real :: error                            ! Maximum error
    integer :: i1, i2                        ! Error location indices
    integer :: ierror, jerror                ! Error indices
    real, allocatable :: emu(:,:)            ! Error values
    real, allocatable :: vc(:)               ! Velocity values
    real :: wi                               ! Relaxation parameter
    real, allocatable :: pold(:,:)           ! Old potential
  end type error_type

  ! Tridiagonal solver
  type :: tridiagonal_type
    real, allocatable :: diag(:)             ! Diagonal elements
    real, allocatable :: rhs(:)              ! Right-hand side
    real, allocatable :: sub(:)              ! Subdiagonal elements
    real, allocatable :: sup(:)              ! Superdiagonal elements
  end type tridiagonal_type

  ! Additional geometry
  type :: additional_geom
    real, allocatable :: xmid(:), ymid(:)    ! Midpoint coordinates
    real, allocatable :: dtop(:), dbot(:)    ! Distances
    real, allocatable :: dup(:), ddown(:)    ! Distances
    real, allocatable :: vtop(:), vbot(:)    ! Velocities
    real, allocatable :: vup(:), vdown(:)    ! Velocities
    real, allocatable :: cpl(:), cpu(:)      ! Pressure coefficients
    integer :: idla                          ! Index parameter
    real, allocatable :: pjump(:)            ! Jump values
  end type additional_geom

  ! Plotting data
  type :: plot_data_type
    real, allocatable :: cpup(:), cplo(:), cps(:), xp(:) ! CP plot arrays
    real, allocatable :: cplw(:), cpuw(:)               ! Wall CP arrays
    real, allocatable :: vlw(:), vuw(:)                 ! Wall velocity arrays
    real, allocatable :: xslprt(:), yslprt(:)           ! Sonic line plot arrays
    integer, allocatable :: jlin(:)                    ! J-lines for prtfld
  end type plot_data_type

  ! Wake and shock data
  type :: wake_type
    integer :: nwdge                         ! Number of wedges
    real, allocatable :: wslp(:,:)           ! Wedge slopes
    real, allocatable :: xshk(:,:)           ! Shock positions
    real, allocatable :: thamax(:,:)         ! Maximum values
    real, allocatable :: am1(:,:)            ! Mach numbers
    real, allocatable :: zeta(:,:)           ! Zeta values
    integer, allocatable :: nvwprt(:)        ! Print parameters
    real :: wconst                           ! Wedge constant
    real :: reynld                           ! Reynolds number
    integer :: nishk                         ! Shock counter
  end type wake_type

  ! Additional parameters  
  type :: additional_params
    real :: bigrl                            ! Large value
    integer :: irl, jrl                      ! Indices
    real, allocatable :: theta(:,:)          ! Theta values
    integer :: bctype                        ! Boundary condition type
    real :: fhinv                            ! Inverse parameter
    real :: por                              ! Porosity parameter
    real :: f, h                             ! Grid parameters
  end type additional_params

  ! Output coefficients
  type :: output_type
    real :: cl                               ! Lift coefficient
    real :: cd                               ! Drag coefficient
    real :: cm                               ! Moment coefficient
  end type output_type
  
  ! Global variables used throughout the program
  type(grid_type) :: grid
  type(grid_indices) :: idx
  type(flow_params) :: flow
  type(airfoil_geom) :: airfoil
  type(boundary_type) :: boundary
  type(solver_params) :: solver
  type(solution_control) :: control
  type(mesh_type) :: mesh
  type(circulation_type) :: circ
  type(tunnel_type) :: tunnel
  type(old_solution) :: old
  type(scaling_factors) :: scale
  type(boundary_params) :: bparam
  type(coefficient_arrays) :: coeff
  type(error_type) :: err
  type(tridiagonal_type) :: tri
  type(additional_geom) :: ageom
  type(plot_data_type) :: plot_data
  type(wake_type) :: wake
  type(wake_type) :: shock  ! Using wake_type as alias for shock data
  type(additional_params) :: aparam
  type(output_type) :: output
  
  ! Constants
  real, parameter :: SMALL = 1.0e-6

contains

  ! Allocate and initialize all data structures
  subroutine initialize_data_structures()
    
    ! This subroutine allocates all arrays with proper sizes
    
    ! Grid arrays
    allocate(grid%p(102, 101))
    allocate(grid%x(100), grid%y(100))
    
    ! Airfoil geometry
    allocate(airfoil%fl(100), airfoil%fxl(100))
    allocate(airfoil%fu(100), airfoil%fxu(100))
    allocate(airfoil%camber(100), airfoil%thick(100))
    allocate(airfoil%xfoil(100))
    allocate(airfoil%cpu(100), airfoil%cpl(100))
    
    ! Mesh arrays
    allocate(mesh%xin(100), mesh%yin(100))
    allocate(mesh%xdiff(100), mesh%ydiff(100))
    
    ! Boundary arrays
    allocate(boundary%xl(100), boundary%xu(100))
    allocate(boundary%yl(100), boundary%yu(100))
    
    ! Tunnel arrays
    allocate(tunnel%yfree(100), tunnel%ytun(100))
    
    ! Old solution arrays
    allocate(old%xold(100), old%yold(100))
    
    ! Coefficient arrays
    allocate(coeff%fxlbc(100), coeff%fxubc(100))
    allocate(coeff%cyyc(100), coeff%cyyd(100), coeff%cyyu(100))
    allocate(coeff%cxc(100), coeff%cxl(100), coeff%cxr(100))
    allocate(coeff%cxxc(100), coeff%cxxl(100), coeff%cxxr(100))
    allocate(coeff%c1(100))
    
    ! Error arrays
    allocate(err%emu(100,2), err%vc(100), err%pold(100,2))
    
    ! Tridiagonal solver arrays
    allocate(tri%diag(100), tri%rhs(100), tri%sub(100), tri%sup(100))
    
    ! Additional geometry arrays
    allocate(ageom%xmid(100), ageom%ymid(100))
    allocate(ageom%dtop(100), ageom%dbot(100))
    allocate(ageom%dup(100), ageom%ddown(100))
    allocate(ageom%vtop(100), ageom%vbot(100))
    allocate(ageom%vup(100), ageom%vdown(100))
    allocate(ageom%cpl(100), ageom%cpu(100))
    allocate(ageom%pjump(100))

    ! Plotting data arrays
    allocate(plot_data%jlin(100))
    
    ! Shock data arrays
    allocate(shock%wslp(100,2), shock%xshk(2,3))
    allocate(shock%thamax(2,3), shock%am1(2,3))
    allocate(shock%zeta(2,3), shock%nvwprt(2))
    
    ! Additional parameter arrays
    allocate(aparam%theta(100,100))
    
    ! Initialize all arrays to zero    grid%p = 0.0
    grid%x = 0.0
    grid%y = 0.0
    
    airfoil%fl = 0.0
    airfoil%fxl = 0.0
    airfoil%fu = 0.0
    airfoil%fxu = 0.0
    airfoil%camber = 0.0
    airfoil%thick = 0.0
    airfoil%xfoil = 0.0
    airfoil%cpu = 0.0
    airfoil%cpl = 0.0
    
    mesh%xin = 0.0
    mesh%yin = 0.0
    mesh%xdiff = 0.0
    mesh%ydiff = 0.0
    
    boundary%xl = 0.0
    boundary%xu = 0.0
    boundary%yl = 0.0
    boundary%yu = 0.0
    
    tunnel%yfree = 0.0
    tunnel%ytun = 0.0
    
    old%xold = 0.0
    old%yold = 0.0
    
    coeff%fxlbc = 0.0
    coeff%fxubc = 0.0
    coeff%cyyc = 0.0
    coeff%cyyd = 0.0
    coeff%cyyu = 0.0
    coeff%cxc = 0.0
    coeff%cxl = 0.0
    coeff%cxr = 0.0
    coeff%cxxc = 0.0
    coeff%cxxl = 0.0
    coeff%cxxr = 0.0
    coeff%c1 = 0.0
    
    err%emu = 0.0
    err%vc = 0.0
    err%pold = 0.0
    
    tri%diag = 0.0
    tri%rhs = 0.0
    tri%sub = 0.0
    tri%sup = 0.0
    
    ageom%xmid = 0.0
    ageom%ymid = 0.0
    ageom%dtop = 0.0
    ageom%dbot = 0.0
    ageom%dup = 0.0
    ageom%ddown = 0.0
    ageom%vtop = 0.0
    ageom%vbot = 0.0
    ageom%vup = 0.0
    ageom%vdown = 0.0
    ageom%cpl = 0.0
    ageom%cpu = 0.0
    ageom%pjump = 0.0
    
    shock%wslp = 0.0
    shock%xshk = 0.0
    shock%thamax = 0.0
    shock%am1 = 0.0
    shock%zeta = 0.0
    shock%nvwprt = 0
    
    aparam%theta = 0.0

  end subroutine initialize_data_structures
  
  ! Deallocate all arrays to prevent memory leaks
  subroutine cleanup_data_structures()

    deallocate(grid%p, grid%x, grid%y)
    deallocate(airfoil%fl, airfoil%fxl, airfoil%fu, airfoil%fxu)
    deallocate(airfoil%camber, airfoil%thick, airfoil%xfoil)
    deallocate(airfoil%cpu, airfoil%cpl)
    deallocate(mesh%xin, mesh%yin, mesh%xdiff, mesh%ydiff)
    deallocate(boundary%xl, boundary%xu, boundary%yl, boundary%yu)
    deallocate(tunnel%yfree, tunnel%ytun)
    deallocate(old%xold, old%yold)
    deallocate(coeff%fxlbc, coeff%fxubc)
    deallocate(coeff%cyyc, coeff%cyyd, coeff%cyyu)
    deallocate(coeff%cxc, coeff%cxl, coeff%cxr)
    deallocate(coeff%cxxc, coeff%cxxl, coeff%cxxr)
    deallocate(coeff%c1)
    deallocate(err%emu, err%vc, err%pold)
    deallocate(tri%diag, tri%rhs, tri%sub, tri%sup)
    deallocate(ageom%xmid, ageom%ymid)
    deallocate(ageom%dtop, ageom%dbot, ageom%dup, ageom%ddown)
    deallocate(ageom%vtop, ageom%vbot, ageom%vup, ageom%vdown)
    deallocate(ageom%cpl, ageom%cpu, ageom%pjump)
    deallocate(shock%wslp, shock%xshk)
    deallocate(shock%thamax, shock%am1, shock%zeta, shock%nvwprt)
    deallocate(aparam%theta)
  end subroutine cleanup_data_structures

end module tsfoil_data

