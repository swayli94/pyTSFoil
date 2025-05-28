! This module handles input/output operations for the TSFOIL program

module tsfoil_io
  use tsfoil_data
  implicit none

contains

  subroutine write_header()
    ! Write program header information
    
    integer :: io_unit = 6   ! Standard output
    integer :: file_unit = 15 ! Output file unit
    
    ! Format definitions
    character(len=*), parameter :: fmt_line = '(4X,69("*"))'
    character(len=*), parameter :: fmt_edge = '(4X,"*",67X,"*")'
    
    ! Write to screen
    write(io_unit, fmt_line)
    write(io_unit, fmt_edge)
    write(io_unit, '(4X,"*",25X,"PROGRAM TSFOIL",27X,"*")')
    write(io_unit, '(4X,"*",29X,"SOLVES",31X,"*")')
    write(io_unit, '(4X,"*",5X,"INVISCID FLOW PAST THIN TWO DIMENSIONAL LIFTING AIRFOIL",7X,"*")')
    write(io_unit, '(4X,"*",29X,"USING",32X,"*")')
    write(io_unit, '(4X,"*",15X,"TRANSONIC SMALL DISTURBANCE THEORY",17X,"*")')
    write(io_unit, '(4X,"*",9X,"FULLY CONSERVATIVE FINITE DIFFERENCE EQUATIONS",12X,"*")')
    write(io_unit, '(4X,"*",17X,"SUCCESSIVE LINE OVERRELAXATION",19X,"*")')
    write(io_unit, fmt_edge)
    write(io_unit, '(4X,"*",27X,"WRITTEN BY",29X,"*")')
    write(io_unit, fmt_edge)
    write(io_unit, '(4X,"*",15X,"EARLL M. MURMAN AND FRANK R. BAILEY",16X,"*")')
    write(io_unit, '(4X,"*",19X,"NASA-AMES RESEARCH CENTER",22X,"*")')
    write(io_unit, '(4X,"*",19X,"MOFFETT FIELD, CALIFORNIA",22X,"*")')
    write(io_unit, '(4X,"*",31X,"AND",32X,"*")')
    write(io_unit, '(4X,"*",23X,"MARGARET L. JOHNSON",24X,"*")')
    write(io_unit, '(4X,"*",18X,"COMPUTER SCIENCES CORPORATION",19X,"*")')
    write(io_unit, '(4X,"*",20X,"MOUNTAIN VIEW, CALIFORNIA",21X,"*")')
    write(io_unit, '(4X,"*",67X,"*")')
    write(io_unit, '(4X,"*",12X,"Documented in NASA SP-347 and NASA CR-3064",12X,"*")')
    write(io_unit, fmt_edge)
    write(io_unit, '(4X,"*",27X,"Version for",28X,"*")')
    write(io_unit, '(4X,"*",15X,"VT Aerospace Design Software Series",16X,"*")')
    write(io_unit, '(4X,"*",67X,"*")')
    write(io_unit, '(4X,"*",15X,"Contact info:",38X,"*")')
    write(io_unit, '(4X,"*",15X,"Dr. William H. Mason",31X,"*")')
    write(io_unit, '(4X,"*",15X,"Aerospace & Ocean Engineering, Virginia Tech",7X,"*")')
    write(io_unit, '(4X,"*",15X,"Blacksburg, VA24061",32X,"*")')
    write(io_unit, '(4X,"*",15X,"Email: whmason@vt.edu",30X,"*")')
    write(io_unit, fmt_edge)
    write(io_unit, fmt_line)
    
    ! Write to file
    write(file_unit, fmt_line)
    write(file_unit, fmt_edge)
    write(file_unit, '(4X,"*",25X,"PROGRAM TSFOIL",27X,"*")')
    write(file_unit, '(4X,"*",29X,"SOLVES",31X,"*")')
    write(file_unit, '(4X,"*",5X,"INVISCID FLOW PAST THIN TWO DIMENSIONAL LIFTING AIRFOIL",7X,"*")')
    write(file_unit, '(4X,"*",29X,"USING",32X,"*")')
    write(file_unit, '(4X,"*",15X,"TRANSONIC SMALL DISTURBANCE THEORY",17X,"*")')
    write(file_unit, '(4X,"*",9X,"FULLY CONSERVATIVE FINITE DIFFERENCE EQUATIONS",12X,"*")')
    write(file_unit, '(4X,"*",17X,"SUCCESSIVE LINE OVERRELAXATION",19X,"*")')
    write(file_unit, fmt_edge)
    write(file_unit, '(4X,"*",27X,"WRITTEN BY",29X,"*")')
    write(file_unit, fmt_edge)
    write(file_unit, '(4X,"*",15X,"EARLL M. MURMAN AND FRANK R. BAILEY",16X,"*")')
    write(file_unit, '(4X,"*",19X,"NASA-AMES RESEARCH CENTER",22X,"*")')
    write(file_unit, '(4X,"*",19X,"MOFFETT FIELD, CALIFORNIA",22X,"*")')
    write(file_unit, '(4X,"*",31X,"AND",32X,"*")')
    write(file_unit, '(4X,"*",23X,"MARGARET L. JOHNSON",24X,"*")')
    write(file_unit, '(4X,"*",18X,"COMPUTER SCIENCES CORPORATION",19X,"*")')
    write(file_unit, '(4X,"*",20X,"MOUNTAIN VIEW, CALIFORNIA",21X,"*")')
    write(file_unit, '(4X,"*",67X,"*")')
    write(file_unit, '(4X,"*",12X,"Documented in NASA SP-347 and NASA CR-3064",12X,"*")')
    write(file_unit, fmt_edge)
    write(file_unit, '(4X,"*",27X,"Version for",28X,"*")')
    write(file_unit, '(4X,"*",15X,"VT Aerospace Design Software Series",16X,"*")')
    write(file_unit, '(4X,"*",67X,"*")')
    write(file_unit, '(4X,"*",15X,"Contact info:",38X,"*")')
    write(file_unit, '(4X,"*",15X,"Dr. William H. Mason",31X,"*")')
    write(file_unit, '(4X,"*",15X,"Aerospace & Ocean Engineering, Virginia Tech",7X,"*")')
    write(file_unit, '(4X,"*",15X,"Blacksburg, VA24061",32X,"*")')
    write(file_unit, '(4X,"*",15X,"Email: whmason@vt.edu",30X,"*")')
    write(file_unit, fmt_edge)
    write(file_unit, fmt_line)
    
  end subroutine write_header
  
  ! Setup the working directory
  subroutine setup_directory()

    character(len=200) :: newdir
    integer :: istatus
    integer :: n_args
    
    ! Get the directory from command line or prompt user
    newdir = ' '
    
    ! Get number of command line arguments
    n_args = command_argument_count()
    
    ! Get first argument if present
    if (n_args >= 1) then
      call get_command_argument(1, newdir)
    end if
    
    ! If no argument provided, ask user for directory
    if (len_trim(newdir) == 0) then
      write(*, *) 'Please enter a new directory name: '
      read(*, *) newdir
    end if
    
    ! Try to change to the specified directory
    call execute_command_line('cd ' // trim(adjustl(newdir)), exitstat=istatus)
    
    if (istatus /= 0) then
      write(*, *) 'Error changing to directory: ', trim(adjustl(newdir))
      stop
    else
      write(*, *) 'Directory changed to ', trim(adjustl(newdir))
    end if
    
    ! Open output file
    open(unit=15, file='tsfoil2.out', status='replace')
    
  end subroutine setup_directory
  
  ! Read all input data for the program using Fortran namelist
  subroutine read_input_data()

    ! Define namelist variables
    real :: alpha, delta, emach, dub, cverge, dverge
    real :: gam, clset, wcirc, h, f, por
    integer :: maxit, iprter, bcfoil, bctype, prtflo, simdef
    logical :: amesh, phys, fcr, kutta, psave
    integer :: icut, pstart, nu, nl
    integer :: nwdge, iflap
    real :: reynld, wconst, delflp, flploc
    real, dimension(3) :: we
    real, dimension(100) :: xin, yin, xu, yu, xl, yl
    
    ! Define the namelist
    namelist /INP/ ALPHA, DELTA, EMACH, DUB, CVERGE, DVERGE, &
                  GAM, CLSET, WCIRC, MAXIT, IPRTER, BCFOIL, &
                  BCTYPE, PRTFLO, SIMDEF, AMESH, PHYS, FCR, &
                  KUTTA, PSAVE, ICUT, PSTART, NU, NL, &
                  NWDGE, REYNLD, WCONST, IFLAP, DELFLP, FLPLOC, &
                  WE, H, F, POR, XIN, YIN, XU, YU, XL, YL
    
    ! Set default values before reading input
    alpha = 0.0
    delta = 0.1
    emach = 0.75
    dub = 0.0
    cverge = 0.0001
    dverge = 1000.0
    gam = 1.4
    clset = 0.0
    wcirc = 0.0
    maxit = 100
    iprter = 10
    bcfoil = 3
    bctype = 1
    prtflo = 1
    simdef = 3
    amesh = .false.
    phys = .true.
    fcr = .true.
    kutta = .true.
    psave = .false.
    icut = 0
    pstart = 1
    nu = 100
    nl = 100
    nwdge = 0
    reynld = 4.0e6
    wconst = 4.0
    iflap = 0
    delflp = 5.0
    flploc = 0.77
    we = [1.85, 1.75, 1.65]
    h = 0.0
    f = 0.0
    por = 0.0
    xin = 0.0
    yin = 0.0
    xu = 0.0
    yu = 0.0
    xl = 0.0
    yl = 0.0
    
    ! Read the namelist from input file
    read(5, nml=INP)
    
    ! Copy the read values to the appropriate variables in the modules
    flow%alpha = alpha
    flow%delta = delta
    flow%emach = emach
    flow%dub = dub
    solver%cverge = cverge
    solver%dverge = dverge
    tunnel%gam = gam
    circ%clset = clset
    circ%wcirc = wcirc
    solver%maxit = maxit
    solver%iprter = iprter
    boundary%bcfoil = bcfoil
    aparam%bctype = bctype
    control%prtflo = prtflo
    control%simdef = simdef
    control%amesh = amesh
    flow%phys = phys
    circ%fcr = fcr
    circ%kutta = kutta
    old%psave = psave
    control%icut = icut
    old%pstart = pstart
    boundary%nu = nu
    boundary%nl = nl
    shock%nwdge = nwdge
    shock%reynld = reynld
    shock%wconst = wconst
    boundary%iflap = iflap
    boundary%delflp = delflp
    boundary%flploc = flploc
    solver%we = we
    aparam%h = h
    aparam%f = f
    ! por moved directly to the boundary conditions
    
    ! Copy mesh and airfoil coordinates
    mesh%xin(1:100) = xin(1:100)
    mesh%yin(1:100) = yin(1:100)
    boundary%xu(1:100) = xu(1:100)
    boundary%yu(1:100) = yu(1:100)
    boundary%xl(1:100) = xl(1:100)
    boundary%yl(1:100) = yl(1:100)
    
    ! Set derived parameters
    flow%gam1 = (tunnel%gam + 1.0) / 2.0
    
    ! Calculate transonic similarity parameter
    if (flow%emach < 1.0) then
      flow%ak = (1.0 - flow%emach**2) / (flow%delta * flow%gam1 * flow%emach**2)
      flow%rtk = sqrt(abs(flow%ak))
    else
      flow%ak = (flow%emach**2 - 1.0) / (flow%delta * flow%gam1 * flow%emach**2)
      flow%rtk = -sqrt(abs(flow%ak))
    end if
    
    ! Set mesh dimensions if not provided
    if (mesh%xin(1) == 0.0 .and. mesh%xin(2) == 0.0) then
      call setup_default_mesh()
    end if
    
    ! Initialize grid dimensions
    idx%imin = 1
    idx%jmin = 1    
    idx%imax = 41  ! Default value unless specified otherwise
    idx%jmax = 41  ! Default value unless specified otherwise
    old%imaxi = idx%imax
    old%jmaxi = idx%jmax
    
    ! Set critical pressure coefficient
    if (flow%phys) then
      scale%cpstar = 2.0 / (tunnel%gam * flow%emach**2) * &
                    ((2.0 / (tunnel%gam + 1.0))**(tunnel%gam / (tunnel%gam - 1.0)) - 1.0)
    else
      scale%cpstar = -1.0 / flow%gam1
    end if
    
    ! Initialize control parameters
    control%iref = 0
    control%abort1 = .false.
    control%kstep = 0
  end subroutine read_input_data
  
  ! Setup default computational mesh if none provided
  subroutine setup_default_mesh()
    integer :: i, j
    real :: dx, dy, x0, y0
    
    ! Set up default x-mesh
    dx = 0.05
    x0 = -1.0
    
    do i = 1, 41
      mesh%xin(i) = x0 + (i-1) * dx
    end do
    
    ! Set up default y-mesh
    dy = 0.05
    y0 = -1.0
    
    do j = 1, 41
      mesh%yin(j) = y0 + (j-1) * dy
    end do
    
    ! Update grid dimensions
    idx%imax = 41
    idx%jmax = 41
    old%imaxi = 41
    old%jmaxi = 41
  end subroutine setup_default_mesh
  
end module tsfoil_io
