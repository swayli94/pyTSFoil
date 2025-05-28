! Main program

program tsfoil2
    
  use tsfoil_data
  use tsfoil_io
  use spline_module
  use numerical_solvers
  use grid_module
  use airfoil_module
  use solver_module
  use output_module
  implicit none
  
  character(len=80) :: title_card, finish_string = "FINISHED"
  logical :: end_of_cases
    ! Initialize all data structures
  call initialize_data_structures()
  
  ! Initialize spline module
  call initialize_spline(200)
  
  ! Initialize solver module
  call init_solver(100)
  
  ! Write program header
  call write_header()
  
  ! Setup working directory and open files
  call setup_directory()
  
  ! Process all cases in the input file
  end_of_cases = .false.
  do while (.not. end_of_cases)
    ! Read the title card for this case
    read(5, '(A)', end=900) title_card
    
    ! Check if this is the end of input
    if (title_card(1:8) == finish_string) then
      end_of_cases = .true.
      cycle
    end if
    
    ! Store the title
    old%title = title_card
    
    ! Read input data for this case
    call read_input_data()
    
    ! Check compatibility of input data
    call check_input_data()
    
    ! Calculate mesh indices
    call islit(mesh%xin)
    call jslit(mesh%yin)
    call ckmesh()
    
    ! Generate the mesh
    call generate_mesh()
    
    ! Set initial values for flow variables
    call guessp()
    
    ! Calculate finite difference coefficients
    call difcoe()
    
    ! Set boundary conditions
    call setbc(0)
    
    ! Solve the flow equations
    call solve()
    
    ! If the solution is complete, print the results
    if (control%iref <= 0 .or. control%abort1) then
      call print_results()
    else
      ! Solution needs refinement
      call print1()
      call refine()
      call difcoe()
      call setbc(0)
      call solve()
    end if
    
    ! Store solution for next case
    call savep()
  end do
  
900 continue
  ! Clean up and exit
  call cleanup_data_structures()
  call cleanup_spline()
  
  ! Close output file
  close(15)
  
  ! Pause at end of program
  write(*, *) "Program completed. Press Enter to exit."
  read(*, *)
  
contains

  ! Check input data for inconsistencies
  subroutine check_input_data()
    integer :: i, j
    
    ! Check grid dimensions
    if (idx%imax > 100 .or. idx%jmax > 100) then
      write(15, *) "Error: Grid dimensions exceed limits (100x100)"
      stop
    end if
    
    ! Check mesh for monotonicity
    do i = idx%imin, idx%imax-1
      if (mesh%xin(i) >= mesh%xin(i+1)) then
        write(15, *) "Error: X-mesh not monotonically increasing at i =", i
        stop
      end if
    end do
    
    do j = idx%jmin, idx%jmax-1
      if (mesh%yin(j) >= mesh%yin(j+1)) then
        write(15, *) "Error: Y-mesh not monotonically increasing at j =", j
        stop
      end if
    end do
    
    ! Check flow parameters
    if (flow%emach < 0.5 .or. flow%emach > 2.0) then
      write(15, *) "Error: Mach number out of range (0.5 - 2.0)"
      stop
    end if
    
    if (flow%alpha < -9.0 .or. flow%alpha > 9.0) then
      write(15, *) "Error: Angle of attack out of range (-9 - 9 degrees)"
      stop
    end if
    
    if (flow%delta < 0.0 .or. flow%delta > 1.0) then
      write(15, *) "Error: Thickness ratio out of range (0 - 1)"
      stop
    end if
  end subroutine check_input_data
  
  ! Initial guess for potential
  subroutine guessp()
    
    ! Initialize potential function
    select case (old%pstart)
    case (1)
      ! Start with zero potential
      grid%p = 0.0
      flow%dub = 0.0
      circ%circff = 0.0
      circ%circte = 0.0
    
    case (2, 3)
      ! Start with previous solution
      ! (Would need interpolation from old grid to new grid)
    
    end select
  end subroutine guessp
  
  ! Save potential for next case
  subroutine savep()
    integer :: i, j
    
    ! Save current solution parameters
    old%imino = idx%imin
    old%jmino = idx%jmin
    old%imaxo = idx%imax
    old%jmaxo = idx%jmax
    old%clold = flow%cl
    old%emacho = flow%emach
    old%alphao = flow%alpha
    old%deltao = flow%delta
    old%volo = airfoil%vol
    old%dubo = flow%dub
    
    ! Save grid coordinates
    do i = idx%imin, idx%imax
      old%xold(i) = grid%x(i)
    end do
    
    do j = idx%jmin, idx%jmax
      old%yold(j) = mesh%yin(j)
    end do
    
    ! Save title
    old%titleo = old%title
    
    ! Write restart file if requested
    if (old%psave) then
      ! Code to write restart file would go here
    end if
  end subroutine savep
  
end program tsfoil2
