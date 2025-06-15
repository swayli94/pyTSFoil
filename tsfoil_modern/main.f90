! main.f90
! Main program for TSFOIL modernized code

program tsfoil_main
  use common_data
  use io_module
  use math_module
  use spline_module
  use airfoil_module
  use mesh_module
  use solver_module
  use numerical_solvers
  implicit none

  ! Program header
  write(*,'(A)') '==============================================='
  write(*,'(A)') '     TSFOIL - Transonic Small-Perturbation     '
  write(*,'(A)') '          Airfoil Analysis Program             '
  write(*,'(A)') '         Modernized Fortran Version            '
  write(*,'(A)') '        Developed by: Runze Li, 2025           '
  write(*,'(A)') '==============================================='

  ! Initialize data structures
  call initialize_common()
  call initialize_spline(N_MESH_POINTS*2)

  ! Call READIN to read one case - it handles termination internally with STOP
  write(*,'(A)') 'Reading input data...'
  call READIN()
    
  ! SCALE: Rescale all physical variables to transonic similarity form
  write(*,'(A)') 'Scaling variables to similarity form...'
  call SCALE()
  
  ! FARFLD: Set far field boundary conditions
  write(*,'(A)') 'Setting far-field boundary conditions...'
  call FARFLD()
  
  ! BODY: Compute airfoil geometry and print geometrical information
  write(*,'(A)') 'Computing airfoil geometry...'
  call BODY()
  
  ! DIFCOE: Compute difference coefficients in field
  write(*,'(A)') 'Computing finite difference coefficients...'
  call DIFCOE()
  
  ! SETBC: Set boundary conditions
  write(*,'(A)') 'Setting boundary conditions...'
  call SETBC(0)
  
  ! SOLVE: Execute main relaxation solution
  write(*,'(A)') 'Solving transonic flow equations...'
  call SOLVE()
    
  ! Print final results
  write(*,'(A)') 'Printing final results...'
  call PRINT()

  write(*,'(A)') 'Case completed successfully'
  write(UNIT_OUTPUT,'(A)') 'Case completed successfully'

  ! Close all files and end program
  call cleanup_spline()
  call close_output_files()

end program tsfoil_main

