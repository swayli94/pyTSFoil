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

  character(len=80) :: title_card
  logical :: end_of_cases
  integer :: case_number

  ! Program header
  write(*,'(A)') '================================================='
  write(*,'(A)') '     TSFOIL - Transonic Small-Perturbation     '
  write(*,'(A)') '         Airfoil Analysis Program              '
  write(*,'(A)') '            Modernized Fortran Version         '
  write(*,'(A)') '================================================='
  write(*,*)  ! Initialize data structures
  call initialize_common(101, 102)  ! Use original TSFOIL array dimensions
  call initialize_spline(200)

  ! Open input and output files  
  open(unit=2, file='tsfoil.inp', status='old', iostat=case_number)
  if (case_number /= 0) then
    write(*,'(A)') 'Error: Cannot open input file tsfoil.inp'
    stop 1
  end if

  open(unit=15, file='tsfoil.out', status='replace')
  write(15,'(A)') 'TSFOIL Output File'
  write(15,'(A)') '=================='
  write(15,*)
  ! Call READIN which handles all input processing like the original
  call READIN()
  
  ! Continue with complete TSFOIL solution workflow
  write(*,'(A)') 'Starting TSFOIL solution sequence...'
  write(15,'(A)') 'TSFOIL Solution Sequence'
  write(15,'(A)') '========================'
  
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
  
  ! Check for mesh refinement or final results
  if (IREF > 0 .and. .not. ABORT1) then
    write(*,'(A)') 'Printing intermediate results...'
    call PRINT1()
    
    if (.not. ABORT1) then
      write(*,'(A)') 'Refining mesh and continuing...'
      call REFINE()
      call DIFCOE()
      call SETBC(0) 
      call SOLVE()
    end if
  end if
  
  ! Print final results
  write(*,'(A)') 'Printing final results...'
  call PRINT()
  
  ! Additional mesh refinement if requested
  if (IREF > 0) then
    call REFINE()
    call REFINE()
  end if
  
  write(*,'(A)') 'TSFOIL analysis completed successfully'
  write(15,'(A)') 'Analysis completed successfully'
  
  ! Cleanup
  call cleanup_spline()
  close(2)
  close(15)
  
  write(*,'(A)') 'Program completed normally'

end program tsfoil_main
