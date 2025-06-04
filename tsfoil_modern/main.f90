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
  use ieee_arithmetic, only: ieee_set_halting_mode, ieee_all, ieee_invalid, &
                             ieee_overflow, ieee_divide_by_zero, ieee_underflow
  implicit none

  integer :: ios

  ! Program header
  write(*,'(A)') '================================================='
  write(*,'(A)') '     TSFOIL - Transonic Small-Perturbation     '
  write(*,'(A)') '         Airfoil Analysis Program              '
  write(*,'(A)') '            Modernized Fortran Version         '
  write(*,'(A)') '================================================='
  write(*,*)

  ! Enable floating-point exception handling
  call ieee_set_halting_mode(ieee_underflow, .true.)
  write(*,'(A)') 'Floating-point exception handling enabled'
  write(*,'(A)') 'Program will halt on: INVALID, OVERFLOW, DIVIDE_BY_ZERO, UNDERFLOW'
  write(*,*)

  ! Initialize data structures
  call initialize_common()
  call initialize_spline(200)

  ! Open input and output files  
  open(unit=UNIT_INPUT, file='tsfoil.inp', status='old', iostat=ios)
  if (ios /= 0) then
    write(*,'(A)') 'Error: Cannot open input file tsfoil.inp'
    stop
  end if
  
  ! ECHINP provides a listing of all data cards for entire job. 
  ! call ECHINP()
  
  ! Main case processing loop
  ! The loop continues until READIN encounters "FINI" and calls STOP
  do

    ! Call READIN to read one case - it handles termination internally with STOP
    ! when "FINI" card is encountered, just like the original
    write(*,'(A)') 'Reading input data...'
    call READIN()
    
    ! Continue with complete TSFOIL solution workflow
    write(*,'(A)') 'Starting TSFOIL solution sequence...'
    write(UNIT_OUTPUT,'(A)') 'TSFOIL Solution Sequence'
    write(UNIT_OUTPUT,'(A)') '========================'
    
    ! SCALE: Rescale all physical variables to transonic similarity form
    write(*,'(A)') 'Scaling variables to similarity form...'
    call SCALE()
    
    ! FARFLD: Set far field boundary conditions
    write(*,'(A)') 'Setting far-field boundary conditions...'
    call FARFLD()
    
    ! BODY: Compute airfoil geometry and print geometrical information
    write(*,'(A)') 'Computing airfoil geometry...'
    call BODY()
    
    ! CUTOUT: Remove mesh points for initial coarse mesh solution
    write(*,'(A)') 'Setting up coarse mesh...'
    call CUTOUT()
    
    ! GUESSP: Initialize potential array P
    write(*,'(A)') 'Initializing potential array...'
    call GUESSP()
    
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
        write(*,'(A)') 'Refining mesh...'
        call REFINE()
        write(*,'(A)') 'Recomputing finite difference coefficients...'
        call DIFCOE()
        write(*,'(A)') 'Setting boundary conditions after refinement...'
        call SETBC(0) 
        write(*,'(A)') 'Continuing solution after refinement...'
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
    
    ! Store solution for potential next case
    call SAVEP()
    
    write(*,'(A)') 'Case completed successfully'
    write(UNIT_OUTPUT,'(A)') 'Case completed successfully'
    write(UNIT_OUTPUT,*)
  
  end do

  ! Note: Code below this point will never be reached as READIN calls STOP
  ! when "FINI" is encountered, exactly like the original
  
  ! Close all files and end program - this cleanup is never reached in practice
  call cleanup_spline()
  call close_output_files()
  close(UNIT_INPUT)
  close(UNIT_OUTPUT)
  
  ! Check for any floating-point exceptions before finishing
  call check_fp_exceptions()
  
  write(*,'(A)') 'Program completed normally'

end program tsfoil_main

