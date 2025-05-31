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
  write(*,*)

  ! Initialize data structures
  call initialize_common(81, 81)  ! Default mesh size
  call initialize_spline(200)

  ! Open input and output files
  open(unit=5, file='tsfoil.inp', status='old', iostat=case_number)
  if (case_number /= 0) then
    write(*,'(A)') 'Error: Cannot open input file tsfoil.inp'
    stop 1
  end if

  open(unit=15, file='tsfoil.out', status='replace')
  write(15,'(A)') 'TSFOIL Output File'
  write(15,'(A)') '=================='
  write(15,*)

  ! Process cases
  end_of_cases = .false.
  case_number = 0

  do while (.not. end_of_cases)
    ! Read title card
    read(5, '(A)', end=900) title_card
    
    ! Check for end of input
    if (trim(title_card) == 'END' .or. trim(title_card) == 'FINISHED') then
      exit
    end if

    case_number = case_number + 1
    write(*,'(A,I0,A,A)') 'Processing case ', case_number, ': ', trim(title_card)
    write(15,'(A,I0,A,A)') 'Case ', case_number, ': ', trim(title_card)
    write(15,'(A)') repeat('-', 50)

    ! Read input parameters for this case
    call READIN()
    
    if (ABORT1) then
      write(*,'(A)') 'Input error - skipping case'
      cycle
    end if

    ! Generate airfoil geometry
    call BODY()
    
    ! Generate mesh if needed
    if (AMESH) then
      call AYMESH()
    end if
    
    ! Check and adjust mesh
    call CKMESH()
    
    ! Initialize solution
    call GUESSP()
    
    ! Set up finite difference coefficients
    call DIFCOE()
    
    ! Solve the flow problem
    call SOLVE()
    
    ! Print results
    call PRINT()
    
    write(*,'(A)') 'Case completed successfully'
    write(15,*)
    
  end do

900 continue
  
  write(*,'(A,I0,A)') 'Processed ', case_number, ' cases successfully'
  write(15,'(A,I0,A)') 'Total cases processed: ', case_number
  
  ! Cleanup
  call cleanup_spline()
  close(5)
  close(15)
  
  write(*,'(A)') 'Program completed normally'

end program tsfoil_main
