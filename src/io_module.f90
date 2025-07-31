! io_module.f90
! Module for input/output routines

module io_module
  use common_data
  implicit none
  private

  ! Declare public procedures
  public :: open_output_file, open_summary_file, close_output_files

contains
  
  ! Open all output files with unique file units
  subroutine open_output_file()
    use common_data, only: UNIT_OUTPUT
    implicit none
    open(unit=UNIT_OUTPUT, file='tsfoil2.out', status='replace', action='write')   ! Unit 15
  end subroutine open_output_file

  subroutine open_summary_file()
    use common_data, only: UNIT_OUTPUT, UNIT_SUMMARY
    implicit none
    open(unit=UNIT_SUMMARY, file='smry.out', status='replace', action='write')     ! Unit 16
  end subroutine open_summary_file

  ! Close all output files
  subroutine close_output_files()
    use common_data, only: UNIT_OUTPUT, UNIT_SUMMARY
    implicit none

    ! Fortran does not have a standard 'open()' function to check if a unit is open.
    ! The following simply closes the units; closing an already closed unit is safe in most compilers.
    close(UNIT_OUTPUT)    ! tsfoil2.out
    close(UNIT_SUMMARY)   ! smry.out

  end subroutine close_output_files

end module io_module
