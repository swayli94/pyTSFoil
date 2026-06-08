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
    use common_data, only: UNIT_OUTPUT, FLAG_OUTPUT
    implicit none
    logical :: unit_opened=.false.

    if (FLAG_OUTPUT /= 1) then
      return
    end if

    ! Check if the unit is already opened
    inquire(unit=UNIT_OUTPUT, opened=unit_opened)
    
    ! Only open if not already opened
    if (.not. unit_opened) then
      open(unit=UNIT_OUTPUT, file='tsfoil2.out', status='replace', action='write')   ! Unit 15
    end if
  end subroutine open_output_file

  subroutine open_summary_file()
    use common_data, only: UNIT_SUMMARY, FLAG_OUTPUT
    implicit none
    logical :: unit_opened=.false.
    
    if (FLAG_OUTPUT /= 1) then
      return
    end if

    ! Check if the unit is already opened
    inquire(unit=UNIT_SUMMARY, opened=unit_opened)
    
    ! Only open if not already opened
    if (.not. unit_opened) then
      open(unit=UNIT_SUMMARY, file='smry.out', status='replace', action='write')     ! Unit 16
    end if
  end subroutine open_summary_file

  ! Close all output files
  subroutine close_output_files()
    use common_data, only: UNIT_OUTPUT, UNIT_SUMMARY
    implicit none
    logical :: unit_opened=.false.

    ! Check and close UNIT_OUTPUT if opened
    inquire(unit=UNIT_OUTPUT, opened=unit_opened)
    if (unit_opened) then
      close(UNIT_OUTPUT)    ! tsfoil2.out
    end if
    
    ! Check and close UNIT_SUMMARY if opened
    inquire(unit=UNIT_SUMMARY, opened=unit_opened)
    if (unit_opened) then
      close(UNIT_SUMMARY)   ! smry.out
    end if

  end subroutine close_output_files

end module io_module
