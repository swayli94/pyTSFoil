! Module for output and visualization functions
module output_module
  use tsfoil_data, only: PI, flow, grid, idx, coeff, err, plot_data, old, aparam ! Specify entities to use from tsfoil_data
  use numerical_solvers
  use solver_module ! Added to make px, py, emach1 available
  implicit none

  ! real, parameter :: PI = acos(-1.0) ! PI is available from tsfoil_data
  real, parameter :: RADDEG = 180.0 / PI ! Define RADDEG for radians to degrees conversion

contains

  ! Main output routine
  subroutine print_results()
    ! First call routine to print basic information
    call print1()
    
    ! Print Mach number contours
    call prtmc()
    
    ! Print Mach number map
    call machmp()
    
    ! Check for abort condition
    if (control%abort1) return
    
    ! Plot CP distribution
    call fixplt()
    
    ! Print wall information if applicable
    if (aparam%bctype /= 1 .and. aparam%bctype /= 3) then
      call prtwal()
    end if
    
    ! Print M=1 line information
    call m1line()
    
    ! Print detailed flow field if requested
    if (control%prtflo /= 1) then
      call prtfld(15) ! Pass 15 as the unit number directly
    end if
  end subroutine print_results
  
  ! Print basic information
  subroutine print1()
    real :: alphvf
    
    ! Convert angle of attack to physical value
    if (flow%phys) then
      alphvf = flow%alpha
    else
      alphvf = flow%alpha * flow%vfact
    end if
    
    ! Print case title
    write(15, '(A)') '1'
    write(15, '(4X, 20A4)') old%title
    
    ! Print main flow parameters
    write(15, '(A)') ' '
    write(15, '(A)') ' Flow Parameters:'
    
    ! Print parameters based on physical flag
    if (flow%phys) then
      write(15, '(A, F10.6)') ' Mach number =', flow%emach
      write(15, '(A, F10.6)') ' Thickness ratio =', flow%delta
    end if
    
    ! Print angle of attack and transonic similarity parameter
    write(15, '(A, F10.6)') ' Angle of attack =', alphvf
    write(15, '(A, F10.6)') ' Transonic parameter K =', flow%ak
    
    ! Print additional parameters if applicable
    if (flow%ak > 0.0) then
      write(15, '(A, F10.6)') ' Far-field correction DUB =', flow%dub
    end if
    
    ! Print scaling factors if physical
    if (flow%phys) then
      write(15, '(A)') ' '
      write(15, '(A)') ' Scaling Factors:'
      write(15, '(A, F10.6)') ' CP factor =', scale%cpfact
      write(15, '(A, F10.6)') ' CD factor =', scale%cdfact
      write(15, '(A, F10.6)') ' CM factor =', scale%cmfact
      write(15, '(A, F10.6)') ' CL factor =', scale%clfact
      write(15, '(A, F10.6)') ' Y factor =', flow%yfact
      write(15, '(A, F10.6)') ' V factor =', flow%vfact
    end if
  end subroutine print1
  
  ! Print Mach contours  
  subroutine prtmc()
    ! Subroutine to print a character for each point in the grid describing the type of flow
    ! S: Shock point
    ! H: Hyperbolic point (supersonic)
    ! P: Parabolic point (sonic)
    ! -: Elliptic point (subsonic)
    integer :: i, j, k
    real :: vt1, vt2
    character :: ipc(100)
    character, parameter :: IHP = 'P', IHH = 'H', IHS = 'S', IHD = '-', IB = ' '
    
    ! Print header
    write(15, '(A)') ' Flow at each grid point.  P Parabolic'
    write(15, '(28X, A)') 'H Hyperbolic'
    write(15, '(28X, A)') 'S Shock'
    write(15, '(28X, A)') '- Elliptic'
    write(15, '(A)') ' '
    
    ! Initialize character array
    do i = 1, 100
      ipc(i) = IB
    end do
    
    ! Initialize velocity array
    do j = idx%jmin, idx%jmax
      vt1 = coeff%c1(2)
    end do
    
    ! Process each grid point
    do k = idx%jmin, idx%jmax
      j = idx%jmax - k + 1
      
      do i = idx%iup, idx%idown
        ! Calculate local Mach number parameter 1-M^2 at current and previous points
        vt2 = vt1
        vt1 = coeff%c1(i) - (coeff%cxl(i)*grid%p(j,i-1) + &
                            coeff%cxc(i)*grid%p(j,i) + &
                            coeff%cxr(i)*grid%p(j,i+1))
        
        ! Determine flow type based on local Mach number
        if (vt1 > 0.0) then
          if (vt2 < 0.0) then
            ! Shock point (supersonic to subsonic transition)
            ipc(i) = IHS
          else
            ! Elliptic (subsonic) point
            ipc(i) = IHD
          end if
        else
          if (vt2 < 0.0) then
            ! Hyperbolic (supersonic) point
            ipc(i) = IHH
          else
            ! Parabolic (sonic) point
            ipc(i) = IHP
          end if
        end if
      end do
      
      ! Print row of flow type characters
      write(15, '(10X,I3,5X,100A1)') j, (ipc(i), i=idx%iup, idx%idown)
    end do
  end subroutine prtmc
  
  ! Print Mach number map
  subroutine machmp()
    integer :: i, j
    character :: mm(100)
    character, parameter :: mchars(0:9) = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
    real :: mach_value
    
    write(15, '(A)') '1  Mach No. Map.   Rounded to Nearest .1'
    write(15, '(A)') ' '
    write(15, '(A)') ' '
    
    ! Calculate and print Mach number map
    do j = idx%jmax, idx%jmin, -1
      do i = idx%imin, idx%imax
        ! Calculate Mach number at this point (simplified version)
        mach_value = local_mach_number(i, j)
        
        ! Convert to character representation
        if (mach_value < 0.05) then
          mm(i) = ' '
        else if (mach_value > 0.95) then
          mm(i) = '9'
        else
          mm(i) = mchars(nint(mach_value * 10.0))
        end if
      end do
      
      ! Print this row of the map
      write(15, '(1X, I3, 1X, 99A1)') j, (mm(i), i = idx%imin, idx%imax)
    end do
    
    ! Print x-axis labels
    write(15, '(5X, 99I1)') (mod(i, 10), i = idx%imin, idx%imax)
  end subroutine machmp
  
  ! Return local Mach number at a grid point
  function local_mach_number(i, j) result(mach_local)
    integer, intent(in) :: i, j
    real :: mach_local, velocity
    
    ! This is a simplified placeholder
    ! In the real code, this would calculate the Mach number based on
    ! the potential values and local flow conditions
    
    ! Get velocity (dp/dx)
    velocity = calculate_velocity(i, j)
    
    ! Convert velocity to Mach number
    if (flow%ak <= 0.0) then
      ! Subsonic freestream
      mach_local = flow%emach * (1.0 + 0.5 * flow%gam1 * flow%emach * velocity)
    else
      ! Supersonic freestream
      mach_local = flow%emach * (1.0 - 0.5 * flow%gam1 * flow%emach * velocity)
    end if
    
    mach_local = max(0.0, min(1.5, mach_local))
  end function local_mach_number
  
  ! Calculate velocity at a grid point
  function calculate_velocity(i, j) result(velocity)
    integer, intent(in) :: i, j
    real :: velocity
    
    ! This is a simplified placeholder
    ! In the real code, this would calculate dp/dx using finite differences
    
    velocity = 0.0
    if (i > idx%imin .and. i < idx%imax) then
      velocity = (grid%p(j, i+1) - grid%p(j, i-1)) / (grid%x(i+1) - grid%x(i-1))
    end if
  end function calculate_velocity
  
  ! Plot CP distribution
  subroutine fixplt()
    ! Sets up arrays for CP plotting
    ! Called by - print_results
    
    real :: ymx, ymn, qcp, qc1, qc2
    integer :: k_idx, imp_idx 
    integer :: i ! Declare loop variable i
    
    ! Arrays for plotting are now directly from plot_data in tsfoil_data
    
    ! Allocate arrays if not already allocated
    if (.not. allocated(plot_data%cpup)) allocate(plot_data%cpup(idx%imax+1))
    if (.not. allocated(plot_data%cplo)) allocate(plot_data%cplo(idx%imax+1))
    if (.not. allocated(plot_data%cps)) allocate(plot_data%cps(idx%imax+1))
    if (.not. allocated(plot_data%xp)) allocate(plot_data%xp(idx%imax+1))
    
    ! No longer need local aliases via pointer assignment
    
    ymx = 5.0 * scale%cpfact ! Use scale%cpfact
    ymn = -5.0 * scale%cpfact ! Use scale%cpfact
    
    k_idx = 0
    do i = idx%imin, idx%imax
      k_idx = k_idx + 1
      
      ! Upper surface
      qcp = -airfoil%cpu(i)
      qcp = max(qcp, ymn)
      qcp = min(qcp, ymx)
      plot_data%cpup(k_idx) = qcp ! Use plot_data directly
      
      ! Lower surface
      qc1 = -airfoil%cpl(i)
      qc1 = max(qc1, ymn)
      qc1 = min(qc1, ymx)
      plot_data%cplo(k_idx) = qc1 ! Use plot_data directly
      
      ! Critical CP
      qc2 = -scale%cpstar ! Use scale%cpstar
      qc2 = max(qc2, ymn)
      qc2 = min(qc2, ymx)
      plot_data%cps(k_idx) = qc2 ! Use plot_data directly
      
      plot_data%xp(k_idx) = grid%x(i) ! Use plot_data directly
    end do
    
    imp_idx = k_idx + 1
    plot_data%cpup(imp_idx) = ymx
    plot_data%cplo(imp_idx) = ymn
    plot_data%cps(imp_idx) = 0.0
    plot_data%xp(imp_idx) = grid%x(idx%imax) + 0.001
    
    call cp_plot(plot_data%xp, plot_data%cpup, plot_data%cplo, plot_data%cps, imp_idx)
    
  contains
  
    subroutine cp_plot(x_arr, cp_up, cp_lo, cp_star, np)
      ! Modern implementation of CP plotting
      real, dimension(:), intent(in) :: x_arr, cp_up, cp_lo, cp_star
      integer, intent(in) :: np
      
      ! Placeholder for actual plotting routine that would replace CPPLOT
      ! In a modern implementation, this might use a plotting library
      ! For now, just write data to file for external plotting
      
      integer :: plot_idx, output_unit_val ! Renamed i to plot_idx
      character(len=100) :: output_file
      
      output_file = 'cp_data.dat'
      open(newunit=output_unit_val, file=output_file, status='replace')
      
      write(output_unit_val, '(A)') '# X-coordinate, CP-upper, CP-lower, CP-critical'
      do plot_idx = 1, np-1
        write(output_unit_val, '(4F15.6)') x_arr(plot_idx), cp_up(plot_idx), cp_lo(plot_idx), cp_star(plot_idx)
      end do
      
      close(output_unit_val)
      write(6, '(A,A)') ' CP plot data written to file: ', trim(output_file) ! Use 6 for standard output
    end subroutine cp_plot
    
  end subroutine fixplt
    
  ! Prints pressure coefficient and flow angle on Y=-H and Y=+H, and plots CP along side of tabulation
  subroutine prtwal()
    
    real :: thh, porf
    integer :: i1_bc, i2_bc, i_loop ! Renamed i1 to i1_bc, i2 to i2_bc, i to i_loop
    character(len=15) :: bct
    character(len=4), dimension(15) :: bc_names
    
    ! Initialize boundary condition type names array
    bc_names = [character(len=4) :: '    ', 'FREE', ' AIR', '  SO', 'LID ', & 
                'WALL', '    ', 'FREE', ' JET', 'SLOT', 'TED ', & 
                'WALL', ' POR', 'OUS ', 'WALL']
    
    ! Calculate indices for boundary type names
    i2_bc = 3 * aparam%bctype ! Use aparam%bctype
    i1_bc = i2_bc - 2
    
    ! Build boundary condition type string
    bct = bc_names(i1_bc) // bc_names(i1_bc+1) // bc_names(i1_bc+2)
    
    ! Write header information
    write(6, '(A, A)') ' Boundary condition type: ', trim(bct) ! Use 6 for standard output
    
    ! Convert tunnel height to physical coordinates
    thh = aparam%h * flow%yfact ! Use aparam%h
    write(6, '(A, F10.5)') ' Tunnel height (physical) = ', thh ! Use 6 for standard output
    
    ! Additional information for specific boundary condition types
    if (aparam%bctype >= 5) then ! Use aparam%bctype
      porf = aparam%por / flow%yfact ! Use aparam%por
      write(6, '(A, F10.5)') ' Porosity factor = ', porf ! Use 6 for standard output
    end if
    
    if (aparam%bctype == 4 .or. aparam%bctype == 6) then ! Use aparam%bctype
      write(6, '(A, F10.5)') ' Slot parameter = ', aparam%f ! Use aparam%f, and 6 for standard output
    end if
    
    ! Print table header
    write(6, '(/A)') ' Upper and Lower Wall Pressure Distributions' ! Use 6 for standard output
    write(6, '(A)') ' -------------------------------------' ! Use 6 for standard output
    write(6, '(A)') ' X-coord      Upper Wall CP      Lower Wall CP      Upper Wall Angle    Lower Wall Angle' ! Use 6 for standard output
    
    ! Initialize plotting arrays if not already allocated
    if (.not. allocated(plot_data%cplw)) allocate(plot_data%cplw(idx%imax))
    if (.not. allocated(plot_data%cpuw)) allocate(plot_data%cpuw(idx%imax))
    if (.not. allocated(plot_data%vlw)) allocate(plot_data%vlw(idx%imax))
    if (.not. allocated(plot_data%vuw)) allocate(plot_data%vuw(idx%imax))
    
    ! Calculate and print wall values
    do i_loop = idx%imin, idx%imax
      ! Calculate pressure coefficients and flow angles at upper and lower walls
      call calculate_wall_values(i_loop, idx%jmax, plot_data%cpuw(i_loop), plot_data%vuw(i_loop))
      call calculate_wall_values(i_loop, idx%jmin, plot_data%cplw(i_loop), plot_data%vlw(i_loop))
      
      ! Print the values with proper formatting
      write(6, '(F10.4, 4F18.5)') grid%x(i_loop), plot_data%cpuw(i_loop), &
                                        plot_data%cplw(i_loop), plot_data%vuw(i_loop), &
                                        plot_data%vlw(i_loop) ! Removed redundant & from start of this line
    end do
    
  contains
  
    subroutine calculate_wall_values(i_wall, j_wall, cp_val, angle_val) ! Renamed i to i_wall, j to j_wall
      integer, intent(in) :: i_wall, j_wall
      real, intent(out) :: cp_val, angle_val
      
      real :: px_val, py_val, v_mag_sq, v_mag ! Declare v_mag
      
      ! Calculate velocity components (partial derivatives of potential)
      px_val = calculate_velocity(i_wall, j_wall)
      py_val = calculate_vertical_velocity(i_wall, j_wall)
      
      ! Calculate velocity magnitude
      v_mag_sq = px_val**2 + py_val**2
      v_mag = sqrt(v_mag_sq)
      
      ! Calculate pressure coefficient
      cp_val = 1.0 - v_mag_sq / flow%vfact**2
      
      ! Calculate flow angle in degrees
      angle_val = atan2(py_val, px_val) * 180.0 / PI
    end subroutine calculate_wall_values
    
    function calculate_vertical_velocity(i_vel, j_vel) result(velocity) ! Renamed i to i_vel, j to j_vel
      integer, intent(in) :: i_vel, j_vel
      real :: velocity
      
      ! This is a simplified placeholder
      ! In the real code, this would calculate dp/dy using finite differences
      if (j_vel > idx%jmin .and. j_vel < idx%jmax) then
        velocity = (grid%p(j_vel+1, i_vel) - grid%p(j_vel-1, i_vel)) / (grid%y(j_vel+1) - grid%y(j_vel-1))
      else
        velocity = 0.0
      end if
    end function calculate_vertical_velocity
    
  end subroutine prtwal
  
  ! Print flow field
  subroutine prtfld(unit_num) ! unit_num is an argument
    ! Prints pressure coefficient, flow angle and Mach number in flow field.
    ! Number of J lines printed is determined from the input value of PRTFLO:
    !   PRTFLO = 1: None
    !   PRTFLO = 2: All J lines except J0
    !   PRTFLO = 3: Three J lines around JERROR
    ! Called by - print_results
    
    integer, intent(in) :: unit_num ! unit_num is an argument
    integer :: jl, m_loop, mprend, mq_idx = 0, jq_val, is_idx, ie_idx, kt_val, i_xpos 
    integer :: mpr_loop 
    real :: u_val 
    real :: px_val, py_val
    
    ! Arrays for printing
    real, dimension(3) :: cppr, pypr, yprint, em1
    character(len=4), dimension(10) :: prt 
    character(len=2), dimension(2) :: tmac
    
    if (unit_num <= 0) return ! Do not proceed if unit_num is not valid

    ! Initialize data
    tmac = ['M1', 'K1'] ! Corrected array constructor
    prt = [character(len=4) :: 'MACH', ' NUM', 'BERS', '    ', '    ', & 
            'SIMI', 'LARI', 'TY P', 'ARAM', 'ETER'] ! Corrected array constructor
    
    ! Determine which J lines to print
    if (control%prtflo == 2) then
      ! Print all J lines
      jl = idx%jmax - idx%jmin + 1
      
      ! Fill jlin array with values of J
      do m_loop = 1, jl
        plot_data%jlin(m_loop) = idx%jmin + m_loop - 1
      end do
    else
      ! Print 3 lines around error point
      jl = 3
      
      ! Locate lines around JERROR
      if (err%jerror == idx%jmin .or. err%jerror == idx%jup) then
        ! Error at bottom or bottom-interior boundary
        plot_data%jlin(1) = err%jerror
        plot_data%jlin(2) = err%jerror + 1
        plot_data%jlin(3) = err%jerror + 2
      else if (err%jerror == idx%jlow .or. err%jerror == idx%jmax) then
        ! Error at top-interior or top boundary
        plot_data%jlin(1) = err%jerror - 2
        plot_data%jlin(2) = err%jerror - 1
        plot_data%jlin(3) = err%jerror
      else
        ! Error in middle of domain
        plot_data%jlin(1) = err%jerror - 1
        plot_data%jlin(2) = err%jerror
        plot_data%jlin(3) = err%jerror + 1
      end if
    end if
    
    ! Print flow field in 3 J lines per page
    do mpr_loop = 1, jl, 3
      mprend = min(mpr_loop+2, jl)
      
      mq_idx = 0 ! Initialize mq_idx to satisfy compiler warning
      ! Get y-coordinates for each line
      do m_loop = mpr_loop, mprend
        mq_idx = m_loop - mpr_loop + 1
        jq_val = plot_data%jlin(m_loop)
        yprint(mq_idx) = grid%y(jq_val) * flow%yfact
      end do
      
      ! Write page header
      is_idx = 1
      if (flow%phys) is_idx = 6
      ie_idx = is_idx + 4
      
      ! Corrected format string conversion from Hollerith
      write(unit_num, '( "1PRESSURE COEFFICIENTS, FLOW ANGLES, AND LOCAL ", 5A4, /, " ON Y=CONSTANT LINES/", "CPSTAR =", F12.7, /, / )') (prt(i_xpos), i_xpos=is_idx, ie_idx), scale%cpstar
      
      write(unit_num, '(13X,3(15X,"J=",I4,15X))') (plot_data%jlin(m_loop), m_loop=mpr_loop, mprend)
      write(unit_num, '(13X,3(12X,"Y=",F10.6,12X))') (yprint(m_loop), m_loop=1, mq_idx) 
      
      kt_val = 2
      if (flow%phys) kt_val = 1
      
      write(unit_num, '(4H0  I,8X,1HX,5X,3(6X,"CP",8X,"THETA",7X,A2,6X)//)') tmac(kt_val), tmac(kt_val), tmac(kt_val)
      
      ! Loop through x-positions
      do i_xpos = idx%imin, idx%imax
        ! Calculate values for each j-line
        do m_loop = mpr_loop, mprend
          mq_idx = m_loop - mpr_loop + 1
          jq_val = plot_data%jlin(m_loop)
          
          ! Calculate px and py
          px_val = px(i_xpos, jq_val) ! Use function px
          py_val = py(i_xpos, jq_val) ! Use function py
          
          ! Calculate CP
          u_val = 1.0 + px_val
          cppr(mq_idx) = 1.0 - u_val*u_val - (py_val*py_val) ! Simplified CP calculation
          
          ! Calculate flow angle (THETA)
          pypr(mq_idx) = atan2(py_val, u_val) * RADDEG
          
          ! Calculate local Mach number or K parameter
          if (flow%phys) then
            em1(mq_idx) = emach1(u_val) ! Use function emach1
          else
            em1(mq_idx) = flow%ak * (1.0 - u_val*u_val)
          end if
        end do
        
        ! Write line of data
        write(unit_num, '(I4, F8.4, 3(F12.4, F12.4, F10.4))') i_xpos, grid%x(i_xpos), (cppr(m_loop), pypr(m_loop), em1(m_loop), m_loop=1, mq_idx)
      end do
    end do
    
  end subroutine prtfld
  
  ! Print M=1 line information
  subroutine m1line()
    ! Prints coordinates where sonic velocity is computed
    ! Linear interpolation between mesh points is used
    ! Called by - print_results
    
    integer :: npts, kmin, kmax, jp, j, i
    
    character :: ipc(100) ! Declare ipc
    character, parameter :: IHP = 'P', IHH = 'H', IHS = 'S', IHD = '-', IB = ' ' ! Declare parameters
    real :: vt1, vt2 ! Declare vt1, vt2
    integer :: k ! Declare k for loop
    
    ! Initialize counter for sonic points
    npts = 0
    
    ! Define scanning range
    kmin = idx%jmin
    kmax = idx%jmax
    jp = idx%jmax + idx%jmin
    
    ! Allocate arrays for sonic line if not already allocated
    if (.not. allocated(plot_data%xslprt)) allocate(plot_data%xslprt(200))
    if (.not. allocated(plot_data%yslprt)) allocate(plot_data%yslprt(200))
    
    ! Print header
    write(6, '(/A)') ' Sonic Line Coordinates' ! Use 6 for standard output
    write(6, '(A)') ' --------------------'   ! Use 6 for standard output
    
    ! Initialize ipc array
    do i = 1, 100
      ipc(i) = IB
    end do
    
    ! Initialize vt1 (assuming it should be initialized before the loop, e.g. from coeff%c1)
    ! This might need adjustment based on the logic from prtmc or other context
    if (idx%imax > 0) then ! Basic check to avoid out-of-bounds if coeff%c1 is not large enough
        vt1 = coeff%c1(2) ! Example initialization, adjust as needed
    else
        vt1 = 0.0
    end if

    ! Loop through grid points to find sonic (M=1) points
    do k = kmin, kmax
      j = jp - k
      
      do i = idx%iup, idx%idown
        ! Calculate local Mach number parameter 1-M^2 at current and previous points
        vt2 = vt1
        vt1 = coeff%c1(i) - (coeff%cxl(i)*grid%p(j,i-1) + & 
                            coeff%cxc(i)*grid%p(j,i) + & 
                            coeff%cxr(i)*grid%p(j,i+1))
        
        ! Determine flow type based on local Mach number
        if (vt1 > 0.0) then
          if (vt2 < 0.0) then
            ! Shock point (supersonic to subsonic transition)
            ipc(i) = IHS
          else
            ! Elliptic (subsonic) point
            ipc(i) = IHD
          end if
        else
          if (vt2 < 0.0) then
            ! Hyperbolic (supersonic) point
            ipc(i) = IHH
          else
            ! Parabolic (sonic) point
            ipc(i) = IHP
          end if
        end if
      end do
      
      ! Print row of flow type characters
      write(15, '(10X,I3,5X,100A1)') j, (ipc(i), i=idx%iup, idx%idown)
    end do
  end subroutine m1line
  
end module output_module
