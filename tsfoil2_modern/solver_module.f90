! Module for flow solver functionality

module solver_module
  use tsfoil_data
  use numerical_solvers
  use grid_module
  implicit none
  
  ! Jump values for Kutta condition
  real, allocatable :: pjump(:)

contains

  ! Initialize solver module
  subroutine init_solver(size)
    integer, intent(in) :: size
    
    ! Allocate memory for the jump values
    if (allocated(pjump)) deallocate(pjump)
    allocate(pjump(size))
    pjump = 0.0
  end subroutine init_solver
    ! Function to compute U = dp/dx at point (i,j)
  function px(i, j) result(u)
    integer, intent(in) :: i, j
    real :: u
    real :: pji
    
    ! Test to locate end points
    if (i == idx%imin) then
      ! Upstream boundary
      u = 1.5 * mesh%xdiff(i+1) * (grid%p(j,i+1) - grid%p(j,i)) - &
          0.5 * mesh%xdiff(i+2) * (grid%p(j,i+2) - grid%p(j,i+1))
    else if (i == idx%imax) then
      ! Downstream boundary
      u = 1.5 * mesh%xdiff(i) * (grid%p(j,i) - grid%p(j,i-1)) - &
          0.5 * mesh%xdiff(i-1) * (grid%p(j,i-1) - grid%p(j,i-2))
    else
      ! Interior mesh point
      pji = grid%p(j,i)
      u = 0.5 * (mesh%xdiff(i+1) * (grid%p(j,i+1) - pji) + &
                 mesh%xdiff(i) * (pji - grid%p(j,i-1)))
    end if
  end function px
    ! Function to compute V = dp/dy at point (i,j)
  function py(i, j) result(v)
    integer, intent(in) :: i, j
    real :: v
    real :: pji, vminus, vplus
    integer :: ic
    
    ! Test for end points or points near airfoil slit
    if (j == idx%jmin) then
      ! Lower boundary. Use one sided derivative
      v = 1.5 * mesh%ydiff(j+1) * (grid%p(j+1,i) - grid%p(j,i)) - &
          0.5 * mesh%ydiff(j+2) * (grid%p(j+2,i) - grid%p(j+1,i))
    else if (j == idx%jlow) then
      ! Row of mesh points below airfoil
      vminus = mesh%ydiff(j) * (grid%p(j,i) - grid%p(j-1,i))
      
      ! Test to see if i,j is ahead, under, or behind slit
      if (i < idx%ile) then
        ! Ahead of airfoil
        v = 0.5 * ((grid%p(idx%jup,i) - grid%p(idx%jlow,i)) * &
                  mesh%ydiff(idx%jup) + vminus)
      else if (i > idx%ite) then
        ! Behind airfoil
        v = 0.5 * ((grid%p(idx%jup,i) - pjump(i) - grid%p(idx%jlow,i)) * &
                  mesh%ydiff(idx%jup) + vminus)
      else
        ! Under airfoil. Use derivative boundary condition
        ic = i - idx%ile + 1
        v = 0.5 * (airfoil%fxl(ic) - flow%alpha + vminus)
      end if
    else if (j == idx%jup) then
      ! Row of mesh points above airfoil
      vplus = mesh%ydiff(j+1) * (grid%p(j+1,i) - grid%p(j,i))
      
      ! Test to see if i is ahead of, over, or behind airfoil slit
      if (i < idx%ile) then
        ! Ahead of airfoil
        v = 0.5 * ((grid%p(idx%jup,i) - grid%p(idx%jlow,i)) * &
                  mesh%ydiff(idx%jup) + vplus)
      else if (i > idx%ite) then
        ! Behind airfoil
        v = 0.5 * ((grid%p(idx%jup,i) - pjump(i) - grid%p(idx%jlow,i)) * &
                  mesh%ydiff(idx%jup) + vplus)
      else
        ! Over airfoil
        ic = i - idx%ile + 1
        v = 0.5 * (vplus + airfoil%fxu(ic) - flow%alpha)
      end if
    else if (j == idx%jmax) then
      ! Top row of mesh points. Use one sided formula
      v = 1.5 * mesh%ydiff(j) * (grid%p(j,i) - grid%p(j-1,i)) - &
          0.5 * mesh%ydiff(j-1) * (grid%p(j-1,i) - grid%p(j-2,i))
    else
      ! Interior point
      pji = grid%p(j,i)
      v = 0.5 * (mesh%ydiff(j+1) * (grid%p(j+1,i) - pji) + &
                 mesh%ydiff(j) * (pji - grid%p(j-1,i)))
    end if
  end function py
  
  ! Function to compute local Mach number or similarity parameter
  function emach1(u) result(local_mach)
    real, intent(in) :: u
    real :: local_mach
    real :: ak1, arg
    
    ! Compute similarity parameter based on local velocity
    ak1 = flow%ak - flow%gam1 * u
    
    if (.not. flow%phys) then
      ! Return value of local similarity parameter
      local_mach = ak1
    else
      ! Compute value of local Mach number based on scaling method
      arg = flow%delrt2 * ak1
      
      ! Spreiter scaling
      if (control%simdef == 2) then
        arg = arg * flow%emroot * flow%emroot
      end if
      
      ! Krupp scaling
      if (control%simdef == 3) then
        arg = arg * flow%emach
      end if
      
      ! Calculate local Mach number
      arg = 1.0 - arg
      local_mach = 0.0
      if (arg > 0.0) then
        local_mach = sqrt(arg)
      end if
    end if
  end function emach1

  ! Set up the difference coefficients
  subroutine difcoe()
    use tsfoil_data, only: idx, grid, coeff ! Ensure types are accessible
    implicit none
    integer :: i, j
    real :: dx1, dx2, dx3, dx4, denom
    
    ! Compute finite difference coefficients for second derivatives
    ! in the x-direction
    do i = idx%imin + 1, idx%imax - 1
      dx1 = grid%x(i) - grid%x(i-1)
      dx2 = grid%x(i+1) - grid%x(i)
      dx3 = grid%x(i+1) - grid%x(i-1)
      dx4 = dx1 * dx2
      
      denom = dx3 * dx4
      coeff%cxxl(i) = 2.0 / (dx1 * denom)
      coeff%cxxr(i) = 2.0 / (dx2 * denom)
      coeff%cxxc(i) = -2.0 / (dx4 * denom) * dx3 * dx3
      
      coeff%cxl(i) = -dx2 / (dx1 * dx3)
      coeff%cxr(i) = dx1 / (dx2 * dx3)
      coeff%cxc(i) = (dx2 - dx1) / (dx1 * dx2)
    end do
    
    ! Handle special case at upstream boundary
    i = idx%imin
    dx1 = grid%x(i+1) - grid%x(i)
    dx2 = grid%x(i+2) - grid%x(i+1)
    dx3 = grid%x(i+2) - grid%x(i)
    
    coeff%cxc(i) = -(dx2 + 2.0 * dx1) / (dx1 * dx3)
    coeff%cxr(i) = dx3 / (dx1 * (dx1 + dx2))
    coeff%cxr(i) = dx1 / (dx2 * dx3) ! Corrected cx2r to cxr
    
    ! Handle special case at downstream boundary
    i = idx%imax
    dx1 = grid%x(i-1) - grid%x(i-2)
    dx2 = grid%x(i) - grid%x(i-1)
    dx3 = grid%x(i) - grid%x(i-2)
    
    coeff%cxc(i) = (dx1 + 2.0 * dx2) / (dx2 * dx3)
    coeff%cxl(i) = -dx3 / (dx2 * (dx1 + dx2))
    coeff%cxl(i) = -dx2 / (dx1 * dx3) ! Corrected cx2l to cxl
    
    ! Compute finite difference coefficients for second derivatives
    ! in the y-direction
    ! For simplicity, using central differences
    do j = idx%jmin + 1, idx%jmax - 1
      coeff%cyyc(j) = -2.0 / ((grid%y(j+1) - grid%y(j)) * (grid%y(j) - grid%y(j-1)))
      coeff%cyyu(j) = 2.0 / ((grid%y(j+1) - grid%y(j)) * (grid%y(j+1) - grid%y(j)))
      coeff%cyyd(j) = 2.0 / ((grid%y(j) - grid%y(j-1)) * (grid%y(j+1) - grid%y(j-1)))
    end do
    
    ! Define body coefficients at j=jup and j=jlow
    ! These are special cases for the airfoil boundary
    ! ...
  end subroutine difcoe
  
    ! Set boundary conditions  
  subroutine setbc(ijump)
    use tsfoil_data, only: idx, flow, control, airfoil, wake, coeff, aparam ! Ensure types are accessible
    implicit none
    integer, intent(in) :: ijump
    integer :: i, n, if_val, int_val, jint, nfoil ! Declare nfoil
    
    ! Branch based on the jump parameter
    if (ijump > 0) goto 20
    
    ! Set limits on i and j indices
    int_val = 0
    if (flow%ak < 0.0) int_val = 1
    idx%iup = idx%imin + 1 + int_val
    idx%idown = idx%imax - 1 + int_val
    
    jint = 0
    if (aparam%bctype == 1 .and. flow%ak > 0.0) jint = 1
    if (aparam%bctype == 3) jint = 1
    if (aparam%bctype == 5 .and. aparam%por > 1.5) jint = 1
    
    idx%jbot = idx%jmin + jint
    idx%jtop = idx%jmax - jint
    idx%j1 = idx%jbot + 1
    idx%j2 = idx%jtop - 1
    
20  continue
    ! Airfoil body boundary condition
    ! Zero elements in arrays for upper and lower body boundary conditions
    do i = idx%imin, idx%imax
      coeff%fxlbc(i) = 0.0
      coeff%fxubc(i) = 0.0
    end do
    
    ! Enter body slopes at mesh points on airfoil into arrays for body boundary conditions
    if (control%iref <= 0) control%kstep = 1
    if (control%iref == 1) control%kstep = 2
    if (control%iref == 2) control%kstep = 4
    
    ! Calculate number of points on airfoil
    nfoil = idx%ite - idx%ile + 1
    
    ! Adjust indices for boundary condition calculation
    if_val = airfoil%ifoil + control%kstep
    i = idx%ite + 1
    
    ! Set boundary conditions at each mesh point on airfoil
    do n = 1, nfoil
      i = i - 1
      if_val = if_val - control%kstep
      
      ! Apply boundary conditions with angle of attack and any wake/shock effects
      coeff%fxlbc(i) = coeff%cyyblu * (airfoil%fxl(if_val) - flow%alpha + wake%wslp(i, 2))
      coeff%fxubc(i) = coeff%cyybud * (airfoil%fxu(if_val) - flow%alpha + wake%wslp(i, 1))
    end do
  end subroutine setbc
  
  ! Main solver
  subroutine solve()
    use tsfoil_data, only: control, solver, err, idx, grid ! Ensure types are accessible
    implicit none
    integer :: iter, maxitm, j ! Declare j
    real :: wep
    logical :: converged
    
    converged = .false. ! Initialize converged
    ! Setup for solver
    if (control%iref == 2) maxitm = solver%maxit / 4
    if (control%iref == 1) maxitm = solver%maxit / 2
    if (control%iref == 0) maxitm = solver%maxit
    
    wep = solver%we(3 - control%iref)
    err%wi = 1.0 / wep
    
    write(15, '(A, F6.3, A, E10.3, A, I5)') &
      ' Relaxation factor =', wep, ' Convergence criterion =', solver%eps, &
      ' Maximum iterations =', maxitm
    
    write(15, '(A)') ' Iteration   Maximum Error   Location'
    
    ! Main iteration loop
    do iter = 1, maxitm
      ! Initialize error tracking
      err%i1 = 1
      err%i2 = 2
      
      ! Save old values for error calculation
      do j = idx%jmin, idx%jmax
        err%pold(j, err%i2) = grid%p(j, idx%iup-1)
        err%emu(j, err%i2) = 0.0
      end do
      
      ! Update circulation
      call recirc()
      
      ! Perform successive line overrelaxation
      call syor()
      
      ! Check for convergence
      converged = err%error < solver%eps
      
      ! Print iteration info if needed
      if (mod(iter, solver%iprter) == 0) then
        write(15, '(I8, E16.6, 2I5)') iter, err%error, err%ierror, err%jerror
      end if
      
      if (converged) then
        write(15, '(A, I8)') ' Solution converged at iteration', iter
        exit
      end if
      
      ! Check for divergence
      if (err%error > solver%dverge) then
        write(15, '(A)') ' *** Solution diverged ***'
        control%abort1 = .true.
        return
      end if
    end do
    
    ! Handle non-convergence
    if (.not. converged) then
      write(15, '(A, I8, A)') ' Solution did not converge after', maxitm, ' iterations'
    end if
  end subroutine solve
    ! Recirculation boundary conditions
  subroutine recirc()
    use tsfoil_data, only: circ, mesh, grid, idx, scale ! Ensure types are accessible
    implicit none
    real :: pup, plow, circo, cteold, factor
    integer :: i
    
    ! Compute jump in potential at trailing edge
    cteold = circ%circte
    pup = mesh%cjup * grid%p(idx%jup, idx%ite) - mesh%cjup1 * grid%p(idx%jup+1, idx%ite)
    plow = mesh%cjlow * grid%p(idx%jlow, idx%ite) - mesh%cjlow1 * grid%p(idx%jlow-1, idx%ite)
    circ%circte = pup - plow
    
    ! Compute far field circulation
    circo = circ%circff
    if (circ%kutta) then
      circ%circff = (1.0 - circ%wcirc) * circo + circ%circte * circ%wcirc
    else
      circ%circff = 0.5 * circ%clset / scale%clfact
    end if
    
    ! Fix jump in P at airfoil trailing edge if kutta=.false.
    ! and lift of airfoil exceeds clset
    if (.not. circ%kutta) circ%circte = circ%circff
    circ%dcirc = circ%circte - cteold
    
    ! Set jump in P along y = 0, x > 1
    factor = (circ%circff - circ%circte) / (grid%x(idx%imax) - 1.0)
    do i = idx%ite, idx%imax
      pjump(i) = circ%circte + (grid%x(i) - 1.0) * factor
    end do
  end subroutine recirc
    ! Modify the diagonal and right-hand-side vectors for boundary conditions
  subroutine bcend()
    use tsfoil_data, only: coeff, flow, grid, idx, tri, mesh, aparam ! Ensure types are accessible
    implicit none
    integer :: i, ii
    real :: dfacl, dfacu, rfacl, rfacu
    real :: pjmin, pjmax, term
    
    pjmin = 0.0 ! Initialize pjmin
    pjmax = 0.0 ! Initialize pjmax
    term = 0.0 ! Initialize term
    
    i = coeff%ival
    
    ! Branch to appropriate address for boundary type
    select case (aparam%bctype)
    
    case (1) ! BCTYPE = 1, FREE AIR
      ! Dirchlet boundary condition for subsonic freestream
      if (flow%ak > 0.0) return
      
      ! Neuman boundary condition for supersonic freestream
      dfacl = -coeff%cyyd(idx%jbot) * flow%rtk * mesh%xdiff(i)
      dfacu = -coeff%cyyu(idx%jtop) * flow%rtk * mesh%xdiff(i)
      rfacl = dfacl * (grid%p(idx%jmin, i) - grid%p(idx%jmin, i-1))
      rfacu = dfacu * (grid%p(idx%jmax, i) - grid%p(idx%jmax, i-1))
      
      ! Apply Neuman boundary conditions
      go to 95
      
    case (2) ! BCTYPE = 2, SOLID WALL
      ! Neuman boundary condition = 0
      ! No modification necessary to DIAG or RHS
      return
      
    case (3) ! BCTYPE = 3, FREE JET
      ! Dirchlet boundary condition
      if (flow%ak < 0.0) then
        pjmin = 0.0
        pjmax = 0.0
      else
        pjmin = -0.75 * circ%circff
        pjmax = -0.25 * circ%circff
      end if
      
      ! Apply Dirchlet boundary conditions
      go to 90
      
    case (4) ! BCTYPE = 4, IDEAL SLOTTED WALL
      ! Neuman boundary condition
      dfacl = -aparam%fhinv * coeff%cyyd(idx%jbot)
      dfacu = -aparam%fhinv * coeff%cyyu(idx%jtop)
      
      if (flow%ak < 0.0) then
        rfacl = dfacl * grid%p(idx%jbot, i)
        rfacu = dfacu * grid%p(idx%jtop, i)
      else
        rfacl = dfacl * (0.75 * circ%circff + grid%p(idx%jbot, i))
        rfacu = dfacu * (0.25 * circ%circff + grid%p(idx%jtop, i))
      end if
      
      ! Apply Neuman boundary conditions
      go to 95
      
    case (5) ! BCTYPE = 5, POROUS/PERFORATED WALL
      if (aparam%por <= 1.5) then
        ! Neuman boundary condition for POR < 1.5
        dfacl = -coeff%cyyd(idx%jbot) * aparam%por * mesh%xdiff(i)
        dfacu = -coeff%cyyu(idx%jtop) * aparam%por * mesh%xdiff(i)
        rfacl = dfacl * (grid%p(idx%jmin, i) - grid%p(idx%jmin, i-1))
        rfacu = dfacu * (grid%p(idx%jmax, i) - grid%p(idx%jmax, i-1))
        
        ! Apply Neuman boundary conditions
        go to 95
      else
        ! Dirchlet boundary condition for POR > 1.5
        if (i /= idx%iup) return
        
        ! Set values of P on boundary by integrating PX using old values of potential
        pjmin = grid%p(idx%jmin, idx%iup)
        term = -0.5 / (aparam%por * (grid%y(idx%jmin) - grid%y(idx%jmin+1)))
        
        do ii = idx%iup, idx%idown
          grid%p(idx%jmin, ii) = grid%p(idx%jmin, ii-1) - term * (grid%x(ii) - grid%x(ii-1)) * &
                              (grid%p(idx%jmin, ii) + grid%p(idx%jmin, ii-1) - &
                               grid%p(idx%jmin+1, ii) - grid%p(idx%jmin+1, ii-1))
        end do
        
        pjmax = grid%p(idx%jmax, idx%iup)
        term = 0.5 / (aparam%por * (grid%y(idx%jmax) - grid%y(idx%jmax-1)))
        
        do ii = idx%iup, idx%idown
          grid%p(idx%jmax, ii) = grid%p(idx%jmax, ii-1) - term * (grid%x(ii) - grid%x(ii-1)) * &
                              (grid%p(idx%jmax, ii) + grid%p(idx%jmax, ii-1) - &
                               grid%p(idx%jmax-1, ii) - grid%p(idx%jmax-1, ii-1))
        end do
        
        tri%rhs(idx%jbot) = tri%rhs(idx%jbot) - &
                             (coeff%cyyd(idx%jbot) * (grid%p(idx%jbot-1, i) - pjmin))
        tri%rhs(idx%jtop) = tri%rhs(idx%jtop) - &
                             (coeff%cyyu(idx%jtop) * (grid%p(idx%jtop+1, i) - pjmax))
        return
      end if
      
    case (6) ! BCTYPE = 6, GENERAL WALL BOUNDARY CONDITION
      ! General wall boundary conditions not implemented
      write(15, '(A)') 'ABNORMAL STOP IN SUBROUTINE BCEND'
      write(15, '(A)') 'BCTYPE=6 IS NOT USEABLE'
      stop
      
    end select
    
    ! Dirchlet boundary conditions
90  continue
    tri%rhs(idx%jbot) = tri%rhs(idx%jbot) - &
                         (coeff%cyyd(idx%jbot) * (pjmin - grid%p(idx%jbot-1, i)))
    tri%rhs(idx%jtop) = tri%rhs(idx%jtop) - &
                         (coeff%cyyu(idx%jtop) * (pjmax - grid%p(idx%jtop+1, i)))
    return
    
    ! Neuman boundary conditions
95  continue
    tri%diag(idx%jbot) = tri%diag(idx%jbot) + dfacl
    tri%diag(idx%jtop) = tri%diag(idx%jtop) + dfacu
    tri%rhs(idx%jbot) = tri%rhs(idx%jbot) - rfacl + coeff%cyyd(idx%jbot) * grid%p(idx%jbot-1, i)
    tri%rhs(idx%jtop) = tri%rhs(idx%jtop) - rfacu + coeff%cyyu(idx%jtop) * grid%p(idx%jtop+1, i)
    return
  end subroutine bcend
  
  ! Successive line overrelaxation
  subroutine syor()
    use tsfoil_data, only: idx, flow, solver, coeff, err, grid, circ, tri, ageom, aparam ! Ensure types are accessible, added ageom for pjump
    implicit none
    integer :: i, j, ja, jb, k, im2, isave
    real :: epsx, dnom, arhs
    real, dimension(idx%jmax) :: save
    
    ! Set initial indices
    im2 = idx%iup - 1
    if (flow%ak < 0.0) im2 = idx%iup - 2
    
    ! Set j-indices for solver
    idx%j1 = idx%jbot + 1
    idx%j2 = idx%jtop - idx%jbot
    
    ! Main loop over i-coordinates
    do i = idx%iup, idx%idown
      epsx = solver%eps / ((grid%x(i) - grid%x(i-1))**2)
      
      ! Compute VC = 1 - M^2
      do j = idx%jbot, idx%jtop
        err%vc(j) = coeff%c1(i) - (coeff%cxl(i) * err%pold(j, err%i2) + &
                      coeff%cxc(i) * grid%p(j, i) + &
                      coeff%cxr(i) * grid%p(j, i+1))
        err%emu(j, err%i1) = 0.0
        err%pold(j, err%i1) = grid%p(j, i)
      end do
      
      ! Check for supersonic points
      do j = idx%jbot, idx%jtop
        if (err%vc(j) < 0.0) err%emu(j, err%i1) = err%vc(j)
      end do
      
      ! Handle circulation
      if (.not. circ%fcr) then
        do j = idx%jbot, idx%jtop
          err%emu(j, err%i2) = err%emu(j, err%i1)
        end do
      end if
      
      ! Compute elements of matrix
      do j = idx%jbot, idx%jtop
        tri%diag(j) = (err%emu(j, err%i1) - err%vc(j)) * coeff%cxxc(i) * err%wi + &
                        err%emu(j, err%i2) * coeff%cxxr(i-1) - coeff%cyyc(j)
        tri%sup(j) = coeff%cyyd(j)
        tri%sub(j) = coeff%cyyu(j)
      end do
      
      ! Compute residual
      do j = idx%jbot, idx%jtop
        tri%rhs(j) = -(err%vc(j) - err%emu(j, err%i1)) * &
                     (coeff%cxxl(i) * grid%p(j, i-1) - coeff%cxxc(i) * grid%p(j, i) + &
                      coeff%cxxr(i) * grid%p(j, i+1))
      end do
      
      do j = idx%jbot, idx%jtop
        tri%rhs(j) = tri%rhs(j) - (err%emu(j, err%i2) * &
                     (coeff%cxxl(i-1) * grid%p(j, im2) - &
                      coeff%cxxc(i-1) * grid%p(j, i-1) + &
                      coeff%cxxr(i-1) * grid%p(j, i)))
      end do
      
      ! Add y-derivative terms to RHS
      ja = idx%jbot + 1
      jb = idx%jtop - 1
      do j = ja, jb
        tri%rhs(j) = tri%rhs(j) - (coeff%cyyd(j) * grid%p(j-1, i) - &
                                    coeff%cyyc(j) * grid%p(j, i) + &
                                    coeff%cyyu(j) * grid%p(j+1, i))
      end do
      
      ! Handle bottom boundary
      tri%rhs(idx%jbot) = tri%rhs(idx%jbot) - &
                           (-coeff%cyyc(idx%jbot) * grid%p(idx%jbot, i) + &
                             coeff%cyyu(idx%jbot) * grid%p(idx%jbot+1, i))
      if (idx%jbot /= idx%jmin) then
        tri%rhs(idx%jbot) = tri%rhs(idx%jbot) - &
                             coeff%cyyd(idx%jbot) * grid%p(idx%jbot-1, i)
      end if
      
      ! Handle top boundary
      tri%rhs(idx%jtop) = tri%rhs(idx%jtop) - &
                           (coeff%cyyd(idx%jtop) * grid%p(idx%jtop-1, i) - &
                            coeff%cyyc(idx%jtop) * grid%p(idx%jtop, i))
      if (idx%jtop /= idx%jmax) then
        tri%rhs(idx%jtop) = tri%rhs(idx%jtop) - &
                             coeff%cyyu(idx%jtop) * grid%p(idx%jtop+1, i)
      end if
      
      ! Check for airfoil B.C. and Kutta slice
      if (i >= idx%ile .and. i <= idx%ite) then
        ! Airfoil boundary condition
        j = idx%jup
        tri%diag(j) = tri%diag(j) + coeff%cyyc(j) - coeff%cyybuc
        tri%sup(j) = 0.0
        tri%sub(j) = coeff%cyybuu
        tri%rhs(j) = tri%rhs(j) + coeff%cyyd(j) * grid%p(j-1, i) - &
                      coeff%cyyc(j) * grid%p(j, i) + &
                      coeff%cyyu(j) * grid%p(j+1, i) - &
                      (-coeff%cyybuc * grid%p(j, i) + coeff%cyybuu * grid%p(j+1, i) + coeff%fxubc(i))
        
        j = idx%jlow
        tri%diag(j) = tri%diag(j) + coeff%cyyc(j) - coeff%cyyblc
        tri%sup(j) = coeff%cyybld
        tri%sub(j) = 0.0
        tri%rhs(j) = tri%rhs(j) + coeff%cyyd(j) * grid%p(j-1, i) - &
                      coeff%cyyc(j) * grid%p(j, i) + &
                      coeff%cyyu(j) * grid%p(j+1, i) - &
                      (-coeff%cyyblc * grid%p(j, i) + coeff%cyybld * grid%p(j-1, i) + coeff%fxlbc(i))
      else if (i > idx%ite) then
        ! Kutta slice change
        tri%rhs(idx%jlow) = tri%rhs(idx%jlow) + coeff%cyyu(idx%jlow) * ageom%pjump(i) ! Corrected pjump to ageom%pjump
        tri%rhs(idx%jup) = tri%rhs(idx%jup) - coeff%cyyd(idx%jup) * ageom%pjump(i) ! Corrected pjump to ageom%pjump
      end if
      
      ! Insert wall boundary condition
      coeff%ival = i
      call bcend()
      
      ! Compute max residual for error tracking
      if (control%outerr) then
        do j = idx%jbot, idx%jtop
          arhs = abs(tri%rhs(j))
          if (arhs > aparam%bigrl) then
            aparam%bigrl = arhs
            aparam%irl = i
            aparam%jrl = j
          end if
        end do
      end if
      
      ! Add PXT term
      do j = idx%jbot, idx%jtop
        tri%diag(j) = tri%diag(j) - epsx
        tri%rhs(j) = tri%rhs(j) - epsx * (grid%p(j, i-1) - err%pold(j, err%i2))
      end do
      
      ! Solve tridiagonal matrix equation
      dnom = 1.0 / tri%diag(idx%jbot)
      save(idx%jbot) = tri%sub(idx%jbot) * dnom
      tri%rhs(idx%jbot) = tri%rhs(idx%jbot) * dnom
      
      do j = idx%j1, idx%jtop
        dnom = 1.0 / (tri%diag(j) - tri%sup(j) * save(j-1))
        save(j) = tri%sub(j) * dnom
        tri%rhs(j) = (tri%rhs(j) - tri%sup(j) * tri%rhs(j-1)) * dnom
      end do
      
      do k = 1, idx%j2
        j = idx%jtop - k
        tri%rhs(j) = tri%rhs(j) - save(j) * tri%rhs(j+1)
      end do
      
      ! Compute new P and update with relaxation
      do j = idx%jbot, idx%jtop
        grid%p(j, i) = grid%p(j, i) + tri%rhs(j)
      end do
      
      ! Compute max error
      if (control%outerr) then
        do j = idx%jbot, idx%jtop
          arhs = abs(tri%rhs(j))
          if (arhs > err%error) then
            err%error = arhs
            err%ierror = i
            err%jerror = j
          end if
        end do
      end if
      
      ! Special handling for supersonic freestream
      if (flow%ak <= 0.0 .and. i == idx%idown-1) then
        ! Set P(IDOWN+1) = P(IDOWN-1) to obtain centered velocity
        ! at IDOWN for supersonic freestream flow
        do j = idx%jmin, idx%jmax
          grid%p(j, idx%idown+1) = grid%p(j, idx%idown-1)
        end do
      end if
      
      ! Update indices for next iteration
      isave = err%i2
      err%i2 = err%i1
      err%i1 = isave
      im2 = i - 1
    end do
  end subroutine syor

end module solver_module
