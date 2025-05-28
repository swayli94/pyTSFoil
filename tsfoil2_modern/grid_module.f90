! Module for grid-related functionality

module grid_module
  use tsfoil_data
  implicit none

contains

  ! Find leading and trailing edge mesh points
  subroutine islit(x_mesh)
    real, intent(in) :: x_mesh(:)
    integer :: i, imin_local, imax_local
    
    ! Set local variables to the current bounds
    imin_local = idx%imin
    imax_local = idx%imax
    
    ! Find leading edge point (minimum x value)
    idx%ile = imin_local
    do i = imin_local + 1, imax_local
      if (x_mesh(i) < x_mesh(idx%ile)) then
        idx%ile = i
      end if
    end do
    
    ! Find trailing edge point (maximum x value)
    idx%ite = imin_local
    do i = imin_local + 1, imax_local
      if (x_mesh(i) > x_mesh(idx%ite)) then
        idx%ite = i
      end if
    end do
    
    ! Set upstream and downstream indices
    idx%iup = idx%ile - 1
    if (idx%iup < imin_local) idx%iup = imin_local
    
    idx%idown = idx%ite + 1
    if (idx%idown > imax_local) idx%idown = imax_local
  end subroutine islit
  
  ! Find the upper and lower airfoil surface mesh points
  subroutine jslit(y_mesh)
    real, intent(in) :: y_mesh(:)
    integer :: j, jmin_local, jmax_local
    real :: ymin, ymax, yjup, yjlow
    
    ! Set local variables to the current bounds
    jmin_local = idx%jmin
    jmax_local = idx%jmax
    
    ! Find points closest to y = 0 both above and below
    ymin = 1.0e6
    ymax = -1.0e6
    
    ! Find approximations to jlow and jup
    do j = jmin_local, jmax_local
      if (y_mesh(j) < -0.00001 .and. y_mesh(j) > ymax) then
        ymax = y_mesh(j)
        idx%jlow = j
      end if
      
      if (y_mesh(j) > 0.00001 .and. y_mesh(j) < ymin) then
        ymin = y_mesh(j)
        idx%jup = j
      end if
    end do
    
    ! If no points found above and below centerline, set default values
    if (ymin == 1.0e6) then
      idx%jup = (jmin_local + jmax_local) / 2 + 1
    end if
    
    if (ymax == -1.0e6) then
      idx%jlow = (jmin_local + jmax_local) / 2
    end if
    
    ! Find remaining j-indices for the grid
    yjup = y_mesh(idx%jup)
    yjlow = y_mesh(idx%jlow)
    
    ! Set bottom and top grid indices
    idx%jbot = jmin_local
    idx%jtop = jmax_local
    
    ! Set j1 and j2 (intermediate indices)
    idx%j1 = (idx%jlow + idx%jbot) / 2
    idx%j2 = (idx%jup + idx%jtop) / 2
  end subroutine jslit
  
  ! Check and adjust the mesh for proper dimensions
  subroutine ckmesh()
    ! integer :: ncut ! Removed ncut
    
    ! Make sure the mesh has odd number of points in certain regions
    
    ! Check if imax - imin is even
    if (mod(idx%imax - idx%imin, 2) == 0) then
      ! Add a point if possible
      if (idx%imax < 100) then
        idx%imax = idx%imax + 1
        idx%ile = idx%ile + 1
        mesh%xin(idx%imax) = 2.0 * mesh%xin(idx%imax - 1) - mesh%xin(idx%imax - 2)
      else
        write(*, *) "Warning: Cannot add point as imax would exceed 100"
        ! Alternative: drop a point
        idx%imin = idx%imin + 1
      end if
    end if
    
    ! Check if jlow - jmin is odd
    if (mod(idx%jlow - idx%jmin, 2) /= 0) then
      ! Add a point below if possible
      if (idx%jmin > 1) then
        idx%jmin = idx%jmin - 1
        mesh%yin(idx%jmin) = 2.0 * mesh%yin(idx%jmin + 1) - mesh%yin(idx%jmin + 2)
        call jslit(mesh%yin)
      else
        write(*, *) "Warning: Cannot decrease jmin below 1"
      end if
    end if
    
    ! Check if jmax - jup is odd
    if (mod(idx%jmax - idx%jup, 2) /= 0) then
      ! Add a point above if possible
      if (idx%jmax < 100) then
        idx%jmax = idx%jmax + 1
        mesh%yin(idx%jmax) = 2.0 * mesh%yin(idx%jmax - 1) - mesh%yin(idx%jmax - 2)
      else
        write(*, *) "Warning: Cannot add point as jmax would exceed 100"
      end if
    end if
  end subroutine ckmesh
  
  ! Refine the mesh
  subroutine refine()
    integer :: i, j ! Removed k, je, jst
    
    if (control%iref /= -1) then
      ! Prepare for mesh refinement
      call refine_prepare()
      return
    end if
    
    ! Cannot refine mesh, load initial mesh
    do i = idx%imin, idx%imax
      grid%x(i) = mesh%xin(i)
    end do
    
    do j = idx%jmin, idx%jmax
      grid%y(j) = mesh%yin(j)
    end do
    
    control%iref = 0
  end subroutine refine
  
  ! Prepare for mesh refinement
  subroutine refine_prepare()
    integer :: i, j ! Removed k, je, jst
    
    do i = idx%imin, idx%imax, 2
      ageom%xmid((i - idx%imin) / 2 + idx%imin) = mesh%xin(i)
    end do
    
    idx%imax = (idx%imax - idx%imin) / 2 + idx%imin
    call islit(ageom%xmid)
    
    do j = idx%jmin, idx%jlow - 1, 2
      ageom%ymid((j - idx%jmin) / 2 + idx%jmin) = mesh%yin(j)
    end do
    
    do j = idx%jup + 1, idx%jmax, 2
      ageom%ymid((j - (idx%jup + 1)) / 2 + (idx%jlow - idx%jmin) / 2 + idx%jmin) = mesh%yin(j)
    end do
    
    idx%jmax = (idx%jmax - idx%jmin) / 2 + idx%jmin
    call jslit(ageom%ymid)
  end subroutine refine_prepare
  
  subroutine generate_mesh()
    ! Generate computational mesh from input mesh
    ! This subroutine generates the actual X and Y mesh points from the
    ! input mesh data. It performs necessary scaling and adjustments.
    integer :: i, j
    real :: factor, xmin, ymin
    
    ! Apply refinement factor to mesh
    factor = 2.0**control%iref
    
    ! Reset limits and initialize arrays
    idx%imin = 1
    idx%imax = nint((mesh%xin(100) - mesh%xin(1)) / factor + 1.5) ! Use nint for conversion
    if (idx%imax > 100) idx%imax = 100
    
    idx%jmin = 1
    idx%jmax = nint((mesh%yin(100) - mesh%yin(1)) / factor + 1.5) ! Use nint for conversion
    if (idx%jmax > 100) idx%jmax = 100
    
    ! Generate X mesh
    xmin = mesh%xin(1)
    do i = 1, idx%imax
      grid%x(i) = xmin + (i - 1) * factor
      mesh%xdiff(i) = factor
    end do
    
    ! Generate Y mesh
    ymin = mesh%yin(1)
    do j = 1, idx%jmax
      grid%y(j) = ymin + (j - 1) * factor
      mesh%ydiff(j) = factor
    end do
    
    ! Find leading edge, trailing edge, and cut points
    call islit(grid%x)
    call jslit(grid%y)
    
    ! Initialize potential array
    do j = idx%jmin, idx%jmax
      do i = idx%imin, idx%imax
        grid%p(j, i) = 0.0
      end do
    end do
      ! Apply initialization parameters if available
    if (old%pstart == 1) then
      ! Use previous solution if available
      do j = old%jmino, old%jmaxo
        do i = old%imino, old%imaxo
          grid%p(j, i) = err%pold(j, i)
        end do
      end do
    end if
  end subroutine generate_mesh

end module grid_module
