! Module for spline interpolation

module spline_module
  implicit none
  
  ! Common variables for spline interpolation
  real, allocatable :: a(:), b(:)  ! Spline coefficients
  real :: dy1, dy2                ! Derivatives at endpoints
  integer :: k1, k2              ! Type of boundary conditions
  real :: xp, yp, dyp            ! Interpolation inputs and results
  
  ! Global variables for spline interpolation
  real :: xp_global, yp_global, dyp_global

contains  

  ! Initialize spline module
  subroutine initialize_spline(max_points)
    integer, intent(in) :: max_points
    
    ! Allocate arrays for spline coefficients
    if (allocated(a)) deallocate(a)
    if (allocated(b)) deallocate(b)
    allocate(a(2*max_points), b(2*max_points))
    
    ! Initialize to zero
    a = 0.0
    b = 0.0
  end subroutine initialize_spline
    ! Cleanup spline module
  subroutine cleanup_spline()
    if (allocated(a)) deallocate(a)
    if (allocated(b)) deallocate(b)
  end subroutine cleanup_spline
  
  ! Subroutine for spline setup  
  subroutine spln1(x, y, n)
    implicit none
    integer, intent(in) :: n
    real, intent(in) :: x(n), y(n)

    ! Local variables
    integer :: i, j, n1
    real :: c1, c2, c3, c4, c5, c6, c7

    ! Calculate coefficients for cubic spline interpolation
    n1 = n - 2
    c1 = x(2) - x(1)

    if (k1 /= 2) then
        b(1) = 0.0
        a(1) = (dy1 - (y(2) - y(1)) / c1) / c1
    else
        b(1) = -c1
        a(1) = -dy1 / 2.0
    end if

    j = 1

    if (n == 2) then
        c3 = real(k1)
        c2 = 1.0 / c3
        
        if (k2 /= 2) then
        a(j+1) = ((y(2) - y(1)) / c1 - a(j) * c1 - dy2) / (c1 * c1) * c2
        else
        a(j+1) = c3 * ((dy2 + 2.0 * a(1)) / (4.0 * c1))
        end if
    else
        ! Process interior points
        do i = 1, n1
        j = j + 1
        c1 = x(i+1) - x(i)
        c2 = x(i+2) - x(i+1)
        c3 = y(i+1) - y(i)
        c4 = y(i+2) - y(i+1)
        c5 = c3 / c1 - c4 / c2
        c6 = c1 / c2
        c7 = c1 * c2
        
        b(j) = 1.0 / (c6 * (c1 - b(j-1)))
        a(j) = (c5 / c2 - c6 * a(j-1)) * b(j)
        
        j = j + 1
        b(j) = 1.0 / ((-c1 - c2) / c7 - c6 * b(j-1))
        a(j) = (-c5 / c7 - c6 * a(j-1)) * b(j)
        end do
        
        ! Set up boundary condition at upper end
        if (k2 /= 2) then
        a(j+1) = (dy2 - c4 / c2 + c2 * a(j)) / (c2 * (b(j) - c2))
        else
        a(j+1) = (dy2 / 2.0 + a(j)) / (-2.0 * c2 + b(j))
        end if
    end if

    ! Back substitution
    j = 2 * (n - 1)
    do while (j > 0)
        a(j) = a(j) - b(j) * a(j+1)
        j = j - 1
    end do

  end subroutine spln1

    ! Subroutine for spline interpolation
  subroutine spln1x(x, y, n)
    implicit none
    integer, intent(in) :: n
    real, intent(in) :: x(n), y(n)

    ! Local variables
    integer :: i, k
    real :: c, d, slope

    ! Check if we need to extrapolate below the lower end
    if (xp <= x(1)) then
        c = x(2) - x(1)
        dyp = (y(2) - y(1)) / c + a(1) * c
        yp = y(1) + dyp * (xp - x(1))
        return
    end if

    ! Check if we need to extrapolate above the upper end
    if (xp >= x(n)) then
        c = x(n) - x(n-1)
        dyp = (y(n) - y(n-1)) / c - a(2*n-3) * c - a(2*n-2) * c * c
        yp = y(n) + dyp * (xp - x(n))
        return
    end if

    ! Find the interval containing xp
    i = 1
    do while (i < n)
        if (x(i+1) > xp) exit
        i = i + 1
    end do

    ! Now xp is bracketed so that x(i) <= xp < x(i+1)
    c = xp - x(i)
    d = x(i+1) - xp
    k = 2 * i - 1

    slope = (y(i+1) - y(i)) / (x(i+1) - x(i))
    yp = y(i) + (slope + (a(k) + a(k+1) * c) * d) * c
    dyp = slope + a(k) * (d - c) + a(k+1) * (2.0 * d - c) * c

  end subroutine spln1x

end module spline_module
