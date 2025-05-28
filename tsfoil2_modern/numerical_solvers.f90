! Module for numerical solvers used in TSFOIL

module numerical_solvers
  implicit none

contains

  ! Trapezoidal rule integration
  subroutine trap(x, y, n, result)
    integer, intent(in) :: n
    real, intent(in) :: x(n), y(n)
    real, intent(out) :: result
    
    integer :: i
    real :: sum
    
    sum = 0.0
    do i = 1, n-1
      sum = sum + 0.5 * (y(i) + y(i+1)) * (x(i+1) - x(i))
    end do
    
    result = sum
  end subroutine trap
  
  ! Simpson's rule integration
  subroutine simp(result, x, y, n, ier)
    integer, intent(in) :: n
    real, intent(in) :: x(n), y(n)
    real, intent(out) :: result
    integer, intent(out) :: ier
    
    integer :: i, nm1
    real :: p, r, s1, s2, s3, s4
    
    result = 0.0
    ier = 0
    
    ! Check if we have at least 2 points
    if (n <= 1) then
      ier = 2
      return
    end if
    
    ! Check if first two points are identical
    if (x(1) == x(2)) then
      ier = 1
      return
    end if
    
    nm1 = n - 1
    
    ! Check if x is monotonically increasing or decreasing
    if (x(2) > x(1)) then
      ! Increasing
      do i = 2, nm1
        if (x(i+1) <= x(i)) then
          ier = 1
          return
        end if
      end do
    else
      ! Decreasing
      do i = 2, nm1
        if (x(i+1) >= x(i)) then
          ier = 1
          return
        end if
      end do
    end if
    
    ! Special case for n=3 (parabolic rule)
    if (n == 3) then
      s1 = x(2) - x(1)
      s2 = x(3) - x(2)
      s3 = x(3) - x(1)
      s4 = s1 + s2
      result = ((y(1)*s2*s2*(2.0*s1-s2) + y(2)*s3*s3*s4 &
                + y(3)*s1*s1*(s2-2.0*s1))/(s1*s2*s3*6.0)) * s3
      return
    end if
    
    ! Even number of intervals
    if (mod(n, 2) == 1) then
      ! Odd number of points (even intervals)
      result = y(1) + y(n)
      do i = 2, nm1, 2
        result = result + 4.0 * y(i)
      end do
      do i = 3, nm1-1, 2
        result = result + 2.0 * y(i)
      end do
      result = result * (x(n) - x(1)) / (3.0 * (n-1))
    else
      ! Even number of points (odd intervals)
      ! Use Simpson's for all but last interval, then trapezoidal for last
      p = (y(n-2)*(x(n)-x(n-1)) - y(n-1)*(x(n)-x(n-2)) + &
           y(n)*(x(n-1)-x(n-2))) / ((x(n-2)-x(n-1))*(x(n-2)-x(n))*(x(n-1)-x(n)))
      r = y(n-2) - p*(x(n-2)-x(n-1))*(x(n-2)-x(n))
      
      ! Simpson's rule for first n-2 points
      result = y(1) + y(n-2)
      do i = 2, n-3, 2
        result = result + 4.0 * y(i)
      end do
      do i = 3, n-4, 2
        result = result + 2.0 * y(i)
      end do
      result = result * (x(n-2) - x(1)) / (3.0 * (n-3))
      
      ! Add last interval using trapezoidal rule
      result = result + 0.5 * (y(n-2) + y(n)) * (x(n) - x(n-2))
    end if
  end subroutine simp
  
  ! Solve tridiagonal system
  subroutine tridiag_solve(n, a, b, c, d, x)
    integer, intent(in) :: n
    real, intent(in) :: a(n), b(n), c(n), d(n)
    real, intent(out) :: x(n)
    
    integer :: i
    real :: gamma
    real, allocatable :: cprime(:), dprime(:)
    
    allocate(cprime(n), dprime(n))
    
    ! Initialize the coefficients
    cprime(1) = c(1) / b(1)
    dprime(1) = d(1) / b(1)
    
    ! Forward elimination
    do i = 2, n
      gamma = b(i) - a(i) * cprime(i-1)
      dprime(i) = (d(i) - a(i) * dprime(i-1)) / gamma
      if (i < n) cprime(i) = c(i) / gamma
    end do
    
    ! Back substitution
    x(n) = dprime(n)
    do i = n-1, 1, -1
      x(i) = dprime(i) - cprime(i) * x(i+1)
    end do
    
    deallocate(cprime, dprime)
  end subroutine tridiag_solve

  ! Computes theta at every mesh point
  ! This is used for boundary conditions in wind tunnel simulations
  subroutine angle(grid_x, grid_y, imin, imax, jmin, jmax, rtk, pi, twopi, xsing, theta)
    real, intent(in) :: grid_x(:), grid_y(:)        ! Grid coordinates
    integer, intent(in) :: imin, imax, jmin, jmax   ! Grid indices
    real, intent(in) :: rtk, pi, twopi, xsing       ! Constants
    real, intent(out) :: theta(:,:)                 ! Output angle array
    
    integer :: i, j
    real :: xx, yy, r, atn, q, r2pi
    
    r2pi = 1.0 / twopi
      do i = imin, imax
      xx = grid_x(i) - xsing
      do j = jmin, jmax
        yy = grid_y(j) * rtk
        r = sqrt(grid_y(j)**2 + xx**2)
        atn = atan2(yy, xx)
        q = pi - sign(pi, yy)
        theta(j, i) = -(atn + q) * r2pi
        if (r <= 1.0) theta(j, i) = theta(j, i) * r
      end do
    end do
  end subroutine angle
  
  ! Compute constants for doublet solution in slotted wind tunnel
  subroutine droots(f, pi, rtkpor, alpha0, alpha1, alpha2, omega0, omega1, omega2) ! Removed h, halfpi, twopi
    real, intent(in) :: f        ! Slot parameter
    real, intent(in) :: pi       ! Constant
    real, intent(in) :: rtkpor   ! RTK/POR value
    
    real, intent(out) :: alpha0, alpha1, alpha2  ! Alpha parameters
    real, intent(out) :: omega0, omega1, omega2  ! Omega parameters
    
    real :: error, temp, q, dalpha
    integer :: i
    real :: twopi ! Declare twopi as a local variable

    twopi = 2.0 * pi ! Initialize twopi using the pi argument
    error = 1.0e-5
    
    ! Compute alpha0 - first root
    alpha0 = 0.0
    do i = 1, 100
      temp = alpha0
      q = f * temp - rtkpor
      alpha0 = pi/2.0 - atan(q)
      dalpha = abs(alpha0 - temp)
      if (dalpha < error) exit
      if (i == 100) then
        write(15, '(A,I1)') 'ABNORMAL STOP: Non-convergence of iteration for ALPHA', 0
        stop
      end if
    end do
    
    ! Compute alpha1 - second root
    alpha1 = 0.0
    do i = 1, 100
      temp = alpha1
      q = f * (temp - pi) - rtkpor
      alpha1 = pi/2.0 - atan(q)
      dalpha = abs(alpha1 - temp)
      if (dalpha < error) exit
      if (i == 100) then
        write(15, '(A,I1)') 'ABNORMAL STOP: Non-convergence of iteration for ALPHA', 1
        stop
      end if
    end do
    
    ! Compute alpha2 - third root
    alpha2 = 0.0
    do i = 1, 100
      temp = alpha2
      q = f * (temp - twopi) - rtkpor
      alpha2 = pi/2.0 - atan(q)
      dalpha = abs(alpha2 - temp)
      if (dalpha < error) exit
      if (i == 100) then
        write(15, '(A,I1)') 'ABNORMAL STOP: Non-convergence of iteration for ALPHA', 2
        stop
      end if
    end do
    
    ! Compute omega parameters
    temp = 1.0 / tan(alpha0)
    omega0 = 1.0 / (1.0 + f / (1.0 + temp*temp))
    
    temp = 1.0 / tan(alpha1)
    omega1 = 1.0 / (1.0 + f / (1.0 + temp*temp))
    
    temp = 1.0 / tan(alpha2)
    omega2 = 1.0 / (1.0 + f / (1.0 + temp*temp))
  end subroutine droots
  
  ! Compute constants for vortex solution in slotted wind tunnel
  subroutine vroots(f, pi, rtkpor, beta0, beta1, beta2, psi0, psi1, psi2) ! Removed h, halfpi, twopi
    real, intent(in) :: f        ! Slot parameter
    real, intent(in) :: pi       ! Constant
    real, intent(in) :: rtkpor   ! RTK/POR value
    
    real, intent(out) :: beta0, beta1, beta2  ! Beta parameters
    real, intent(out) :: psi0, psi1, psi2     ! Psi parameters
    
    real :: error, temp, q, dbeta
    integer :: i
    
    error = 1.0e-5
    
    ! Calculate beta0 - first root
    beta0 = 0.0
    do i = 1, 100
      temp = beta0
      q = -f * temp + rtkpor
      beta0 = atan(q)
      dbeta = abs(temp - beta0)
      if (dbeta < error) exit
      if (i == 100) then
        write(15, '(A,I1)') 'ABNORMAL STOP: Non-convergence of iteration for BETA', 0
        stop
      end if
    end do
    
    ! Calculate beta1 - second root
    beta1 = 0.0
    do i = 1, 100
      temp = beta1
      q = -f * (temp + pi) + rtkpor
      beta1 = atan(q)
      dbeta = abs(beta1 - temp)
      if (dbeta < error) exit
      if (i == 100) then
        write(15, '(A,I1)') 'ABNORMAL STOP: Non-convergence of iteration for BETA', 1
        stop
      end if
    end do
    
    ! Calculate beta2 - third root
    beta2 = 0.0
    do i = 1, 100
      temp = beta2
      q = -f * (temp - pi) + rtkpor
      beta2 = atan(q)
      dbeta = abs(beta2 - temp)
      if (dbeta < error) exit
      if (i == 100) then
        write(15, '(A,I1)') 'ABNORMAL STOP: Non-convergence of iteration for BETA', 2
        stop
      end if
    end do
    
    ! Compute psi parameters
    temp = tan(beta0)
    psi0 = 1.0 / (1.0 + f / (1.0 + temp*temp))
    
    temp = tan(beta1)
    psi1 = 1.0 / (1.0 + f / (1.0 + temp*temp))
    
    temp = tan(beta2)
    psi2 = 1.0 / (1.0 + f / (1.0 + temp*temp))
  end subroutine vroots
  
end module numerical_solvers
