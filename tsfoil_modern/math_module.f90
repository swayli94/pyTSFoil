! math_module.f90
! Module for general mathematical utilities

module math_module
  implicit none
  public :: ARF, SIMP

contains

  ! Rational approximation for the error function erf(X)
  ! Error < 1.5E-7 by rational approximation 7.1.26 from 
  ! Handbook of Math. Functions, U.S. Dept. of Commerce, NBS Appl Math Ser 55
  function ARF(X) result(Y)
    implicit none
    real, intent(in) :: X
    real :: Y
    real :: T, POLY, Y_ABS
    real, parameter :: C(5) = [1.061405429, -1.453152027, 1.421413741, &
                              -0.284496736, 0.254829592]
    integer :: I

    Y_ABS = abs(X)
    if (Y_ABS >= 10.0) then
      Y = 1.0
    else
      T = 1.0 / (1.0 + 0.3275911 * Y_ABS)
      POLY = 0.0
      do I = 1, 5
        POLY = (POLY + C(I)) * T
      end do
      Y = 1.0 - POLY * exp(-Y_ABS * Y_ABS)
    end if
    
    if (X < 0.0) Y = -Y
  end function ARF

  ! Simpson's rule integration over N points
  ! Integrates Y(X) from X(1) to X(N) using Simpson's rule
  subroutine SIMP(R, X, Y, N, IER)
    implicit none
    integer, intent(in) :: N
    real, intent(in) :: X(N), Y(N)
    real, intent(out) :: R
    integer, intent(out) :: IER
    integer :: I, NM1, NM2, NO2, NP1, NS2
    real :: H, S1, S2

    R = 0.0
    IER = 0
    
    ! Check for valid input
    if (N <= 1) then
      IER = 2
      return
    end if
    
    ! Check for duplicate X values
    if (X(1) == X(2)) then
      IER = 1
      return
    end if
    
    NM1 = N - 1
    
    ! Handle case with only 2 points (trapezoidal rule)
    if (N == 2) then
      R = 0.5 * (Y(1) + Y(2)) * abs(X(2) - X(1))
      return
    end if
    
    ! Check if X is monotonically increasing or decreasing
    if (X(1) < X(2)) then
      ! Test for monotonically increasing
      do I = 2, NM1
        if (X(I+1) <= X(I)) then
          IER = 1
          return
        end if
      end do
    else
      ! Test for monotonically decreasing
      do I = 2, NM1
        if (X(I+1) >= X(I)) then
          IER = 1
          return
        end if
      end do
    end if
    
    NM2 = N - 2
    
    ! Check if N is odd (even number of intervals)
    NO2 = N / 2
    if (2 * NO2 == N) then
      ! N is even - use Simpson's rule for all intervals
      NP1 = NO2 + 1
      NS2 = NO2 - 1
      H = abs(X(N) - X(1)) / NM1
      
      S1 = Y(1) + Y(N)
      do I = 2, NM1, 2
        S1 = S1 + 4.0 * Y(I)
      end do
      do I = 3, NM2, 2
        S1 = S1 + 2.0 * Y(I)
      end do
      R = S1 * H / 3.0
    else
      ! N is odd - use Simpson's for first N-1 points, then add last interval
      H = abs(X(NM1) - X(1)) / NM2
      
      S1 = Y(1) + Y(NM1)
      do I = 2, NM2, 2
        S1 = S1 + 4.0 * Y(I)
      end do
      do I = 3, NM2-1, 2
        S1 = S1 + 2.0 * Y(I)
      end do
      
      ! Add last interval using trapezoidal rule
      S2 = 0.5 * (Y(NM1) + Y(N)) * abs(X(N) - X(NM1))
      R = S1 * H / 3.0 + S2
    end if
    
  end subroutine SIMP

end module math_module
