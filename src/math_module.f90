! math_module.f90
! Module for general mathematical utilities

module math_module
  implicit none

contains

    ! Simpson's rule integration for non-uniform spacing
    ! Integrates Y(X) from X(1) to X(N) using variable-spacing Simpson's rule
    subroutine SIMP(R, X_arr, Y_arr, N, IER)
        implicit none
        integer, intent(in) :: N
        real, intent(in) :: X_arr(N), Y_arr(N)
        real, intent(out) :: R
        integer, intent(out) :: IER
        integer :: I=0, NM1=0, NM2=0, N1=0
        real :: S1=0.0, S2=0.0, S3=0.0, S4=0.0, P=0.0

        R = 0.0
        
        ! Check for valid input
        if (N <= 1) then
            IER = 2
            return
        end if
        
        ! Check for identical first two points
        if (X_arr(1) == X_arr(2)) then
            IER = 4
            return
        end if
        
        NM1 = N - 1
        
        ! Handle N=2 case with trapezoidal rule
        if (N == 2) then
            R = (X_arr(2) - X_arr(1)) * (Y_arr(1) + Y_arr(2)) / 2.0
            IER = 1
            return
        end if
        
        ! Test for monotonically increasing or decreasing X
        if (X_arr(1) < X_arr(2)) then
            ! Test for monotonically increasing
            do I = 2, NM1
                if (X_arr(I+1) <= X_arr(I)) then
                IER = 4
                return
                end if
            end do

        else
            ! Test for monotonically decreasing
            do I = 2, NM1
                if (X_arr(I+1) >= X_arr(I)) then
                IER = 4
                return
                end if
            end do

        end if
        
        NM2 = N - 2
        P = 0.0
        
        ! Handle even N case - fit polynomial through first 3 points
        if (mod(N, 2) == 0) then
            S1 = X_arr(2) - X_arr(1)
            S2 = X_arr(3) - X_arr(1)
            S3 = Y_arr(2) - Y_arr(1)
            S4 = Y_arr(3) - Y_arr(1)
            P = S1/6.0 * (2.0*S3 + 6.0*Y_arr(1) + (S2*S2*S3 - S1*S1*S4)/(S2*(S2-S1)))
            N1 = 2
        else
            N1 = 1
        end if
        
        ! Apply Simpson's rule with non-uniform spacing
        S1 = X_arr(N1+1) - X_arr(N1)
        S2 = X_arr(N1+2) - X_arr(N1+1)
        S3 = X_arr(NM1) - X_arr(NM2)
        S4 = X_arr(N) - X_arr(NM1)
        
        R = (2.0*S1*S1 + S1*S2 - S2*S2)/S1 * Y_arr(N1) + &
            (2.0*S4*S4 + S3*S4 - S3*S3)/S4 * Y_arr(N)
        
        N1 = N1 + 1
        
        ! Middle points with odd indices
        do I = N1, NM1, 2
            S1 = X_arr(I) - X_arr(I-1)
            S2 = X_arr(I+1) - X_arr(I)
            R = R + (S1 + S2)**3 / (S1 * S2) * Y_arr(I)
        end do
        
        ! Handle points with even indices if N >= 5
        if (N >= 5) then
            N1 = N1 + 1
            do I = N1, NM2, 2
                S1 = X_arr(I-1) - X_arr(I-2)
                S2 = X_arr(I) - X_arr(I-1)
                S3 = X_arr(I+1) - X_arr(I)
                S4 = X_arr(I+2) - X_arr(I+1)
                R = R + ((2.0*S2*S2 + S1*S2 - S1*S1)/S2 + &
                        (2.0*S3*S3 + S3*S4 - S4*S4)/S3) * Y_arr(I)
            end do
        end if
        
        R = R/6.0 + P
        IER = 1
        
    end subroutine SIMP

    ! Integrates Y DX by trapezoidal rule
    subroutine TRAP(X_arr, Y_arr, N, SUM)
        implicit none
        integer, intent(in) :: N
        real, intent(in) :: X_arr(N), Y_arr(N)
        real, intent(out) :: SUM
        integer :: I_loop=0, NM1=0
        real :: Z=0.0, W=0.0
        
        SUM = 0.0
        NM1 = N - 1
        do I_loop = 1, NM1
            Z = X_arr(I_loop+1) - X_arr(I_loop)
            W = Y_arr(I_loop+1) + Y_arr(I_loop)
            SUM = SUM + Z*W
        end do
        SUM = 0.5*SUM

    end subroutine TRAP

end module math_module
