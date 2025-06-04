! spline_module.f90
! Module for cubic spline interpolation routines

module spline_module
  implicit none
  private
  public :: SPLN1, SPLN1X, initialize_spline, cleanup_spline, set_boundary_conditions

  ! Spline coefficients and work arrays
  real, allocatable :: A(:), B(:)
  real :: DY1, DY2  ! Derivatives at endpoints
  integer :: K1, K2 ! Boundary condition types
  integer :: max_n  ! Maximum number of points

contains

  ! Initialize spline coefficients and allocate arrays
  subroutine initialize_spline(max_points)
    implicit none
    integer, intent(in) :: max_points
    max_n = max_points
    if (allocated(A)) deallocate(A)
    if (allocated(B)) deallocate(B)
    allocate(A(2*max_points), B(2*max_points))
    A = 0.0
    B = 0.0
  end subroutine initialize_spline

  ! Cleanup routine to deallocate spline arrays
  subroutine cleanup_spline()
    implicit none
    if (allocated(A)) deallocate(A)
    if (allocated(B)) deallocate(B)
  end subroutine cleanup_spline

  ! Set boundary conditions for spline interpolation
  subroutine set_boundary_conditions(K1_in, K2_in, DY1_in, DY2_in)
    implicit none
    integer, intent(in) :: K1_in, K2_in
    real, intent(in) :: DY1_in, DY2_in
    
    K1 = K1_in
    K2 = K2_in
    DY1 = DY1_in
    DY2 = DY2_in
  end subroutine set_boundary_conditions

  ! Set up cubic spline coefficients
  subroutine SPLN1(X, Y, N)
    implicit none
    integer, intent(in) :: N
    real, intent(in) :: X(N), Y(N)
    integer :: I, J, N1
    real :: C1, C2, C3, C4, C5, C6, C7

    ! Ensure arrays are allocated
    if (.not. allocated(A) .or. .not. allocated(B)) then
      call initialize_spline(max(N, 100))
    end if

    N1 = N - 2
    C1 = X(2) - X(1)

    ! Set up boundary condition at lower end
    if (K1 /= 2) then
      B(1) = 0.0
      A(1) = (DY1 - (Y(2) - Y(1))/C1) / C1
    else
      B(1) = -C1
      A(1) = -DY1 / 2.0
    end if

    J = 1

    if (N == 2) then
      C3 = real(K1)
      C2 = 1.0 / C3
      if (K2 /= 2) then
        A(J+1) = ((Y(2) - Y(1))/C1 - A(J)*C1 - DY2) / (C1*C1) * C2
      else
        A(J+1) = C3 * ((DY2 + 2.0*A(1)) / (4.0*C1))
      end if
    else
      ! Process interior points
      do I = 1, N1
        J = J + 1
        C1 = X(I+1) - X(I)
        C2 = X(I+2) - X(I+1)
        C3 = Y(I+1) - Y(I)
        C4 = Y(I+2) - Y(I+1)
        C5 = C3/C1 - C4/C2
        C6 = C1/C2
        C7 = C1*C2
        
        B(J) = 1.0 / (C6*(C1 - B(J-1)))
        A(J) = (C5/C2 - C6*A(J-1)) * B(J)
        
        J = J + 1
        B(J) = 1.0 / ((-C1 - C2)/C7 - C6*B(J-1))
        A(J) = (-C5/C7 - C6*A(J-1)) * B(J)
      end do
      
      ! Set up boundary condition at upper end
      if (K2 /= 2) then
        A(J+1) = (DY2 - C4/C2 + C2*A(J)) / (C2*(B(J) - C2))
      else
        A(J+1) = (DY2/2.0 + A(J)) / (-2.0*C2 + B(J))
      end if
    end if

    ! Back substitution
    J = 2*(N - 1)
    do while (J > 0)
      A(J) = A(J) - B(J)*A(J+1)
      J = J - 1
    end do

  end subroutine SPLN1

  ! Evaluate spline at point XP
  subroutine SPLN1X(X, Y, N, XP, YP, DYP)
    implicit none
    integer, intent(in) :: N
    real, intent(in) :: X(N), Y(N), XP
    real, intent(out) :: YP, DYP
    integer :: I, K
    real :: C, D, SLOPE

    ! Check for extrapolation below lower end
    if (XP <= X(1)) then
      C = X(2) - X(1)
      DYP = (Y(2) - Y(1))/C + A(1)*C
      YP = Y(1) + DYP*(XP - X(1))
      return
    end if

    ! Check for extrapolation above upper end
    if (XP >= X(N)) then
      C = X(N) - X(N-1)
      DYP = (Y(N) - Y(N-1))/C - A(2*N-3)*C - A(2*N-2)*C*C
      YP = Y(N) + DYP*(XP - X(N))
      return
    end if

    ! Find interval containing XP
    I = 1
    do while (I < N)
      if (X(I+1) > XP) exit
      I = I + 1
    end do

    ! Interpolate within interval
    C = XP - X(I)
    D = X(I+1) - XP
    K = 2*I - 1

    SLOPE = (Y(I+1) - Y(I)) / (X(I+1) - X(I))
    YP = Y(I) + (SLOPE + (A(K) + A(K+1)*C)*D) * C
    DYP = SLOPE + A(K)*(D - C) + A(K+1)*(2.0*D - C)*C

  end subroutine SPLN1X

end module spline_module
