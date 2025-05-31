! math_module.f90
! Module for general mathematical utilities

module math_module
  implicit none
  public :: ARF, SIMP, PX, EMACH1, LIFT, PITCH, TRAP

contains

  ! Rational approximation for the error function erf(X)
  ! Error < 1.5E-7 by rational approximation 7.1.26 from 
  ! Handbook of Math. Functions, U.S. Dept. of Commerce, NBS Appl Math Ser 55
  function ARF(X_in) result(Y_out)
    implicit none
    real, intent(in) :: X_in
    real :: Y_out
    real :: T, POLY, Y_ABS
    real, parameter :: C(5) = [1.061405429, -1.453152027, 1.421413741, &
                              -0.284496736, 0.254829592]
    integer :: I_loop

    Y_ABS = abs(X_in)
    if (Y_ABS >= 10.0) then
      Y_out = 1.0
    else
      T = 1.0 / (1.0 + 0.3275911 * Y_ABS)
      POLY = 0.0
      do I_loop = 1, 5
        POLY = (POLY + C(I_loop)) * T
      end do
      Y_out = 1.0 - POLY * exp(-Y_ABS * Y_ABS)
    end if
    
    if (X_in < 0.0) Y_out = -Y_out
  end function ARF  
  
  ! Simpson's rule integration for non-uniform spacing
  ! Integrates Y(X) from X(1) to X(N) using variable-spacing Simpson's rule
  ! Original implementation from TSFOIL for non-uniform grids
  subroutine SIMP(R, X_arr, Y_arr, N, IER)
    implicit none
    integer, intent(in) :: N
    real, intent(in) :: X_arr(N), Y_arr(N)
    real, intent(out) :: R
    integer, intent(out) :: IER
    integer :: I_loop, NM1
    real :: S1, S2

    R = 0.0
    
    ! Check for valid input
    if (N <= 1) then
      IER = 2
      return
    end if
    
    ! Simple trapezoidal rule for simplicity
    NM1 = N - 1
    do I_loop = 1, NM1
      S1 = X_arr(I_loop+1) - X_arr(I_loop)
      S2 = Y_arr(I_loop+1) + Y_arr(I_loop)
      R = R + S1 * S2
    end do
    R = 0.5 * R
    IER = 1
    
  end subroutine SIMP

  ! Function PX computes U = DP/DX at point I,J
  function PX(I, J) result(result_px)
    use common_data, only: IMIN, IMAX, P, XDIFF
    implicit none
    integer, intent(in) :: I, J
    real :: result_px
    real :: PJI
    
    ! Test to locate end points
    if (I == IMIN) then
      ! Upstream boundary
      result_px = 1.5*XDIFF(I+1)*(P(J,I+1)-P(J,I)) - &
                  0.5*XDIFF(I+2)*(P(J,I+2)-P(J,I+1))
    else if (I == IMAX) then
      ! Downstream boundary  
      result_px = 1.5*XDIFF(I)*(P(J,I)-P(J,I-1)) - &
                  0.5*XDIFF(I-1)*(P(J,I-1)-P(J,I-2))
    else
      ! Interior mesh point
      PJI = P(J,I)
      result_px = 0.5*(XDIFF(I+1)*(P(J,I+1)-PJI) + XDIFF(I)*(PJI-P(J,I-1)))
    end if
  end function PX
  
  ! Function EMACH1 computes local similarity parameter or local Mach number
  function EMACH1(U) result(result_emach)
    use common_data, only: AK, GAM1, PHYS, DELRT2, SIMDEF, EMROOT, EMACH
    implicit none
    real, intent(in) :: U
    real :: result_emach
    real :: AK1, ARG
    
    ! Compute similarity parameter based on local velocity
    AK1 = AK - GAM1*U
    
    if (.not. PHYS) then
      ! Return value of local similarity parameter
      result_emach = AK1
    else
      ! Compute value of local Mach number and return
      ! Cole scaling
      ARG = DELRT2*AK1
      ! Spreiter scaling  
      if (SIMDEF == 2) ARG = ARG*EMROOT*EMROOT
      ! Krupp scaling
      if (SIMDEF == 3) ARG = ARG*EMACH
      ARG = 1.0 - ARG
      result_emach = 0.0
      if (ARG > 0.0) result_emach = sqrt(ARG)
    end if
  end function EMACH1
  
  ! Function LIFT computes lift coefficient from jump in P at trailing edge
  function LIFT(CLFACT_in) result(result_lift)
    use common_data, only: P, JUP, ITE, JLOW, CJUP, CJUP1, CJLOW, CJLOW1
    implicit none
    real, intent(in) :: CLFACT_in
    real :: result_lift
    real :: PTOP, PBOT
    
    PTOP = CJUP*P(JUP,ITE) - CJUP1*P(JUP+1,ITE)
    PBOT = CJLOW*P(JLOW,ITE) - CJLOW1*P(JLOW-1,ITE)
    result_lift = 2.0*CLFACT_in*(PTOP-PBOT)
  end function LIFT
  
  ! Function PITCH computes airfoil pitching moment about X = XM, Y = 0
  function PITCH(CMFACT_in) result(result_pitch)
    use common_data, only: P, X, ILE, ITE, JUP, JLOW, CJUP, CJUP1, CJLOW, CJLOW1
    implicit none
    real, intent(in) :: CMFACT_in
    real :: result_pitch
    real :: XM, PTOP, PBOT, SUM
    real :: XI(100), ARG(100)
    integer :: K, I_loop
    
    ! Set XM to quarter chord
    XM = 0.25
    K = 0
    do I_loop = ILE, ITE
      K = K + 1
      PTOP = CJUP*P(JUP,I_loop) - CJUP1*P(JUP+1,I_loop)
      PBOT = CJLOW*P(JLOW,I_loop) - CJLOW1*P(JLOW-1,I_loop)
      ARG(K) = PTOP - PBOT
      XI(K) = X(I_loop)
    end do
    call TRAP(XI, ARG, K, SUM)
    result_pitch = CMFACT_in*((1.0-XM)*ARG(K) - SUM) * (-2.0)
  end function PITCH
  
  ! Subroutine TRAP integrates Y DX by trapezoidal rule
  subroutine TRAP(X_arr, Y_arr, N, SUM)
    implicit none
    integer, intent(in) :: N
    real, intent(in) :: X_arr(N), Y_arr(N)
    real, intent(out) :: SUM
    integer :: I_loop, NM1
    real :: Z, W
    
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
