! math_module.f90
! Module for general mathematical utilities

module math_module
  implicit none
  public :: ARF, SIMP, PX, PY, EMACH1, LIFT, PITCH, TRAP
  public :: VWEDGE, WANGLE, FINDSK, DROOTS, VROOTS, NEWISK, MACHMP

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
  
  ! Function PY computes V = DP/DY at point I,J
  function PY(I, J) result(result_py)
    use common_data, only: IMIN, IMAX, JMIN, JMAX, JUP, JLOW, ILE, ITE, P, Y, &
                          YDIFF, ALPHA, FXL, FXU, PJUMP
    implicit none
    integer, intent(in) :: I, J
    real :: result_py
    real :: PJI, VMINUS, VPLUS
    integer :: IC
    
    ! Test for end points or points near airfoil slit
    if (J == JMIN) then
      ! I,J is on lower boundary. Use one sided derivative
      result_py = 1.5*YDIFF(J+1)*(P(J+1,I) - P(J,I)) - &
                  0.5*YDIFF(J+2)*(P(J+2,I) - P(J+1,I))
      return
    else if (J == JLOW) then
      ! I,J is on row of mesh points below airfoil
      VMINUS = YDIFF(J)*(P(J,I) - P(J-1,I))
      
      ! Test to see if I,J is ahead, under, or behind slit
      if (I < ILE) then
        ! I,J is ahead of airfoil
        result_py = 0.5*((P(JUP,I) - P(JLOW,I)) * YDIFF(JUP) + VMINUS)
      else if (I > ITE) then
        ! I,J is behind airfoil
        result_py = 0.5*((P(JUP,I) - PJUMP(I) - P(JLOW,I)) * YDIFF(JUP) + VMINUS)
      else
        ! I,J is under airfoil. Use derivative boundary condition
        IC = I - ILE + 1
        result_py = 0.5 * (FXL(IC) - ALPHA + VMINUS)
      end if
      return
    else if (J == JUP) then
      ! I,J is on row of mesh points above airfoil
      VPLUS = YDIFF(J+1)*(P(J+1,I) - P(J,I))
      
      ! Test to see if I is ahead of, over, or behind airfoil slit
      if (I < ILE) then
        ! I,J is ahead of airfoil
        result_py = 0.5*((P(JUP,I) - P(JLOW,I)) * YDIFF(JUP) + VPLUS)
      else if (I > ITE) then
        ! I,J is behind airfoil
        result_py = 0.5*((P(JUP,I) - PJUMP(I) - P(JLOW,I)) * YDIFF(JUP) + VPLUS)
      else
        IC = I - ILE + 1
        result_py = 0.5 * (VPLUS + FXU(IC) - ALPHA)
      end if
      return
    else if (J == JMAX) then
      ! I,J is on top row of mesh points. Use one sided formula
      result_py = 1.5*YDIFF(J)*(P(J,I) - P(J-1,I)) - &
                  0.5*YDIFF(J-1)*(P(J-1,I) - P(J-2,I))
      return
    else
      ! I,J is an interior point
      PJI = P(J,I)
      result_py = 0.5*(YDIFF(J+1)*(P(J+1,I)-PJI) + YDIFF(J)*(PJI-P(J-1,I)))
    end if
  end function PY
  
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
  
  ! Function LIFT computes pressure drag coefficient by integrating
  ! U*V around airfoil using trapezoidal rule.
  function DRAG(CDFACT_in) result(result_drag)
    use common_data, only: P, X, ILE, ITE, JUP, JLOW, CJUP, CJUP1, CJLOW, CJLOW1
    use common_data, only: FXU, FXL, XI, ARG
    implicit none
    real, intent(in) :: CDFACT_in
    real :: result_drag
    real :: PXUP, PXLOW, SUM
    integer :: K, I
    
    K = 1
    ARG(1) = 0.0
    XI(1) = X(ILE-1)
    do I = ILE, ITE
        K = K + 1
        PXUP = CJUP*PX(I,JUP) - CJUP1*PX(I,JUP+1)
        PXLOW = CJLOW*PX(I,JLOW) - CJLOW1*PX(I,JLOW-1)
        ARG(K) = FXU(K-1)*PXUP - FXL(K-1)*PXLOW
        XI(K) = X(I)
    end do
    K = K + 1
    ARG(K) = 0.0
    XI(K) = X(ITE+1)
    call TRAP(XI, ARG, K, SUM)
    result_drag = -SUM*CDFACT_in*2.0
  end function DRAG

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
  
  ! Helper subroutine for convergence error reporting
  subroutine report_convergence_error(subroutine_name, variable_name, iteration_number)
    use common_data, only: UNIT_OUTPUT
    implicit none
    character(len=*), intent(in) :: subroutine_name, variable_name
    integer, intent(in) :: iteration_number
    
    write(*,'(A,A)') 'ABNORMAL STOP IN SUBROUTINE ', subroutine_name
    write(*,'(A,A,I0)') 'NONCONVERGENCE OF ITERATION FOR ', variable_name, iteration_number
    write(UNIT_OUTPUT,'(A,A)') 'ABNORMAL STOP IN SUBROUTINE ', subroutine_name  
    write(UNIT_OUTPUT,'(A,A,I0)') 'NONCONVERGENCE OF ITERATION FOR ', variable_name, iteration_number
    stop
  end subroutine report_convergence_error

  ! Compute constants ALPHA0, ALPHA1, ALPHA2, OMEGA0, OMEGA1, OMEGA2
  ! Used in formula for doublet in slotted wind tunnel with subsonic freestream
  subroutine DROOTS
    use common_data, only: F, H, HALFPI, PI, RTKPOR, TWOPI
    use common_data, only: ALPHA0, ALPHA1, ALPHA2, XSING, OMEGA0, OMEGA1, OMEGA2, JET
    implicit none
    real :: ERROR, TEMP, Q, DALPHA
    integer :: I
    logical :: converged
    
    ERROR = 0.00001
    
    ! Compute ALPHA0
    ALPHA0 = 0.0
    converged = .false.
    do I = 1, 100
      TEMP = ALPHA0
      Q = F*TEMP - RTKPOR
      ALPHA0 = HALFPI - atan(Q)
      DALPHA = abs(ALPHA0 - TEMP)
      if (DALPHA < ERROR) then
        converged = .true.
        exit
      end if
    end do
    if (.not. converged) then
      call report_convergence_error('DROOTS', 'ALPHA', 0)
    end if
    
    ! Compute ALPHA1
    ALPHA1 = 0.0
    converged = .false.
    do I = 1, 100
      TEMP = ALPHA1
      Q = F*(TEMP - PI) - RTKPOR
      ALPHA1 = HALFPI - atan(Q)
      DALPHA = abs(ALPHA1 - TEMP)
      if (DALPHA < ERROR) then
        converged = .true.
        exit
      end if
    end do
    if (.not. converged) then
      call report_convergence_error('DROOTS', 'ALPHA', 1)
    end if
    
    ! Compute ALPHA2
    ALPHA2 = 0.0
    converged = .false.
    do I = 1, 100
      TEMP = ALPHA2
      Q = F*(TEMP - TWOPI) - RTKPOR
      ALPHA2 = HALFPI - atan(Q)
      DALPHA = abs(ALPHA2 - TEMP)
      if (DALPHA < ERROR) then
        converged = .true.
        exit
      end if
    end do
    if (.not. converged) then
      call report_convergence_error('DROOTS', 'ALPHA', 2)
    end if
    
    ! Compute OMEGA0, OMEGA1, OMEGA2
    TEMP = 1.0 / tan(ALPHA0)
    OMEGA0 = 1.0 / (1.0 + F/(1.0 + TEMP*TEMP))
    TEMP = 1.0 / tan(ALPHA1)
    OMEGA1 = 1.0 / (1.0 + F/(1.0 + TEMP*TEMP))
    TEMP = 1.0 / tan(ALPHA2)
    OMEGA2 = 1.0 / (1.0 + F/(1.0 + TEMP*TEMP))
    
  end subroutine DROOTS

  ! Compute constants BETA0, BETA1, BETA2, PSI0, PSI1, PSI2
  ! Used in formula for vortex in slotted wind tunnel with subsonic freestream
  subroutine VROOTS
    use common_data, only: F, H, HALFPI, PI, RTKPOR, TWOPI
    use common_data, only: B, BETA0, BETA1, BETA2, PSI0, PSI1, PSI2
    implicit none
    real :: ERROR, TEMP, Q, DBETA
    integer :: I
    logical :: converged
    
    ERROR = 0.00001
    
    ! Calculate BETA0
    BETA0 = 0.0
    converged = .false.
    do I = 1, 100
      TEMP = BETA0
      Q = -F*TEMP + RTKPOR
      BETA0 = atan(Q)
      DBETA = abs(TEMP - BETA0)
      if (DBETA < ERROR) then
        converged = .true.
        exit
      end if
    end do
    if (.not. converged) then
      call report_convergence_error('VROOTS', 'BETA', 0)
    end if
    
    ! Calculate BETA1  
    BETA1 = 0.0
    converged = .false.
    do I = 1, 100
      TEMP = BETA1
      Q = -F*(TEMP + PI) + RTKPOR
      BETA1 = atan(Q)
      DBETA = abs(BETA1 - TEMP)
      if (DBETA < ERROR) then
        converged = .true.
        exit
      end if
    end do
    if (.not. converged) then
      call report_convergence_error('VROOTS', 'BETA', 1)
    end if
    
    ! Calculate BETA2
    BETA2 = 0.0
    converged = .false.
    do I = 1, 100
      TEMP = BETA2
      Q = -F*(TEMP - PI) + RTKPOR
      BETA2 = atan(Q)
      DBETA = abs(BETA2 - TEMP)
      if (DBETA < ERROR) then
        converged = .true.
        exit
      end if
    end do
    if (.not. converged) then
      call report_convergence_error('VROOTS', 'BETA', 2)
    end if
    
    ! Compute PSI0, PSI1, PSI2
    TEMP = tan(BETA0)
    PSI0 = 1.0 / (1.0 + F/(1.0 + TEMP*TEMP))
    TEMP = tan(BETA1)
    PSI1 = 1.0 / (1.0 + F/(1.0 + TEMP*TEMP))
    TEMP = tan(BETA2)
    PSI2 = 1.0 / (1.0 + F/(1.0 + TEMP*TEMP))
    
  end subroutine VROOTS
  
  ! Computes Murman or Yoshihara viscous wedge and modifies slope conditions
  ! to account for jump in displacement thickness due to shock/boundary layer interaction
  subroutine VWEDGE
    use common_data, only: P, X, Y, IMIN, IMAX, IUP, IDOWN, ILE, ITE
    use common_data, only: JMIN, JMAX, JUP, JLOW, JTOP, JBOT, J1, J2
    use common_data, only: AK, ALPHA, DUB, GAM1, RTK, XDIFF, YDIFF
    use common_data, only: CL, DELTA, DELRT2, EMACH, EMROOT, PHYS, PRTFLO, SIMDEF
    use common_data, only: SONVEL, VFACT, YFACT
    use common_data, only: NWDGE, WSLP, XSHK, THAMAX, AM1, ZETA, NVWPRT, WCONST, REYNLD, NISHK
    implicit none
    integer :: I, J, N, M, ISK, ISK3, ISK1, ISTART, JMP, NISHK_LOC
    real :: SIGN, U, V1, AM1SQ, REYX, CF, DSTAR1, DXS, AETA, XEND
    logical :: found_shock

    ! Zero out previous wedge slopes
    do J = 1, 2
      do I = ILE, ITE
        WSLP(I,J) = 0.0
      end do
      NVWPRT(J) = 0
    end do
    
    SIGN = 1.0
    NISHK_LOC = 0
    N = 1
    ISTART = ILE
    JMP = 0
    
    ! Locate shock on upper surface and compute wedge if shock exists
    M = 1
    
    do while (M <= 2)
      call FINDSK(ISTART, ITE, merge(JUP, JLOW, M==1), ISK)
      if (ISK < 0) then
        if (M == 1) then
          ! Move to lower surface
          N = 1
          ISTART = ILE
          SIGN = -SIGN
          M = 2
          cycle
        else
          exit  ! No more shocks
        end if
      end if
      
      NISHK_LOC = NISHK_LOC + 1
      NVWPRT(M) = NVWPRT(M) + 1
      
      ! Compute X position of shock by interpolation
      V1 = PX(ISK-1, merge(JUP, JLOW, M==1))
      XSHK(M,N) = X(ISK-1) + (SONVEL - V1) / ((PX(ISK, merge(JUP, JLOW, M==1)) - V1) * XDIFF(ISK))
      
      ! Compute flow properties 3 points upstream
      ISK3 = ISK - 3
      U = PX(ISK3, merge(JUP, JLOW, M==1))
      AM1(M,N) = EMACH1(U)
      AM1SQ = AM1(M,N) * AM1(M,N)
      
      if (AM1SQ <= 1.0) then
        JMP = 1
      else
        THAMAX(M,N) = WANGLE(AM1SQ, NWDGE, GAM1) * SIGN
        
        ! If NWDGE = 2, compute Yoshihara wedge
        if (NWDGE == 1) then
          ! Murman wedge
          REYX = REYNLD * XSHK(M,N)
          CF = 0.02666 / (REYX**0.139)
          DSTAR1 = 0.01738 * REYX**0.861 / REYNLD
          
          if (N > 1 .and. JMP == 0) then
            DXS = XSHK(M,N) - XSHK(M,N-1)
            if (DXS < ZETA(M,N-1)) then
              AETA = DXS / ZETA(M,N-1)
              DSTAR1 = DXS * THAMAX(M,N-1) * (1.0 + AETA * (AETA/3.0 - 1.0))
            else
              DSTAR1 = ZETA(M,N-1) * THAMAX(M,N-1) / 3.0
            end if
          end if
          
          JMP = 0
          ZETA(M,N) = WCONST * sqrt((AM1SQ - 1.0) / CF) * DSTAR1
          
          ! Compute wedge slopes
          XEND = XSHK(M,N) + ZETA(M,N)
          do I = ISK, ITE
            if (X(I) >= XEND) exit
            AETA = (X(I) - XSHK(M,N)) / ZETA(M,N)
            WSLP(I,M) = THAMAX(M,N) * (1.0 - AETA)**2 / DELTA
          end do
        else if (NWDGE == 2) then
          ! Yoshihara wedge
          ISK1 = ISK - 1
          do I = ISK1, ISK
            WSLP(I,M) = THAMAX(M,N) / DELTA
          end do
        end if
      end if
      
      ! Check for additional shock on surface
      N = N + 1
      if (N >= 4) then
        if (M == 1) then
          ! Move to lower surface
          N = 1
          ISTART = ILE
          SIGN = -SIGN
          M = 2
        else
          exit
        end if
      else
        ISTART = ISK + 2
      end if
    end do
    
    NISHK = NISHK_LOC

  end subroutine VWEDGE

  ! Compute wedge angle for viscous correction
  function WANGLE(AM2, NW, G) result(angle)
    implicit none
    real, intent(in) :: AM2, G
    integer, intent(in) :: NW
    real :: angle
    real :: AM3, AM4, AM7, RM, RS, S2TM, S2TS, TM, TS, TTM, TTS, TDM, TDS
    
    if (NW == 1) then
      ! Murman wedge
      angle = 4.0 * ((AM2 - 1.0) / 3.0)**1.5 / G
    else
      ! Yoshihara wedge
      AM3 = 3.0 * AM2
      AM4 = 4.0 * AM2
      AM7 = 7.0 * AM2
      RM = sqrt(3.0 * (AM3 * AM2 + AM4 + 20.0))
      RS = sqrt(3.0 * (AM3 * AM2 - AM4 + 13.0))
      S2TM = (AM3 - 5.0 + RM) / AM7
      S2TS = (AM3 - 2.0 + RS) / AM7
      TM = asin(sqrt(S2TM))
      TS = asin(sqrt(S2TS))
      TTM = tan(TM)
      TTS = tan(TS)
      TDM = 5.0 * (AM2 * S2TM - 1.0) / (TTM * (5.0 + AM2 * (6.0 - 5.0 * S2TM)))
      TDS = 5.0 * (AM2 * S2TS - 1.0) / (TTS * (5.0 + AM2 * (6.0 - 5.0 * S2TS)))
      angle = 0.5 * (atan(TDM) + atan(TDS))
    end if
  end function WANGLE

  ! Subroutine to find shock location on line J between ISTART and IEND
  subroutine FINDSK(ISTART, IEND, J, ISK)
    use common_data, only: SONVEL
    implicit none
    integer, intent(in) :: ISTART, IEND, J
    integer, intent(out) :: ISK
    real :: U1, U2
    
    ISK = ISTART - 1
    U2 = PX(ISK, J)
    
    do
      ISK = ISK + 1
      U1 = U2
      U2 = PX(ISK, J)
      if (U1 > SONVEL .and. U2 <= SONVEL) exit
      if (ISK >= IEND) then
        ISK = -IEND
        exit
      end if
    end do
  end subroutine FINDSK

  ! Find new location of shockwave (ISKNEW) on line J
  ! given an initial guess for location (ISKOLD).
  ! Shock location is defined as location of shock point.
  ! If no shock is found, ISKNEW is set negative.
  ! Called by - CDCOLE.
  subroutine NEWISK(ISKOLD, J, ISKNEW)
    use common_data, only: SONVEL
    implicit none
    integer, intent(in) :: ISKOLD, J
    integer, intent(out) :: ISKNEW
    integer :: I2
    real :: U1, U2
    
    I2 = ISKOLD + 2
    ISKNEW = ISKOLD - 3
    U2 = PX(ISKNEW, J)
    
    do
      ISKNEW = ISKNEW + 1
      U1 = U2
      U2 = PX(ISKNEW, J)
      if (U1 > SONVEL .and. U2 <= SONVEL) exit
      if (ISKNEW >= I2) then
        ! No shock point found, tip of shock reached
        ISKNEW = -ISKNEW
        exit
      end if
    end do
  end subroutine NEWISK

  ! Subroutine to print map of Mach no. rounded to nearest .1
  ! Matches original MACHMP functionality exactly
  subroutine MACHMP()
    use common_data
    implicit none
    
    integer :: K, J, I, MM(100)
    real :: U, EM
    character(len=1) :: IJC
    character(len=1), parameter :: IB = ' ', IP = '+', IM = '-', IL = 'L', IT = 'T'
    
    ! Write header to main output file (Unit 15)
    write(UNIT_OUTPUT, '(40H1  MACH NO. MAP.   ROUNDED TO NEAREST .1///)')
    
    do K = 2, JMAX
      J = JMAX - K + 2
      IJC = IB
      if (J == JUP) IJC = IP
      if (J == JLOW) IJC = IM
      
      ! Initialize MM array with blanks
      do I = 1, IMAX
        MM(I) = ichar(IB)
      end do
      
      do I = 2, IMAX
        U = PX(I, J)
        EM = EMACH1(U)
        
        ! Write to mmap.out file (Unit 18) - Mach number data
        write(UNIT_MMAP, '(F16.12)', ADVANCE="NO") EM
        
        ! Write to cpmp.out file (Unit 21) - Pressure coefficient data  
        write(UNIT_CPMP, '(F16.12)', ADVANCE="NO") -2.0 * U * CPFACT
        
        if (EM <= 0.0) then
          MM(I) = ichar('0')
        else
          ! Handle Mach numbers > 1.05 by subtracting 1.0 repeatedly
          do while (EM > 1.05)
            EM = EM - 1.0
          end do
          MM(I) = int(10.0 * EM + 0.5)
        end if
      end do
      
      ! End lines in data files
      write(UNIT_MMAP, '(A)') ""
      write(UNIT_CPMP, '(A)') ""
      
      ! Write map line to main output
      write(UNIT_OUTPUT, '(11X,A1,99I1)') IJC, (MM(I), I=2, IMAX)
    end do
    
    ! Mark leading and trailing edge positions
    do I = 1, IMAX
      MM(I) = ichar(IB)
      if (I == ILE) MM(I) = ichar(IL)
      if (I == ITE) MM(I) = ichar(IT)
    end do
    
    ! Write final line with airfoil markers
    write(UNIT_OUTPUT, '(12X,99A1)') (char(MM(I)), I=2, IMAX)
    
  end subroutine MACHMP

end module math_module
