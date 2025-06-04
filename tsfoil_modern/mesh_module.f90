! mesh_module.f90
! Module for mesh generation and refinement routines

module mesh_module
  implicit none
  public :: AYMESH, CKMESH, CUTOUT, REFINE, ISLIT, JSLIT

contains

  ! Build analytical X and Y mesh points
  subroutine AYMESH()
    use common_data, only: XIN, YIN, IMINO, IMAXI, JMINO, JMAXI, BCTYPE
    use common_data, only: F, H, HALFPI, PI, RTKPOR, TWOPI
    use math_module, only: ARF
    implicit none
    integer :: I, IMA, IM2, IM2P1, JM2, JMA, J
    real :: A0,A1,A2,A3,A4,A5,A6,A7
    real :: CT1,CT2,CT3,CF1,CF2,CF3
    real :: EX,EX2,EX22,EX72,TEX2,TEX7
    real :: DX,FACT,DETA,C,C1_LOCAL,C2,C3
    real :: TOOPI
    real :: BT1,BHT1,XHT1,T1
    real :: BT2,BHT2,BHT3,T2,T12,BT12,TBT2
    real :: BT3,BHT4,BHT5,BHT6,XHT3,T3
    real, dimension(401) :: XH, BXH
    real, dimension(100) :: BX

    ! Initialize constants
    A0 = 0.225; A1 = 1.4; A2 = 1.6; A3 = 0.6188
    A4 = 0.75; A5 = 30.0; A6 = 0.603; A7 = 2.0
    CT1 = 2.0; CT2 = 2.0; CT3 = 1.0
    CF1 = 1.0; CF2 = 1.0; CF3 = 5.2

    ! Prepare X mesh
    if (IMAXI == 77) IMAXI = 81
    IMA = (IMAXI - 1) / 2
    IM2 = IMA * 2
    IM2P1 = IM2 + 1
    FACT = HALFPI * 0.005
    do I = 201, 400
      EX = tan(FACT*(I-201))
      XH(I) = EX
      EX2 = EX*EX
      EX22 = A2*A2 * EX2
      EX72 = A7*A7 * EX2
      if (EX72 >= 173.0) then
        TEX7 = 0.0
      else
        TEX7 = exp(-EX72)
      end if
      if (EX22 >= 173.0) then
        TEX2 = 0.0
      else
        TEX2 = exp(-EX22)
      end if
      BXH(I) = A1*EX*TEX2 + (1.0 - TEX7)*ARF(A4*EX)
    end do
    XH(401) = 1.0E30; BXH(401) = 1.0
    do I = 1, 200
      XH(I) = -XH(402-I)
      BXH(I) = -BXH(402-I)
    end do
    TOOPI = 2.0 / PI
    do I = 1, 401
      BXH(I) = BXH(I)*(1.0-A0) + TOOPI*A0*atan(A5*(XH(I)+A6))
    end do

    ! Map to XIN
    DX = 1.0/IMA
    do I = 1, IM2P1
      if (I == 1) then
        BX(I) = -0.999
      else if (I == IM2P1) then
        BX(I) =  0.999
      else
        BX(I) = (I-1)*DX - 1.0
      end if
      J = 1
      do while (BX(I) > BXH(J))
        J = J + 1
      end do
      if (BX(I) == BXH(J)) then
        XIN(I) = XH(J) + A3
      else
        BT1 = BX(I) - BXH(J-1)
        BHT1 = BXH(J) - BXH(J-1)
        XHT1 = XH(J)  - XH(J-1)
        T1 = XHT1/BHT1
        XIN(I) = XH(J-1) + BT1*T1 + A3
        if (J > 2) then
          BT2 = BX(I) - BXH(J)
          BHT2 = BXH(J) - BXH(J-2)
          BHT3 = BXH(J-1) - BXH(J-2)
          T12 = T1 - (XH(J-1)-XH(J-2))/BHT3
          BT12 = BT1*BT2
          TBT2 = T12/BHT2
          XIN(I) = XIN(I) + BT12*TBT2
          if (J < 400) then
            BT3 = BX(I) - BXH(J-2)
            BHT4 = BXH(J+1) - BXH(J-2)
            BHT5 = BXH(J+1) - BXH(J)
            BHT6 = BXH(J+1) - BXH(J-1)
            XHT3 = XH(J+1) - XH(J)
            T3 = XHT3/BHT5
            XIN(I) = XIN(I) + BT12*(BT3/BHT4)*((T3-T1)/BHT6 - TBT2)
          end if
        end if
      end if
    end do
    IMAXI = IM2P1

    ! Build Y mesh
    JM2 = JMAXI; JMA = JM2/2
    if (BCTYPE == 1) then
      C1_LOCAL = CF1; C2 = CF2; C3 = CF3
    else
      C1_LOCAL = CT1; C2 = CT2; C3 = CT3
    end if
    DETA = 1.0/(JMA*C1_LOCAL)
    if (BCTYPE == 1) DETA = 1.0/((JMA+1.0)*C1_LOCAL)
    C = C3/(tan(HALFPI*DETA*JMA))**C2
    do I = 1, JMA
      J = JMA + I
      YIN(J) = C*(tan(HALFPI*(I*DETA)))**C2
      YIN(J-2*I+1) = -YIN(J)
    end do
  end subroutine AYMESH

  ! Ensure odd/even mesh counts before/after tail and slit
  subroutine CKMESH()
    use common_data, only: XIN, YIN, IMIN, IMAX, ILE, ITE, JMIN, JMAX, JLOW, JUP, ICUT, IREF, UNIT_OUTPUT
    implicit none
    integer :: I, LP, L, J

    ! Check ICUT first - if <= 0, set IREF=-1 and return
    if (ICUT <= 0) then
      IREF = -1
      return
    end if    
    
    ! Test to be sure that adjusting the number of points won't make IMAX or JMAX larger than 100
    if (IMAX > 98 .or. JMAX > 98) then
      write(UNIT_OUTPUT, '(A)') &
        'THE MESH CANNOT BE ADJUSTED FOR CUTOUT, BECAUSE IMAX OR JMAX IS TOO CLOSE TO THE LIMIT OF 100.'
      write(UNIT_OUTPUT, '(A)') 'IREF WAS SET TO  0'
      IREF = -1
      return
    end if
    
    ! Add extra X-point ahead of airfoil if needed (check for even number of points)
    if (mod(ITE - IMIN + 1, 2) == 0) then
      LP = IMAX + IMIN + 1
      do I = IMIN, IMAX
        L = LP - I
        XIN(L) = XIN(L - 1)
      end do
      IMAX = IMAX + 1
      XIN(IMIN) = 2.0 * XIN(IMIN + 1) - XIN(IMIN + 2)
      call ISLIT(XIN)
    end if

    ! Add extra X-point after airfoil if needed (check for even number of points)
    if (mod(IMAX - ITE + 1, 2) == 0) then
      IMAX = IMAX + 1
      XIN(IMAX) = 2.0 * XIN(IMAX - 1) - XIN(IMAX - 2)
    end if    
    
    ! Check Y mesh and adjust to contain even number of points above and below slit
    ! Add extra Y-point below slit if needed (check for even number of points)
    if (mod(JLOW - JMIN, 2) == 0) then
      LP = JMAX + JMIN + 1
      do J = JMIN, JMAX
        L = LP - J
        YIN(L) = YIN(L - 1)
      end do
      JMAX = JMAX + 1
      YIN(JMIN) = 2.0 * YIN(JMIN + 1) - YIN(JMIN + 2)
      call JSLIT(YIN)
    end if

    ! Add extra Y-point above slit if needed (check for even number of points)
    if (mod(JMAX - JUP, 2) == 0) then
      JMAX = JMAX + 1
      YIN(JMAX) = 2.0 * YIN(JMAX - 1) - YIN(JMAX - 2)
    end if
  end subroutine CKMESH

  ! Coarsen mesh by halving for initial solution pass
  subroutine CUTOUT()
    use common_data, only: X, Y, XMID, YMID, IREF, ICUT, XIN, YIN, IMIN, IMAX, JMIN, JMAX, JLOW, JUP, ITE
    implicit none
    integer :: I, J, K, JE, JST

    ! Check if IREF = -1 (mesh cannot be refined)
    if (IREF == -1) then
      ! Load XIN,YIN into X,Y
      do I = IMIN, IMAX
        X(I) = XIN(I)
      end do
      do J = JMIN, JMAX
        Y(J) = YIN(J)
      end do
      IREF = 0
      return
    end if

    ! First halving: X-direction
    K = IMIN - 1
    do I = IMIN, IMAX, 2
      K = K + 1
      XMID(K) = XIN(I)
    end do
    IMAX = (IMAX - IMIN) / 2 + IMIN
    call ISLIT(XMID)
    
    ! First halving: Y-direction, splitting above and below slit
    K = JMIN - 1
    JE = JLOW - 1
    do J = JMIN, JE, 2
      K = K + 1
      YMID(K) = YIN(J)
    end do
    JST = JUP + 1
    do J = JST, JMAX, 2
      K = K + 1
      YMID(K) = YIN(J)
    end do
    JMAX = (JMAX - JMIN) / 2 + JMIN
    call JSLIT(YMID)

    ! Set IREF to 1 indicating first halving
    IREF = 1
    
    ! First halving complete. Check if no. of points is odd.
    if (ICUT == 1 .or. &
        mod(ITE - IMIN + 1, 2) == 0 .or. &
        mod(IMAX - ITE + 1, 2) == 0 .or. &
        mod(JLOW - JMIN, 2) == 0 .or. &
        mod(JMAX - JUP, 2) == 0) then
      ! Only one mesh refinement possible.
      do I = IMIN, IMAX
        X(I) = XMID(I)
      end do
      do J = JMIN, JMAX
        Y(J) = YMID(J)
      end do
      return
    end if

    ! All points are odd so cut again.
    K = IMIN - 1
    do I = IMIN, IMAX, 2
      K = K + 1
      X(K) = XMID(I)
    end do
    IMAX = (IMAX - IMIN) / 2 + IMIN
    call ISLIT(X)
    
    K = JMIN - 1
    JE = JLOW - 1
    do J = JMIN, JE, 2
      K = K + 1
      Y(K) = YMID(J)
    end do
    JST = JUP + 1
    do J = JST, JMAX, 2
      K = K + 1
      Y(K) = YMID(J)
    end do
    JMAX = (JMAX - JMIN) / 2 + JMIN
    call JSLIT(Y)
    
    ! Set IREF to 2 indicating second halving
    IREF = 2
  end subroutine CUTOUT
  
  ! Refine mesh and interpolate solution onto finer grid
  subroutine REFINE()
    use common_data, only: XIN, YIN, XMID, YMID, P, X, Y, IMIN, IMAX, JMIN, JMAX, ILE, ITE, JLOW, JUP, IREF, ICUT
    use common_data, only: NWDGE, WSLP
    implicit none
    integer :: I, J, K, JMAXO_LOCAL, JE, JST, IM2, JM2, JL
    integer :: ILEO, INC, M, ISTEP, ISTRT, IEND, IM, IMM
    real :: PT(100)
    real :: D1, D2, CL1, CL2, CU1, CU2, RATIO
    real :: XLEO_LOCAL  ! Must be REAL to store X-coordinate

    ! Store original leading edge position and index for viscous wedge processing
    XLEO_LOCAL = X(ILE)
    ILEO = ILE
    JMAXO_LOCAL = JMAX

    ! Compute new grid size
    IMAX = 2*(IMAX - IMIN) + IMIN
    JMAX = 2*(JMAX - JMIN) + JMIN + 1
    IM2 = IMAX - 2
    JM2 = JMAX - 2

    ! Choose source mesh (coarse or mid) based on IREF
    if (IREF <= 1) then
      do I = IMIN, IMAX
        X(I) = XIN(I)
      end do
      do J = JMIN, JMAX
        Y(J) = YIN(J)
      end do
      IREF = 0
    else
      do I = IMIN, IMAX
        X(I) = XMID(I)
      end do
      do J = JMIN, JMAX
        Y(J) = YMID(J)
      end do
      IREF = 1
    end if

    ! Update mesh indices
    call ISLIT(X)
    call JSLIT(Y)    ! Spread P(J,I) to alternate I(X-MESH) points
    do J = JMIN, JMAXO_LOCAL
      K = IMIN - 1
      do I = IMIN, IMAX, 2
        K = K + 1
        PT(I) = P(J,K)
      end do
      do I = IMIN, IMAX, 2
        P(J,I) = PT(I)
      end do
    end do

    ! Spread P(J,I) to alternate J (Y-MESH) points
    do I = IMIN, IMAX, 2
      K = JMIN - 1
      JE = JLOW - 1
      JL = JLOW - 2
      do J = JMIN, JE, 2
        K = K + 1
        PT(J) = P(K,I)
      end do
      JST = JUP + 1
      do J = JST, JMAX, 2
        K = K + 1
        PT(J) = P(K,I)
      end do
      do J = JMIN, JE, 2
        P(J,I) = PT(J)
      end do
      do J = JST, JMAX, 2
        P(J,I) = PT(J)
      end do
    end do

    ! Interpolate to fill in the missing P values
    do I = IMIN, IM2
      PT(I) = (X(I+1)-X(I)) / (X(I+2)-X(I))
    end do
    do J = JMIN, JE, 2
      do I = IMIN, IM2, 2
        P(J,I+1) = P(J,I) + PT(I) * (P(J,I+2) - P(J,I))
      end do
    end do
    do J = JST, JMAX, 2
      do I = IMIN, IM2, 2
        P(J,I+1) = P(J,I) + PT(I) * (P(J,I+2) - P(J,I))
      end do
    end do
    do J = JMIN, JM2
      PT(J) = (Y(J+1)-Y(J)) / (Y(J+2)-Y(J))
    end do
    do I = IMIN, IMAX
      do J = JMIN, JL, 2
        P(J+1,I) = P(J,I) + PT(J) * (P(J+2,I) - P(J,I))
      end do
      do J = JST, JM2, 2
        P(J+1,I) = P(J,I) + PT(J) * (P(J+2,I) - P(J,I))
      end do
    end do

    ! Use extrapolation for JLOW, JUP
    D1 = Y(JLOW) - Y(JLOW-1)
    D2 = Y(JLOW-1) - Y(JLOW-2)
    CL1 = (D1 + D2) / D2
    CL2 = D1/D2
    D1 = Y(JUP+1) - Y(JUP)
    D2 = Y(JUP+2) - Y(JUP+1)
    CU1 = (D1 + D2) / D2
    CU2 = D1 / D2
    do I = IMIN, IMAX
      P(JUP,I) = CU1*P(JUP+1,I) - CU2*P(JUP+2,I)
      P(JLOW,I) = CL1*P(JLOW-1,I) - CL2*P(JLOW-2,I)
    end do

    ! Expand viscous wedge slopes to new grid
    if (NWDGE == 0) return
    INC = 0
    if (X(ILE) < XLEO_LOCAL) INC = 1
    M = 0
    do while (M < 2)
      M = M + 1
      do I = IMIN, IMAX
        PT(I) = 0.0
      end do
      ISTEP = ILEO - 1
      ISTRT = ILE + INC
      IEND = ITE + INC
      do I = ISTRT, IEND, 2
        ISTEP = ISTEP + 1
        PT(I) = WSLP(ISTEP,M)
      end do
      do I = ISTRT, IEND, 2
        IM = I - 1
        IMM = IM - 1
        WSLP(I,M) = PT(I)
        RATIO = (X(IM)-X(IMM))/(X(I)-X(IMM))
        WSLP(IM,M) = PT(IMM)+(PT(I)-PT(IMM))*RATIO
      end do
      if (M >= 2) exit
    end do
  end subroutine REFINE
  
  ! Compute ILE and ITE for mesh X array
  subroutine ISLIT(X_MESH)
    use common_data, only: IMIN, IMAX, ILE, ITE
    implicit none
    real, intent(in) :: X_MESH(:)
    integer :: i

    ! Find first point where X >= 0.0 (leading edge)
    ! Exactly matching original FORTRAN logic with bounds checking
    i = IMIN - 1
    do
      i = i + 1
      if (i > IMAX) then
        ! If no point found with X >= 0.0, set ILE to IMIN
        ILE = IMIN
        exit
      end if
      if (X_MESH(i) >= 0.0) then
        ILE = i
        exit
      end if
    end do
    
    ! Find first point where X > 1.0 (trailing edge) 
    do
      i = i + 1
      if (i > IMAX) then
        ! If no point found with X > 1.0, set ITE to IMAX
        ITE = IMAX
        exit
      end if
      if (X_MESH(i) > 1.0) then
        ITE = i - 1
        exit
      end if
    end do
  end subroutine ISLIT
    
  ! Compute JLOW and JUP for mesh Y array
  subroutine JSLIT(Y_MESH)
    use common_data, only: JMIN, JMAX, JLOW, JUP
    implicit none
    real, intent(in) :: Y_MESH(:)
    integer :: j

    j = JMIN - 1
    do
      j = j + 1
      if (Y_MESH(j) >= 0.0) exit
    end do
    JLOW = j - 1
    JUP  = j
  end subroutine JSLIT

end module mesh_module
