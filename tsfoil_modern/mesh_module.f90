! mesh_module.f90
! Module for mesh generation and refinement routines

module mesh_module
  use common_data
  implicit none
  public :: AYMESH, CKMESH, CUTOUT, REFINE, ISLIT, JSLIT
  ! Public subroutines for mesh generation and refinement

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
    real :: DX,FACT,DETA,C,C1,C2,C3
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
      TEX7 = merge(exp(-EX72), 0.0, EX72 < 173.0)
      TEX2 = merge(exp(-EX22), 0.0, EX22 < 173.0)
      BXH(I) = A1*EX*TEX2 + (1.0 - TEX7)*ARF(A4*EX)
    end do
    XH(401) = 1.0E30; BXH(401) = 1.0
    do I = 1, 200
      XH(I) = -XH(402-I)
      BXH(I) = -BXH(402-I)
    end do
    TOOPI = TWOPI
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
      C1 = CF1; C2 = CF2; C3 = CF3
    else
      C1 = CT1; C2 = CT2; C3 = CT3
    end if
    DETA = 1.0/(JMA*C1)
    if (BCTYPE == 1) DETA = 1.0/((JMA+1.0)*C1)
    C = C3/(tan(HALFPI*DETA*JMA))**C2
    do I = 1, JMA
      J = JMA + I
      YIN(J) = C*(tan(HALFPI*(I*DETA)))**C2
      YIN(J-2*I+1) = -YIN(J)
    end do
  end subroutine AYMESH

  ! Ensure odd/even mesh counts before/after tail and slit
  subroutine CKMESH()
    use common_data, only: XIN, YIN, IMIN, IMAX, ILE, ITE, JMIN, JMAX, JLOW, JUP, ICUT, IREF
    implicit none
    integer :: I, LP, L, J

    ! If mesh has been coarsened, skip adjustment
    if (ICUT > 0) then
      IREF = -1
      return
    end if

    ! Add extra X-point ahead of airfoil if needed
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

    ! Add extra X-point after airfoil if needed
    if (mod(IMAX - ITE + 1, 2) == 0) then
      IMAX = IMAX + 1
      XIN(IMAX) = 2.0 * XIN(IMAX - 1) - XIN(IMAX - 2)
    end if

    ! Add extra Y-point below slit if needed
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

    ! Add extra Y-point above slit if needed
    if (mod(JMAX - JUP, 2) == 0) then
      JMAX = JMAX + 1
      YIN(JMAX) = 2.0 * YIN(JMAX - 1) - YIN(JMAX - 2)
    end if
  end subroutine CKMESH

  ! Coarsen mesh by halving for initial solution pass
  subroutine CUTOUT()
    use common_data, only: XMID, YMID, IREF, ICUT, XIN, YIN, IMIN, IMAX, JMIN, JMAX, JLOW, JUP
    implicit none
    integer :: I, J, K, JE, JST

    ! On first call (IREF=-1), load current mesh into XMID/YMID arrays
    if (IREF == -1) then
      do I = IMIN, IMAX
        XMID(I) = XIN(I)
      end do
      do J = JMIN, JMAX
        YMID(J) = YIN(J)
      end do
      IREF = 0
      return
    end if

    ! Halve mesh in X-direction
    K = IMIN - 1
    do I = IMIN, IMAX, 2
      K = K + 1
      XMID(K) = XIN(I)
    end do
    IMAX = (IMAX - IMIN) / 2 + IMIN
    call ISLIT(XMID)

    ! Halve mesh in Y-direction, splitting above and below slit
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

    ! Update IREF flag
    IREF = 1

    ! If only one refinement requested, load XMID/YMID back into XIN/YIN
    if (ICUT == 1) then
      do I = IMIN, IMAX
        XIN(I) = XMID(I)
      end do
      do J = JMIN, JMAX
        YIN(J) = YMID(J)
      end do
      return
    end if

    ! Additional halving not implemented: further refinements can be added here
    ! If two refinements requested, perform second halving
    if (ICUT >= 2) then
      ! Halve mesh in X-direction again
      K = IMIN - 1
      do I = IMIN, IMAX, 2
        K = K + 1
        XIN(K) = XMID(I)
      end do
      IMAX = (IMAX - IMIN) / 2 + IMIN
      call ISLIT(XIN)

      ! Halve mesh in Y-direction again, splitting above and below slit
      K = JMIN - 1
      JE = JLOW - 1
      do J = JMIN, JE, 2
        K = K + 1
        YIN(K) = YMID(J)
      end do
      JST = JUP + 1
      do J = JST, JMAX, 2
        K = K + 1
        YIN(K) = YMID(J)
      end do
      JMAX = (JMAX - JMIN) / 2 + JMIN
      call JSLIT(YIN)

      IREF = 2
      return
    end if
  end subroutine CUTOUT

  ! Refine mesh and interpolate solution onto finer grid
  subroutine REFINE()
    use common_data, only: XIN, YIN, XMID, YMID, P, IMIN, IMAX, JMIN, JMAX, ILE, ITE, JLOW, JUP, IREF, ICUT
    implicit none
    integer :: I, J, K, IMAXO, JMAXO, JE, JST, IM2, JM2
    real :: PT(100)

    ! Save original limits
    IMAXO = IMAX; JMAXO = JMAX

    ! Compute new grid size
    IMAX = 2*(IMAX - IMIN) + IMIN
    JMAX = 2*(JMAX - JMIN) + JMIN + 1
    IM2 = IMAX - 2; JM2 = JMAX - 2

    ! Choose source mesh (coarse or mid)
    if (IREF <= 1) then
      do I = IMIN, IMAXO
        XMID(I) = XIN(I)
      end do
      do J = JMIN, JMAXO
        YMID(J) = YIN(J)
      end do
      IREF = 1
    else
      do I = IMIN, IMAXO
        XMID(I) = XMID(I)
      end do
      do J = JMIN, JMAXO
        YMID(J) = YMID(J)
      end do
    end if

    ! Spread grid
    call ISLIT(XMID)
    call JSLIT(YMID)

    ! X-direction: copy P to new odd points
    do J = JMIN, JMAXO
      K = IMIN - 1
      do I = IMIN, IMAX, 2
        K = K + 1
        P(J,I) = P(J,K)
      end do
    end do

    ! Y-direction: copy P to new odd points
    do I = IMIN, IMAX
      K = JMIN - 1
      JE = JLOW - 1
      do J = JMIN, JE, 2
        K = K + 1
        P(J,I) = P(K,I)
      end do
      JST = JUP + 1
      do J = JST, JMAX, 2
        K = K + 1
        P(J,I) = P(K,I)
      end do
    end do

    ! Interpolate missing values in X direction
    do I = IMIN, IM2
      PT(I) = (XIN(I+1)-XIN(I)) / (XIN(I+2)-XIN(I))
    end do
    do J = JMIN, JMAX, 2
      do I = IMIN, IM2, 2
        P(J,I+1) = P(J,I) + PT(I)*(P(J,I+2)-P(J,I))
      end do
    end do

    ! Interpolate missing values in Y direction
    do J = JMIN, JM2
      PT(J) = (YIN(J+1)-YIN(J)) / (YIN(J+2)-YIN(J))
    end do
    do I = IMIN, IMAX
      do J = JMIN, JM2, 2
        P(J+1,I) = P(J,I) + PT(J)*(P(J+2,I)-P(J,I))
      end do
    end do
  end subroutine REFINE

  ! Compute ILE and ITE for mesh X array
  subroutine ISLIT(X)
    use common_data, only: IMIN, ILE, ITE
    implicit none
    real, intent(in) :: X(:)
    integer :: i

    i = IMIN - 1
    do
      i = i + 1
      if (X(i) >= 0.0) exit
    end do
    ILE = i

    do
      i = i + 1
      if (X(i) > 1.0) exit
    end do
    ITE = i - 1
  end subroutine ISLIT

  ! Compute JLOW and JUP for mesh Y array
  subroutine JSLIT(Y)
    use common_data, only: JMIN, JLOW, JUP
    implicit none
    real, intent(in) :: Y(:)
    integer :: j

    j = JMIN - 1
    do
      j = j + 1
      if (Y(j) >= 0.0) exit
    end do
    JLOW = j - 1
    JUP  = j
  end subroutine JSLIT

end module mesh_module
