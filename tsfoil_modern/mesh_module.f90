! mesh_module.f90
! Module for mesh generation and refinement routines

module mesh_module
  implicit none
  public :: CKMESH, ISLIT, JSLIT, setup_mesh

contains

  ! Setup mesh
  ! This subroutine contains functions that are originally in READIN (io_module.f90)
  subroutine setup_mesh()
    use common_data, only: X, Y, XIN, YIN, IMIN, IMAX
    use common_data, only: JMIN, JMAX, IMAXI, JMAXI, JMXF, JMXT
    use common_data, only: N_MESH_POINTS
    use common_data, only: BCTYPE, YFREE, YTUN, INPERR
    use common_data, only: EMACH, ALPHA, DELTA, NWDGE
    implicit none
    integer :: J_VAR, IDX, JDX, I_ITER, J_ITER

    ! Handle YIN initialization
    if (YIN(JMIN) == 0.0) then
      ! YIN needs default initialization for tunnel or free air case
      if (BCTYPE == 1) then
        ! Free air case
        JMAXI = JMXF
        do J_VAR = JMIN, JMAXI
          YIN(J_VAR) = YFREE(J_VAR)
        end do
      else
        ! Tunnel case
        JMAXI = JMXT
        do J_VAR = JMIN, JMAXI
          YIN(J_VAR) = YTUN(J_VAR)
        end do
      end if
    end if

    ! Set derived constants
    IMAX = IMAXI
    JMAX = JMAXI

    ! Check array bounds (any call to INPERR causes message to be printed and execution stopped)
    if (IMAXI > N_MESH_POINTS .or. JMAXI > N_MESH_POINTS) call INPERR(1)
    
    ! Check input mesh for monotonically increasing values
    do IDX = IMIN, IMAX - 1
      if (XIN(IDX) >= XIN(IDX+1)) call INPERR(2)
    end do
    
    do JDX = JMIN, JMAX - 1
      if (YIN(JDX) >= YIN(JDX+1)) call INPERR(3)
    end do
    
    ! Check parameter ranges
    if (EMACH < 0.5 .or. EMACH > 2.0) call INPERR(4)
    if (ALPHA < -9.0 .or. ALPHA > 9.0) call INPERR(5)
    if (DELTA < 0.0 .or. DELTA > 1.0) call INPERR(6)
    if (NWDGE > 0 .and. EMACH > 1.0) call INPERR(8)
    
    ! Compute ILE and ITE (leading and trailing edge)
    call ISLIT(XIN)
    call JSLIT(YIN)

    ! Check number of mesh points, if not odd add points to appropriate areas to make odd no.
    call CKMESH()
    
    ! Load XIN,YIN into X,Y
    do I_ITER = IMIN, IMAX
      X(I_ITER) = XIN(I_ITER)
    end do
    do J_ITER = JMIN, JMAX
      Y(J_ITER) = YIN(J_ITER)
    end do

  end subroutine SETUP_MESH

  ! Ensure odd/even mesh counts before/after tail and slit
  subroutine CKMESH()
    use common_data, only: XIN, YIN, IMIN, IMAX, ITE
    use common_data, only: JMIN, JMAX, JLOW, JUP
    use common_data, only: UNIT_OUTPUT, N_MESH_POINTS
    use common_data, only: BCTYPE, H
    implicit none
    integer :: I, LP, L, J, JDX
    real :: TERM, HTM, HTP, YS, YE
    
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

    ! Check bounds of YIN (mesh y coordinates) for tunnel calculations
    if (BCTYPE /= 1) then
      HTM = H - 0.00001
      HTP = H + 0.00001
      YS = abs(YIN(JMIN))
      YE = abs(YIN(JMAX))
      if (.not. ((YS >= HTM .and. YS <= HTP) .and. (YE >= HTM .and. YE <= HTP))) then
        ! Rescale Y mesh to -H,+H bounds
        TERM = -H / YIN(JMIN)
        do JDX = JMIN, JLOW
          YIN(JDX) = TERM * YIN(JDX)
        end do
        TERM = H / YIN(JMAX)
        do JDX = JUP, JMAX
          YIN(JDX) = TERM * YIN(JDX)
        end do
      end if
    end if

  end subroutine CKMESH
  
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
  ! JSLIT computes:
  ! 1.) JLOW = index of first point where Y >= 0.0
  ! 2.) JUP = index of first point where Y > 0.0
  ! Called by - CKMESH, CUTOUT, REFINE
  subroutine JSLIT(Y_MESH)
    use common_data, only: JMIN, JLOW, JUP
    implicit none
    real, intent(in) :: Y_MESH(:)
    integer :: j

    ! Y_MESH is the Y-mesh array, size(Y_MESH) = JMAXI
    ! JMIN is the minimum index of the Y-mesh array
    ! JMAXI is the maximum index of the Y-mesh array

    j = JMIN - 1
    do
      j = j + 1
      if (Y_MESH(j) >= 0.0) exit
    end do
    JLOW = j - 1
    JUP  = j
  end subroutine JSLIT

end module mesh_module
