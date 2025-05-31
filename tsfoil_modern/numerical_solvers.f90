! numerical_solvers.f90
! Module for SOR solver and iteration control routines

module numerical_solvers
  use common_data
  implicit none
  public :: SYOR, SOLVE, RECIRC, REDUB, RESET

contains

  ! Perform one SOR sweep computing residuals and updating P
  subroutine SYOR()
    use common_data, only: P, X, Y, IMIN, IMAX, IUP, IDOWN, ILE, ITE
    use common_data, only: JMIN, JMAX, JUP, JLOW, JTOP, JBOT, J1, J2
    use common_data, only: AK, GAM1, CVERGE, ERROR, IERROR, JERROR
    use common_data, only: CXL, CXC, CXR, CXXL, CXXC, CXXR, C1
    use common_data, only: CYYC, CYYD, CYYU, DIAG, RHS, SUB, SUP
    use common_data, only: CYYBUC, CYYBUU, CYYBLC, CYYBLD, FXUBC, FXLBC
    use common_data, only: PJUMP, FCR, KUTTA
    implicit none
    integer :: I, J, K, IM2, JA, JB, J1_LOC, J2_LOC
    real :: EPSX, VC_VAL, EMU_VAL, ARHS, DNOM
    real, dimension(100) :: VC, EMU_CUR, EMU_PREV, SAVE_ARR, POLD_CUR
    real, parameter :: EPS = 1.0E-6, WI = 1.0

    IM2 = IUP - 1
    if (AK < 0.0) IM2 = IUP - 2

    J1_LOC = JBOT + 1
    J2_LOC = JTOP - JBOT

    do I = IUP, IDOWN
      EPSX = EPS / ((X(I) - X(I-1))**2)

      ! Compute VC = 1 - M**2 and EMU
      do J = JBOT, JTOP
        VC(J) = C1(I) - (CXL(I)*POLD_CUR(J) + CXC(I)*P(J,I) + CXR(I)*P(J,I+1))
        EMU_CUR(J) = 0.0
        POLD_CUR(J) = P(J,I)
        if (VC(J) < 0.0) EMU_CUR(J) = VC(J)
      end do

      if (.not. FCR) then
        EMU_PREV = EMU_CUR
      end if

      ! Compute matrix elements
      do J = JBOT, JTOP
        DIAG(J) = (EMU_CUR(J) - VC(J)) * CXXC(I) * WI + EMU_PREV(J) * CXXR(I-1) - CYYC(J)
        SUP(J) = CYYD(J)
        SUB(J) = CYYU(J)
      end do

      ! Compute residual
      do J = JBOT, JTOP
        RHS(J) = -(VC(J) - EMU_CUR(J)) * &
                 (CXXL(I)*P(J,I-1) - CXXC(I)*P(J,I) + CXXR(I)*P(J,I+1))
        RHS(J) = RHS(J) - (EMU_PREV(J) * &
                 (CXXL(I-1)*P(J,IM2) - CXXC(I-1)*P(J,I-1) + CXXR(I-1)*P(J,I)))
      end do

      ! Interior points
      JA = JBOT + 1
      JB = JTOP - 1
      do J = JA, JB
        RHS(J) = RHS(J) - (CYYD(J)*P(J-1,I) - CYYC(J)*P(J,I) + CYYU(J)*P(J+1,I))
      end do

      ! Bottom boundary
      RHS(JBOT) = RHS(JBOT) - (-CYYC(JBOT)*P(JBOT,I) + CYYU(JBOT)*P(JBOT+1,I))
      if (JBOT /= JMIN) then
        RHS(JBOT) = RHS(JBOT) - CYYD(JBOT)*P(JBOT-1,I)
      end if

      ! Top boundary
      RHS(JTOP) = RHS(JTOP) - (CYYD(JTOP)*P(JTOP-1,I) - CYYC(JTOP)*P(JTOP,I))
      if (JTOP /= JMAX) then
        RHS(JTOP) = RHS(JTOP) - CYYU(JTOP)*P(JTOP+1,I)
      end if

      ! Check for airfoil boundary condition
      if (I >= ILE .and. I <= ITE) then
        ! Upper surface
        J = JUP
        DIAG(J) = DIAG(J) + CYYC(J) - CYYBUC
        SUP(J) = 0.0
        SUB(J) = CYYBUU
        RHS(J) = RHS(J) + CYYD(J)*P(J-1,I) - CYYC(J)*P(J,I) + CYYU(J)*P(J+1,I) &
                - (-CYYBUC*P(J,I) + CYYBUU*P(J+1,I) + FXUBC(I))

        ! Lower surface
        J = JLOW
        DIAG(J) = DIAG(J) + CYYC(J) - CYYBLC
        SUP(J) = CYYBLD
        SUB(J) = 0.0
        RHS(J) = RHS(J) + CYYD(J)*P(J-1,I) - CYYC(J)*P(J,I) + CYYU(J)*P(J+1,I) &
                - (-CYYBLC*P(J,I) + CYYBLD*P(J-1,I) + FXLBC(I))
      else if (I > ITE) then
        ! Kutta slice
        RHS(JLOW) = RHS(JLOW) + CYYU(JLOW)*PJUMP(I)
        RHS(JUP) = RHS(JUP) - CYYD(JUP)*PJUMP(I)
      end if

      ! Apply wall boundary conditions
      call BCEND()

      ! Add artificial dissipation
      do J = JBOT, JTOP
        DIAG(J) = DIAG(J) - EPSX
        RHS(J) = RHS(J) - EPSX*(P(J,I-1) - POLD_CUR(J))
      end do

      ! Solve tridiagonal system
      DNOM = 1.0 / DIAG(JBOT)
      SAVE_ARR(JBOT) = SUB(JBOT) * DNOM
      RHS(JBOT) = RHS(JBOT) * DNOM

      do J = J1_LOC, JTOP
        DNOM = 1.0 / (DIAG(J) - SUP(J)*SAVE_ARR(J-1))
        SAVE_ARR(J) = SUB(J) * DNOM
        RHS(J) = (RHS(J) - SUP(J)*RHS(J-1)) * DNOM
      end do

      do K = 1, J2_LOC
        J = JTOP - K
        RHS(J) = RHS(J) - SAVE_ARR(J) * RHS(J+1)
      end do

      ! Update solution and track maximum error
      do J = JBOT, JTOP
        P(J,I) = P(J,I) + RHS(J)
        ARHS = abs(RHS(J))
        if (ARHS > ERROR) then
          ERROR = ARHS
          IERROR = I
          JERROR = J
        end if
      end do

      EMU_PREV = EMU_CUR
    end do

  end subroutine SYOR

  ! Main iteration loop: solver, convergence, and flow updates
  subroutine SOLVE()
    use common_data, only: MAXIT, ERROR, CVERGE, IPRTER, PRTFLO
    implicit none
    integer :: ITER
    logical :: CONVERGED

    CONVERGED = .false.
    
    do ITER = 1, MAXIT
      ERROR = 0.0
      
      ! Perform SOR sweep
      call SYOR()
      
      ! Check convergence
      if (ERROR < CVERGE) then
        CONVERGED = .true.
        write(*,'(A,I0,A,E12.4)') 'Converged in ', ITER, ' iterations, error = ', ERROR
        exit
      end if
      
      ! Print progress
      if (mod(ITER, IPRTER) == 0 .or. ITER == 1) then
        write(*,'(A,I0,A,E12.4)') 'Iteration ', ITER, ', max error = ', ERROR
      end if
      
      ! Update circulation and doublet strength
      call RECIRC()
      call REDUB()
      
      ! Reset boundary conditions
      call RESET()
    end do
    
    if (.not. CONVERGED) then
      write(*,'(A,I0,A,E12.4)') 'Warning: Did not converge after ', MAXIT, &
                               ' iterations, final error = ', ERROR
    end if

  end subroutine SOLVE

  ! Update circulation-jump boundary after Kutta or M divergence
  subroutine RECIRC()
    use common_data, only: PJUMP, ITE, JLOW, JUP, P
    implicit none
    integer :: I

    do I = ITE+1, IMAX
      PJUMP(I) = P(JUP,I) - P(JLOW,I)
    end do

  end subroutine RECIRC

  ! Update doublet strength DUB for nonlinear correction
  subroutine REDUB()
    use common_data, only: DUB, ALPHA, KUTTA
    implicit none
    
    if (KUTTA == 0) return
    
    ! Simple update - in practice this would use circulation calculation
    DUB = DUB + 0.1 * ALPHA

  end subroutine REDUB

  ! Reset far-field boundary values after mesh change or Kutta
  subroutine RESET()
    use common_data, only: P, IMIN, IMAX, JMIN, JMAX, JUP, JLOW, KSTEP
    use common_data, only: DUP, DDOWN, DTOP, DBOT, VUP, VDOWN, VTOP, VBOT
    use common_data, only: CIRCFF, DUB, BCTYPE
    implicit none
    integer :: J, I, K

    ! Set boundary conditions at upstream and downstream ends
    K = JMIN - KSTEP
    do J = JMIN, JMAX
      K = K + KSTEP
      if (J == JUP) K = K + KSTEP - 1
      P(J,IMIN) = CIRCFF*VUP(K) + DUB*DUP(K)
      P(J,IMAX) = CIRCFF*VDOWN(K) + DUB*DDOWN(K)
    end do

    if (BCTYPE == 1) then
      ! Update boundary conditions on top and bottom
      K = IMIN - KSTEP
      do I = IMIN, IMAX
        K = K + KSTEP
        P(JMIN,I) = CIRCFF*VBOT(K) + DUB*DBOT(K)
        P(JMAX,I) = CIRCFF*VTOP(K) + DUB*DTOP(K)
      end do
    end if

  end subroutine RESET

end module numerical_solvers
