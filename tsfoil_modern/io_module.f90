! io_module.f90
! Module for input/output routines

module io_module
  use common_data
  implicit none
  public :: READIN, SCALE, ECHINP, PRINT, PRINT1, PRTFLD, PRTMC, PRTSK, PRTWAL, SAVEP

contains

  subroutine READIN()
    ! Read namelist parameters and initialize data
    use common_data, only: AMESH, ICUT, IREF, PSTART, ABORT1
    use common_data, only: INP
    implicit none
    character(len=*), parameter :: nl = 'INP'

    call ECHINP()
    call SCALE()
    if (AMESH) then
      call AYMESH()
    end if
    call CKMESH()
    if (ICUT > 0) then
      IREF = -1
      call CUTOUT()
    end if
    return
  end subroutine READIN

  subroutine SCALE()
    ! Convert physical variables to similarity variables
    use common_data, only: PHYS, DELTA, EMACH, SIMDEF
    use common_data, only: AK, YFACT, CPFACT, CLFACT, CDFACT, CMFACT, VFACT
    implicit none
    real :: BETA, DELRT1, DELRT2, EMROOT

    if (.not. PHYS) then
      CPFACT = 1.0; CLFACT = 1.0; CDFACT = 1.0; CMFACT = 1.0; YFACT = 1.0; VFACT = 1.0
      AK = 0.0
      return
    end if

    ! Compute scaling parameters
    BETA = 1.0 - EMACH**2
    DELRT1 = DELTA**(1.0/3.0)
    DELRT2 = DELTA**(2.0/3.0)

    select case (SIMDEF)
    case (1)
      ! Cole scaling
      AK = BETA / DELRT2
      YFACT = 1.0 / DELRT1
      CPFACT = DELRT2
      CLFACT = DELRT2
      CMFACT = DELRT2
      CDFACT = DELRT2 * DELTA
      VFACT = DELTA * 57.295779

    case (2)
      ! Spreiter scaling
      EMROOT = EMACH**(2.0/3.0)
      AK = BETA / (DELRT2 * EMROOT * EMROOT)
      YFACT = 1.0 / (DELRT1 * EMROOT)
      CPFACT = DELRT2 / EMROOT
      CLFACT = CPFACT
      CMFACT = CPFACT
      CDFACT = CPFACT * DELTA
      VFACT = DELTA * 57.295779

    case (3)
      ! Krupp scaling
      AK = BETA / (DELRT2 * EMACH)
      YFACT = 1.0 / (DELRT1 * sqrt(EMACH))
      CPFACT = DELRT2 / (EMACH**0.75)
      CLFACT = CPFACT
      CMFACT = CPFACT
      CDFACT = CPFACT * DELTA
      VFACT = DELTA * 57.295779

    case default
      stop 'Unsupported SIMDEF'
    end select

  end subroutine SCALE

  subroutine ECHINP()
    ! Echo input cards for logging to file unit 15
    implicit none
    write(15,'(A)') 'Input parameters:'
    write(15,'(A,F1.0)') '  AMESH = ', AMESH
    write(15,'(A,F1.0)') '  SIMDEF = ', SIMDEF
    write(15,'(A,F7.3)') '  EMACH = ', EMACH
    write(15,'(A,F7.3)') '  DELTA = ', DELTA
    write(15,'(A,I4)') '  BCFOIL = ', BCFOIL
    write(15,'(A,I4)') '  BCTYPE = ', BCTYPE
  end subroutine ECHINP

  subroutine PRINT()
    ! Main print driver: calls PRINT1, PRTMC, etc.
    implicit none
    call PRINT1()
    call PRTFLD()
    call PRTMC()
    call PRTSK()
    call PRTWAL()
  end subroutine PRINT

  subroutine PRINT1()
    ! Print Cp and Mach along body and build plot arrays, writing to file unit 15
    use common_data, only: CPFACT, XIN, ILE, ITE, P, CPL, CPU
    implicit none
    integer :: I
    real :: CP
    do I = ILE, ITE
      CP = -2.0*(P(JUP,I)-P(JLOW,I)) / (XIN(I+1)-XIN(I-1))
      CPL(I-ILE+1) = CP; CPU(I-ILE+1) = CP
      write(15,'(F6.3,2X,F8.4)') XIN(I), CP
    end do
  end subroutine PRINT1

  subroutine PRTFLD()
    ! Print Cp, flow angle (theta), and Mach number on selected j-lines
    use common_data, only: P, X, Y, JMIN, JMAX, JUP, JLOW, JERROR, CPFACT, VFACT, SIMDEF, PHYS, PRTFLO
    use common_data, only: JLIN
    implicit none
    integer :: JL, MPR, MPREND, M, MQ, I, J, IS
    real :: U
    real, dimension(3) :: YPRINT
    real, dimension(3) :: CPPR, PYPR
    real, dimension(3) :: EM1
    character(len=12), parameter :: HDR = "CP, THETA, M"

    ! Determine lines to print
    if (PRTFLO == 2) then
      JL = JMAX - JMIN + 1
      do M = 1, JL
        JLIN(M) = JMIN + M - 1
      end do
    else if (PRTFLO == 3) then
      ! three lines around JERROR
      if (JERROR <= JMIN+1) then
        JLIN = (/ JMIN, JMIN+1, JMIN+2 /)
      else if (JERROR >= JMAX-1) then
        JLIN = (/ JMAX-2, JMAX-1, JMAX /)
      else
        JLIN = (/ JERROR-1, JERROR, JERROR+1 /)
      end if
      JL = 3
    else
      return
    end if

    ! Print header
    print *
    print *, '--- Field data (', HDR, ') on selected lines ---'

    ! Loop pages of 3 lines
    do MPR = 1, JL, 3
      MPREND = min(MPR+2, JL)
      ! Y positions
      do MQ = MPR, MPREND
        YPRINT(MQ-MPR+1) = Y(JLIN(MQ))*VFACT
      end do
      print '(A)', 'Y =', (YPRINT(i), i=1, MPREND-MPR+1)
      ! Loop over I
      print '(A)', '   I   X   ' // trim(HDR) // ' on lines'
      write(*,'(2I6)') JLIN(MPR), JLIN(MPREND)
      do I = JMIN, IMAX
        write(*,'(I4,2X,F8.4)', advance='no') I, X(I)
        do MQ = MPR, MPREND
          J = JLIN(MQ)
          U = (P(J,I+1)-P(J,I-1))/(X(I+1)-X(I-1))
          CPPR(MQ-MPR+1) = -2.0 * CPFACT * U
          PYPR(MQ-MPR+1) = VFACT * U
          EM1(MQ-MPR+1) = U  ! placeholder for Mach calculation
          write(*,'(3F10.4)', advance='no') CPPR(MQ-MPR+1), PYPR(MQ-MPR+1), EM1(MQ-MPR+1)
        end do
        print *
      end do
    end do
  end subroutine PRTFLD

  subroutine PRTMC()
    ! Print map of flow types at each grid point, writing to file unit 15
    use common_data, only: P, IMIN, IMAX, IUP, IDOWN, JMIN, JMAX, IPC, VT, C1, CXL, CXC, CXR
    implicit none
    integer :: I, J, K
    character(len=1), parameter :: ch_par='P', ch_hyp='H', ch_shock='S', ch_ell='-'

    ! Header
    write(15,'(A)') '1FLOW AT EACH GRID POINT. P PARABOLIC, H HYPERBOLIC, S SHOCK, - ELLIPTIC'

    ! Initialize IPC and VT
    IPC(IUP:IDOWN) = ch_ell
    VT(JMIN:JMAX,1) = C1(2)

    ! Classify flow type and write each row to unit 15
    do K = JMIN, JMAX
      J = JMAX - K + 1
      do I = IUP, IDOWN
        VT(J,2) = VT(J,1)
        VT(J,1) = C1(I) - (CXL(I)*P(J,I-1) + CXC(I)*P(J,I) + CXR(I)*P(J,I+1))
        if (VT(J,1) <= 0.0) then
          if (VT(J,2) < 0.0) then
            IPC(I) = ch_hyp
          else
            IPC(I) = ch_par
          end if
        else
          if (VT(J,2) < 0.0) then
            IPC(I) = ch_shock
          else
            IPC(I) = ch_ell
          end if
        end if
      end do
      write(15,'(I3,5X,100A1)') J, IPC(IUP:IDOWN)
    end do
  end subroutine PRTMC

  subroutine PRTSK(Z,ARG,L,NSHOCK,CDSK,LPRT1)
    ! Print shock wave drag contributions and total pressure loss along shock wave
    use common_data, only: CDFACT, GAM1, DELTA, YFACT
    implicit none
    real, intent(in) :: Z(:), ARG(:)
    integer, intent(in) :: L, NSHOCK, LPRT1
    real, intent(inout) :: CDSK(:)
    real :: CDYCOF, POYCOF, YY, CDY, POY
    integer :: K

    ! Compute coefficients
    CDYCOF = -CDFACT * GAM1 / (6.0 * YFACT)
    POYCOF = DELTA**2 * GAM1 * (GAM1 - 1.0) / 12.0

    ! Header for individual shock profiles
    if (NSHOCK == 1) then
      write(15,'(A)') '1INVISCID WAKE PROFILES FOR INDIVIDUAL SHOCK WAVES WITHIN MOMENTUM CONTOUR'
    end if

    ! Print shock drag summary
    write(15,'(6H0SHOCK,I3,/26H WAVE DRAG FOR THIS SHOCK=,F12.6,/6X,1HY,9X,5HCD(Y),8X,8HPO/PO/)') NSHOCK, CDSK(NSHOCK)

    ! Print shock profile data
    do K = 1, L
      YY = Z(K) * YFACT
      CDY = CDYCOF * ARG(K)
      POY = 1.0 + POYCOF * ARG(K)
      write(15,'(1X,3F12.8)') YY, CDY, POY
    end do

    ! Footer if shock extends outside contour
    if (LPRT1 == 1) then
      write(15,'(35H0SHOCK WAVE EXTENDS OUTSIDE CONTOUR/61H PRINTOUT OF SHOCK LOSSES ARE NOT AVAILABLE FOR REST OF SHOCK)')
    end if
  end subroutine PRTSK

  subroutine PRTWAL()
    ! Print Cp and flow angles on tunnel walls (stub)
    implicit none
    print *, 'PRTWAL: wall Cp/angle print not implemented'
  end subroutine PRTWAL

  subroutine SAVEP()
    ! Save current solution P to restart arrays or file
    use common_data, only: P, X, Y
    implicit none
    print *, 'SAVEP: save solution not implemented'
  end subroutine SAVEP
end module io_module
