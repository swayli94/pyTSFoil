! solver_base.f90
! Base functions (that are not affected by methods of scaling, correction, etc.):
!   - Finite difference functions
!   - Post-processing functions

module solver_base
    implicit none
    private

    ! Public functions
    public :: PX, PY

contains

    ! Computes U = DP/DX at point I,J
    function PX(I, J) result(result_px)
        use common_data, only: IMIN, IMAX, XDIFF
        use solver_data, only: P
        implicit none
        integer, intent(in) :: I, J
        real :: result_px
        real :: PJI=0.0
        
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
      
    ! Computes V = DP/DY at point I,J
    function PY(I, J) result(result_py)
        use common_data, only: JMIN, JMAX, JUP, JLOW, ILE, ITE
        use common_data, only: YDIFF, ALPHA, FXU, FXL
        use solver_data, only: P, PJUMP
        implicit none
        integer, intent(in) :: I, J
        real :: result_py
        real :: PJI=0.0, VMINUS=0.0, VPLUS=0.0
        integer :: IC=0
        
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
    
        
end module solver_base
