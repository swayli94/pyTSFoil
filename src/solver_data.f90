! solver_data.f90
! Data structure: 
!   - Control parameters
!   - Variables for post-processing
!   - Variables for numerical solver

module solver_data
    use common_data, only: N_MESH_POINTS, NMP_plus2, NMP_plus1
    implicit none


    real :: P(NMP_plus2, NMP_plus1)    ! Potential solution array

    real :: THETA(N_MESH_POINTS,N_MESH_POINTS)  ! Angle array for each mesh point
    real, parameter :: XSING = 0.5 ! Location of singular vortex and doublet (X=XSING, Y=0)
    integer, parameter :: KSTEP = 1 ! Step size for circulation-jump boundary update

    real :: CIRCFF = 0.0    ! Circulation at far field boundary
    real :: FHINV = 0.0     ! Inverse of Froude number
    real :: SONVEL = 0.0    ! Sonic velocity
    real :: DUB = 0.0       ! doublet strength

    real :: VFACT = 1.0 ! Scaling factor for velocity
    real :: YFACT = 1.0 ! Scaling factor for Y-coordinate

    real :: CYYC(N_MESH_POINTS), CYYD(N_MESH_POINTS), CYYU(N_MESH_POINTS) ! Boundary differencing coefficients
    real :: CYYBLC, CYYBLD, CYYBLU, CYYBUC, CYYBUD, CYYBUU ! Special boundary coefficient arrays

    real :: CXC(N_MESH_POINTS), CXL(N_MESH_POINTS), CXR(N_MESH_POINTS) ! (P)X central differencing coefficients
    real :: CXXC(N_MESH_POINTS), CXXL(N_MESH_POINTS), CXXR(N_MESH_POINTS) ! (P)XX central differencing coefficients
    real :: C1(N_MESH_POINTS) ! Velocity coefficient

    real :: DIAG(N_MESH_POINTS), RHS(N_MESH_POINTS) ! Tri-diagonal solver arrays

    real :: DTOP(N_MESH_POINTS), DBOT(N_MESH_POINTS) ! Far-field boundary arrays
    real :: DUP(N_MESH_POINTS), DDOWN(N_MESH_POINTS) ! Far-field boundary arrays
    real :: VTOP(N_MESH_POINTS), VBOT(N_MESH_POINTS) ! Far-field boundary arrays
    real :: VUP(N_MESH_POINTS), VDOWN(N_MESH_POINTS) ! Far-field boundary arrays

    real :: FXLBC(N_MESH_POINTS) = 0.0 ! Lower surface boundary condition array
    real :: FXUBC(N_MESH_POINTS) = 0.0 ! Upper surface boundary condition array

    real :: PJUMP(N_MESH_POINTS) = 0.0 ! Pressure jump array

    real :: POLD(N_MESH_POINTS,2) = 0.0     ! old potential values  
    real :: EMU(N_MESH_POINTS,2) = 0.0      ! circulation factors
    real :: WI = 1.05               ! SOR relaxation factor
    real :: CIRCTE = 0.0            ! Circulation at trailing edge (save the value for iterations)

    ! COM7: boundary extrapolation/coefficient flags
    real :: CJUP = 0.0, CJUP1 = 0.0, CJLOW = 0.0, CJLOW1 = 0.0
    
    ! COM13: coefficient scaling factors
    real :: CDFACT = 0.0, CLFACT = 0.0, CMFACT = 0.0, CPFACT = 0.0, CPSTAR = 0.0


    ! Control flags and refinement (from /COM3/)
    logical :: ABORT1 = .false. ! input abort flag

    ! Public variables for solver_functions
    real :: ALPHA0, ALPHA1, ALPHA2, OMEGA0, OMEGA1, OMEGA2, JET ! Far-field root parameters
    real :: B_COEF, BETA0, BETA1, BETA2, PSI0, PSI1, PSI2   ! Vortex/doublet parameters
    real :: WSLP(N_MESH_POINTS,2)   ! Viscous wedge slopes
    real :: RTKPOR = 0.0

end module solver_data


