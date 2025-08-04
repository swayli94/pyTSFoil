
**FigureState: Comprehensive Airfoil State Representation**

This state representation combines parametric geometric features, aerodynamic coefficients, and visual information through wall Mach number distribution plots. The state consists of 17 parameters that comprehensively describe airfoil characteristics:

**State Parameters:**
1. **t_max** - maximum thickness
   - Range: 0.0 to 0.2
   - Units: dimensionless
2. **x_t_max** - x-coordinate of maximum thickness
   - Range: 0.2 to 0.8
   - Units: dimensionless
3. **volume** - volume
   - Range: 0.0 to 0.2
   - Units: dimensionless
4. **r_LE** - leading edge radius
   - Range: 0.0 to 0.05
   - Units: dimensionless
5. **a_LE** - leading edge slope angle (degree)
   - Range: 0.0 to 90.0
   - Units: degrees
6. **a_TEW** - trailing edge wedge angle (degree)
   - Range: 0.0 to 90.0
   - Units: degrees
7. **a_TES** - trailing edge slope angle (degree)
   - Range: 0.0 to 90.0
   - Units: degrees
8. **t_20p** - thickness at 20 percent chord
   - Range: 0.0 to 0.1
   - Units: dimensionless
9. **t_70p** - thickness at 70 percent chord
   - Range: 0.0 to 0.1
   - Units: dimensionless
10. **c_avg** - average camber
   - Range: 0.0 to 0.1
   - Units: dimensionless
11. **x_u_crest** - x-coordinate of upper crest point
   - Range: 0.2 to 0.8
   - Units: dimensionless
12. **y_u_crest** - y-coordinate of upper crest point
   - Range: 0.0 to 0.1
   - Units: dimensionless
13. **x_l_crest** - x-coordinate of lower crest point
   - Range: 0.2 to 0.8
   - Units: dimensionless
14. **y_l_crest** - y-coordinate of lower crest point
   - Range: -0.1 to 0.0
   - Units: dimensionless
15. **Cl** - lift coefficient
   - Range: 0.0 to 1.0
   - Units: dimensionless
16. **Cd_wave** - wave drag coefficient
   - Range: 0.0 to 0.05
   - Units: dimensionless
17. **Cm** - moment coefficient
   - Range: -0.5 to 0.5
   - Units: dimensionless

**Key Features:**
- Combines geometric parameters (thickness, camber, leading/trailing edge characteristics)
- Includes aerodynamic performance metrics (lift, drag, moment coefficients)
- Provides visual representation through geometry and wall Mach number distribution plots
- Wall Mach number distribution is another representation of the pressure coefficient distribution
- The area of the wall Mach number distribution indicates the lift
- The area of the wall Mach number distribution in the rear region indicates the magnitude of the pitching moment
- Sonic line (M=1) indicates the supersonic region (M>1) and subsonic region (M<1)
- The abrupt change of the wall Mach number distribution from M<1 to M>1 indicates the shock wave
- The change of wall Mach number in the shock wave indicates the wave drag (Cd_wave), which is a major source of drag that can be reduced.

