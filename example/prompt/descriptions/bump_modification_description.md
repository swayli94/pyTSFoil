
**Airfoil Bump Modification Strategy:**
This action adds localized bumps to both the upper and lower surfaces of the airfoil. You must specify the location, height, and width characteristics for these bumps to achieve desired aerodynamic properties.

**Physical Understanding:**
- **Upper Surface Bumps:** 
  * Positive height → increases local thickness and curvature (outward bump)
  * Negative height → decreases local thickness and curvature (inward bump)
- **Lower Surface Bumps:**
  * Positive height → decreases local thickness and curvature (outward bump)  
  * Negative height → increases local thickness and curvature (inward bump)

**Action Parameters:**
The modification consists of 6 parameters:
1. **UBL** (range: 0.01 to 0.99)
   - Controls the chordwise location of the bump on the upper surface
   - Position along chord: 0.0 = leading edge, 1.0 = trailing edge
   - Units: chord fraction
2. **UBH** (range: -0.005 to 0.005)
   - Controls the bump height of the bump on the upper surface
   - Positive = outward bump, negative = inward bump
   - Units: chord fraction
3. **UBW** (range: 0.2 to 0.8)
   - Controls the bump width of the bump on the upper surface
   - Controls bump spread: larger values = wider, more gradual bumps
   - Units: dimensionless
4. **LBL** (range: 0.01 to 0.99)
   - Controls the chordwise location of the bump on the lower surface
   - Position along chord: 0.0 = leading edge, 1.0 = trailing edge
   - Units: chord fraction
5. **LBH** (range: -0.01 to 0.01)
   - Controls the bump height of the bump on the lower surface
   - Positive = outward bump, negative = inward bump
   - Units: chord fraction
6. **LBW** (range: 0.2 to 0.8)
   - Controls the bump width of the bump on the lower surface
   - Controls bump spread: larger values = wider, more gradual bumps
   - Units: dimensionless

**Technical Implementation:**
- Uses Hicks-Henne bump functions for smooth, aerodynamically-reasonable modifications
- Bumps are only applied if their height exceeds the critical threshold (0.001) to avoid insignificant changes
- Maintains original maximum thickness during modification

