
**Global Geometric Modification Strategy:**
This action performs global modifications to airfoil geometry by adjusting fundamental shape characteristics. Unlike local bump modifications, these changes affect the overall airfoil shape through systematic parameter adjustments that maintain aerodynamic smoothness and physical realizability.

**Physical Understanding:**
Global modifications work by adjusting the underlying mathematical representation of the airfoil:
- **Thickness Parameters:** Control the overall thickness distribution and maximum thickness location
- **Camber Parameters:** Adjust the mean line curvature and camber distribution
- **Edge Parameters:** Modify leading and trailing edge characteristics for suction peak and separation control
- **Localized Thickness:** Fine-tune thickness at specific chord locations for pressure distribution control

**Action Parameters:**
The modification consists of 11 parameters organized by function:
1. **dTHK** - airfoil THickness
   - Range: ±0.0 (chord fraction)
   - Minimum increment: 0.0005
   - Category: Thickness Control
   - Effect: Changes overall maximum thickness - affects structural capacity, drag characteristics, and internal volume. Positive increases thickness, negative decreases thickness.
2. **dCAM** - airfoil CAMber
   - Range: ±0.002 (chord fraction)
   - Minimum increment: 0.0005
   - Category: Camber Control
   - Effect: Modifies overall camber (mean line curvature) - directly impacts lift generation and pitching moment. Positive increases camber, negative decreases camber.
3. **dMTL** - Maximum airfoil Thickness Location
   - Range: ±0.05 (chord fraction)
   - Minimum increment: 0.01
   - Category: Thickness Control
   - Effect: Shifts chordwise position of maximum thickness - affects pressure distribution, transition location, and structural efficiency. Positive moves aft, negative moves forward.
4. **dCF6** - average Camber of Front 60 percent of the airfoil
   - Range: ±0.001 (chord fraction)
   - Minimum increment: 0.0005
   - Category: Camber Control
   - Effect: Adjusts average camber of front 60% of airfoil - controls early flow acceleration and nose-down pitching moment. Critical for lift coefficient and stall characteristics.
5. **dCR4** - average Camber of Rear 40 percent of the airfoil
   - Range: ±0.001 (chord fraction)
   - Minimum increment: 0.0002
   - Category: Camber Control
   - Effect: Modifies average camber of rear 40% of airfoil - influences flow recovery and trailing edge pressure. Affects lift effectiveness and moment characteristics.
6. **dLER** - Leading Edge Radius
   - Range: ±0.003 (chord fraction)
   - Minimum increment: 0.001
   - Category: Edge Geometry
   - Effect: Changes leading edge radius - affects stagnation point size, pressure gradient, and boundary layer behavior. Larger radius improves high-angle performance.
7. **dLES** - Leading Edge Slope angle (degree)
   - Range: ±2.0 (degrees)
   - Minimum increment: 0.8
   - Category: Edge Geometry
   - Effect: Adjusts leading edge slope angle - controls flow acceleration around the nose and transition behavior. Steeper angles increase acceleration.
8. **dTEW** - Trailing Edge Wedge angle (degree)
   - Range: ±4.0 (degrees)
   - Minimum increment: 1.5
   - Category: Edge Geometry
   - Effect: Modifies trailing edge wedge angle - affects wake formation, base pressure, and overall drag. Smaller wedge angles typically reduce pressure drag.
9. **dTES** - Trailing Edge Slope angle (degree)
   - Range: ±0.5 (degrees)
   - Minimum increment: 0.2
   - Category: Edge Geometry
   - Effect: Changes trailing edge slope angle - influences flow separation point and wake characteristics. Affects pressure recovery in trailing edge region.
10. **dTH2** - THickness at 20 percent chord
   - Range: ±0.004 (chord fraction)
   - Minimum increment: 0.002
   - Category: Thickness Control
   - Effect: Adjusts thickness at 20% chord - controls forward loading and pressure distribution. Affects boundary layer development and transition location.
11. **dTH7** - THickness at 70 percent chord
   - Range: ±0.005 (chord fraction)
   - Minimum increment: 0.002
   - Category: Thickness Control
   - Effect: Modifies thickness at 70% chord - influences aft loading and pressure recovery. Critical for adverse pressure gradient management and separation control.

**Technical Implementation:**
- Actions are only applied if they exceed their minimum increment threshold (0.0002 minimum) to avoid imperceptible changes
- Modifications are relative to the current airfoil geometry (additive/delta changes)

