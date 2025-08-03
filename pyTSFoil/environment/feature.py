'''
Functions for feature extraction of airfoils.
'''

import copy
from typing import Tuple

import numpy as np
from scipy.interpolate import interp1d


def calculate_aft_loading(xxu: np.ndarray, xxl: np.ndarray, 
                cpu: np.ndarray, cpl: np.ndarray, x0_aft=0.7, x1_aft=1.0) -> float:
    '''
    Calculate airfoil aft loading, i.e., 
    the lift produced in the airfoil aft region (x in [0.7,1.0]).
    
    Parameters
    ---------------
    xxu, xxl: np.ndarray
        x-coordinates of the upper/lower surface mesh
    cpu, cpl: np.ndarray
        pressure coefficient of the upper/lower surface
    x0_aft, x1_aft: float
        x-coordinates of the aft region
        
    Returns
    --------------
    Cl_aft: float
        aft loading
    '''
    xx = np.linspace(x0_aft, x1_aft, 101)
    
    f_cpu = interp1d(xxu, cpu)
    f_cpl = interp1d(xxl, cpl)
    
    interpolated_delta_cp = f_cpl(xx) - f_cpu(xx)

    Cl_aft = 0.0
    for i in range(xx.shape[0]-1):
        Cl_aft += (xx[i+1]-xx[i]) * interpolated_delta_cp[i]

    return Cl_aft

def calculate_suction_peak(xxu: np.ndarray, mwu: np.ndarray, x1_LE=0.2) -> float:
    '''
    Calculate suction peak of airfoil upper surface, i.e., 
    the maximum wall Mach number in the airfoil leading edge region (x in [0.0,0.2]).
    
    Parameters
    ---------------
    xxu: np.ndarray
        x-coordinates of the upper surface mesh
    mwu: np.ndarray
        wall Mach number of the upper surface
    x1_LE: float
        x-coordinate of the trailing edge
        
    Returns
    --------------
    Mw_sp: float
        suction peak
    '''
    Mw_sp = 0.0
    
    for i in range(xxu.shape[0]):
        
        if xxu[i] > x1_LE:
            break
        
        Mw_sp = max(Mw_sp, mwu[i])

    return Mw_sp

def calculate_distance_to_line(x0: np.ndarray, x1: np.ndarray, x: np.ndarray) -> Tuple[float, float]:
    '''
    Calculate distance s to vector x1-x0.
    
    Parameters
    ---------------
    x0, x1: np.ndarray
        start and end point of the vector
    x: np.ndarray
        current point
        
    Returns
    --------------
    s: float
        distance to line
    t: float
        ratio of (projected |x0x|) / |x0x1|
    '''
    l0 = np.linalg.norm(x0-x1) + 1e-20
    l1 = np.linalg.norm(x0-x ) + 1e-20
    v  = (x1-x0)/l0
    l2 = np.dot(v, x-x0)
    t  = l2/l1
    s  = np.sqrt(l1**2 - l2**2 + 1E-9)

    return s, t

def calculate_isentropic_cp(mach: np.ndarray|float, Minf: float, g: float=1.4) -> np.ndarray|float:
    ''' 
    Calculate pressure coefficient by Mach number for isentropic flow.
    
    Parameters
    ---------------
    mach: np.ndarray|float
        Mach number
    Minf: float
        free stream Mach number
    g: float
        ratio of the specific heats
        
    Returns
    --------------
    cp: np.ndarray|float
        pressure coefficient
    '''
    xx = (2.0+(g-1.0)*Minf**2)/(2.0+(g-1.0)*mach**2)
    xx = xx**(g/(g-1.0))
    cp = 2.0/g/Minf**2*(xx-1.0)

    return cp

def calculate_wall_mach_number(cp: np.ndarray, Minf: float, n_ref_points: int=100, max_mach: float=2.0) -> np.ndarray:
    '''
    Convert pressure coefficient to wall Mach number.
    
    Parameters
    ---------------
    cp: np.ndarray
        pressure coefficient
    Minf: float
        free stream Mach number
    n_ref_points: int
        number of reference points
    max_mach: float
        maximum Mach number
        
    Returns
    --------------
    Mw: np.ndarray
        wall Mach number
    '''
    mach_ref = np.linspace(0.0, max_mach, n_ref_points)
    cp_ref = calculate_isentropic_cp(mach_ref, Minf)
    f   = interp1d(cp_ref, mach_ref, kind='cubic')
    cp_ = np.clip(cp, cp_ref[-1], cp_ref[0])
    return f(cp_)

def calculate_shape_factor(distance_to_wall: np.ndarray, velocity_parallel_to_wall: np.ndarray, 
                           wall_temperature: float, index_of_Ue: int, neglect_error: bool=False) -> Tuple[float, float]:
    '''
    Calculate shape factor Hi & Hc by mesh points on a line perpendicular to the wall.

    Parameters
    ---------------
    distance_to_wall: np.ndarray
        distance of mesh points to wall
    velocity_parallel_to_wall: np.ndarray
        velocity component of mesh points (parallel to the wall)
    wall_temperature: float
        wall temperature (K)
    index_of_Ue: int
        index of mesh point locating the outer velocity Ue
    neglect_error: bool
        if True, set shape factor to 0 when error occurs

    Returns
    --------------
    Hi: float
        incompressible shape factor
    Hc: float
        compressible shape factor

    Notes:  
    --------------
    XR  => Reference point on the wall; data points are considered along the normal direction nR from XR
    VtS => Velocity component of each data point in the direction normal to the wall
    displacement_thickness  => 𝛿*, displacement thickness (se)
    momentum_thickness  => θ, momentum thickness (tt)
    Ue  => Outer layer velocity component (parallel to the wall)
    
    Test results show that directly taking the maximum Ue is more reasonable; averaging over a certain range or using a fixed grid value is less effective.
    '''
    n_data_points = distance_to_wall.shape[0]
    Ue = velocity_parallel_to_wall[index_of_Ue]
    displacement_thickness = 0.0 # 𝛿*
    momentum_thickness = 0.0 # θ

    if index_of_Ue>=n_data_points or index_of_Ue<=int(0.2*n_data_points):
        if neglect_error:
            return 0.0, 0.0
        else:
            print('velocity_parallel_to_wall: ', velocity_parallel_to_wall)
            print('index_of_Ue: ', index_of_Ue)
            print('n_data_points: ', n_data_points, ' (n_data_points >= index_of_Ue >= 0.2*n_data_points)')
            raise Exception('Error: index_of_Ue not reasonable')

    for i in range(index_of_Ue-1):
        a1 = Ue-velocity_parallel_to_wall[i]
        a2 = Ue-velocity_parallel_to_wall[i+1]
        displacement_thickness += 0.5*(a1+a2)*(distance_to_wall[i+1]-distance_to_wall[i])

    for i in range(index_of_Ue-1):
        a1 = velocity_parallel_to_wall[i  ]*(Ue-velocity_parallel_to_wall[i  ])
        a2 = velocity_parallel_to_wall[i+1]*(Ue-velocity_parallel_to_wall[i+1])
        momentum_thickness += 0.5*(a1+a2)*(distance_to_wall[i+1]-distance_to_wall[i])

    Hi = displacement_thickness/momentum_thickness*Ue
    Hc = wall_temperature*Hi+wall_temperature-1

    return Hi, Hc

def calculate_shape_factor_from_field_data(X: np.ndarray, Y: np.ndarray, U: np.ndarray, V: np.ndarray, T: np.ndarray, 
          j0: int, j1: int, nHi: int, neglect_error: bool=False) -> Tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
    '''
    Calculate shape factor Hi & Hc from field data

    Parameters
    --------------
    X: np.ndarray (nj,nk)
        x-coordinates of the airfoil surface
    Y: np.ndarray (nj,nk)
        y-coordinates of the airfoil surface
    U: np.ndarray (nj,nk)
        x-component of velocity
    V: np.ndarray (nj,nk)
        y-component of velocity
    T: np.ndarray (nj,nk)
        temperature
    j0: int
        j index of the lower surface TE
    j1: int
        j index of the upper surface TE
    nHi: int
        maximum number of mesh points in k direction for boundary layer
    neglect_error: bool
        if True, set shape factor to 0 when error occurs

    Returns
    --------------
    Hi: np.ndarray (j1-j0)
        incompressible shape factor
    Hc: np.ndarray (j1-j0)
        compressible shape factor
    Tw: np.ndarray (j1-j0)
        wall temperature
    dudy: np.ndarray (j1-j0)
        du/dy

    Notes:
    --------------
    j: 1  - nj  from far field of lower surface TE to far field of upper surface TE
    j: j0 - j1  from lower surface TE to upper surface TE
    k: 1  - nk  from surface to far field (assuming perpendicular to the wall)
    '''

    iLE = int(0.5*(j0+j1))
    nj = X.shape[0]
    nk = X.shape[1]
    nn = j1-j0

    Hi = np.zeros(nn)
    Hc = np.zeros(nn)
    Tw = np.zeros(nn)
    dudy = np.zeros(nn)

    #* Locate boundary layer edge index iUe & calculate du/dy
    sS  = np.zeros([nn,nHi])
    VtS = np.zeros([nn,nHi])
    iUe = np.zeros(nn, dtype=int)

    for j in range(nn):
        jj = j0+j
        XR = np.array([X[jj,0], Y[jj,0]])
        tR = np.array([X[jj+1,0]-X[jj-1,0], Y[jj+1,0]-Y[jj-1,0]])
        tR = tR/np.linalg.norm(tR)
        if tR[0]<0.0:
            tR = -tR

        for i in range(nHi-1):
            XS = np.array([X[jj,i+1], Y[jj,i+1]])
            VS = np.array([U[jj,i+1], V[jj,i+1]])

            sS [j,i+1] = np.linalg.norm(XR-XS)
            VtS[j,i+1] = np.dot(tR,VS)

        iUe[j]  = np.argmax(np.abs(VtS[j,:]))
        dudy[j] = VtS[j,1]/sS[j,1]
        Tw[j]   = T[jj,0]

    #* Smooth iUe at shock wave foot
    nspan = 4
    for j in range(nn-2*nspan):
        jj = j+nspan
        r1 = 0.5*(iUe[jj-nspan]+iUe[jj+nspan])
        r2 = abs(iUe[jj+nspan]-iUe[jj-nspan])
        r3 = abs(iUe[jj]-iUe[jj-nspan]) + abs(iUe[jj]-iUe[jj+nspan])
        if r3>r2:
            iUe[jj] = int(r1)

    #* Calculate Hi & Hc
    for j in range(nn):
        Hi[j], Hc[j] = calculate_shape_factor(sS[j,:], VtS[j,:], 
                        Tw[j], iUe[j], neglect_error=neglect_error)

    #* Limit leading edge Hi
    r1 = 1.0
    r2 = 1.0
    r3 = 1.0
    r4 = 1.0
    for j in range(nn):
        jj = j0+j
        if (X[jj,0]-0.05)*(X[jj+1,0]-0.05)<=0.0 and jj<iLE:
            r1 = Hi[j]
            r3 = Hc[j]
        if (X[jj,0]-0.05)*(X[jj+1,0]-0.05)<=0.0 and jj>=iLE:
            r2 = Hi[j]
            r4 = Hc[j]

    for j in range(nn):
        jj = j0+j
        if X[jj,0]<0.05 and jj<iLE:
            Hi[j] = r1
            Hc[j] = r3
        if X[jj,0]<0.05 and jj>=iLE:
            Hi[j] = r2
            Hc[j] = r4

    return Hi, Hc, Tw, dudy

def locate_shock_wave(x: np.ndarray, mach: np.ndarray, 
                dMach: np.ndarray, d2Mach: np.ndarray, critical_dMach: float = -1.0) -> Tuple[int, int, int, int]:
    '''
    Find the shock wave location index (i_F, i_1, i_U, i_3) from the wall Mach number (Mw) distribution.

    Parameters
    ---------------
    x: np.ndarray
        x-coordinates of the airfoil geometry
    mach: np.ndarray
        wall Mach number of the airfoil geometry
    dMach: np.ndarray
        slope of wall Mach number
    d2Mach: np.ndarray
        second derivative of wall Mach number
    critical_dMach: float
        critical value of dMach to locate shock wave front

    Returns
    --------------
    i_F: int
        index of shock wave foot
    i_1: int
        index of shock wave front
    i_U: int
        index of local sonic position
    i_3: int
        index of just downstream the shock
    '''
    nn  = x.shape[0]

    #* F => shock foot position
    i_F = np.argmin(dMach)
    x_F = x[i_F]

    #* 1 => shock wave front position
    # Find the kink position of dMach in range [x_F-0.2, x_F], defined as dMach = -1
    i_1 = 0
    i_cri = 0
    i_md2 = 0
    for i in np.arange(i_F, 1, -1):

        # 1. Within the range of [x_F-0.2, x_F]
        if x[i]<x_F-0.2:
            break

        # 2. Locate dMach = critical_dMach (tend to go too much upstream)
        if dMach[i]>=critical_dMach and dMach[i+1]<critical_dMach and i_cri==0:
            i_cri = i

        # 3. Locate min d2Mach/dx2 (tend to go too much downstream)
        if d2Mach[i]<=d2Mach[i-1] and d2Mach[i]>d2Mach[i+1] and i_md2==0:
            i_md2 = i
    
    if i_md2-i_cri > 2*(i_F-i_md2):
        i_1 = i_md2
    elif 2*(i_md2-i_cri) < i_F-i_md2:
        i_1 = i_cri
    else:
        i_1 = int(0.5*(i_cri+i_md2))

    #* 3 => position of just downstream the shock
    # Find the first flat position of Mw in range [x_F, x_F+0.2], defined as dMach = 0 or -1
    i_3 = 0
    i_cri = 0
    i_md2 = 0
    i_flat = 0
    for i in np.arange(i_F, nn-1, 1):

        # 1. Within the range of [x_F, x_F+0.2]
        if x[i]>x_F+0.2:
            break

        # 2. Locate dMach = critical_dMach (tend to go too much downstream)
        if dMach[i]<=critical_dMach and dMach[i+1]>critical_dMach and i_cri==0:
            i_cri = i

        # 3. Locate min d2Mach/dx2 (tend to go too much upstream)
        if d2Mach[i]<=d2Mach[i-1] and d2Mach[i]>d2Mach[i+1] and i_md2==0:
            i_md2 = i

        # 4. Locate the first flat position of Mw
        if dMach[i]<=0.0 and dMach[i+1]>0.0:
            i_flat = i

    if i_flat!=0 and i_flat-i_F < 2*(i_cri-i_F):
        i_3 = i_flat
    elif i_cri-i_md2 > 2*(i_md2-i_F):
        i_3 = i_md2
    elif 2*(i_cri-i_md2) < i_md2-i_F:
        i_3 = i_cri
    else:
        i_3 = int(0.5*(i_cri+i_md2))

    #* U => local sonic position
    i_U = 0
    for i in np.arange(i_1, i_3, 1):
        if mach[i]>=1.0 and mach[i+1]<1.0:
            i_U = i
            break
    
    #* Neglect small Mw bump near leading edge
    if x[i_1]<0.1 and mach[i_1]<1.10:
        i_1=0; i_U=0; i_3=0

    return i_F, i_1, i_U, i_3

def check_single_shock_wave(x: np.ndarray, mach: np.ndarray, 
                dMach: np.ndarray, d2Mach: np.ndarray, critical_dMach: float = -1.0, 
                info: bool = False) -> Tuple[int, int, int, int, int]:
    '''
    Check whether is single shock wave or not
    
    Parameters
    --------------
    x: np.ndarray
        x-coordinates of the airfoil geometry
    mach: np.ndarray
        wall Mach number of the airfoil geometry
    dMach: np.ndarray
        slope of wall Mach number
    d2Mach: np.ndarray
        second derivative of wall Mach number
    critical_dMach: float
        critical value of dMach to locate shock wave front

    Returns
    --------------
    flag: int
        flag of shock wave. 1: single shock wave, 0: no shock wave, -1: multiple shock waves.
    i_F: int
        index of shock wave foot
    i_1: int
        index of shock wave front
    i_U: int
        index of local sonic position
    i_3: int
        index of just downstream the shock
    '''
    #* Get 1st shock
    i_F, i_1, i_U, i_3 = locate_shock_wave(x, mach, dMach, d2Mach, critical_dMach)
    d_F = dMach[i_F]

    #* Check if no shock wave
    # Check if Mw jump exists and M1>1.0
    if d_F>critical_dMach or mach[i_1]<1.0 or i_1==0:
        if info:
            print('  No shock wave: XF=%.2f MF=%.2f dM/dX=%.2f'%(x[i_F], mach[i_F], d_F))
        return 0, 0, 0, 0, 0

    #* Check if multiple shock waves exist
    # Remove first shock
    dm  = dMach.copy()
    d2m = d2Mach.copy()
    nn  = x.shape[0]
    for i in np.arange(i_F, nn, 1, dtype=int):
        if dm[i]<=0.0:
            dm[i]=0.0
            d2m[i]=0.0
        else:
            break
    for i in np.arange(i_F, 0, -1, dtype=int):
        if dm[i]<=0.0:
            dm[i]=0.0
            d2m[i]=0.0
        else:
            break
    
    # Locate second shock
    critical_dMach_F = max(critical_dMach, 0.5*d_F)
    _iF, _i1, _iU, _i3 = locate_shock_wave(x, mach, dm, d2m, critical_dMach)
    
    if dm[_iF]<critical_dMach_F and _i1!=0 and _i3!=0:
        # Locate sharp change of Mw

        if mach[_i1]>1.0 and mach[_i3]<1.05:
            # Check supersonic wave front and 'subsonic' wave hind
            if info:
                print('  Second shock: X1=%.2f M1=%.2f M2=%.2f'%(x[_i1], mach[_i1], mach[_i3]))
            return -1, 0, 0, 0, 0

    return 1, i_F, i_1, i_U, i_3

def locate_suction_peak(x: np.ndarray, mach: np.ndarray, critical_dMach: float = 1.0) -> Tuple[int, float]:
    '''
    Locate the index and position of suction peak near leading edge on upper surface,
    where x in [0, 0.2].

    Parameters
    --------------
    x: np.ndarray
        x-coordinates of the airfoil surface
    mach: np.ndarray
        wall Mach number on the airfoil surface
    critical_dMach: float
        critical value of dMach to locate suction peak

    Returns
    --------------
    ii: int
        index of suction peak
    _x: float
        x-coordinate of suction peak
    '''
    nn  = x.shape[0]

    ii = 0
    for i in range(nn):
        if x[i] <= 0.0:
            continue
        elif x[i] > 0.2:
            break
        else:
            if mach[i-1]<=mach[i] and mach[i]>=mach[i+1]:
                ii = i
                break

    if ii == 0:
        dMw2 = 0.0
        for i in range(nn):
            if x[i] <= 0.0:
                continue
            elif x[i] > 0.2:
                break
            else:
                dMw1 = dMw2
                dMw2 = (mach[i+1]-mach[i])/(x[i+1]-x[i])
                if dMw1>=critical_dMach and dMw2<critical_dMach:
                    ii = i
                    break

    return ii, x[ii]

def locate_maximum_mach(x: np.ndarray, mach: np.ndarray) -> Tuple[int, float]:
    '''
    Locate the index and position of maximum Mach number on the airfoil surface,
    where x in [0, 1.0].
    
    Parameters
    --------------
    x: np.ndarray
        x-coordinates of the airfoil surface
    mach: np.ndarray
        wall Mach number on the airfoil surface

    Returns
    --------------
    ii: int
        index of maximum Mach number
    _x: float
        x-coordinate of maximum Mach number
    '''
    nn  = x.shape[0]

    ii = 0
    for i in range(nn):
        if x[i] <= 0.0:
            continue
        elif x[i] > 1.0:
            break
        else:
            if mach[i-1]<=mach[i] and mach[i]>=mach[i+1] and mach[i]>max1:
                ii = i
                max1 = mach[i]

    return ii, x[ii]

def locate_separation_region(x: np.ndarray, dudy: np.ndarray) -> dict:
    '''
    Locate the index and position of separation and reattachment regions,
    where x in [0, 1.0].

    Parameters
    --------------
    x: np.ndarray
        x-coordinates of the airfoil surface
    dudy: np.ndarray
        slope of wall Mach number

    Returns
    --------------
    separation_dict: dict
        dictionary of separation and reattachment points.
        keys: 'S', 'R', 'mUy'
        values: [index, x-coordinate]
    '''
    nn  = x.shape[0]
    separation_dict = {}

    #* S => separation start position
    #* R => reattachment position
    #* mUy => position of min(du/dy)
    min_Uy = 1e6
    i_S = 0
    for i in range(nn):
        
        if x[i] <= 0.0:
            continue
        elif x[i] > 0.98:
            break

        if dudy[i]>=0.0 and dudy[i+1]<0.0 and i_S==0:
            i_S = i
            separation_dict['S'] = [i, (0.0-dudy[i])*(x[i+1]-x[i])/(dudy[i+1]-dudy[i])+x[i]]

        if dudy[i]<=0.0 and dudy[i+1]>0.0 and i_S!=0:
            separation_dict['R'] = [i, (0.0-dudy[i])*(x[i+1]-x[i])/(dudy[i+1]-dudy[i])+x[i]]
    
        if dudy[i]<min_Uy and dudy[i-1]>=dudy[i] and dudy[i+1]>=dudy[i]:
            min_Uy = dudy[i]
            separation_dict['mUy'] = [i, x[i]]
    
    return separation_dict

def locate_geometry_related_features(x: np.ndarray, 
                yu: np.ndarray, yl: np.ndarray, AoA: float) -> dict:
    '''
    Locate the index and position of geometry related flow features,
    where x in [0, 1.0].

    Parameters
    --------------
    x: np.ndarray
        x-coordinates of the airfoil surface
    yu: np.ndarray
        upper surface coordinates
    yl: np.ndarray
        lower surface coordinates
    AoA: float
        angle of attack (deg)

    Returns
    --------------
    geometry_dict: dict
        dictionary of geometry related flow features.
        keys: 'Cu', 'Cl', 'tu', 'tl', 'tm'
        values: [index, x-coordinate]
    '''
    n0 = x.shape[0]
    geometry_dict = {}
    
    #* tm => maximum thickness
    #* tu => highest point on upper surface
    #* tl => lowest point on lower surface
    x_max = x[np.argmax(yu-yl)]
    x_mu  = x[np.argmax(yu)]
    x_ml  = x[np.argmin(yl)]

    geometry_dict['tm'] = [np.argmin(np.abs(x-x_max)), x_max]
    geometry_dict['tu'] = [np.argmin(np.abs(x-x_mu)), x_mu]
    geometry_dict['tl'] = [np.argmin(np.abs(x-x_ml)), x_ml]

    #* Cu => crest point on upper surface
    aa = AoA/180.0*np.pi
    x0 = np.array([0.0, 0.0])
    x1 = np.array([np.cos(aa), np.sin(aa)])

    ds = np.zeros(n0)
    for i in range(n0):
        xt = np.array([x[i], yu[i]])
        if x[i] > 0.9:
            continue
        ds[i], _ = calculate_distance_to_line(x0, x1, xt)
    ii = np.argmax(ds)

    geometry_dict['Cu'] = [np.argmin(np.abs(x-x[ii])), x[ii]]

    #* Cl => crest point on lower surface
    ds = np.zeros(n0)
    for i in range(n0):
        if x[i] > 0.9:
            continue
        xt = np.array([x[i], yl[i]])
        ds[i], _ = calculate_distance_to_line(x0, x1, xt)
    ii = np.argmax(ds)

    geometry_dict['Cl'] = [np.argmin(np.abs(x-x[ii])), x[ii]]

    return geometry_dict

def locate_boundary_layer_related_features(x: np.ndarray, hi: np.ndarray) -> dict:
    '''
    Locate the index and position of boundary layer related flow features,
    where x in [0, 1.0].

    Parameters
    --------------
    x: np.ndarray
        x-coordinates of the airfoil surface
    hi: np.ndarray
        boundary layer thickness

    Returns
    --------------
    boundary_layer_dict: dict
        dictionary of boundary layer related flow features.
        keys: 'N', 'Hi'
        values: [index, x-coordinate]
    '''
    nn  = x.shape[0]
    boundary_layer_dict = {}

    #* Hi, Hc => position of maximum Hi, Hc after shock wave front
    # For cases when shock wave is weak, and Hc just keeps growing, set 0
    i_H = 0
    max1 = 0.0
    for i in range(nn):

        if x[i] <= 0.1:
            continue
        elif x[i] > 0.9:
            break

        if hi[i-1]<=hi[i] and hi[i+1]<=hi[i] and hi[i]>max1:
            max1 = hi[i]
            i_H = i
            
    x_H = x[i_H]
    boundary_layer_dict['Hi'] = [np.argmin(np.abs(x-x_H)), x_H]

    #* N => starting position of new flat boundary
    # i.e., position of minimum Hc after shock wave front
    # For cases when shock wave is weak, and Hc just keeps growing, set 0
    i_N = 0
    min1 = 1000.0
    for i in range(nn):
        if x[i] <= 0.1:
            continue
        elif x[i] > 0.9:
            break

        if hi[i-1]>=hi[i] and hi[i+1]<=hi[i] and hi[i]<min1:
            min1 = hi[i]
            i_N = i

    x_N = x[i_N]
    boundary_layer_dict['N'] = [np.argmin(np.abs(x-x_N)), x_N]

    return boundary_layer_dict


#TODO
class AirfoilFeature():
    '''
    Extracting flow features of an airfoil
    
    Parameters
    --------------
    Minf: float
        Free stream Mach number
    AoA: float
        Angle of attack (deg)
    Re: float
        Reynolds number per meter
    '''
    def __init__(self, Minf: float, AoA: float, Re: float):

        self.Minf = Minf
        self.AoA  = AoA
        self.Re   = Re
        
        self.feature_dict = {
            'Cu': {'index': 0, 'x': 0.0, 'meaning': 'crest point on upper surface'},
            'Cl': {'index': 0, 'x': 0.0, 'meaning': 'crest point on lower surface'},
            'tu': {'index': 0, 'x': 0.0, 'meaning': 'highest point on upper surface'},
            'tl': {'index': 0, 'x': 0.0, 'meaning': 'lowest point on lower surface'},
            'tm': {'index': 0, 'x': 0.0, 'meaning': 'maximum thickness position'},
            
            'L': {'index': 0, 'x': 0.0, 'meaning': 'suction peak near leading edge on upper surface'},
            'Q': {'index': 0, 'x': 0.0, 'meaning': 'suction peak near leading edge on lower surface'},
            'H': {'index': 0, 'x': 0.0, 'meaning': 'upper surface maximum Mach number'},
            'M': {'index': 0, 'x': 0.0, 'meaning': 'lower surface maximum Mach number'},
            
            'S': {'index': 0, 'x': 0.0, 'meaning': 'separation start position'},
            'R': {'index': 0, 'x': 0.0, 'meaning': 'reattachment position'},
            'mUy': {'index': 0, 'x': 0.0, 'meaning': 'position of min(du/dy)'},
            
            'F': {'index': 0, 'x': 0.0, 'meaning': 'shock foot position'},
            '1': {'index': 0, 'x': 0.0, 'meaning': 'shock front position'},
            '3': {'index': 0, 'x': 0.0, 'meaning': 'shock hind position'},
            'D': {'index': 0, 'x': 0.0, 'meaning': 'dent on plateau position'},
            'U': {'index': 0, 'x': 0.0, 'meaning': 'local sonic position'},
            'B': {'index': 0, 'x': 0.0, 'meaning': 'first dent just after suction peak, [X_L, X_L+0.1]'},
            # Note: for weak shock waves, may not reach Mw=1, 
            # define position of U as Mw minimal extreme point after shock foot
            
            'A': {'index': 0, 'x': 0.0, 'meaning': 'maximum Mw after shock'},
            'N': {'index': 0, 'x': 0.0, 'meaning': 'starting position of new flat boundary'}, # most of the time, A == N
            'Hi': {'index': 0, 'x': 0.0, 'meaning': 'maximum Hi'},
            
            'L1U': {'value': 0.0, 'meaning': 'length 1~U'},
            'L13': {'value': 0.0, 'meaning': 'length 1~3'}, 
            'LSR': {'value': 0.0, 'meaning': 'length S~R'},
            'lSW': {'value': 0.0, 'meaning': 'single shock wave flag (1: single shock wave, 0: no shock wave, -1: multiple shock waves)'},
            'DCp': {'value': 0.0, 'meaning': 'shock strength (Cp change through shock wave)'},
            'Err': {'value': 0.0, 'meaning': 'suction plateau Cp area (Cp integral of suction plateau fluctuation)'},
            'FSp': {'value': 0.0, 'meaning': 'Mw fluctuation of suction plateau'},
            'DMp': {'value': 0.0, 'meaning': 'delta Mw of the dent on suction plateau'},
            'CLU': {'value': 0.0, 'meaning': 'CL of upper surface'},
            'CLL': {'value': 0.0, 'meaning': 'CL of lower surface'},
            'CdU': {'value': 0.0, 'meaning': 'Cdp of upper surface'},
            'CdL': {'value': 0.0, 'meaning': 'Cdp of lower surface'},
            'CLw': {'value': 0.0, 'meaning': 'windward CL (before crest point)'},
            'Cdw': {'value': 0.0, 'meaning': 'windward Cdp (before crest point)'},
            'CLl': {'value': 0.0, 'meaning': 'leeward CL (behind crest point)'},
            'Cdl': {'value': 0.0, 'meaning': 'leeward Cdp (behind crest point)'},
            'kaf': {'value': 0.0, 'meaning': 'average Mw slope of the aft upper surface (3/N~T)'},
        }

    def setdata(self, x, y, Cp, Tw, Hi, Hc, dudy):
        '''
        Set the data of this foil or section.

        Data:   ndarray, start from lower surface trailing edge
        '''
        self.x = copy.deepcopy(x)
        self.y = copy.deepcopy(y)
        self.Cp = copy.deepcopy(Cp)
        self.Mw = calculate_wall_mach_number(self.Cp, self.Minf)
        self.Tw = copy.deepcopy(Tw)
        self.Hi = copy.deepcopy(Hi)
        self.Hc = copy.deepcopy(Hc)
        self.dudy = copy.deepcopy(dudy)

        iLE = np.argmin(self.x)
        self.x -= self.x[iLE]
        self.y -= self.y[iLE]
        self.x[0] = 1.0
        self.x[-1] = 1.0

        fmw = interp1d(self.x[iLE:], self.Mw[iLE:], kind='cubic')
        fhu = interp1d(self.x[iLE:], self.Hc[iLE:], kind='cubic')
        gu  = interp1d(self.x[iLE:], self.y [iLE:], kind='cubic')
        x_  = np.append(self.x[iLE:0:-1], self.x[0])
        y_  = np.append(self.y[iLE:0:-1], self.y[0])
        gl  = interp1d(x_, y_, kind='cubic')

        self.xx = np.arange(0.0, 1.0, 0.001)
        self.yu = gu(self.xx)
        self.yl = gl(self.xx)
        self.mu = fmw(self.xx)
        self.hu = fhu(self.xx)
        
        self.iLE = iLE

    def set_Mw(self, x, Mw):
        '''
        Set the Mw distribution of this foil or section.

        Data:   ndarray, start from lower surface trailing edge
        '''
        self.x  = copy.deepcopy(x)
        self.Mw = copy.deepcopy(Mw)

        iLE = np.argmin(self.x)
        self.iLE = iLE

        fmw = interp1d(self.x[iLE:], self.Mw[iLE:], kind='cubic')
        self.xx = np.arange(0.0, 1.0, 0.001)
        self.mu = fmw(self.xx)



    def locate_shock(self, critical_dMach=-1.0, info=False):
        '''
        Locate the index and position of shock wave related flow features.

        ### Get value of: 1, 3, F, U, D, A, B
        
        ### Inputs:
        ```text
        critical_dMach: critical value locating shock wave front
        ```
        '''
        X   = self.x        # [n]
        xx  = self.xx       # [1000]
        mu  = self.mu       # [1000]
        nn  = xx.shape[0]
        iLE = self.iLE

        dMach = np.zeros(nn)
        for i in range(nn-1):
            if xx[i]<=0.02:
                continue
            if xx[i]>=0.98:
                continue
            dMach[i] = (mu[i+1]-mu[i])/(xx[i+1]-xx[i])
            dMach[i] = min(dMach[i], 2)

        d2Mach = np.zeros(nn)
        for i in range(nn-1):
            if xx[i]<0.02 or xx[i]>0.95:
                continue
            
            #d2Mach[i] = (dMach[i+2]+dMach[i+1]-dMach[i]-dMach[i-1])/2/(xx[i+1]-xx[i-1])
            #d2Mach[i] = (dMach[i+1]-dMach[i-1])/(xx[i+1]-xx[i-1])
            d2Mach[i] = (0.5*dMach[i+7]+0.5*dMach[i+4]+2*dMach[i+1]-
                        2*dMach[i]-0.5*dMach[i-3]-0.5*dMach[i-6])/4.5/(xx[i+1]-xx[i-1])

        #* Check shock and shock properties
        flag, i_F, i_1, i_U, i_3 = check_single_shock_wave(xx, mu, dMach, d2Mach, critical_dMach, info=info)

        self.xf_dict['lSW'][1] = flag
        if not flag==1:
            return 0

        #* F => shock foot position
        self.xf_dict['F'][1] = np.argmin(np.abs(X[iLE:]-xx[i_F])) + iLE
        self.xf_dict['F'][2] = xx[i_F]

        #* 1 => shock wave front position
        self.xf_dict['1'][1] = np.argmin(np.abs(X[iLE:]-xx[i_1])) + iLE
        self.xf_dict['1'][2] = xx[i_1]

        #* 3 => position of just downstream the shock
        self.xf_dict['3'][1] = np.argmin(np.abs(X[iLE:]-xx[i_3])) + iLE
        self.xf_dict['3'][2] = xx[i_3]

        #* U => local sonic position
        self.xf_dict['U'][1] = np.argmin(np.abs(X[iLE:]-xx[i_U])) + iLE
        self.xf_dict['U'][2] = xx[i_U]

        #* D => dent on the suction plateau
        # maximum (linear Mw - actual Mw) between L and 1
        x_1 = self.xf_dict['1'][2]
        x_L = max(self.xf_dict['L'][2], 0.05)
        m_1 = self.getValue('1','Mw')
        m_L = self.getValue('L','Mw')
        lL1 = x_1-x_L
        i_D = 0
        min_D = 0.0
        for i in np.arange(2, i_1-1, 1):

            if xx[i]<x_L:
                continue

            tt = (xx[i]-x_L)/lL1
            ss = (1-tt)*m_L + tt*m_1
            dM = ss - mu[i]

            if dM > min_D:
                i_D = i
                min_D = dM

        if i_D==0:
            self.xf_dict['D'][1] = self.xf_dict['L'][1]
            self.xf_dict['D'][2] = self.xf_dict['L'][2]
        else:
            self.xf_dict['D'][1] = np.argmin(np.abs(X[iLE:]-xx[i_D])) + iLE
            self.xf_dict['D'][2] = xx[i_D]

        #* B => first dent after suction peak [X_L, X_L+0.1]
        # minimum Mw between L and L+0.1
        x_L = self.xf_dict['L'][2]
        i_B = 0
        for i in np.arange(2, i_1-1, 1):

            if xx[i]<x_L or xx[i]>x_L+0.1:
                continue

            if mu[i-1]>=mu[i] and mu[i]<=mu[i+1] and i_B==0:
                i_B = i

        if i_B == 0:
            self.xf_dict['B'][1] = self.xf_dict['L'][1]
            self.xf_dict['B'][2] = self.xf_dict['L'][2]
        else:
            self.xf_dict['B'][1] = np.argmin(np.abs(X[iLE:]-xx[i_B])) + iLE
            self.xf_dict['B'][2] = xx[i_B]

        #* A => maximum Mw after shock
        # Find the maximum position of Mw in range [x_3, 0.9]
        i_A = 0
        max_A = 0.0
        for i in np.arange(i_3, nn-1, 1):
            if xx[i]>0.9:
                break
            if mu[i]>max_A:
                i_A = i
                max_A = mu[i]
            elif mu[i]>=mu[i_3]*0.8 and mu[i]>mu[i-1] and mu[i]>mu[i+1]:
                i_A = i

        x_A = xx[i_A]
        self.xf_dict['A'][1] = np.argmin(np.abs(X[iLE:]-x_A)) + iLE
        self.xf_dict['A'][2] = x_A

        return i_1


    def aux_features(self):
        '''
        Calculate auxiliary features based on basic, geo, and shock features.

        ### Get value of: Length, lSW, DCp, Err, DMp, FSp, kaf, 
        ### CLU, CLL, CLw, Cdw, CLl, Cdl
        '''
        X  = self.x
        Y  = self.y
        x1 = self.xf_dict['1'][2]
        n0 = len(X)
        
        self.xf_dict['L1U'][1] = self.xf_dict['U'][2] - x1
        self.xf_dict['L13'][1] = self.xf_dict['3'][2] - x1
        self.xf_dict['LSR'][1] = self.xf_dict['R'][2] - self.xf_dict['S'][2]
        self.xf_dict['DCp'][1] = self.getValue('3','Cp') - self.getValue('1','Cp')

        cosA = np.cos(self.AoA/180.0*np.pi)
        sinA = np.sin(self.AoA/180.0*np.pi)
        #* Err => Cp integral of suction plateau fluctuation
        #* DMp => Mw dent on suction plateau
        #* FSp => Mw fluctuation of suction plateau
        # If can not find suction peak, err = 0, DMp = 0.0, FSp = 0.0
        Err = 0.0
        DMp = 0.0
        FSp = 0.0
        iL  = self.xf_dict['L'][1]
        if iL!=0:
            i1 = self.xf_dict['1'][1]
            xL  = self.xf_dict['L'][2]

            Cp0 = np.array([xL, self.getValue('L','Cp')])
            Cp1 = np.array([x1, self.getValue('1','Cp')])

            Mw0 = self.getValue('L','Mw')
            Mw1 = self.getValue('1','Mw')
            lL1 = x1-xL
            bump_ = 0.0
            dent_ = 0.0

            for i in np.arange(iL, i1, 1):

                vec = np.array([X[i], self.Cp[i]])
                s, _ = calculate_distance_to_line(Cp0, Cp1, vec)
                Err += s*(X[i+1]-X[i])

                tt = (X[i]-xL)/lL1
                ss = (1-tt)*Mw0 + tt*Mw1
                DMp = max(DMp, ss-self.Mw[i])

                local_avg_mw = (self.Mw[i-2]+self.Mw[i]+self.Mw[i+2])/3.0

                if self.Mw[i-4]>=local_avg_mw and local_avg_mw<=self.Mw[i+4] and dent_<=0.0:
                    if bump_>0.0:
                        FSp += bump_ - local_avg_mw
                    dent_ = local_avg_mw
                    bump_ = 0.0
                elif self.Mw[i-4]<=local_avg_mw and local_avg_mw>=self.Mw[i+4] and bump_<=0.0:
                    if dent_>0.0:
                        FSp += local_avg_mw - dent_
                    bump_ = local_avg_mw
                    dent_ = 0.0

        self.xf_dict['Err'][1] = abs(Err)*cosA
        self.xf_dict['DMp'][1] = DMp
        self.xf_dict['FSp'][1] = FSp

        #* kaf => average Mw slope of the aft upper surface (3/N~T)
        xN  = self.xf_dict['N'][2]
        mN  = self.getValue('N','Mw')
        xT  = self.xf_dict['T'][2]
        mT  = self.getValue('T','Mw')
        if xN < 0.1:
            xN = self.xf_dict['3'][2]
            mN = self.getValue('3','Mw')

        self.xf_dict['kaf'][1] = (mT-mN)/(xT-xN)

        #* CLU => CL of upper surface
        # wall vector = [dx,dy]
        # outward wall vector = [-dy,dx]
        # outward pressure force vector = Cp*[dy,-dx]
        PFy = 0.0   # y direction pressure force
        PFx = 0.0   # x direction pressure force

        for i in np.arange(self.iLE, n0-1, 1):
            Cp_ = 0.5*(self.Cp[i]+self.Cp[i+1])
            PFx += Cp_*(Y[i+1]-Y[i])
            PFy += Cp_*(X[i]-X[i+1])
        self.xf_dict['CLU'][1] = PFy*cosA - PFx*sinA
        self.xf_dict['CdU'][1] = PFy*sinA + PFx*cosA
        
        PFx = 0.0; PFy = 0.0
        for i in np.arange(0, self.iLE, 1):
            Cp_ = 0.5*(self.Cp[i]+self.Cp[i+1])
            PFx += Cp_*(Y[i+1]-Y[i])
            PFy += Cp_*(X[i]-X[i+1])
        self.xf_dict['CLL'][1] = PFy*cosA - PFx*sinA
        self.xf_dict['CdL'][1] = PFy*sinA + PFx*cosA

        #* Windward and leeward pressure force (CL, Cdp)
        icu = self.xf_dict['Cu'][1]
        icl = self.xf_dict['Cl'][1]

        PFx = 0.0; PFy = 0.0
        for i in np.arange(0, icl, 1):          # Leeward (lower surface)
            Cp_  = 0.5*(self.Cp[i]+self.Cp[i+1])
            PFx += Cp_*(Y[i+1]-Y[i])
            PFy += Cp_*(X[i]-X[i+1])
        for i in np.arange(icu, n0-1, 1):       # Leeward (upper surface)
            Cp_  = 0.5*(self.Cp[i]+self.Cp[i+1])
            PFx += Cp_*(Y[i+1]-Y[i])
            PFy += Cp_*(X[i]-X[i+1])
        self.xf_dict['CLl'][1] = PFy*cosA - PFx*sinA
        self.xf_dict['Cdl'][1] = PFy*sinA + PFx*cosA

        PFx = 0.0; PFy = 0.0
        for i in np.arange(icl, icu, 1):        # Windward
            Cp_ = 0.5*(self.Cp[i]+self.Cp[i+1])
            PFx += Cp_*(Y[i+1]-Y[i])
            PFy += Cp_*(X[i]-X[i+1])
        self.xf_dict['CLw'][1] = PFy*cosA - PFx*sinA
        self.xf_dict['Cdw'][1] = PFy*sinA + PFx*cosA



