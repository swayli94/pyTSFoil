'''
Module for feature extraction of airfoils.
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

def getHi(X, Y, U, V, T, j0: int, j1: int, nHi: int, neglect_error=False):
    '''
    Calculate shape factor Hi & Hc from field data

    ### Inputs:
    ```text
    Field data: ndarray (nj,nk), X, Y, U, V, T
    j0:     j index of the lower surface TE
    j1:     j index of the upper surface TE
    nHi:    maximum number of mesh points in k direction for boundary layer
    neglect_error:  if True, set shape factor to 0 when error occurs
    ```

    ### Return:
    ```text
    Hi, Hc: ndarray (j1-j0)
    info:   tuple of ndarray (Tw, dudy)
    ```

    ### Note:
    ```text
    Tw:     wall temperature
    dudy:   du/dy
    iUe:    index of mesh point locating the outer velocity Ue
    XR:     reference position on the wall
    ```

    ### Filed data (j,k) index
    ```text
    j: 1  - nj  from far field of lower surface TE to far field of upper surface TE
    j: j0 - j1  from lower surface TE to upper surface TE
    k: 1  - nk  from surface to far field (assuming pertenticular to the wall)
    ```
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

    return Hi, Hc, (Tw, dudy)








class AirfoilFeature():
    '''
    Extracting flow features of an airfoil
    '''
    _i  = 0     # index of the mesh point
    _X  = 0.0   # location of the feature location
    _value = 0.0

    #* Dictionary of flow features (identify the index and location)
    xf_dict = {
        'Cu':  ['upper crest', _i, _X],             # crest point on upper surface
        'Cl':  ['lower crest', _i, _X],             # crest point on lower surface
        'tu':  ['upper highest', _i, _X],           # highest point on upper surface
        'tl':  ['lower highest', _i, _X],           # lowest point on lower surface
        'tm':  ['max thickness', _i, _X],           # maximum thickness position

        'L': ['upper LE', _i, _X],                  # suction peak near leading edge on upper surface
        'T': ['upper TE', _i, _X],                  # trailing edge upper surface (98% chord length)
        'H': ['upper surface max Ma', _i, _X],      # position of lower upper maximum Mach number
        'S': ['separation start', _i, _X],          # separation start position
        'R': ['reattachment', _i, _X],              # reattachment position
        'Q': ['lower LE', _i, _X],                  # suction peak near leading edge on lower surface
        'M': ['lower surface max Ma', _i, _X],      # position of lower surface maximum Mach number
        'mUy': ['min(du/dy)', _i, _X],              # position of min(du/dy)

        'F': ['shock foot', _i, _X],                # shock foot position
        '1': ['shock front', _i, _X],               # shock wave front position
        '3': ['shock hind', _i, _X],                # position of just downstream the shock
        'D': ['dent on plateau', _i, _X],           # largest dent on the suction plateau
        'U': ['local sonic', _i, _X],               # local sonic position
        'B': ['1st dent after L', _i, _X],          # first dent after suction peak [X_L, X_L+0.1]
        #                                           # Note: for weak shock waves, may not reach Mw=1
        #                                           #       define position of U as Mw minimal extreme point after shock foot
        'A': ['maximum Mw after shock', _i, _X],    # maximum wall Mach number after shock wave (or equal to 3)
        'N': ['new flat boundary', _i, _X],         # starting position of new flat boundary
        #                                           # most of the time, A == N
        'Hi':  ['maximum Hi', _i, _X],              # position of maximum Hi
        'Hc':  ['maximum Hc', _i, _X],              # position of maximum Hc
        
        'L1U': ['length 1~U', _value],              # XU-X1
        'L13': ['length 1~3', _value],              # X3-X1
        'LSR': ['length S~R', _value],              # XR-XS
        'lSW': ['single shock', _value],            # single shock wave flag
        'DCp': ['shock strength', _value],          # Cp change through shock wave
        'Err': ['suc Cp area', _value],             # Cp integral of suction plateau fluctuation
        'FSp': ['fluctuation suc-plat', _value],    # Mw fluctuation of suction plateau
        'DMp': ['Mw dent on plateau', _value],      # dMw of Mw dent on suction plateau
        'CLU': ['upper CL', _value],                # CL of upper surface
        'CLL': ['lower CL', _value],                # CL of lower surface
        'CdU': ['upper Cd', _value],                # Cdp of upper surface
        'CdL': ['lower Cd', _value],                # Cdp of lower surface
        'CLw': ['windward CL', _value],             # CL of windward surfaces (before crest point)
        'Cdw': ['windward Cdp', _value],            # Cdp of windward surfaces (before crest point)
        'CLl': ['leeward CL', _value],              # CL of leeward surfaces (behind crest point)
        'Cdl': ['leeward Cdp', _value],             # Cdp of leeward surfaces (behind crest point)
        'kaf': ['slope aft', _value]                # average Mw slope of the aft upper surface (3/N~T)
    }

    def __init__(self, Minf, AoA, Re):
        '''
        ### Inputs:
        ```text
        Minf:       Free stream Mach number
        AoA:        Angle of attack (deg)
        Re:         Reynolds number per meter
        ```
        '''
        self.Minf = Minf
        self.AoA  = AoA
        self.Re   = Re
        self.xf_dict = copy.deepcopy(AirfoilFeature.xf_dict)

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

    @property
    def n_point(self):
        '''
        Number of points in this section
        '''
        return self.x.shape[0]



    def getValue(self, feature: str, key='key') -> float:
        '''
        Get value of given feature.

        ### Inputs:
        ```text
        feature:    key of feature dictionary
        key:        'i', 'X', 'Cp', 'Mw', 'Tw', 'Hi', 'Hc', 'dudy'
        ```
        '''

        if not feature in self.xf_dict.keys():
            print('  Warning: feature [%s] not valid'%(feature))
            return 0.0

        aa = self.xf_dict[feature]

        if len(aa)==2:
            return aa[1]

        if key == 'i':
            return aa[1]
        if key == 'X':
            return aa[2]
        if key == 'Cp':
            yy = self.Cp
        elif key == 'Mw':
            yy = self.Mw
        elif key == 'Tw':
            yy = self.Tw
        elif key == 'Hi':
            yy = self.Hi
        elif key == 'Hc':
            yy = self.Hc
        elif key == 'dudy':
            yy = self.dudy
        else:
            raise Exception('  key %s not valid'%(key))

        ii = aa[1]
        xx = aa[2]

        if xx <= 1e-6:
            return 0.0

        if ii >= self.iLE:
            i0 = max(self.iLE, ii-4)
            i1 = i0 + 7
        else:
            i1 = min(self.iLE, ii+4)
            i0 = i1 - 7

        X = self.x[i0:i1]
        Y = yy[i0:i1]

        f = interp1d(X, Y, kind='cubic')

        return f(xx) 

    #TODO: locate the position of flow features
    def locate_basic(self, dMwcri_L=1.0):
        '''
        Locate the index and position of basic flow features.

        ### Get value of: L, T, Q, M
        '''
        X = self.x
        M = self.Mw

        nn  = X.shape[0]
        iLE = self.iLE

        #TODO: Basic features
        #* L => suction peak near leading edge on upper surface
        # 1: maximum extreme point
        # 2: dMw/dx = 1
        i_L = 0
        for i in range(int(0.25*nn)):
            ii = i + iLE
            if X[ii] > 0.2:
                break
            if M[ii-1]<=M[ii] and M[ii]>=M[ii+1]:
                i_L = ii
                break
    
        if i_L == 0:
            dMw2 = 0.0
            for i in range(int(0.25*nn)):
                ii = i + iLE+1
                dMw1 = dMw2
                dMw2 = (M[ii+1]-M[ii])/(X[ii+1]-X[ii])
                if dMw1>=dMwcri_L and dMw2<dMwcri_L:
                    i_L = ii
                    break

        self.xf_dict['L'][1] = i_L
        self.xf_dict['L'][2] = X[i_L]

        #* T => trailing edge upper surface (98% chord length)
        for i in range(int(0.2*nn)):
            ii = nn-i-1
            if X[ii]<=0.98 and X[ii+1]>0.98:
                self.xf_dict['T'][1] = ii
                self.xf_dict['T'][2] = 0.98
                break
        
        #* H => position of upper surface maximum Mach number
        i_H = 0
        max1 = -1.0
        for i in np.arange(iLE, nn-2, 1):
            if M[i-1]<=M[i] and M[i+1]<=M[i] and M[i]>max1:
                max1 = M[i]
                i_H = i

        self.xf_dict['H'][1] = i_H
        self.xf_dict['H'][2] = X[i_H]

        #* Q => suction peak near leading edge on lower surface
        for i in range(int(0.2*nn)):
            ii = iLE - i
            if M[ii-1]<=M[ii] and M[ii]>=M[ii+1]:
                self.xf_dict['Q'][1] = ii
                self.xf_dict['Q'][2] = X[ii]
                break

        #* M => position of lower surface maximum Mach number
        i_M = 0
        max1 = -1.0
        for i in np.arange(1, iLE, 1):
            if M[i-1]<=M[i] and M[i+1]<=M[i] and M[i]>max1:
                max1 = M[i]
                i_M = i

        self.xf_dict['M'][1] = i_M
        self.xf_dict['M'][2] = X[i_M]

    def locate_sep(self):
        '''
        Locate the index and position of flow features about du/dy.

        ### Get value of: S, R, mUy
        '''
        X = self.x
        dudy = self.dudy

        nn  = X.shape[0]
        iLE = self.iLE

        #* S => separation start position
        #* R => reattachment position
        #* mUy => position of min(du/dy)
        min_Uy = 1e6
        i_S = 0
        for i in range(int(0.5*nn)):
            ii = iLE + i
            if X[ii]<0.02:
                continue
            if X[ii]>0.98:
                break

            if dudy[ii]>=0.0 and dudy[ii+1]<0.0 and i_S==0:
                i_S = ii
                self.xf_dict['S'][1] = ii
                self.xf_dict['S'][2] = (0.0-dudy[ii])*(X[ii+1]-X[ii])/(dudy[ii+1]-dudy[ii])+X[ii]

            if dudy[ii]<=0.0 and dudy[ii+1]>0.0 and i_S!=0:
                self.xf_dict['R'][1] = ii
                self.xf_dict['R'][2] = (0.0-dudy[ii])*(X[ii+1]-X[ii])/(dudy[ii+1]-dudy[ii])+X[ii]
        
            if dudy[ii]<min_Uy and dudy[ii-1]>=dudy[ii] and dudy[ii+1]>=dudy[ii]:
                min_Uy = dudy[ii]
                self.xf_dict['mUy'][1] = ii
                self.xf_dict['mUy'][2] = X[ii]

    def locate_geo(self):
        '''
        Locate the index and position of geometry related flow features.\n

        ### Get value of: Cu, Cl, tu, tl, tm
        '''
        X  = self.x
        xx = self.xx
        yu = self.yu
        yl = self.yl
        iLE = self.iLE
        n0 = xx.shape[0]

        #* tm => maximum thickness
        #* tu => highest point on upper surface
        #* tl => lowest point on lower surface
        x_max = xx[np.argmax(yu-yl)]
        x_mu  = xx[np.argmax(yu)]
        x_ml  = xx[np.argmin(yl)]

        self.xf_dict['tm'][1] = np.argmin(np.abs(X[iLE:]-x_max)) + iLE
        self.xf_dict['tm'][2] = x_max
        self.xf_dict['tu'][1] = np.argmin(np.abs(X[iLE:]-x_mu )) + iLE
        self.xf_dict['tu'][2] = x_mu
        self.xf_dict['tl'][1] = np.argmin(np.abs(X[:iLE]-x_ml ))
        self.xf_dict['tl'][2] = x_ml

        #* Cu => crest point on upper surface
        aa = self.AoA/180.0*np.pi
        x0 = np.array([0.0, 0.0])
        x1 = np.array([np.cos(aa), np.sin(aa)])

        ds = np.zeros(n0)
        for i in range(n0):
            xt = np.array([xx[i], yu[i]])
            if xx[i] > 0.9:
                continue
            ds[i], _ = calculate_distance_to_line(x0, x1, xt)
        ii = np.argmax(ds)

        self.xf_dict['Cu'][1] = np.argmin(np.abs(X[iLE:]-xx[ii])) + iLE
        self.xf_dict['Cu'][2] = xx[ii]

        #* Cl => crest point on lower surface
        ds = np.zeros(n0)
        for i in range(n0):
            if xx[i] > 0.9:
                continue
            xt = np.array([xx[i], yl[i]])
            ds[i], _ = calculate_distance_to_line(x0, x1, xt)
        ii = np.argmax(ds)

        self.xf_dict['Cl'][1] = np.argmin(np.abs(X[:iLE]-xx[ii]))
        self.xf_dict['Cl'][2] = xx[ii]

    def locate_shock(self, dMwcri_1=-1.0, info=False):
        '''
        Locate the index and position of shock wave related flow features.

        ### Get value of: 1, 3, F, U, D, A, B
        
        ### Inputs:
        ```text
        dMwcri_1: critical value locating shock wave front
        ```
        '''
        X   = self.x        # [n]
        xx  = self.xx       # [1000]
        mu  = self.mu       # [1000]
        nn  = xx.shape[0]
        iLE = self.iLE

        dMw = np.zeros(nn)
        for i in range(nn-1):
            if xx[i]<=0.02:
                continue
            if xx[i]>=0.98:
                continue
            dMw[i] = (mu[i+1]-mu[i])/(xx[i+1]-xx[i])
            dMw[i] = min(dMw[i], 2)

        d2Mw = np.zeros(nn)
        for i in range(nn-1):
            if xx[i]<0.02 or xx[i]>0.95:
                continue
            
            #d2Mw[i] = (dMw[i+2]+dMw[i+1]-dMw[i]-dMw[i-1])/2/(xx[i+1]-xx[i-1])
            #d2Mw[i] = (dMw[i+1]-dMw[i-1])/(xx[i+1]-xx[i-1])
            d2Mw[i] = (0.5*dMw[i+7]+0.5*dMw[i+4]+2*dMw[i+1]-
                        2*dMw[i]-0.5*dMw[i-3]-0.5*dMw[i-6])/4.5/(xx[i+1]-xx[i-1])

        #* Check shock and shock properties
        flag, i_F, i_1, i_U, i_3 = self.check_single_shock_wave(xx, mu, dMw, d2Mw, dMwcri_1, info=info)

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

    def locate_BL(self, i_1):
        '''
        Locate the index and position of boundary layer related flow features. \n
        
        i-1: index of shock wave front position in self.xx

        ### Get value of: N, Hi, Hc
        '''
        X   = self.x
        xx  = self.xx
        hu  = self.hu
        nn  = xx.shape[0]
        iLE = self.iLE

        #* Hi, Hc => position of maximum Hi, Hc after shock wave front
        # For cases when shock wave is weak, and Hc just keeps growing, set 0
        i_H = 0
        max1 = 0.0
        for i in np.arange(i_1, nn-2, 1):

            if xx[i] > 0.95:
                break

            if hu[i-1]<=hu[i] and hu[i+1]<=hu[i] and hu[i]>max1:
                max1 = hu[i]
                i_H = i
                
        x_H = xx[i_H]
        self.xf_dict['Hc'][1] = np.argmin(np.abs(X[iLE:]-x_H)) + iLE
        self.xf_dict['Hc'][2] = x_H
        self.xf_dict['Hi'][1] = self.xf_dict['Hc'][1]
        self.xf_dict['Hi'][2] = x_H

        #* N => starting position of new flat boundary
        # i.e., position of minimum Hc after shock wave front
        # For cases when shock wave is weak, and Hc just keeps growing, set 0
        i_N = 0
        min1 = 1000.0
        for i in np.arange(i_1, nn-1, 1):
            if hu[i-1]>=hu[i] and hu[i+1]<=hu[i] and hu[i]<min1:
                min1 = hu[i]
                i_N = i

        x_N = xx[i_N]
        self.xf_dict['N'][1] = np.argmin(np.abs(X[iLE:]-x_N)) + iLE
        self.xf_dict['N'][2] = x_N

    @staticmethod
    def shock_property(xu, mu, dMw, d2Mw, dMwcri_1):
        '''
        >>> i_F, i_1, i_U, i_3 = shock_property(xu, mu, dMw, d2Mw, dMwcri_1)

        ### Return:
        ```text
        Index of xu for: F, 1, U, 3
        ```
        '''
        nn  = xu.shape[0]

        #* F => shock foot position
        i_F = np.argmin(dMw)
        x_F = xu[i_F]

        #* 1 => shock wave front position
        # Find the kink position of dMw in range [x_F-0.2, x_F], defined as dMw = -1
        i_1 = 0
        i_cri = 0
        i_md2 = 0
        for i in np.arange(i_F, 1, -1):

            # 1. Within the range of [x_F-0.2, x_F]
            if xu[i]<x_F-0.2:
                break

            # 2. Locate dMw = dMwcri_1 (tend to go too much upstream)
            if dMw[i]>=dMwcri_1 and dMw[i+1]<dMwcri_1 and i_cri==0:
                i_cri = i

            # 3. Locate min d2Mw/dx2 (tend to go too much downstream)
            if d2Mw[i]<=d2Mw[i-1] and d2Mw[i]>d2Mw[i+1] and i_md2==0:
                i_md2 = i
        
        if i_md2-i_cri > 2*(i_F-i_md2):
            i_1 = i_md2
        elif 2*(i_md2-i_cri) < i_F-i_md2:
            i_1 = i_cri
        else:
            i_1 = int(0.5*(i_cri+i_md2))

        '''
        print(i_cri, i_md2, i_F, xu[i_cri], xu[i_md2], dMw[i_md2], dMw[i_F])

        import matplotlib.pyplot as plt
        plt.plot(xu, mu, 'b')
        plt.plot(xu, d2Mw/1000, 'r')
        plt.plot([xu[i_cri], xu[i_md2]], [mu[i_cri], mu[i_md2]], 'bo')
        plt.plot([xu[i_1]], [mu[i_1]], 'ro')
        plt.show()
        '''

        #* 3 => position of just downstream the shock
        # Find the first flat position of Mw in range [x_F, x_F+0.2], defined as dMw = 0 or -1
        i_3 = 0
        i_cri = 0
        i_md2 = 0
        i_flat = 0
        for i in np.arange(i_F, nn-1, 1):

            # 1. Within the range of [x_F, x_F+0.2]
            if xu[i]>x_F+0.2:
                break

            # 2. Locate dMw = dMwcri_1 (tend to go too much downstream)
            if dMw[i]<=dMwcri_1 and dMw[i+1]>dMwcri_1 and i_cri==0:
                i_cri = i

            # 3. Locate min d2Mw/dx2 (tend to go too much upstream)
            if d2Mw[i]<=d2Mw[i-1] and d2Mw[i]>d2Mw[i+1] and i_md2==0:
                i_md2 = i

            # 4. Locate the first flat position of Mw
            if dMw[i]<=0.0 and dMw[i+1]>0.0:
                i_flat = i

        if i_flat!=0 and i_flat-i_F < 2*(i_cri-i_F):
            i_3 = i_flat
        elif i_cri-i_md2 > 2*(i_md2-i_F):
            i_3 = i_md2
        elif 2*(i_cri-i_md2) < i_md2-i_F:
            i_3 = i_cri
        else:
            i_3 = int(0.5*(i_cri+i_md2))

        '''
        print('F     %3d  %.2f'%(i_F,   xu[i_F]))
        print('d2Mw  %3d  %.2f'%(i_md2, xu[i_md2]))
        print('cri   %3d  %.2f'%(i_cri, xu[i_cri]))
        print('dMw=0 %3d  %.2f'%(i_flat,xu[i_flat]))
        print('3     %3d  %.2f'%(i_3,   xu[i_3]))
        print()
        '''

        #* U => local sonic position
        i_U = 0
        for i in np.arange(i_1, i_3, 1):
            if mu[i]>=1.0 and mu[i+1]<1.0:
                i_U = i
                break
        
        #* Neglect small Mw bump near leading edge
        if xu[i_1]<0.1 and mu[i_1]<1.10:
            i_1=0; i_U=0; i_3=0

        return i_F, i_1, i_U, i_3

    @staticmethod
    def check_single_shock_wave(xu, mu, dMw, d2Mw, dMwcri_1, info=False):
        '''
        Check whether is single shock wave or not

        >>> flag, i_F, i_1, i_U, i_3 = check_singleshock(xu, mu, dMw, d2Mw, dMwcri_1)

        ### Inputs:
        ```text
        xu:     ndarray, x location
        mu:     ndarray, wall Mach number of upper surface
        dMw:    ndarray, slope of wall Mach number
        dMwcri_1: critical value locating shock wave front
        ```

        ### flag: 
        ```text
         1: single shock wave
         0: shockless
        -1: multiple shock waves 
        ```
        '''
        #* Get 1st shock
        i_F, i_1, i_U, i_3 = AirfoilFeature.shock_property(xu, mu, dMw, d2Mw, dMwcri_1)
        d_F = dMw[i_F]

        #* Check if shockless
        # Check if Mw jump exists and M1>1.0
        if d_F>dMwcri_1 or mu[i_1]<1.0 or i_1==0:
            if info:
                print('  Shockless:    XF=%.2f MF=%.2f dM/dX=%.2f'%(xu[i_F], mu[i_F], d_F))
            return 0, 0, 0, 0, 0

        #* Check if 2nd shock wave exists
        # Remove first shock
        dm  = dMw.copy()
        d2m = d2Mw.copy()
        nn  = xu.shape[0]
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
        dMwcri_F = max(dMwcri_1, 0.5*d_F)
        _iF, _i1, _iU, _i3 = AirfoilFeature.shock_property(xu, mu, dm, d2m, dMwcri_1)
        if dm[_iF]<dMwcri_F and _i1!=0 and _i3!=0:
            # Locate sharp change of Mw

            if mu[_i1]>1.0 and mu[_i3]<1.05:
                # Check supersonic wave front and 'subsonic' wave hind
                if info:
                    print('  Second shock: X1=%.2f M1=%.2f M2=%.2f'%(xu[_i1], mu[_i1], mu[_i3]))
                return -1, 0, 0, 0, 0

        return 1, i_F, i_1, i_U, i_3

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

    def extract_features(self, info=False):
        '''
        Extract flow features list in the dictionart.
        '''
        self.locate_basic()
        self.locate_sep()
        self.locate_geo()
        i_1 = self.locate_shock(info=info)
        self.locate_BL(i_1)
        self.aux_features()


