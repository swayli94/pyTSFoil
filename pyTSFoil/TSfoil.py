import copy
import os
import platform

import numpy as np
from cst_modeling.foil import cst_foil
from scipy.interpolate import interp1d

class TSfoil():
    '''
    Python interface of TSFOIL2.
    '''

    def __init__(self):

        self.path = os.path.dirname(__file__)
        self.local = os.getcwd()
        
        if platform.system() in 'Windows':
            self.runfile = self.path+'\\tsfoil2.exe'
            self.inp = self.local+'\\tsfoil2.inp'
            self.out = self.local+'\\tsfoil2.out'
            self.fCp = self.local+'\\foil-cp.out'

        else:
            self.runfile = self.path+'/tsfoil2'
            self.inp = self.local+'/tsfoil2.inp'
            self.out = self.local+'/tsfoil2.out'
            self.fCp = self.local+'/foil-cp.out'

            if not os.path.exists(self.runfile):
                print('First time use pyTSFoil, compling TSFOIL2')
                command = 'cd '+self.path+' & gfortran -w -o tsfoil2 tsfoil2-modify.f'
                os.system(command)

    def run(self, path=None, show=False):
        '''
        Run TSFOIL2 in path (str). If not provided, run in local directory.
        '''
        if not isinstance(path, str):
            path = self.local

        if platform.system() in 'Windows':
            if show:
                os.system(self.runfile+' '+path)
            else:
                os.system(self.runfile+' '+path+' >nul')
        else:
            if show:
                os.system(self.runfile+' '+path)
            else:
                os.system(self.runfile+' '+path+' >/dev/null 2>&1')
    
    def foil_byCST(self, cst_u, cst_l, t=None):
        '''
        Generate airfoil for TSFOIL by given CST coefficients. \n
            cst-u, cst-l:   list of upper/lower CST coefficients of the airfoil.
            t:              airfoil maximum thickness or None
        '''
        # 81 is the maximum amount of points that TSFOIL can handle
        self.xu, self.yu, self.yl, self.t, R0 = cst_foil(81, cst_u, cst_l, x=None, t=t, tail=0.0)
        self.xl = copy.deepcopy(self.xu)

    def set_foil(self, xu, yu, xl, yl, t):
        '''
        Set airfoil coordinates. \n
            xu, yu, xl, yl: lists of foil coordinates
            t:  airfoil maximum thickness
        '''
        self.xu = copy.deepcopy(xu)
        self.xl = copy.deepcopy(xl)
        self.yu = copy.deepcopy(yu)
        self.yl = copy.deepcopy(yl)
        self.t = t

    def flight_condition(self, Minf, AoA, path=None, **kwargs):
        '''
        Generate input file by given foil and flight conditions.
        Must set foil coordinates in advance.

            Minf:   free stream Mach number (can not be 1.0)
            AoA:    deg
            path:   working directory. None means local directory.

        kwargs: \n
            maxiter: maximum iteration. (default 1500)
            simdef: similarity definition. (default 3)
                    1   Cole
                    2   Spreiter
                    3   Krupp
            wedge:  control for viscous wedge inclusion
                    0   no wedge (default)
                    1   Murman bump, then set Re and wc
                    2   Yoshihara wedge
            Re:     Reynolds number based on chord for wedge = 1 (default, 4.0e6)
                    If provided Re, automatically set wedge to 1
            wc:     wedge constant for wedge = 1 (default, 4.0)
            Amesh:  analytical mesh calculation (bool, default True)
                    T   X and Y mesh values are computed, ni, nj should also be supplied.
                        ni is an odd number (4*n+1), nj is an even number.
                    F   X and Y points are the default values.
            ni, nj: number of X and Y mesh points (less than 100)

        '''
        simdef = 3
        maxiter = 1500
        ni = 97
        nj = 60
        wedge = 0
        Re = 4.0e6
        wc = 4.0
        Amesh = 'T'

        if 'simdef' in kwargs.keys():
            simdef = kwargs['simdef']

        if 'maxiter' in kwargs.keys():
            maxiter = kwargs['maxiter']

        if 'wedge' in kwargs.keys():
            wedge = kwargs['wedge']

        if 'Re' in kwargs.keys():
            Re = kwargs['Re']
            wedge = 1

        if 'wc' in kwargs.keys():
            wc = kwargs['wc']

        if 'ni' in kwargs.keys():
            ni = kwargs['ni']

        if 'nj' in kwargs.keys():
            nj = kwargs['nj']

        if 'nj' in kwargs.keys():
            nj = kwargs['nj']

        if 'Amesh' in kwargs.keys():
            if not kwargs['Amesh']:
                Amesh = 'F'

        with open(self.inp, 'w') as f:
            f.write('DATA POINT INPUT\n')
            f.write(' $INP\n')
            f.write(' EMACH= %.2f,ALPHA=%.2f,DELTA=%.5f,\n'%(Minf, AoA, self.t))
            f.write(' BCFOIL=3,NU=%d,NL=%d,\n'%(len(self.xu), len(self.xl)))
            f.write(' MAXIT=%d,SIMDEF=%d,CVERGE=0.00001,\n'%(maxiter, simdef))
            f.write(' AMESH=%s,IMAXI=%d,JMAXI=%d,\n'%(Amesh, ni, nj))
            f.write(' NWDGE=%d,REYNLD=%.2e, WCONST=%d\n'%(wedge, Re, wc))

            f.write(' XU =    ')
            for i in range(len(self.xu)):
                f.write('%8.5f,'%(self.xu[i]))
                if i%10==9:
                    f.write('\n')
                    f.write('         ')
            f.write('\n')

            f.write(' YU =    ')
            for i in range(len(self.yu)):
                f.write('%8.5f,'%(self.yu[i]))
                if i%10==9:
                    f.write('\n')
                    f.write('         ')
            f.write('\n')

            f.write(' XL =    ')
            for i in range(len(self.xl)):
                f.write('%8.5f,'%(self.xl[i]))
                if i%10==9:
                    f.write('\n')
                    f.write('         ')
            f.write('\n')

            f.write(' YL =    ')
            for i in range(len(self.yl)):
                f.write('%8.5f,'%(self.yl[i]))
                if i%10==9:
                    f.write('\n')
                    f.write('         ')
            f.write('\n')

            f.write(' $END\n')

    def get_result(self):
        '''
        Extract result from tsfoil2.out, foil-cp.out
        '''

        with open(self.out, 'r') as f:
            lines = f.readlines()

            line = lines[-1].split()
            self.Cd = float(line[-1])

            line = lines[-4].split()
            self.Cdw = float(line[-1])

            for i in range(len(lines)):
                if 'FINAL MESH' in  lines[i]:
                    line = lines[i+1].split()
                    self.CL = float(line[-1])
                
                    line = lines[i+2].split()
                    self.Cm = float(line[-1])

        with open(self.fCp, 'r') as f:
            lines = f.readlines()

            n = len(lines)-1
            self.x = np.zeros(n)
            self.cpu = np.zeros(n)
            self.cpl = np.zeros(n)
            self.mwu = np.zeros(n)
            self.mwl = np.zeros(n)

            line = lines[0].split()
            self.CL = float(line[-1])

            for i in range(len(lines)-2):
                line = lines[i+2].split()
                self.x[i+1] = float(line[1])
                self.cpu[i+1] = float(line[2])
                self.mwu[i+1] = float(line[3])
                self.cpl[i+1] = float(line[4])
                self.mwl[i+1] = float(line[5])
    
        #* Interplot leading edge
        xs = np.array([-self.x[2], -self.x[1], self.x[1], self.x[2]])
        cps = np.array([self.cpl[2], self.cpl[1], self.cpu[1], self.cpu[2]])
        mws = np.array([self.mwl[2], self.mwl[1], self.mwu[1], self.mwu[2]])
        fcp = interp1d(xs, cps, kind='cubic')
        fmw = interp1d(xs, mws, kind='cubic')

        self.cpu[0] = fcp(np.array([0.0]))[0]
        self.mwu[0] = fmw(np.array([0.0]))[0]
        self.mwl[0] = self.mwu[0]
        self.cpl[0] = self.cpu[0]

        #* Interplot geometry
        gu = interp1d(np.array(self.xu), np.array(self.yu), kind='cubic')
        gl = interp1d(np.array(self.xl), np.array(self.yl), kind='cubic')
        self.xu = self.x.copy()
        self.xl = self.x.copy()
        self.yu = gu(self.xu)
        self.yl = gl(self.xl)








