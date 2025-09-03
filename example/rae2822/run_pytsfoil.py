
import os
import sys

path = os.path.dirname(os.path.abspath(__file__))
# Add project root to Python path for multi-branch development
project_root = os.path.abspath(os.path.join(path, '..', '..'))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

from pyTSFoil.pytsfoil import PyTSFoil


if __name__ == "__main__":
    
    print('path: ', path)
    
    pytsfoil = PyTSFoil(
        airfoil_file=os.path.join(path, 'rae2822.dat'),
        work_dir=path
    )
    
    pytsfoil.set_config(
        ALPHA=0.5,
        EMACH=0.75,
        MAXIT=9999,
        NWDGE=0,
        n_point_x=200,
        n_point_y=80,
        n_point_airfoil=100,
        EPS=0.2,
        CVERGE=1e-6
    )
    
    pytsfoil.run()
    
    pytsfoil.plot_all_results()



