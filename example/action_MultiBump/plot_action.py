'''
Plot the action of the MultiBumpModificationAction.
'''

import matplotlib.pyplot as plt
import numpy as np
import os

from cst_modeling.foil import cst_foil
from pyTSFoil.environment.basic import MultiBumpModificationAction


cst_u = np.array([ 0.12829643,  0.12670863,  0.16065898,  0.14942386,  0.15102884,  0.22416928,  0.16078175,  0.20998555,  0.18608795,  0.21052324])
cst_l = np.array([-0.12927128, -0.13176061, -0.17044964, -0.07045476, -0.33888064,  0.00991923, -0.20070721, -0.03536713, -0.04397496,  0.06436195])


if __name__ == '__main__':
    
    path = os.path.dirname(os.path.abspath(__file__))
    print('path: ', path)
    
    x, yu, yl, _, _ = cst_foil(501, cst_u, cst_l)

    action = MultiBumpModificationAction()


    for i_bump in range(action.n_bumps):
        
        action_array = np.zeros(action.dim_action)
        action_array[2*i_bump+1] = action.critical_height_for_no_bump + 1e-6
        _, _, yu_new, yl_new, is_action_valid = action.apply_action(action_array, x, yu.copy(), yl.copy())
        
        print('max diff upper', np.max(np.abs(yu_new-yu)))
        
        plt.figure()
        plt.plot(x, yu, 'k-', linewidth=2)
        plt.plot(x, yl, 'k-', linewidth=2)

        plt.plot(x, yu_new, 'r--', linewidth=1)
        plt.plot(x, yl_new, 'r--', linewidth=1)

        plt.xlabel('x')
        plt.ylabel('y')
        plt.title(f'Bump {i_bump}')
        plt.savefig(os.path.join(path, f'bump_upper_{i_bump}.png'))
        plt.close()


        action_array = np.zeros(action.dim_action)
        action_array[2*i_bump+1+2*action.n_bumps] = action.critical_height_for_no_bump + 1e-6
        _, _, yu_new, yl_new, is_action_valid = action.apply_action(action_array, x, yu.copy(), yl.copy())

        print('max diff lower', np.max(np.abs(yl_new-yl)))
        
        plt.figure()
        plt.plot(x, yu, 'k-', linewidth=2)
        plt.plot(x, yl, 'k-', linewidth=2)

        plt.plot(x, yu_new, 'r--', linewidth=1)
        plt.plot(x, yl_new, 'r--', linewidth=1)

        plt.xlabel('x')
        plt.ylabel('y')
        plt.title(f'Bump {i_bump}')
        plt.savefig(os.path.join(path, f'bump_lower_{i_bump}.png'))
        plt.close()

