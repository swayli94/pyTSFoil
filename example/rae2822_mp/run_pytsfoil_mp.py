'''
Run PyTSFoil in multiprocessing mode

This script demonstrates how to run multiple PyTSFoil analyses in parallel
using multiprocessing. Each process gets its own isolated copy of the 
Fortran data to avoid data corruption.
'''
import os
import sys

path = os.path.dirname(os.path.abspath(__file__))
# Add project root to Python path for multi-branch development
project_root = os.path.abspath(os.path.join(path, '..', '..'))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

import multiprocessing as mp
import time
from typing import Dict, Any
import matplotlib.pyplot as plt
import numpy as np

from pyTSFoil.pytsfoil import PyTSFoil


def run_pytsfoil_analysis(params: Dict[str, Any]) -> Dict[str, Any]:
    """
    Worker function to run a single PyTSFoil analysis in a separate process.
    
    Parameters:
    -----------
    params: Dict[str, Any]
        Dictionary containing:
        - 'case_name': Name identifier for this case
        - 'airfoil_file': Path to airfoil file
        - 'work_dir': Working directory
        - 'config': Configuration parameters for PyTSFoil
        
    Returns:
    --------
    Dict[str, Any]: Results dictionary with case info and outcomes
    """
    try:
        print(f"Starting analysis for case: {params['case_name']}")
        start_time = time.time()
        
        # Create PyTSFoil instance (each process gets its own isolated Fortran data)
        pytsfoil = PyTSFoil(
            airfoil_file=params['airfoil_file'],
            work_dir=params['work_dir']
        )
        
        # Set configuration
        pytsfoil.set_config(**params['config'])
        
        pytsfoil.set_config(
            flag_output=False,
            flag_output_summary=False,
            flag_output_shock=False,
            flag_output_field=False,
            flag_print_info=False
            )
        
        # Run analysis
        pytsfoil.run()
        
        # Extract Mach distribution data for plotting
        mach_data = {
            'xx': pytsfoil.mesh['xx'].copy(),
            'mau': pytsfoil.data_summary['mau'].copy(),
            'mal': pytsfoil.data_summary['mal'].copy()
        }
        
        # Get results
        elapsed_time = time.time() - start_time
        
        print(f"Completed analysis for case: {params['case_name']} in {elapsed_time:.2f}s")
        
        return {
            'case_name': params['case_name'],
            'success': True,
            'elapsed_time': elapsed_time,
            'config': params['config'],
            'mach_data': mach_data,
            'message': f"Analysis completed successfully"
        }
        
    except Exception as e:
        print(f"Error in case {params['case_name']}: {str(e)}")
        return {
            'case_name': params['case_name'],
            'success': False,
            'elapsed_time': 0,
            'config': params['config'],
            'error': str(e)
        }


def plot_all_mach_distributions(results, output_dir):
    """
    Plot Mach number distributions from all successful cases in a single figure.
    
    Parameters:
    -----------
    results: List[Dict]
        List of result dictionaries from multiprocessing analysis
    output_dir: str
        Directory to save the combined plot
    """
    successful_results = [r for r in results if r['success']]
    
    if not successful_results:
        print("No successful cases to plot.")
        return
    
    # Create single figure for both upper and lower surfaces
    fig, ax = plt.subplots(1, 1, figsize=(12, 8))
    
    # Define colors for different cases
    colors = plt.cm.tab10(np.linspace(0, 1, len(successful_results)))
    
    for i, result in enumerate(successful_results):
        mach_data = result['mach_data']
        config = result['config']
        alpha = config['ALPHA']
        mach = config['EMACH']
        
        color = colors[i]
        
        # Plot upper surface (solid line with circles)
        ax.plot(mach_data['xx'], mach_data['mau'], 
                linewidth=2, color=color, linestyle='-', markersize=4,
                label=f"AoA={alpha}°, M={mach}")
        
        # Plot lower surface (dashed line with squares)
        ax.plot(mach_data['xx'], mach_data['mal'], 
                linewidth=2, color=color, linestyle='-', markersize=4)
    
    # Add reference line for sonic condition
    ax.axhline(y=1.0, color='k', linestyle=':', linewidth=2, alpha=0.7, label='Sonic (M=1)')
    
    # Configure plot
    ax.set_xlabel('X/c', fontsize=12)
    ax.set_ylabel('Mach Number', fontsize=12)
    ax.set_title('Mach Number Distribution - Upper and Lower Surfaces', fontsize=14, fontweight='bold')
    ax.grid(True, alpha=0.3)
    ax.legend(bbox_to_anchor=(1.05, 1), loc='upper left', fontsize=10)
    ax.set_xlim([-0.1, 1.1])
    ax.set_ylim([0, 1.5])
    
    plt.tight_layout()
    
    # Save the plot
    plot_filename = os.path.join(output_dir, 'combined_mach_distributions.png')
    plt.savefig(plot_filename, dpi=300, bbox_inches='tight')
    print(f"\nCombined Mach distribution plot saved to: {plot_filename}")
    
    # Show the plot
    plt.show()


if __name__ == "__main__":
    
    print('Working directory:', path)
    
    # Define multiple cases to run in parallel
    # Example: varying angle of attack and Mach number
    base_config = {
        'MAXIT': 9999,
        'NWDGE': 0,
        'n_point_x': 200,
        'n_point_y': 80,
        'n_point_airfoil': 100,
        'EPS': 0.2,
        'CVERGE': 1e-6
    }
    
    # Create multiple parameter combinations
    cases = []
    alphas = [0.0, 0.5]  # Different angles of attack
    machs = [0.7, 0.75]  # Different Mach numbers
    
    for i, alpha in enumerate(alphas):
        for j, mach in enumerate(machs):
            case_config = base_config.copy()
            case_config.update({
                'ALPHA': alpha,
                'EMACH': mach
            })
            
            cases.append({
                'case_name': f'alpha_{alpha}_mach_{mach}',
                'airfoil_file': os.path.join(path, 'rae2822.dat'),
                'work_dir': path,
                'config': case_config
            })
    
    # Create output directories for each case
    for case in cases:
        os.makedirs(case['work_dir'], exist_ok=True)
    
    print(f"Running {len(cases)} cases in parallel...")
    
    # Run analyses in parallel using multiprocessing
    start_time = time.time()
    
    # Use multiprocessing.Pool for parallel execution
    # Each process gets its own isolated PyTSFoil/Fortran data
    with mp.Pool() as pool:
        results = pool.map(run_pytsfoil_analysis, cases)
    
    total_time = time.time() - start_time
    
    # Print summary of results
    print(f"\n{'='*60}")
    print("MULTIPROCESSING ANALYSIS SUMMARY")
    print(f"{'='*60}")
    print(f"Total cases: {len(cases)}")
    print(f"Total time: {total_time:.2f}s")
    print(f"Average time per case: {total_time/len(cases):.2f}s")
    
    successful_cases = [r for r in results if r['success']]
    failed_cases = [r for r in results if not r['success']]
    
    print(f"Successful: {len(successful_cases)}")
    print(f"Failed: {len(failed_cases)}")
    
    if successful_cases:
        print(f"\nSuccessful cases:")
        for result in successful_cases:
            alpha = result['config']['ALPHA']
            mach = result['config']['EMACH']
            print(f"  - {result['case_name']}: AoA={alpha}°, Mach={mach} ({result['elapsed_time']:.2f}s)")
    
    if failed_cases:
        print(f"\nFailed cases:")
        for result in failed_cases:
            print(f"  - {result['case_name']}: {result['error']}")
    
    print(f"{'='*60}")
    print("Analysis complete! Check individual result directories for outputs.")
    
    # Plot combined Mach distributions from all cases
    print("\nGenerating combined Mach distribution plot...")
    plot_all_mach_distributions(results, path)



