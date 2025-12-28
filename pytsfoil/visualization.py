"""
TSFoil visualization module

Responsible for visualization and plotting of computation results, including:
- Mach number distribution plots
- Flow field visualization  
- Pressure coefficient distribution
- Comprehensive result charts
"""

import os
import numpy as np
import matplotlib.pyplot as plt


class Visualizer:
    """Visualizer class"""
    
    def __init__(self, core):
        """
        Initialize visualizer
        
        Parameters
        ----------
        core: TSFoilCore
            Core data object
        """
        self.core = core
    
    def plot_all_results(self, filename: str = 'tsfoil_results.png') -> None:
        """
        Plot all results, including:
        - Mesh distribution analysis (read from XIN, YIN in tsfoil2.out)
        - Mach number distribution on Y=0 line (read from cpxs.dat)
        - Mach number field (read from field.dat)

        Plot three sub-plots in one figure (1x3)
        
        Parameters
        ----------
        filename: str
            Output image filename
        """
        # Create figure with 2 subplots arranged in 1 row, 2 columns
        fig, axes = plt.subplots(1, 2, figsize=(16, 5))
        fig.suptitle('TSFOIL Results Analysis', fontsize=16, fontweight='bold')
        
        # Plot 1: Mach number distribution on Y=0 line
        self._plot_mach_distribution_y0(axes[0])
        
        # Plot 2: Mach number field
        self._plot_mach_field(axes[1])
        
        plt.tight_layout()
        plt.savefig(os.path.join(self.core.output_dir, filename), dpi=300)
        plt.close()
        
        if self.core.config['flag_print_info']:
            print(f'Plot saved to: {filename}')
    
    def _plot_mach_distribution_y0(self, ax) -> None:
        """
        Plot Mach number distribution on Y=0 line from cpxs.dat
        
        Parameters
        ----------
        ax: matplotlib.axes.Axes
            Axes to plot on
        """
        # Plot Mach number distribution
        ax.plot(self.core.mesh['xx'], self.core.data_summary['mau'], 'b.-', linewidth=2, label='Upper Surface')
        ax.plot(self.core.mesh['xx'], self.core.data_summary['mal'], 'r.-', linewidth=2, label='Lower Surface')
        
        # Add reference line for sonic condition
        ax.axhline(y=1.0, color='k', linestyle='--', alpha=0.5, label='Sonic (M=1)')
        
        ax.set_xlabel('X/c')
        ax.set_ylabel('Mach Number')
        ax.set_title(f'(Wall) Mach Number Distribution on Y=0')
        ax.grid(True, alpha=0.3)
        ax.legend()
        ax.set_xlim([-0.2, 1.2])
        ax.set_ylim([-0.1, 1.5])
    
    def _plot_mach_field(self, ax) -> None:
        """
        Plot Mach number field from field.dat
        
        Parameters
        ----------
        ax: matplotlib.axes.Axes
            Axes to plot on
        """
        x = self.core.data_summary['field_data'][:, :, 0]
        y = self.core.data_summary['field_data'][:, :, 1]
        mach = self.core.data_summary['field_data'][:, :, 2]
        
        # Create contour plot
        max_mach = max(1.5, np.max(mach))
        levels = np.linspace(0, max_mach, 16)
        contour = ax.contourf(x, y, mach, levels=levels, cmap='jet', extend='max')
        
        # Add colorbar
        cbar = plt.colorbar(contour, ax=ax, shrink=0.8)
        cbar.set_label('Mach Number')
        
        # Add airfoil outline
        x_airfoil = self.core.airfoil['x']
        y_airfoil = self.core.airfoil['y']
        ax.plot(x_airfoil, y_airfoil, 'k--', linewidth=0.5)
        
        ax.set_xlim([-0.5, 1.5])
        ax.set_ylim([-0.5, 0.5])
        ax.set_xlabel('X/c')
        ax.set_ylabel('Y/c')
        ax.set_title(f'Mach Number Field')
        ax.set_aspect('equal')
    
    def plot_pressure_coefficient(self, filename: str = 'pressure_coefficient.png') -> None:
        """
        Plot pressure coefficient distribution
        
        Parameters
        ----------
        filename: str
            Output image filename
        """
        fig, ax = plt.subplots(figsize=(10, 6))
        
        # Plot pressure coefficient distribution
        ax.plot(self.core.mesh['xx'], self.core.data_summary['cpu'], 'b.-', linewidth=2, label='Upper Surface')
        ax.plot(self.core.mesh['xx'], self.core.data_summary['cpl'], 'r.-', linewidth=2, label='Lower Surface')
        
        # Add critical pressure coefficient line
        ax.axhline(y=self.core.data_summary['cpstar'], color='k', linestyle='--', alpha=0.5, 
                  label=f'Cp* = {self.core.data_summary["cpstar"]:.3f}')
        
        ax.set_xlabel('X/c')
        ax.set_ylabel('Cp')
        ax.set_title(f'Pressure Coefficient Distribution (M={self.core.data_summary["mach"]:.3f}, α={self.core.data_summary["alpha"]:.1f}°)')
        ax.grid(True, alpha=0.3)
        ax.legend()
        ax.invert_yaxis()  # Traditionally Cp plots are inverted
        
        plt.tight_layout()
        plt.savefig(os.path.join(self.core.output_dir, filename), dpi=300)
        plt.close()
        
        if self.core.config['flag_print_info']:
            print(f'Pressure coefficient plot saved to: {filename}')
    
    def plot_airfoil_geometry(self, filename: str = 'airfoil_geometry.png') -> None:
        """
        Plot airfoil geometry
        
        Parameters
        ----------
        filename: str
            Output image filename
        """
        fig, ax = plt.subplots(figsize=(12, 4))
        
        # Plot airfoil shape
        x_airfoil = self.core.airfoil['x']
        y_airfoil = self.core.airfoil['y']
        ax.plot(x_airfoil, y_airfoil, 'k-', linewidth=2, label='Airfoil')
        
        # Mark leading edge and trailing edge
        le_idx = np.argmin(x_airfoil)
        ax.plot(x_airfoil[le_idx], y_airfoil[le_idx], 'ro', markersize=8, label='Leading Edge')
        ax.plot([0, 1], [0, 0], 'go', markersize=6, label='Trailing Edge')
        
        ax.set_xlabel('X/c')
        ax.set_ylabel('Y/c')
        ax.set_title(f'Airfoil Geometry (Max thickness: {self.core.airfoil["t_max"]:.4f})')
        ax.grid(True, alpha=0.3)
        ax.legend()
        ax.set_aspect('equal')
        
        plt.tight_layout()
        plt.savefig(os.path.join(self.core.output_dir, filename), dpi=300)
        plt.close()
        
        if self.core.config['flag_print_info']:
            print(f'Airfoil geometry plot saved to: {filename}')
    
    def plot_convergence_history(self, filename: str = 'convergence_history.png') -> None:
        """
        Plot convergence history (if available)
        
        Parameters
        ----------
        filename: str
            Output image filename
            
        Note
        ----
        This feature requires convergence history data from Fortran solver
        Currently just a placeholder implementation
        """
        # TODO: Implement convergence history plotting
        # Need to get convergence history data from Fortran solver
        if self.core.config['flag_print_info']:
            print("Convergence history plotting not yet implemented")
    
    def create_comprehensive_report(self, filename: str = 'tsfoil_comprehensive_report.png') -> None:
        """
        Create comprehensive report with multiple subplots
        
        Parameters
        ----------
        filename: str
            Output image filename
        """
        fig, axes = plt.subplots(2, 2, figsize=(16, 12))
        fig.suptitle('TSFOIL Comprehensive Analysis Report', fontsize=16, fontweight='bold')
        
        # Subplot 1: Airfoil geometry
        ax = axes[0, 0]
        x_airfoil = self.core.airfoil['x']
        y_airfoil = self.core.airfoil['y']
        ax.plot(x_airfoil, y_airfoil, 'k-', linewidth=2)
        ax.set_xlabel('X/c')
        ax.set_ylabel('Y/c')
        ax.set_title('Airfoil Geometry')
        ax.grid(True, alpha=0.3)
        ax.set_aspect('equal')
        
        # Subplot 2: Pressure coefficient distribution
        ax = axes[0, 1]
        ax.plot(self.core.mesh['xx'], self.core.data_summary['cpu'], 'b.-', linewidth=2, label='Upper')
        ax.plot(self.core.mesh['xx'], self.core.data_summary['cpl'], 'r.-', linewidth=2, label='Lower')
        ax.axhline(y=self.core.data_summary['cpstar'], color='k', linestyle='--', alpha=0.5, label='Cp*')
        ax.set_xlabel('X/c')
        ax.set_ylabel('Cp')
        ax.set_title('Pressure Coefficient')
        ax.grid(True, alpha=0.3)
        ax.legend()
        ax.invert_yaxis()
        
        # Subplot 3: Mach number distribution
        ax = axes[1, 0]
        ax.plot(self.core.mesh['xx'], self.core.data_summary['mau'], 'b.-', linewidth=2, label='Upper')
        ax.plot(self.core.mesh['xx'], self.core.data_summary['mal'], 'r.-', linewidth=2, label='Lower')
        ax.axhline(y=1.0, color='k', linestyle='--', alpha=0.5, label='Sonic')
        ax.set_xlabel('X/c')
        ax.set_ylabel('Mach Number')
        ax.set_title('Mach Number Distribution')
        ax.grid(True, alpha=0.3)
        ax.legend()
        
        # Subplot 4: Mach number field (simplified version)
        ax = axes[1, 1]
        x = self.core.data_summary['field_data'][:, :, 0]
        y = self.core.data_summary['field_data'][:, :, 1]
        mach = self.core.data_summary['field_data'][:, :, 2]
        
        max_mach = max(1.5, np.max(mach))
        levels = np.linspace(0, max_mach, 10)
        contour = ax.contourf(x, y, mach, levels=levels, cmap='jet', extend='max')
        ax.plot(x_airfoil, y_airfoil, 'k-', linewidth=1)
        
        ax.set_xlim([-0.5, 1.5])
        ax.set_ylim([-0.5, 0.5])
        ax.set_xlabel('X/c')
        ax.set_ylabel('Y/c')
        ax.set_title('Mach Number Field')
        ax.set_aspect('equal')
        
        # Add overall colorbar
        cbar = fig.colorbar(contour, ax=axes[1, 1], shrink=0.8)
        cbar.set_label('Mach Number')
        
        # Add results summary text box
        summary_text = f"""
        RESULTS SUMMARY:
        Mach = {self.core.data_summary['mach']:.3f}
        Alpha = {self.core.data_summary['alpha']:.2f}°
        CL = {self.core.data_summary['cl']:.4f}
        CM = {self.core.data_summary['cm']:.4f}
        """
        if 'cd' in self.core.data_summary:
            summary_text += f"CD = {self.core.data_summary['cd']:.6f}"
            
        fig.text(0.02, 0.02, summary_text, fontsize=10, 
                bbox=dict(boxstyle="round,pad=0.3", facecolor="lightgray", alpha=0.8))
        
        plt.tight_layout()
        plt.savefig(os.path.join(self.core.output_dir, filename), dpi=300, bbox_inches='tight')
        plt.close()
        
        if self.core.config['flag_print_info']:
            print(f'Comprehensive report saved to: {filename}')

