import pandas as pd 
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.cm import ScalarMappable
from matplotlib.colors import LinearSegmentedColormap
import imageio

NUM_LEVELS = 100
CBAR_TIKCS = 0.1

class Plotter:

	def gen_cmap(self):
		c = ["blue","cyan","lightgreen", "yellow","red"]
		v = np.linspace(0, 1, len(c))
		cmap=LinearSegmentedColormap.from_list('bgr',list(zip(v,c)), N=128)
		return cmap

	def gen_png_filename(self, row):
		values = row[["airfoil", "mach", "alpha"]].to_list()
		return "_".join(map(str, values)) +".png"

	def gen_vertical_label(self, row):
		values = row[["mach", "alpha", "cl", "cm", "total cdwave"]].to_list()
		names  = [x.ljust(8) for x in ["Mach", "Alpha", "CL", "CM", "CDwave"]]
		airfoil= row["airfoil"].upper().center(17)
		details= "\n".join(["{}={:8.5f}".format(k,v) for k,v in zip(names, values)])
		return "{}\n{}".format(airfoil, details)

	def gen_horizontal_header(self):
		return "".join([x.center(8) for x in ["Mach", "Alpha", "CL", "CM", "CDwave"]])

	def gen_horizontal_label(self, row):
		values = row[["mach", "alpha", "cl", "cm", "total cdwave"]].to_list()
		return "".join(["{:8.5f}".format(v) for v in values])

	def gen_cpx_mach_ax(self, mach=False):
		fig, ax = plt.subplots(figsize=(8, 6))
		plt.rcParams['font.family'] = 'monospace'

		ax.set_title("Mach-x" if mach else "Cp-x")
		ax.set_ylabel("Mach" if mach else "Cp")
		ax.set_xlabel("x/c")

		ax.set_xlim(-0.1, 1.1)
		if not mach:
			ax.invert_yaxis()
		return ax

	def legend_magic(self, ax):
		leg = ax.legend(frameon=False, loc="upper right")
		for item in leg.legendHandles:
		    item.set_visible(False)
		    	
	def plot_cpx(self, buffer_row=-1):
		row = self.buffer.iloc[buffer_row]
		ax = self.gen_cpx_mach_ax(mach=False)
		ax.axhline(y=row["cp*"], color='r', linestyle='--', lw=1)
		ax.plot(np.concatenate([row['x_c'], row['x_c'][::-1]]), 
			    np.concatenate([row['cp_up'], row['cp_low'][::-1]]), 
			    lw=0.8, label=self.gen_vertical_label(row))

		self.legend_magic(ax)
		plt.show(block=False)

	def plot_machx(self, buffer_row=-1):
		row = self.buffer.iloc[buffer_row]
		ax = self.gen_cpx_mach_ax(mach=True)
		ax.axhline(y=1, color='r', linestyle='--', lw=1)
		ax.plot(np.concatenate([row['x_c'], row['x_c'][::-1]]), 
			    np.concatenate([row['m_up'], row['m_low'][::-1]]), 
			    lw=0.8, label=self.gen_vertical_label(row))

		self.legend_magic(ax)
		plt.show(block=False)

	def pseq_machx(self, df=None):
		df = df if isinstance(df, pd.core.frame.DataFrame) else self.buffer
		ax = self.gen_cpx_mach_ax(mach=True)
		ax.axhline(y=1, color='r', linestyle='--', lw=1, label=self.gen_horizontal_header())

		for index, row in df.iterrows():
			ax.plot(np.concatenate([row['x_c'], row['x_c'][::-1]]), 
				    np.concatenate([row['m_up'], row['m_low'][::-1]]), 
				    lw=0.8, label=self.gen_horizontal_label(row))

		self.legend_magic(ax)
		plt.show(block=False)

	def gen_contour_fig(self, mach=False):
		fig, ax = plt.subplots(figsize=(8, 6))
		plt.rcParams['font.family'] = 'monospace'

		ax.set_title('Contours of Mach' if mach else 'Contours of Cp')
		ax.set_ylabel("y/c")
		ax.set_xlabel("x/c")
		ax.set_xlim(-0.5, 1.5)
		ax.set_ylim(-0.75, 0.75)

		return fig, ax

	def plot_contour(self, row, mach=False, savepng=False):
		fig, ax  = self.gen_contour_fig(mach)

		X, Y = np.meshgrid(row["mesh_x"], row["mesh_y"])
		Z = row["mach_map"] if mach else row["cp_map"]

		increment = (Z.max() - Z.min())/NUM_LEVELS
		ax.text(-0.4, -0.7, "Increment :{:1.3f}".format(increment), fontsize=8, color='black')

		foil=[(0, 1), (0, 0), 'black']
		ax.plot(*foil, label=self.gen_vertical_label(row))

		cs = ax.contour(X, Y, Z, 
				   		levels=np.linspace(Z.min(), Z.max(), NUM_LEVELS),
				   		linewidths=0.5,
				   		cmap=self.gen_cmap())

		sm = plt.cm.ScalarMappable(cmap = cs.cmap)
		fig.colorbar(sm, ticks=np.arange(Z.min(), Z.max(), CBAR_TIKCS))

		self.legend_magic(ax)

		if savepng:
			filename = self.gen_png_filename(row)
			plt.savefig(filename, dpi=300)
			plt.close()
			return filename
		plt.show(block=False)

	def plot_isomach(self, buffer_row=-1, savepng=False):
		self.plot_contour(self.buffer.iloc[buffer_row], True, savepng)

	def plot_isocp(self, buffer_row=-1, savepng=False):
		self.plot_contour(self.buffer.iloc[buffer_row], False, savepng)

	def plot_grid(self):
		row = self.buffer.iloc[-1]
		fig, ax = plt.subplots(figsize=(8, 6))

		ax.text(-0.4, -0.7, "{} x {}".format(len(row["mesh_x"]), len(row["mesh_y"])), fontsize=8)

		X, Y = np.meshgrid(row["mesh_x"], row["mesh_y"])
		ax.vlines(X[0], *Y[[0,-1],0], lw=0.5)
		ax.hlines(Y[:,0], *X[0, [0,-1]], lw=0.5)

		foil=[(0, 1), (0, 0), 'black']
		ax.plot(*foil)

		ax.set_title('Grid Specification')
		ax.set_ylabel("y/c")
		ax.set_xlabel("x/c")
		ax.set_xlim(-0.5, 1.5)
		ax.set_ylim(-0.75, 0.75)

		plt.show(block=False)

	def gplot(self, x,y, row=None, color='red'):
		fig, ax = plt.subplots(figsize=(8, 6))
		ax.set_title("{} vs {}".format(x.upper(), y.upper()))
		ax.set_xlabel("{}".format(x.upper()))
		ax.set_ylabel("{}".format(y.upper()))
		if row:
			ax.plot(self.buffer.iloc[row][x], self.buffer.iloc[row][y], lw=0.8, color=color)
		else:
			ax.plot(self.buffer[x], self.buffer[y], lw=0.8, color=color)
		plt.show(block=False)

	def animate(self, mach=True):
		images = []
		for index, row in self.buffer.iterrows():
			images.append(imageio.imread(self.plot_contour(row, mach=mach, savepng=True)))
		imageio.mimsave("Animation.gif", images, fps=5)

