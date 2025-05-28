import numpy as np
import pandas as pd
import os
from io import StringIO
from scipy.optimize import fsolve
from plotter import Plotter

class Tsfoil(Plotter):
	def __init__(self):
		self.airfoil 	  = {}
		self.config 	  = {"ICUT" :0, "MAXIT":2500}
		self.current_foil = ""
		self.buffer       = pd.DataFrame()
		self.LEN_HEADER   = 26
		self.basename_foil= ""

	def load(self, datfile):
		x, y = np.loadtxt(datfile, skiprows=1).T
		le_pos = x.argmin()

		xu = x[:le_pos+1][::-1]
		yu = y[:le_pos+1][::-1]
		xl = x[le_pos:]
		yl = y[le_pos:]

		fmt={'float_kind':'{:9.7f}'.format}

		self.airfoil["XU"] = np.array2string(xu, formatter=fmt, separator=", ")[1:-1]
		self.airfoil["YU"] = np.array2string(yu, formatter=fmt, separator=", ")[1:-1]
		self.airfoil["XL"] = np.array2string(xl, formatter=fmt, separator=", ")[1:-1]
		self.airfoil["YL"] = np.array2string(yl, formatter=fmt, separator=", ")[1:-1]

		self.current_foil  = datfile
		self.basename_foil = datfile.replace(".dat", "")
		self.inp_file      = "{}.inp".format(self.basename_foil)

	def set(self, config_dict):
		self.config.update(config_dict)

	def gen_inp(self, filename, title="Input File for TSFOIL"):
		settings	= "\n".join(["{} = {}".format(k,v) for k,v in self.config.items()])
		cordinates 	= "\n".join(["{} = {}".format(k,v) for k,v in self.airfoil.items()])
		with open(filename, 'w') as f:
			f.write("{}\n$INP\n{}\n{}\n$END\n".format(title,settings,cordinates))

	def extract_smry(self, filename="smry.out"):
		df = pd.read_csv(filename, skiprows=9, sep="=", header=None).set_index(0).T
		df.columns = [c.strip().lower() for c in df.columns.str.replace('0',"")]
		return df

	def extract_mesh(self, filename="mesh.out"):
		x, y = open(filename).read().split("0\n\n")[1:]
		x = np.genfromtxt(StringIO(x[self.LEN_HEADER:].replace("\n", "")))[:-1]
		y = np.genfromtxt(StringIO(y[self.LEN_HEADER:].replace("\n", "")))[::-1][:-1]
		return x,y

	def extract_cpxs(self, filename="cpxs.out"):
		return np.genfromtxt(filename, skip_header=5).T

	def extract_mmap(self, filename="mmap.out"):
		return np.loadtxt(filename)

	def extract_cpmp(self, filename="cpmp.out"):
		return np.loadtxt(filename)

	def extract_cnvg(self, filename="smry.out"):
		return "SOLUTION CONVERGED" in open(filename).read()

	def extract_iter(self, filename="cnvg.out"):
		iteration, error = np.loadtxt("cnvg.out", skiprows=2, usecols=(0,5)).T
		return iteration, error

	def extract_res(self):
		df = self.extract_smry()
		df["converged"] = self.extract_cnvg()
		df.insert(0,'airfoil', self.current_foil)
		x, y = self.extract_mesh()
		df["mesh_x"] = [x]
		df["mesh_y"] = [y]

		i, x_c, cp_up, m_up, cp_low, m_low = self.extract_cpxs()
		df["x_c"] 	= [x_c]
		df["cp_up"] = [cp_up]
		df["m_up"] 	= [m_up]
		df["cp_low"]= [cp_low]
		df["m_low"] = [m_low]

		iteration, error = self.extract_iter()
		df["iter"] 	= [iteration]
		df["error"] = [error]

		df["mach_map"] = [self.extract_mmap()]
		df["cp_map"]   = [self.extract_cpmp()]

		self.buffer = self.buffer._append(df)

	def exec(self, key=None, value=None, absorb=True):
		if key:
			self.config[key.upper()] = value

		self.gen_inp(self.inp_file)
		os.system("tsfoil.exe {}".format(self.inp_file))

		if absorb:
			self.extract_res()

	def fixed_cl(self, cl, alpha_0=1):
		def exec_wrapper(alpha):
			self.exec("ALPHA", value=alpha.item(), absorb=False)
			return self.extract_smry()["cl"].item() - cl
		res = fsolve(exec_wrapper, x0=alpha_0, xtol=0.005)
		if res:
			self.extract_res()


if __name__ == "__main__":
	tsfoil = Tsfoil()
	tsfoil.load("rae2822.dat")
	tsfoil.set({"alpha": 0.0})
	for m in np.arange(0.5, 1.0, 0.1):
		tsfoil.exec("EMACH", m)
