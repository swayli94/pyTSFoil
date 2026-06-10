# TSD 圆前缘修正：匹配渐近展开理论与 pyTSFoil 实现参考

> 用途：本文档把 Rusak (1993, *JFM* 248) 的匹配渐近展开（MAE）理论整理为可实现的形式，
> 作为后续改进 **pyTSFoil**（TSFOIL 式 Murman–Cole / SLOR 求解器的 Python 移植）的依据。
> 第 2–6 节是理论，第 7 节是内区数值解，第 8 节是接入 pyTSFoil 的工程方案。
>
> 主要来源：
> - Z. Rusak, *Transonic flow around the leading edge of a thin airfoil with a parabolic nose*, J. Fluid Mech. **248** (1993) 1–26.
> - Z. Rusak & J.-C. Lee, *Transonic Small-Disturbance Theory — A Tool for Aerodynamic Analysis and Design*, Canadian Aeronautics and Space Journal **46** (2) (2000) 74–86.

---

## 1. 符号约定

| 符号 | 含义 |
|---|---|
| $\delta$（亦记 $\varepsilon$） | 翼型厚度比，$\delta\ll1$ |
| $M_\infty$ | 来流马赫数，$M_\infty\sim1$ |
| $\theta,\ A=\theta/\delta$ | 迎角、标度迎角 |
| $K=(1-M_\infty^2)/(\delta^{2/3}M_\infty^2)$ | 跨声速相似参数 |
| $\gamma$ | 比热比 |
| $h$ | 鼻部形状常数，$ct(x/c)\sim 2h(cx)^{1/2}$（$x\to0$） |
| $R_c=2h^2\delta^2 c$ | 抛物线鼻曲率半径 |
| $\Phi=Uq$ | 全位势；$\phi_1,\phi_2$ 外区扰动位势；$\phi_0$ 内区位势 |
| $(x,\tilde y),\ \tilde y=\delta^{1/3}y$ | 外区坐标 |
| $(x^*,y^*),\ x^*=x/\delta^2,\ y^*=y/\delta^2$ | 内区坐标 |
| $\xi=x/\tilde y^{6/7},\ \xi^*=x^*/y^{*6/7}$ | 鼻部相似变量 |
| $s\equiv x/R_c$ | 内区表的归一化轴向坐标 |
| $\alpha_3=80.40878^\circ$ | hodograph 角根（翼型表面在 hodograph 平面的边界） |

---

## 2. 问题：跨声速鼻部奇性

圆（钝）前缘翼型在 $x\to0$ 时厚度函数 $ct(x/c)\sim 2h(cx)^{1/2}$，即抛物线鼻，曲率半径 $R_c=2h^2\delta^2 c$。
TSD 把切向边界条件贴在 $\tilde y=0$ 缝面上，斜率

$$F'_{u,l}(x)\sim h\,c^{1/2}\,x^{-1/2}\to\infty\qquad(x\to0)$$

继承到位势上，使 TSD 解在鼻部给出**无穷速度和压力**——即"跨声速鼻部奇点"。这是因为 TSD 是一个外区近似，其小扰动假设在鼻部 $O(R_c)$ 的小区域内被破坏（驻点处 $u\to-U$，绕鼻部速度梯度大）。实际流动连续滞止于驻点。

MAE 的处置：远离鼻部用 TSD（**外区**），鼻部用**完整方程**（**内区**），在重叠区匹配并合成一致有效解。

---

## 3. 外区：TSD / Kármán–Guderley 问题

坐标 $(x,\tilde y=\delta^{1/3}y)$ 固定，$K$、$A$ 固定。位势展开

$$\Phi = U\Big\{x+\delta^{2/3}\phi_1(x,\tilde y;K,A)+\delta^{4/3}\phi_2(x,\tilde y;K,A)+\dots\Big\}.$$

首阶 $\phi_1$ 满足 Kármán–Guderley（TSD）问题：

$$(\gamma+1)\,\phi_{1x}\phi_{1xx}-\phi_{1\tilde y\tilde y}=K\,\phi_{1xx},$$

等价地 $\big[(\gamma+1)\phi_{1x}-K\big]\phi_{1xx}=\phi_{1\tilde y\tilde y}$，混合型（亚声速椭圆 / 超声速双曲），由 $M^2-1=-\delta^{2/3}\big[K-(\gamma+1)M_\infty^2\phi_{1x}\big]$ 的符号决定。边界条件：

$$\phi_{1\tilde y}(x,0\pm)=F'_{u,l}(x)\quad(0\le x\le c);\qquad
(\phi_{1x},\phi_{1\tilde y})\to0\ (x\to-\infty);$$
$$\phi_{1x}(c,0^+)=\phi_{1x}(c,0^-)\ \text{(尾缘 Kutta)};\qquad
\phi_1(x,0^+)-\phi_1(x,0^-)=\Gamma\ (x\ge c).$$

压力系数：$c_p=-2\delta^{2/3}\phi_{1x}+O(\delta^{4/3})$。

**这正是 pyTSFoil 当前求解的对象**：迎角进切向 BC（有效斜率 $-\alpha$），尾迹缝在 $\tilde y=0$，环量 $\Gamma$ 由尾缘 Kutta 在松弛迭代中确定。其输出的 $c_p$ 记为 $c_{p\text{TSD}}(x)$。

### 3.1 鼻部奇性的渐近结构

在 $x\to0$ 把 $\phi_1$ 展成相似解。主导平衡：非线性项 $(\gamma+1)\phi_{1x}\phi_{1xx}$ 与 $\phi_{1\tilde y\tilde y}$ 平衡给出指数 $m=3k-2$；切向 BC 的 $x^{-1/2}$ 行为定出 $k=6/7$，于是 $m=4/7$：

$$(\gamma+1)\phi_1\sim \tilde y^{4/7}f(\xi),\qquad \xi=x/\tilde y^{6/7},$$

$f$ 满足非线性 ODE

$$\Big(f_\xi-\tfrac{36}{49}\xi^2\Big)f_{\xi\xi}-\tfrac{30}{49}\xi f_\xi+\tfrac{12}{49}f=0.$$

代回压力，得表面附近

$$c_p\sim 2\delta^{2/3}\Big[\underbrace{\big(\tfrac32\big)^{2/3}\cot^{2/3}\!\alpha_3(\gamma+1)^{-1/3}h^{2/3}c^{1/3}\,x^{-1/3}}_{\text{对称（厚度）首阶}}\ \mp\ \underbrace{d_2\,C\,x^{-0.2964}}_{\text{反对称（环量）二阶}}-\tfrac{K}{\gamma+1}+\dots\Big].$$

两项都在 $x\to0$ 发散——非物理。

**关键结论（Rusak 对 Cole–Cook 1986 的纠正）**：抛物线鼻的切向 BC 要求 hodograph 解中常数 $c_2=0$，**消掉了反对称首项**。因此首阶奇性**纯对称**，环量效应只进入更高阶（指数 $\tilde a=4.2219$，对应 $x^{-0.2964}$）。物理含义：跨声速下**驻点对任意 $M_\infty\sim1$ 都钉在前缘点**，与亚声速（环量使驻点沿鼻面移动）本质不同。

外区展开合到 $\phi_2$ 后仍在 $x,\tilde y\to0$ 处错序——当二者都小于 $\sim\delta^2 h^2 c=R_c/2$ 量级时扰动不再小。需重标度。

---

## 4. 内区：声速绕抛物线

径向重标度（$x$、$y$ 同尺度，因鼻部局部各向同性）：

$$x^*=x/\delta^2,\qquad y^*=y/\delta^2,\qquad \Phi=U\delta^2\phi_0(x^*,y^*),\quad \phi_0=x^*+\bar\phi_0.$$

$\bar\phi_0$ 满足 **$M_\infty=1$ 的完整（非线性）位势方程**：

$$\nabla^*\!\cdot\Big\{\big[1+\tfrac{\gamma-1}{2}(1-\phi_{0x^*}^2-\phi_{0y^*}^2)\big]^{1/(\gamma-1)}\nabla^*\phi_0\Big\}=0,$$

配抛物线 $y^*=\pm2h(cx^*)^{1/2}$ 上的切向 BC，及远场匹配条件

$$\phi_0\sim x^*+\frac{y^{*4/7}}{\gamma+1}f(\xi^*)+\frac{y^{*2/7}}{(\gamma+1)^2}f_1(\xi^*)+\dots,\qquad \xi^*=x^*/y^{*6/7},$$

其中 $f,f_1$ **与外区鼻部展开里的同名函数完全相同**——匹配能成立的关键。

内问题的物理与要害：

- **完整方程、速度处处有限**：流动连续滞止于前缘再绕鼻加速，奇性被正则化。
- **普适性**：首阶内问题里 $K$（马赫数）与 $A$（迎角）都掉出。内解只随鼻半径（$h,c$）标度，**对每个翼型只需解一次**。
- **对称性**：抛物线与远场都对称 ⟹ 内解关于 $x^*$ 轴对称，驻点在前缘。

---

## 5. 匹配

中间区 $x_\eta=x/\eta(\delta),\ y_\eta=y/\eta(\delta)$，取 $\delta^2\ll\eta(\delta)\ll1$ 且 $\eta/\delta^2\to\infty$。
在重叠区把内、外展开都用 $(x_\eta,y_\eta)$ 写出并令同阶相等：可验证 $\xi=\xi^*$，正比于 $f$、$f_1$ 的项逐项对上。
匹配既给出内问题的远场条件（使之良定），又收紧 $\eta$ 的范围：

$$\delta^2\ll\eta(\delta)\ll\delta^{0.7720}.$$

---

## 6. 一致有效（复合）解 ★ 实现核心

按"外 + 内 − 公共部分"的加性复合（physical $c_p$）：

$$\boxed{\,c_p(x)=c_p^*\!\Big(\frac{x}{R_c}\Big)+\frac{\rho^*}{\rho_\infty}\,\phi_{0x^*}\Big[\,c_{p\text{TSD}}(x)-c_{pc,p}\!\Big(\frac{x}{R_c}\Big)\,\Big]\,}$$

- $c_p^*,\ \rho^*/\rho_\infty,\ \phi_{0x^*}$ 是**内区数值解**给出的、随 $s=x/R_c$ 变化的量（见第 7 节、图 6）。
- $c_{p\text{TSD}}(x)$ 是 **pyTSFoil 已经算出的外区 TSD 压力**。
- 公共部分（即 TSD 鼻部奇性本身），以 $s=x/R_c$ 表示的紧凑形式：

$$c_{pc,p}(s)=\frac{0.635776}{(\gamma+1)^{1/3}}\,s^{-1/3}.$$

> 系数核对：$0.635776=3^{2/3}\cot^{2/3}\alpha_3$，由 $R_c=2h^2\delta^2c$ 代入 JFM 式 (101) 的
> $2\delta^{2/3}(3/2)^{2/3}\cot^{2/3}\!\alpha_3(\gamma+1)^{-1/3}h^{2/3}c^{1/3}x^{-1/3}$ 化简而来。

### 三段行为（说明对消机理）

| 区域 | $\rho^*/\rho_\infty,\ \phi_{0x^*}$ | 发生的事 | 主导项 |
|---|---|---|---|
| 鼻部内 $0<x<\delta^2h^2c$ | $\phi_{0x^*}\to0$ | $c_{pc,p}$ 对消 $c_{p\text{TSD}}$ 的 $x^{-1/3}$ 奇性（上下表面同时） | 有限、对称的 $c_p^*$，驻点在 LE |
| 中间区 | 过渡 | 平滑过渡，反对称（环量）偏差随距离显现 | 混合 |
| 鼻部外 $x>\delta^{0.772}h^2c$ | $\to1,\ \to1$ | $c_{pc,p}$ 转而对消 $c_p^*$ | 外区 $c_{p\text{TSD}}$ |

结果：复合解处处有限、鼻部对称、远处回到 TSD。

---

## 7. 内区问题的数值解（一次性，产出查表数据）

把抛物线变成坐标线，用抛物坐标

$$x^*=\tfrac12(\bar\mu^2-\bar\eta^2),\qquad y^*=\bar\mu\bar\eta,$$

并以 $h c^{1/2}$ 归一化（$\eta=\bar\eta/(hc^{1/2}),\ \mu=\bar\mu/(hc^{1/2}),\ \phi=\phi_0/(h^2c)$），抛物面落在 $\eta=\sqrt2$。方程

$$\frac{\partial}{\partial\mu}(\rho\phi_\mu)+\frac{\partial}{\partial\eta}(\rho\phi_\eta)=0,\qquad
\rho=\Big[1+\tfrac{\gamma-1}{2}\Big(1-\frac{\phi_\mu^2+\phi_\eta^2}{\mu^2+\eta^2}\Big)\Big]^{1/(\gamma-1)}.$$

边界条件：
- 壁面切向 $\phi_\eta(\mu,\eta=\sqrt2)=0$（用 $\phi(i,1)=\phi(i,3)$ 的镜像行实现）；
- $x$ 轴对称 $\phi_\mu(0,\eta)=0$（$\phi(1,j)=\phi(3,j)$）；
- 远场 $\phi\sim X+\dfrac{1}{\gamma+1}Y^{4/7}\bar f(\bar\xi)+O(Y^{2/7})$，其中 $X=x^*/h^2c,\ Y=y^*/h^2c,\ \bar\xi=(X+1)/Y^{6/7}$，$\bar f$ 由 $f$（JFM 式 29、30、76，hodograph 参数 $\sin\alpha$，$|\alpha|\le\alpha_3$）给出。

离散与迭代：
- 守恒通量盒式格式（JFM 式 89/93），中心差分（内区只出现亚声速，无需迎风）；
- 点超松弛 SOR（式 94），最优 $\Omega\approx1.2$；
- 计算域 $\mu_T=\eta_T=40$，网格 $M=N=40$（与 $M=N=80$ 相差 <0.001 即收敛）；
- 鼻顶为数值方便平移到 $x^*=-h^2c$；
- 表面压力 $c_p^*=\dfrac{2}{\gamma}\big(\rho^{*\gamma}-1\big)$ 在 $x_i^*/c=\tfrac12 h^2\mu_{(i,2)}^2$ 处取得。

**产出三张随 $s=x/R_c$ 的表（即复现图 6）**，供第 6 节复合公式插值：

| 表 | $s\to0$（驻点） | $s\to\infty$ |
|---|---|---|
| $c_p^*(s)$ | 驻点值（$\approx$ 图 6a 峰值） | $\to0$ |
| $\rho^*/\rho_\infty\,(s)$ | 最大 | $\to1$ |
| $\phi_{0x^*}(s)$ | $\to0$ | $\to1$ |

注意 $\alpha_3=80.4087792226^\circ$、$c_1=2.2797(\gamma+1)^2h^2c$ 是远场/标度所需常数。

---

## 8. 接入 pyTSFoil 的工程方案

现状映射：pyTSFoil 的 SLOR 主循环（`main_iteration`，列扫 + y 向三对角追赶 `DIAG/SUB/SUP/RHS`，Murman–Cole 型差分）求的就是第 3 节的外区 $c_{p\text{TSD}}$。**前缘修正是后处理**，不改主求解器的内核。

建议步骤：

1. **几何**：由翼型鼻部拟合 $ct(x/c)\sim2h(cx)^{1/2}$ 得 $h$，算 $R_c=2h^2\delta^2c$。
2. **内区求解器（新模块，一次性）**：实现第 7 节抛物坐标 SOR，生成 $c_p^*(s),\ (\rho^*/\rho_\infty)(s),\ \phi_{0x^*}(s)$ 表（$s\in[0,\sim12]$）。这部分与 $M_\infty$、$\alpha$ 无关，可缓存/打包随代码分发；用 Numba 加速 SOR 扫掠与 Thomas 一致。
3. **外区**：照常跑 SLOR 至**完全收敛**（重要：半收敛 TSD 有时"看似"与完整位势吻合，加密网格后失效，会污染复合结果）。取每条表面网格点的 $c_{p\text{TSD}}(x)$。
4. **复合后处理**：对每个表面点按 $s=x/R_c$ 插表，套第 6 节公式得修正后的 $c_p(x)$；公共部分 $c_{pc,p}(s)=0.635776(\gamma+1)^{-1/3}s^{-1/3}$ 直接闭式。
5. **载荷重积分**：用修正后的 $c_p$ 重新积分 $C_l,\ C_{d,\text{wave}},\ C_m$（鼻部对升力与吸力峰的修正最显著）。

建议模块布局：

```
pytsfoil/
├── solver/            # 现有 SLOR 外区求解器（不动内核）
│   └── main_iteration.py
├── leading_edge/      # 新增：前缘修正
│   ├── inner_parabola.py   # 第 7 节内区 SOR，产出 cp*, rho*/rho_inf, phi0x* 表
│   ├── inner_tables.npz     # 缓存的内区解（随 s 的三张表）
│   └── composite.py         # 第 6 节复合公式 + 公共部分闭式
└── postprocess/
    └── forces.py       # 用修正 cp 重积分载荷
```

**验证基准**：Joukowski 翼型，$\delta=0.10$，$M_\infty=0.8$，$\theta=0^\circ$ 与 $1^\circ$（JFM 图 7）。纯 TSD 与纯抛物线解在前 ~10% 弦长都偏离完整位势/Euler，唯复合解吻合。可同时跑 NACA0012 $M_\infty=0.8/0.838$（CASJ 图 3、4）作 $K$ 相似性核对。

---

## 9. 参考文献

1. Z. Rusak, *Transonic flow around the leading edge of a thin airfoil with a parabolic nose*, J. Fluid Mech. **248** (1993) 1–26.
2. Z. Rusak & J.-C. Lee, *Transonic Small-Disturbance Theory — A Tool for Aerodynamic Analysis and Design*, Can. Aeronaut. Space J. **46**(2) (2000) 74–86.
3. B. L. Keyfitz, R. E. Melnik, B. Grossman, *An analysis of the leading-edge singularity in transonic small-disturbance theory*, Q. J. Mech. Appl. Math. **31** (1978) 137–155.
4. J. D. Cole & L. P. Cook, *Transonic Aerodynamics*, North-Holland (1986).
5. E. M. Murman & J. D. Cole, *Calculation of plane steady transonic flows*, AIAA J. **9**(1) (1971) 114–121.
