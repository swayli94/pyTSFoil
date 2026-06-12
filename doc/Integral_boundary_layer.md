# 积分边界层方法（IBL）

## 概述

`ibl.py` 实现了一套 **二维翼型积分边界层（Integral Boundary Layer，IBL）方法**，用于在 pyTSFoil（跨声速小扰动方程求解器）的无黏外流解基础上，计算翼面附着边界层的厚度与摩擦系数。

IBL 方法将边界层偏微分方程沿法向（$y$ 方向）积分，使其退化为沿弧长 $s$ 演化的常微分方程（ODE），主要求解以下积分量：

| 符号 | 名称 | 定义 |
|------|------|------|
| $\theta$ | 动量厚度 | $\displaystyle\int_0^\infty \frac{\rho u}{\rho_e u_e}\left(1-\frac{u}{u_e}\right)\mathrm{d}y$ |
| $\delta^*$ | 位移厚度 | $\displaystyle\int_0^\infty\left(1-\frac{\rho u}{\rho_e u_e}\right)\mathrm{d}y$ |
| $H$ | 形状因子 | $\delta^* / \theta$ |
| $c_f$ | 壁面摩擦系数 | $\tau_w / (q_\infty)$ |

---

## 方法流程

```
pyTSFoil 输出:  xx, mau, mal, EMACH, REYNLD
        │
        ▼
[1] 等熵关系  →  边缘速度  ue/u∞
        │
        ▼
[2] Thwaites 积分法  →  层流段 θ, H, cf, δ*
        │
        ▼
[3] Michel 转捩准则  →  转捩位置 x_tr
        │
        ▼
[4] Head 卷吸法（可压缩 von Kármán）  →  湍流段 θ, H, cf, δ*
        │
        ▼
输出:  δ*(x) 用于壁面斜率修正；cf(x) 用于摩擦阻力积分
```

---

## 控制方程

### 1. 边缘速度（等熵关系）

由 pyTSFoil 输出的壁面马赫数 $M_e$ 换算为边缘速度比：

$$
\frac{u_e}{u_\infty} = \frac{M_e}{M_\infty}\sqrt{\frac{1+\dfrac{\gamma-1}{2}M_\infty^2}{1+\dfrac{\gamma-1}{2}M_e^2}}
$$

---

### 2. 层流段：Thwaites 积分法（1949）

**动量厚度积分（精确解）：**

$$
\theta^2(s) = \frac{0.45\,\nu}{u_e^6(s)}\int_0^s u_e^5\,\mathrm{d}s
$$

该式由不可压冯·卡门方程在 Thwaites 假设下直接积分而来，无需逐步迭代。

**Thwaites 参数：**

$$
\lambda = \frac{\theta^2}{\nu}\frac{\mathrm{d}u_e}{\mathrm{d}s}, \qquad \lambda \in [-0.09,\; 0.25]
$$

$\lambda < 0$ 为逆压梯度区，$\lambda = -0.09$ 对应层流分离。

**闭合关系（White 2006 多项式拟合）：**

$$
l(\lambda) = 0.22 + 1.402\lambda + \frac{0.018\lambda}{0.107+\lambda}
$$

$$
H(\lambda) = 2 + 4.14z - 83.5z^2 + 854z^3 - 3337z^4 + 4576z^5, \quad z = 0.25 - \lambda
$$

$$
c_f = \frac{2\nu\,l}{u_e\,\theta}, \qquad \delta^* = H\,\theta
$$

---

### 3. 转捩：Michel 准则（1951）

当动量厚度雷诺数超过临界值时，发生层流→湍流转捩：

$$
Re_\theta \geq 1.174\left(1 + \frac{22400}{Re_x}\right)Re_x^{0.46}
$$

其中 $Re_\theta = u_e\theta/\nu$，$Re_x = u_e\,s/\nu$。

---

### 4. 湍流段：Head 卷吸法（1958）+ 可压缩修正

湍流段求解两个耦合 ODE：

**方程一（可压缩 von Kármán 动量积分）：**

$$
\boxed{\frac{\mathrm{d}\theta}{\mathrm{d}s} = \frac{c_f}{2} - \left(2 + H - M_e^2\right)\frac{\theta}{u_e}\frac{\mathrm{d}u_e}{\mathrm{d}s}}
$$

> 与不可压形式相比，增加了 $-M_e^2$ 项。该项来源于等熵流的密度梯度效应，在 $M_e\sim 0.8$ 时可使压力梯度项减小约 20%，对跨声速流不可忽略。

**方程二（Head 卷吸方程）：**

$$
\frac{\mathrm{d}(H_1\,\theta)}{\mathrm{d}s} = 0.0306\,(H_1 - 3)^{-0.6169}
$$

其中 $H_1$ 为 Head 卷吸形状参数（与位移厚度相关但不同于 $H$），通过以下辅助关系由 $H$ 换算：

$$
H_1(H) = \begin{cases}
3.3 + 0.8234\,(H-1.1)^{-1.287} & H < 1.6 \\
3.3 + 1.5501\,(H-0.6778)^{-3.064} & H \geq 1.6
\end{cases}
$$

**摩擦系数（Ludwieg-Tillmann 1950）：**

$$
c_f = 0.246 \times 10^{-0.678H}\,Re_\theta^{-0.268}
$$

---

### 5. 与 pyTSFoil 的耦合：壁面斜率修正

位移厚度的物理意义是"等效增厚翼型"：流体好像在厚度为 $\delta^*$ 的实体上流动。对 pyTSFoil 翼面边界条件的修正：

$$
\left.\frac{\mathrm{d}Y}{\mathrm{d}x}\right|_\text{eff} = \frac{\mathrm{d}Y}{\mathrm{d}x}\bigg|_\text{geo} + \frac{\mathrm{d}\delta^*_\text{upper}}{\mathrm{d}x} \quad \text{（上翼面）}
$$

$$
\left.\frac{\mathrm{d}Y}{\mathrm{d}x}\right|_\text{eff} = \frac{\mathrm{d}Y}{\mathrm{d}x}\bigg|_\text{geo} - \frac{\mathrm{d}\delta^*_\text{lower}}{\mathrm{d}x} \quad \text{（下翼面）}
$$

这等价于在 pyTSFoil 的壁面 BC（$\phi_y = \mathrm{d}Y/\mathrm{d}x \cdot \phi_x$）中，将几何斜率替换为等效斜率。

**摩擦阻力系数：**

$$
C_{d,f} = \int_0^{s_\text{TE}} c_f\,\mathrm{d}s \bigg|_\text{upper} + \int_0^{s_\text{TE}} c_f\,\mathrm{d}s \bigg|_\text{lower}
$$

---

## 代码结构

```
ibl.py
└── class IBL
    ├── __init__(Re, M_inf, gamma)       # 初始化：Reynolds 数、来流 Mach、比热比
    │
    ├── run(xx, mach, yu)                # 单面边界层积分（主入口）
    ├── run_both_surfaces(xx, mau, mal)  # 上下翼面同时计算
    ├── wall_slope_correction(...)       # 壁面斜率修正 ±d(δ*)/dx
    ├── friction_drag(upper, lower)      # 摩擦阻力系数 Cd_f
    │
    ├── _mach_to_ue(M_e)                 # 等熵换算 ue/u∞
    ├── _arc_length(xx, yu)              # 弧长坐标 s
    ├── _thwaites(s, ue)                 # 层流：Thwaites 积分
    ├── _thwaites_correlations(lam)      # 层流闭合：l(λ), H(λ)
    ├── _michel_transition(s, ue, theta) # 转捩：Michel 准则
    ├── _head(s, ue, mach, ...)          # 湍流：Head ODE（RK45）
    ├── _head_euler(...)                 # Head ODE 前向欧拉备用
    ├── _H_to_H1(H)                      # Head 辅助关系
    ├── _H1_to_H(H1)                     # Head 辅助关系（逆）
    └── _ludwieg_tillmann(H, Re_theta)   # 湍流摩擦：Ludwieg-Tillmann
```

---

## 使用示例

```python
from ibl import IBL

# 使用 pyTSFoil 的计算结果
ibl = IBL(Re=pytsfoil.config['REYNLD'], M_inf=pytsfoil.config['EMACH'])

upper, lower = ibl.run_both_surfaces(
    xx  = pytsfoil.mesh['xx'],
    mau = pytsfoil.data_summary['mau'],
    mal = pytsfoil.data_summary['mal'],
    # 如果有翼型几何则提供，弧长更准确：
    # yu = y_upper, yl = y_lower,
)

# 位移厚度（用于壁面斜率修正）
delta_star_upper = upper['delta_star']   # shape (n,)

# 壁面斜率修正量 ±d(δ*)/dx
slope_corr_upper = ibl.wall_slope_correction(xx, upper['delta_star'], upper['s'], upper=True)
slope_corr_lower = ibl.wall_slope_correction(xx, lower['delta_star'], lower['s'], upper=False)

# 摩擦阻力系数
cd_f = ibl.friction_drag(upper, lower)

# 转捩位置
print(f"Upper x_tr = {upper['x_tr']:.3f}")
print(f"Lower x_tr = {lower['x_tr']:.3f}")
```

### `run()` 返回字典

| 键 | 形状 | 含义 |
|---|---|---|
| `'s'` | `(n,)` | 弧长坐标 $s/c$ |
| `'ue'` | `(n,)` | 边缘速度比 $u_e/u_\infty$ |
| `'theta'` | `(n,)` | 动量厚度 $\theta/c$ |
| `'delta_star'` | `(n,)` | 位移厚度 $\delta^*/c$ |
| `'H'` | `(n,)` | 形状因子 $H$ |
| `'cf'` | `(n,)` | 壁面摩擦系数 $c_f$ |
| `'x_tr'` | `float` | 转捩位置 $x_\text{tr}/c$ |
| `'i_tr'` | `int` | 转捩站位索引 |
| `'laminar_mask'` | `(n,)` bool | 层流区域标记 |

---

## 方法局限性

- **层流 Thwaites**：基于不可压方程，跨声速层流段有轻微误差（通常可接受，层流段短）
- **Head 第二方程**：相比 XFOIL/Drela 的动能厚度积分方程（$H^*$ 法）精度略低，无湍流滞后 $C_\tau$ 方程，对强分离流精度有限
- **仅处理附着流**：$H > 4$ 时边界层近似分离，计算结果供参考

---

## 参考文献

1. Thwaites, B. "Approximate Calculation of the Laminar Boundary Layer." *ARC R&M 1314*, 1949.
2. Michel, R. "Étude de la Transition sur les Profils d'Aile." *ONERA Rep. 1/1578A*, 1951.
3. Head, M. R. "Entrainment in the Turbulent Boundary Layer." *ARC R&M 3152*, 1958.
4. Ludwieg, H. & Tillmann, W. "Investigations of the Wall-Shearing Stress in Turbulent Boundary Layers." *NACA TM 1285*, 1950.
5. White, F. M. *Viscous Fluid Flow*, 3rd ed. McGraw-Hill, 2006, Ch. 7.
6. Drela, M. "XFOIL: An Analysis and Design System for Low Reynolds Number Airfoils." *Lecture Notes in Engineering*, Springer, 1989.
