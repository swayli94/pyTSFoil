# pyTSFoil 三维扩展计划

参考 `pyTSFoil_3D_TSD_guide.md`，结合现有 `pytsfoil/` 代码分析，确定三维扩展的计划和思路。

注意，本项目的工作环境是 conda 虚拟环境 `pytsfoil`，应当在这个环境内进行测试。
另外，本项目 vibe coding 中，我允许你直接执行以下操作，不需要我先批准：

- 创建/删除/修改本项目内的所有文件；
- 运行本项目内的 Python 脚本或代码；
- 在虚拟环境 `pytsfoil`内安装/升级/卸载 conda 包（`conda install`、`pip install` 等）；
- 运行非侵入式的系统命令（如 `ls`、`cat`、`echo`、`grep` 等）查看文件内容或目录结构。

以下操作需要得到我的批准：

- 创建 commit 或修改 Git 历史（如 `git commit`、`git rebase`、`git reset` 等）；
- 提交 commit 到 Git 仓库；
- 运行可能修改系统环境的命令（如 `sudo`、`rm -rf /` 等）；
- 运行任何可能对系统安全造成威胁的命令或脚本。

---

## 1. 现有 2D 代码结构分析

### 1.1 模块组织

```
pytsfoil/
  src/
    common_data.f90      # 全局参数、网格坐标、翼型斜率（FXU/FXL）
    solver_data.f90      # 势场 P[NMP+2, NMP+1]、差分系数、边界数组
    solver_base.f90      # PX/PY 有限差分函数、DIFCOE、LIFT/PITCH
    solver_functions.f90 # SETBC（施加壁面 BC）、BCEND、EMACH1
    main_iteration.f90   # SOLVE（外迭代）、SYOR（SOR 列扫）
  pytsfoil.py            # PyTSFoil 类：网格生成、标度、几何导数、后处理
  ibl.py                 # 边界层模块（Thwaites → Michel → Head）
  wrapper.py             # 简便接口 run_airfoil_analysis()
```

### 1.2 核心数据流

```
PyTSFoil.run()
  → initialize_data()          # 初始化 Fortran 模块
  → set_airfoil()              # 读取翼型 → DELTA（最大厚度）
  → set_mesh()                 # clustcos 网格 → X[I], Y[J]
  → compute_mesh_indices()     # ILE, ITE, JUP, JLOW
  → run_fortran_solver()
      → compute_scale()        # 相似变量（AK, CPFACT 等）
      → compute_far_field_bc() # 亚声速远场（偶极子 + 涡）
      → compute_geometry_derivatives() # 翼面斜率 FXU[IC], FXL[IC]
      → DIFCOE()               # 差分系数 CXL/CXC/CXR, CYYC/CYYD/CYYU
      → SETBC(0)               # 将 FXU/FXL 编入 FXUBC/FXLBC（乘网格间距系数）
      → SOLVE()                # SOR 外迭代，每次调用 SYOR()
  → compute_data_summary()     # CL, CM
  → print_summary()
```

### 1.3 SYOR 列扫核心（2D 算子）

`SYOR` 逐 `I = IUP..IDOWN` 列扫，对当列所有 `J` 同时构造三对角系统并用 Thomas 算法求解：

```fortran
VC(J) = C1(I) - (CXL(I)*POLD(J,I2) + CXC(I)*P(J,I) + CXR(I)*P(J,I+1))
! VC > 0 → 亚声速（中心差分）；VC < 0 → 超声速（迎风）Murman-Cole 切换

DIAG(J) = (EMU(J,I1) - VC(J)) * CXXC(I) * WI + EMU(J,I2) * CXXR(I-1) - CYYC(J)
RHS(J)  = -[φ_xx 非线性项] - [φ_yy 三对角项] - [壁面/远场 BC 修正]
! Thomas 算法求解 → 更新 P(J, I)
```

三维扩展时，这套 `VC(J)` + `DIAG(J)` + Thomas 的逻辑**可以原样保留**，只需：
1. 添加外层 `K` 循环
2. 在 `RHS(J)` 中加入展向耦合项（φ_ζζ 的显式滞后贡献）
3. 在 `VC(J)` 系数里乘入度规修正（`1/c²` + `ξ_z²`）

### 1.4 扩展 Fortran 的限制

| 问题 | 具体表现 |
|---|---|
| 固定数组维度 | `P(NMP_plus2, NMP_plus1)` 硬编码 2D，无法直接加展向维 K |
| 模块耦合 | `solver_data`、`common_data`、`main_iteration` 紧耦合，添加第三维须同步修改 5 个 Fortran 文件 |
| f2py 接口 | 重构后需重新生成 `.pyf` 签名文件，调试成本高 |
| 单精度限制 | 现有 Fortran 用 `real`（f32），三维聚集误差更大，建议升 f64 |

**结论：不修改现有 Fortran 代码，新建独立 Python 包 `pytsfoil3d`，用 Numba JIT 重写核心内核。** 现有 Fortran 代码用于 2D 条带理论验证对照。

---

## 2. 三维扩展架构

### 2.1 坐标约定（与指南一致）

- `x`：弦向（来流方向），`ξ = (x - x_LE(z)) / c(z)` ∈ [0,1]
- `y`（或 `ζ`）：法向（竖直），正向向上，翼面在 `ζ = 0` 的 slit
- `z`（或 `η`）：展向，翼根 `z=0`（对称面），翼尖 `z=b/2`

索引约定：`i`（弦向）、`j`（法向）、`k`（展向），势场 `P[k, j, i]`（NumPy C 顺序，展向最慢）。

### 2.2 控制方程（非守恒形式，含几何变换后的完整骨架）

$$
\underbrace{\left[\frac{1}{c^2}\left(1-M_\infty^2 - \frac{(\gamma+1)M_\infty^2}{c}\phi_\xi\right) + \xi_z^2\right]}_{\text{type-dependent 系数 } A(\xi, \eta)} \phi_{\xi\xi}
+ \phi_{\zeta\zeta} + \phi_{\eta\eta} + (Y'^2)\phi_{\zeta\zeta}
+ \underbrace{2\xi_z\phi_{\xi\eta} - 2\xi_z Y' \phi_{\xi\zeta} - 2Y'\phi_{\eta\zeta}}_{\text{交叉项（滞后显式）}}
+ \xi_{zz}\phi_\xi - Y''\phi_\zeta = 0
$$

壁面 BC：`φ_ζ|_{ζ=0} = ∂f±/∂ξ - α(z)`（含扭转、弯度、厚度）

### 2.3 包结构

```
pytsfoil3d/
  __init__.py
  geometry.py      # 平面形定义：x_LE(z), c(z), Y(z), α(z), f±(ξ, z)
  metrics.py       # 度规：ξ_z(i,k), ξ_zz(i,k), 1/c(k), Y'(k), Y''(k)
  grid.py          # 结构化剪切笛卡尔网格（clustcos 弦向、聚 tip 展向、拉伸法向）
  bc.py            # 壁面、尾迹涡面 + Kutta、对称面 ghost、展向远场
  solver.py        # 三维 SLOR 主控：Murman-Cole 判别 + Thomas；K 循环 + 交叉项
  cp.py            # isentropic cp 反演（复用 2D 公式）
  continuation.py  # 低 AR → 高 AR 展向 morphing 热启动
  numba_kernels.py # @njit：列扫 Thomas、AF2 扫掠、展向 φ_zz 传递
  postprocess.py   # 展向 Cp(z)、CL(z)、力/力矩积分
```

---

## 3. 各模块设计

### 3.1 geometry.py

定义平面形几何，以函数形式供外部调用：

```python
class Planform:
    def x_le(self, z: np.ndarray) -> np.ndarray  # 前缘线 x_LE(z)
    def chord(self, z: np.ndarray) -> np.ndarray  # 当地弦长 c(z)
    def dihedral_y(self, z: np.ndarray) -> np.ndarray  # 上反偏置 Y(z)
    def twist(self, z: np.ndarray) -> np.ndarray  # 扭转角 α(z)，弧度
    def upper_surface(self, xi: np.ndarray, z: np.ndarray) -> np.ndarray  # f+(ξ,z)
    def lower_surface(self, xi: np.ndarray, z: np.ndarray) -> np.ndarray  # f-(ξ,z)
```

梯形机翼是最简单的特例（`x_le = z*tanΛ`，`c(z) = c_root - (c_root-c_tip)*z/(b/2)`）。

### 3.2 metrics.py

预计算并存储全局度规数组 `(ni, nk)` 形状（弦向 × 展向），供 solver 直接查表：

```python
def compute_metrics(planform, xi, z):
    """
    Returns dict with:
      xi_z[i,k]    = -(x_LE'(z) + xi[i]*c'(z)) / c(z)   # 关键：后掠+锥削合并
      xi_zz[i,k]   # 二阶项（梯形机翼 ≈ 0）
      inv_c2[k]    = 1/c(z)^2                              # 锥削缩放
      Y_prime[k]   = dY/dz                                 # 上反斜率
      Y_double[k]  = d²Y/dz²                               # 上反曲率
    """
```

这些度规在迭代开始前一次性算出，迭代内部只做查表乘法，不重复计算。

### 3.3 grid.py

三维结构化剪切网格：

- **ξ 方向（弦向）**：复用 `PyTSFoil.clustcos()`，在 LE/TE 处聚点；LE 落在两条网格线之间（与 2D 一致）
- **η 方向（展向）**：从 `z=0`（对称面）到 `z_max ≫ b/2`（展向远场），翼尖附近聚点
- **ζ 方向（法向）**：向远场指数拉伸，与 2D 的 `Y[J]` 分布相同

```python
class Grid3D:
    xi: np.ndarray    # shape (ni,)，物理 x 坐标通过 planform 还原
    eta: np.ndarray   # shape (nk,)，展向 z 坐标
    zeta: np.ndarray  # shape (nj,)，法向 y 坐标
    P: np.ndarray     # shape (nk, nj, ni)，势场（float64）
    
    # 关键索引
    ile: int          # 前缘 i-index
    ite: int          # 后缘 i-index
    jlow: int         # 翼面下方 j-index
    jup: int          # 翼面上方 j-index
    k_tip: int        # 翼尖最近 k-index（η[k_tip] ≈ b/2）
```

**展向域**：`z_max = 3.0 * (b/2)` 是合理起点（3 倍半展长外用 `φ = 0`）。

### 3.4 bc.py

| 边界 | 条件 | 实现 |
|---|---|---|
| 翼面 `ζ=0`，`i∈[ile,ite]`，`k∈[0,k_tip]` | `φ_ζ = ∂f±/∂ξ - α(z)` | 组装 `FXUBC[i,k]`, `FXLBC[i,k]`，乘网格间距系数 |
| 尾迹涡面，`i>ite`，`k∈[0,k_tip]` | `∂(Δφ)/∂ξ = 0`（稳态），Kutta | `PJUMP[i,k]` 持续更新 |
| 对称面 `k=-1`（ghost） | `P[k=-1, j, i] = P[k=+1, j, i]` | ghost cell，`φ_η = 0` |
| 翼尖外 `k > k_tip`，`ζ=0` | 无壁面 BC（普通流场） | 照常解齐次方程 |
| 展向远场 `k=kmax` | `φ = 0`（Dirichlet） | 直接设边界值 |
| 上/下/前/后远场 | 与 2D 相同（偶极子+涡渐近） | 直接移植 `compute_far_field_bc()` |

### 3.5 solver.py（核心）

三维 SLOR 迭代，最外层是展向 `K` 循环，内层复用 2D 的 Thomas 列扫逻辑：

```python
def solve_3d(grid, planform, metrics, config):
    P = grid.P  # (nk, nj, ni)
    
    for outer_iter in range(config.maxit):
        # 保存上一步用于滞后显式
        P_old = P.copy()
        
        for k in range(0, nk):  # 展向站（可并行化）
            # 计算当站展向耦合贡献（显式滞后）
            phi_zz_k = _spanwise_laplacian(P_old, k, eta)  # (nj, ni)
            
            # 计算当站交叉项贡献（显式滞后）
            cross_k = _cross_terms(P_old, k, metrics)       # (nj, ni)
            
            # 当站 type-dependent 系数（含 1/c² 和 ξ_z²）
            A_coeff = metrics.inv_c2[k] * (1 - Minf**2) + metrics.xi_z[:, k]**2
            
            # Thomas 列扫（内核），等同于 2D 的 SYOR
            # RHS 中加入 phi_zz_k 和 cross_k
            _thomas_sweep_k(P, k, phi_zz_k, cross_k, A_coeff, bc, config)
        
        # 更新 Kutta 条件（展向所有 k ≤ k_tip）
        _update_kutta(P, grid, config)
        
        # 检查收敛
        if _check_convergence(P, P_old, config):
            break
```

**Murman-Cole 判别**（与 2D 完全一致，只在 `φ_ξξ` 系数符号）：
```python
# type-dependent 系数（标量，当地计算）
coeff = A_coeff[i] - GAM1 * Minf**2 / c[k] * phi_xi   # 近似于 1 - M²
if coeff > 0:   # 亚声速 → 中心差分
    ...
else:           # 超声速 → 迎风（后差）
    ...
```

### 3.6 numba_kernels.py

**JIT 代价分析与策略**

Numba `@njit` 首次调用需要编译（通常 2–5 s），而 2D TSD 求解本身 < 1 s，因此：

| 使用场景 | JIT 代价 vs. 收益 | 策略 |
|---|---|---|
| **2D 独立生产使用** | 编译时间 > 求解时间，亏本 | 继续用 Fortran `.so`，不动 |
| **3D，~30–50 展向站 × ~1000 次迭代** | 编译一次，摊薄到秒–分钟级求解，合算 | Numba + `cache=True` |

`cache=True` 把编译产物写入 `__pycache__`，后续调用直接加载（毫秒级），只有源码改动才重新编译。这是 3D 的标准配置。

**三对角求解的备选方案**：`scipy.linalg.solve_banded` 调 LAPACK 带状求解器，无 JIT 开销，但 Murman-Cole 分支切换需要在 Python 层重建带状矩阵，引入额外 Python 循环。对 3D 大规模调用，Numba 仍更灵活；对单次 2D 调用可作应急替代。

**关键内核（均标注 `cache=True`）**：

```python
@njit(cache=True)
def thomas_column_sweep(P_k, phi_zz_rhs, cross_rhs, A_coeff,
                        FXUBC_k, FXLBC_k, PJUMP_k,
                        CXL, CXC, CXR, CYYC, CYYD, CYYU,
                        ile, ite, jlow, jup, nj, ni):
    """单展向站的完整列扫，含 Murman-Cole 切换 + Thomas 求解。
    2D 和 3D 共用同一份代码：3D 只是在外层加 k 循环调用此函数。"""

@njit(cache=True)
def spanwise_laplacian(P, k, dz):
    """计算第 k 站的 φ_ζζ（中心差分，对称面 ghost + 远场 Dirichlet）。"""

@njit(cache=True)
def kutta_update_strip(P, k, ite, jup, jlow, wcirc):
    """更新第 k 站的尾迹环量跳跃 PJUMP[i, k]（i > ite）。"""
```

**2D/3D 共用关系**：`thomas_column_sweep` 是 2D SYOR 的 Python 等价实现。先在 2D 场景下单独验证此函数，通过后直接被 3D 的 k 循环复用，不需要改代码。

### 3.7 cp.py

逐表面点 `(i, k)` 独立计算 isentropic Cp，与 2D 公式完全相同：

```python
def surface_cp(P, grid, planform, Minf, gamma=1.4):
    # 提取翼面 phi_x → 局部 Mach → Cp
    # 返回 cpu[k, i_foil], cpl[k, i_foil]（展向 × 弦向）
```

### 3.8 postprocess.py

```python
def spanwise_loading(cpu, cpl, xi, z, planform):
    """积分得展向升力分布 cl(z) = ∫(cpl - cpu) dξ，再积分得 CL。"""

def wave_drag_3d(P, grid, Minf, gamma):
    """动量积分法 wave drag，3D 版本在 (x,z) 平面积分。"""
```

---

## 4. 关键数值决策

### 4.1 展向耦合：滞后显式 vs. 直接求解

**采用滞后显式**（上一迭代步的 `P_old`），原因：
- 保留每展向站独立的三对角结构，Thomas 算法不变
- 展向 φ_ζζ 项是纯线性椭圆项，显式处理不影响稳定性（条件：`Δz` 足够细）
- 实现简单，与 SLOR 框架天然兼容

若收敛性出现问题（展向强梯度区域），可切换为 **Gauss-Seidel** 展向推进（用当前已更新的 `k-1` 站值）。

### 4.2 交叉项处理

`φ_ξη`（后掠）、`φ_ξζ`（后掠+上反）、`φ_ηζ`（上反）全部滞后显式进 `RHS`。对直梯形机翼（`ξ_zz ≈ 0`，`Y'' ≈ 0`），可先忽略二阶度规项，待主干跑通后再补。

### 4.3 守恒 vs. 非守恒

**阶段一用非守恒**：type-dependent 判别最自然，代码最简，弱激波精度足够。后续可以加入守恒选项（主要改通量定义和 Cp 公式），做成 `config.conservative = True/False`。

### 4.4 精度

全面升为 `float64`（不同于 2D Fortran 的 `float32`）。三维聚集误差更大，f32 在激波/翼尖区容易产生舍入噪声。

### 4.5 对称面处理

展向 ghost cell：`P[k=-1, j, i] = P[k=+1, j, i]`，等效于 `φ_η|_{z=0} = 0`。在 Numba 内核中，让 `k=0` 站的展向 Laplacian 用：
```
phi_zz[0] ≈ 2*(P[1] - P[0]) / dz[0]**2   # 对称面单侧差分
```

---

## 5. 落地顺序

### 阶段 0：2D Numba 内核验证（3D 内核的隔离测试）

**目标**：在二维可控场景下实现并验证 `thomas_column_sweep`，确认 Murman-Cole 切换、Thomas 算法、壁面 BC、Kutta 条件的 Python/Numba 实现与现有 Fortran 数值上一致。**这个内核就是后续 3D 每展向站调用的同一份代码，不是额外工作。**

**实现**：
- 用单一展向站（忽略 φ_zz 和交叉项，等价于 2D）调用 `thomas_column_sweep`
- 复用 `PyTSFoil.set_mesh()`、`compute_geometry_derivatives()` 生成相同网格和翼面斜率作为输入
- 用 `@njit(cache=True)` 标注，首次运行后编译产物缓存到 `__pycache__`

**验证点**（与 Fortran 输出逐点对比）：
- RAE2822，Ma=0.73，α=2.79°：上下表面 Cp 分布最大偏差 < 0.5%
- NACA0012，Ma=0.80，α=1.25°（含激波）：激波位置偏差 < 1 个网格间距，CL 偏差 < 0.1%
- 用 `assert np.allclose(cp_numba, cp_fortran, atol=1e-3)` 做自动化回归检查

**注意**：2D 生产用途继续用 Fortran `.so`，此阶段的 2D Numba 路径仅用于验证，不对外暴露接口。

### 阶段 1：矩形等截面无后掠机翼（验证三维主干）

**目标**：在已验证的 `thomas_column_sweep` 基础上，套上展向 k 循环，跑通 φ_zz 展向耦合 + 对称面 + 翼尖外场，不含任何平面形度规（`ξ_z=0`, `1/c²=1`, `Y'=0`）。

**验证点**：
- 取 `Nk=1`（单展向站，φ_zz 贡献为零）→ 结果应与阶段 0 Numba 2D 完全一致
- 取足够多展向点（`Nk≥30`）→ CL 应低于条带理论（三维释压效应）
- 与 2D `PyTSFoil` 对比：矩形翼各截面 Cp 应系统性低于 2D（三维使激波减弱/后移）

**实现步骤**：
1. `grid.py`：矩形翼网格，`z_max = 3.0*(b/2)`，`Nk=30`
2. `bc.py`：对称面 ghost、翼尖外普通流场、远场 Dirichlet
3. `numba_kernels.py`：新增 `spanwise_laplacian`（对称面单侧差分）
4. `solver.py`：SLOR 外迭代 + 展向 k 循环 + Kutta 更新（`thomas_column_sweep` 直接复用）

### 阶段 2：锥削 + 后掠（梯形机翼）

**目标**：开启度规 `ξ_z`、`inv_c2`、交叉项 `φ_ξη`。

**实现步骤**：
1. `geometry.py`：梯形机翼（`Λ`、展弦比 `AR`、锥削比 `λ`）
2. `metrics.py`：计算 `ξ_z[i,k]`、`inv_c2[k]`
3. 修改 `thomas_column_sweep`：在 type-dependent 系数中加 `ξ_z² + inv_c2` 修正；`φ_ξη` 进 RHS
4. **验证对象**：ONERA M6 机翼（Ma=0.84, α=3.06°），与实验 Cp 数据对比

### 阶段 3：扭转 + 上反

**目标**：扭转 `α(z)` 进 BC，上反 `Y(z)` 进度规。

- 扭转：仅修改 `bc.py` 中 `FXUBC[i,k]` 施加时减去 `α(k)`，代价最小
- 上反：计算 `Y'[k]`、`Y''[k]`，补充 `φ_ξζ`、`φ_ηζ` 交叉项和 `(1+Y'²)φ_ζζ` 系数

### 阶段 4：性能优化与高 AR

**目标**：Numba 全面 JIT 化，引入 continuation 热启动，可选 AF2 加速。

1. `numba_kernels.py`：AF2 三次扫掠（弦向→展向→法向），比 SLOR 快约一个量级
2. `continuation.py`：从低 AR（收敛快）解热启动高 AR 计算
3. 展向并行：`P[k, :, :]` 的各展向站无数据依赖（仅 φ_zz 依赖已滞后显式），可 `prange` 并行

---

## 6. 与现有 2D 代码的接口

三维代码**不修改**现有 `pytsfoil/` 包，以只读方式使用 2D 代码进行验证：

```python
# 在 pytsfoil3d/validation.py 中
from pytsfoil import run_airfoil_analysis  # 2D 条带对照

def strip_theory_compare(planform, Minf, alpha, k_station):
    """对比 3D 解与 2D 条带解的 Cp 差异，量化三维释压效果。"""
```

面向用户的入口：

```python
from pytsfoil3d import run_wing_analysis

result = run_wing_analysis(
    planform=TrapezoidalWing(sweep=30, AR=8, taper=0.4),
    airfoil_coords=coords,      # 共用截面翼型
    Mach=0.84, AoA=3.0,
)
# result: CL, CD_wave, Cp_upper[k, i], Cp_lower[k, i], cl_z[k]
```

---

## 7. 预期工作量与风险

| 阶段 | 主要工作 | 风险 |
|---|---|---|
| 1（矩形翼） | grid + bc + SLOR 内核 + 收敛验证 | 展向滞后显式不稳定（Δz 选择） |
| 2（梯形后掠） | metrics + 度规修正 + ONERA M6 验证 | 翼尖 `c(z)→0` 度规奇异需特殊封口 |
| 3（扭转/上反） | BC 修改 + 附加交叉项 | 上反大角度时平面尾迹假设失效 |
| 4（性能） | Numba AF2 + continuation | AF2 边界处理比 SLOR 复杂 |

**最大技术坑**（来自指南 §8）：
- 翼尖 `c(z)→0`：`ξ_z = -(x_LE' + ξ·c')/c` 发散，需在 `k ≥ k_tip` 处做独立封口（单侧差分或奇异正则化）
- 高后掠（>30°）：小扰动假设在前缘附近失真，`w ~ tanΛ·u` 接近 O(1)；验证时注意 ONERA M6 的 `Λ=30°` 正处于临界处

---

## 8. 文件对应关系（2D → 3D）

| 2D Fortran | 3D Python/Numba | 功能 |
|---|---|---|
| `common_data.f90`：`X[I], Y[J], FXU[IC], FXL[IC]` | `grid.Grid3D`：`xi, eta, zeta, P[k,j,i]` | 坐标和势场 |
| `solver_data.f90`：`CXL/CXC/CXR, CYYC/D/U` | `numba_kernels.py` 局部变量 | 差分系数 |
| `solver_base.f90`：`DIFCOE()` | `grid.py` + `metrics.py` | 预计算系数 |
| `solver_functions.f90`：`SETBC()` | `bc.py`：`build_wall_bc(planform, grid, k)` | 壁面 BC 施加 |
| `main_iteration.f90`：`SYOR()` | `numba_kernels.py`：`thomas_column_sweep()` | 列扫 Thomas |
| `main_iteration.f90`：`SOLVE()` | `solver.py`：`solve_3d()` | 外迭代控制 |
| `pytsfoil.py`：`cdcole_python()` | `postprocess.py`：`wave_drag_3d()` | 阻力积分 |
| `pytsfoil.py`：`_cp_isentropic()` | `cp.py`：`surface_cp()` | 等熵 Cp |
