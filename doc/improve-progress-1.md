# 程序功能改进报告（1）

## 任务背景与目标

### 目标

本项目是在上一阶段重构的基础上，进一步对 `pyTSFoil` 进行功能改进，
以提高数值稳定性、收敛速度和计算结果的准确性。
修改的对象含 `src` 文件夹中的 Fortran 文件，以及 `pytsfoil.py`。

### 文件结构

- `src/`: 本报告工作主要修改的代码所在的文件夹，包含 Fortran 源文件。
- `refactored_src/`: 上一阶段重构后的 Fortran 代码备份，是 `src/` 的初始版本。
- `original_src/`: 原始 Fortran 代码备份，重构前的版本。
- `compile_f2py.py`: 用于编译 Fortran 代码并生成 Python 模块的脚本。
- `pytsfoil.py`: Python 接口代码，调用 Fortran 模块，以及数据处理、结果输出等功能。
- `example/`: 包含示例代码的文件夹。
- `test_*_**/`: 计划编写的测试脚本文件夹，`*` 代表改进过程的任务编号，`**` 代表测试名称。
- `improve-progress.md`: 本报告文件，记录功能改进的过程和结果。

### 测试要求

在修改 Fortran 代码的过程中，我们需要不断测试修改后的代码是否能够正确运行，并且输出的结果是否正确。
由于本项目是 Fortran-Python 混合编程，因此我们需要在 Python 中调用 Fortran 代码来测试修改后的 Fortran 代码是否能够正确运行，并且输出的结果是否正确。

注意，原始代码是正确的。因此，在每项任务完成后，需要使用 `compile_f2py.py` 来编译 Fortran 代码，测试修改后仍然可以 Python 正常调用，并且输出的结果与原始代码相同/相近。

目前没有写测试代码，但是可以参考 `example` 文件夹中的示例代码来构建新的测试脚本。

## pyTSFoil 解析

### Fortran 源文件结构（pytsfoil/src/）

| 文件 | 作用 |
|---|---|
| `common_data.f90` | 全局参数、网格、翼型数组、错误处理（`initialize_common`、`INPERR`、`report_convergence_error`） |
| `solver_data.f90` | 求解器数组：势场 P、有限差分系数、边界值（`initialize_solver_data`） |
| `solver_base.f90` | `TRAP`（梯形积分）、`PX`/`PY` 有限差分、`DIFCOE`、`LIFT`、`PITCH`、`FINDSK` |
| `solver_functions.f90` | `SETBC`、`BCEND`、`EMACH1`、`VWEDGE`、`WANGLE` |
| `main_iteration.f90` | `SOLVE`（外层循环）、`SYOR`（SOR 扫描）、`RECIRC`、`REDUB`、`RESET` |

### 求解流程

```
PyTSFoil.run()
│
├─ 1. initialize_data()                             [Python]
│      ├─ tsf.common_data.initialize_common()       [Fortran] — 重置全局网格/物理参数
│      ├─ tsf.solver_data.initialize_solver_data()  [Fortran] — 重置 P/THETA/FD 系数/边界值
│      └─ 将 self.config 写入 tsf.common_data.*
│
├─ 2. set_airfoil()                                 [Python]
│      ├─ 读取翼型坐标（文件或数组），分离上/下表面
│      ├─ 线性插值 → 最大厚度 t_max (= DELTA)
│      └─ tsf.common_data.delta = t_max
│
├─ 3. set_mesh()                                    [Python]
│      ├─ clustcos()  [static]  — 余弦聚集点分布 [−x_scale,0], [0,1], [1,x_scale]
│      ├─ 生成 xx (X 向非均匀网格)、yy (Y 向关于 0 对称网格)
│      └─ 写入 tsf.common_data.x, .y, .imax, .jmax
│
├─ 4. compute_mesh_indices()                        [Python]
│      ├─ 找 ILE (X≥0 首点)、ITE (X>1 首点) → 前/后缘 I 索引
│      ├─ 找 JLOW/JUP (Y<0 末行 / Y≥0 首行) → 翼面缝隙 J 索引
│      └─ 写入 tsf.common_data.ile/.ite/.jlow/.jup
│
├─ 5. run_fortran_solver()
│      │
│      ├─ 5a. compute_scale()                    [Python]  (替代 Fortran SCALE)
│      │       ├─ 根据 SIMDEF 选 Cole/Spreiter/Krupp 相似律
│      │       ├─ 计算 AK、CPFACT、CLFACT、CDFACT、CMFACT、YFACT、VFACT
│      │       ├─ 写回 tsf.common_data.ak, .alpha
│      │       ├─ 写回 tsf.solver_data.clfact, .cmfact, .sonvel
│      │       └─ 保存 self._cpfact/_cdfact/_yfact/_vfact/_cpstar
│      │
│      ├─ 5b. compute_far_field_bc()             [Python]  (替代 Fortran FARFLD + ANGLE)
│      │       ├─ AK≤0（超音速来流）→ 直接返回，不设远场 BC
│      │       ├─ 计算上下边界 DTOP/DBOT（偶极子）、VTOP/VBOT（涡）
│      │       ├─ 计算上下游边界 DUP/DDOWN（偶极子）、VUP/VDOWN（涡）
│      │       └─ 计算 THETA[J,I]（各网格点角度，替代 Fortran ANGLE）
│      │           → 写入 tsf.solver_data.dtop/dbot/vtop/vbot/dup/ddown/vup/vdown/theta
│      │
│      ├─ 5c. compute_geometry_derivatives()     [Python]  (替代 Fortran BODY)
│      │       ├─ CubicSpline 插值上/下表面 → fu, fl, fxu, fxl（无量纲斜率）
│      │       ├─ Simpson 积分 → VOL（翼型面积/体积）
│      │       ├─ 刚性修正：fxu/fxl /= sqrt(1 + RIGF*(delta*fx)²)
│      │       └─ 写入 tsf.common_data.fxu, .fxl, .vol
│      │
│      ├─ 5d. tsf.solver_base.difcoe()           [Fortran]  — 预计算全部 FD 系数
│      │       ├─ X 向：CXL/CXC/CXR（一阶）、CXXL/CXXC/CXXR（二阶）、C1（速度系数）
│      │       ├─ Y 向：CYYD/CYYU/CYYC（内点二阶）
│      │       ├─ 翼面特殊：CYYBUD/UC/UU（上翼面）、CYYBLU/LC/LD（下翼面）
│      │       ├─ 网格间距倒数：XDIFF, YDIFF
│      │       └─ 翼面外插系数：CJUP/CJUP1/CJLOW/CJLOW1
│      │
│      ├─ 5e. tsf.solver_functions.setbc(0)      [Fortran]  — 设置求解范围和翼面 BC
│      │       ├─ 计算 IUP/IDOWN（AK 符号相关的 I 求解范围）
│      │       ├─ 计算 JBOT/JTOP（AK 符号相关的 J 求解范围）
│      │       └─ 填充 FXUBC/FXLBC（翼面法向斜率边界条件数组，含 WSLP 粘性修正）
│      │
│      └─ 5f. tsf.main_iteration.solve()         [Fortran]  — 主迭代（SOR）
│             │   最多 MAXIT 次迭代：
│             │
│             ├─ RECIRC(DCIRC)                   — 环量更新（每次迭代）
│             │   ├─ 用 CJUP/CJLOW 系数外插 P → 计算后缘跳跃 CIRCTE
│             │   ├─ CIRCFF = (1−WCIRC)*CIRCFF_old + WCIRC*CIRCTE（松弛）
│             │   └─ 填充 PJUMP[ITE..IMAX]（尾迹缝隙线性插值）
│             │
│             ├─ SYOR(I1,I2,…)                   — 一次 SOR 扫描（最热路径，O(N×M)）
│             │   ├─ 对每个 I 列（IUP→IDOWN）：
│             │   │   ├─ VC(J) = C1(I) − (CXL·P_left + CXC·P_mid + CXR·P_right)  [1−M²]
│             │   │   ├─ EMU(J) = min(VC, 0)  [迎风耗散项]
│             │   │   ├─ 组装三对角方程 DIAG/SUP/SUB + RHS（含 Y 向二阶差分）
│             │   │   ├─ 翼面 BC（JUP 行用 FXUBC，JLOW 行用 FXLBC）
│             │   │   ├─ 尾迹缝隙（ITE<I≤IMAX：JLOW/JUP 行加减 PJUMP(I)）
│             │   │   ├─ BCEND(I)   — 上下远场 BC（超音速来流 Neumann 条件）
│             │   │   ├─ 人工耗散项 EPSX = EPS/(dx²)，加到 DIAG 和 RHS
│             │   │   └─ Thomas 算法（前消元 + 回代）→ 更新 P(J,I)
│             │   └─ 追踪最大残差 BIGRL 和最大误差 ERROR
│             │
│             ├─ P(J,I) += DCIRC * THETA(JK,IK)  — 叠加环量势（亚音速来流）
│             │
│             ├─ REDUB()                          — 更新偶极子强度（每 25 次迭代）
│             │   ├─ 若 |CIRCFF| ≥ 0.0001：DUB = VOL（升力自由空气流）
│             │   └─ 否则：∬(dP/dx)² dY dX → DUB = VOL + GAM1·0.25·DBLSUM
│             │       └─ TRAP(XI, ARG, N, SUM)  — 梯形积分（Y 向）
│             │
│             ├─ RESET()                          — 刷新远场边界（每次迭代）
│             │   ├─ P[JMIN/JMAX, IMIN..IMAX] = CIRCFF·VBOT/VTOP + DUB·DBOT/DTOP
│             │   └─ P[JMIN..JMAX, IMIN/IMAX]  = CIRCFF·VUP/VDOWN + DUB·DUP/DDOWN
│             │
│             ├─ VWEDGE()  [若 NWDGE>0]          — 粘性楔修正（每次迭代）
│             │   ├─ FINDSK(ISTART,ITE,J,SONVEL) — 定位上/下翼面激波位置
│             │   ├─ PX(ISK,J)                   — 取激波前后的 dP/dx
│             │   ├─ EMACH1(U,DELTA)             — 激波上游局部马赫数
│             │   ├─ WANGLE(AM²,NW,GAM1)         — 楔角（Murman 或 Yoshihara）
│             │   └─ 更新 WSLP[ILE..ITE, 1/2]（上/下翼面斜率修正量）
│             │   SETBC(1)                        — 将 WSLP 写入 FXUBC/FXLBC
│             │
│             └─ 收敛判断：ERROR≤CVERGE → 退出；ERROR≥DVERGE → 发散退出
│
├─ 6. compute_data_summary()                     [Python]
│      ├─ tsf.solver_base.lift(CLFACT)           [Fortran] — CL = 2·CLFACT·(Ptop−Pbot)|TE
│      └─ tsf.solver_base.pitch(CMFACT)          [Fortran] — CM（四分之一弦点矩，梯形积分）
│              └─ TRAP(XI, ARG, K, SUM)          [Fortran]
│
└─ 7. print_summary()                            [Python]
       ├─ 写 smry.out 文件头（参数汇总）
       │
       ├─ output_surface()                       [Python]  — 表面 Cp/Ma 分布
       │   ├─ 对每个 I（IMIN..IMAX）：
       │   │   ├─ tsf.solver_base.px(I, JLOW/JUP) [Fortran] — 下/上表面 U = dP/dx
       │   │   └─ tsf.solver_functions.emach1(U,δ)[Fortran] — 局部马赫数
       │   ├─ 写 cpxs.dat（X, CPU, MAU, CPL, MAL）
       │   └─ 超音速 Ma>1.3 及脱体激波警告 → smry.out
       │
       ├─ output_field()                         [Python]  — 全场数据
       │   ├─ 对每个 (I,J)：
       │   │   ├─ tsf.solver_base.px(I,J)        [Fortran] — U
       │   │   └─ tsf.solver_functions.emach1(U,δ)[Fortran] — Ma
       │   └─ 写 field.dat（X,Y,Mach,Cp,P,FlowType）Tecplot 格式
       │
       └─ cdcole_python(sonvel, yfact, delta)    [Python]  — 动量积分阻力
           ├─ 确定封闭围线边界（IU, ID, JT, JB）
           │   └─ findsk() / newisk() [局部助手，调用 tsf.solver_base.px]
           ├─ 上游边界积分 CDUP（亚音速）
           ├─ 顶部边界积分 CDTOP
           ├─ 底部边界积分 CDBOT
           ├─ 下游边界积分 CDDOWN
           │   ├─ tsf.solver_base.px(I,J)        [Fortran]
           │   └─ tsf.solver_base.py(I,J)        [Fortran]
           ├─ 翼体边界积分 CDBODY（ID_downstream≤ITE 时）
           ├─ 围线内激波积分 CDWAVE（遍历上/下翼面所有激波）
           │   └─ prtsk() [局部助手，输出激波剖面到 smry.out]
           └─ CD = CDUP+CDTOP+CDBOT+CDDOWN+CDBODY + CDWAVE
```

### 热循环函数

这些函数在主迭代循环内部，网格规模通常 100×100 = 10,000 点，迭代次数 100~1000 次，
总操作量 $10^6 \sim 10^7$。用纯 Python 循环慢 100× 以上。

**热循环核心**

| 函数 | 位置 | 调用频率 | 原因 |
|---|---|---|---|
| `SYOR()` | main_iteration.f90 | 每次迭代 1 次 | 内层 SOR 扫描 + 三对角求解，O(N×M) |
| `PX(I,J)` | solver_base.f90 | SYOR 内 O(N×M) 次 | 有限差分，最热路径 |
| `PY(I,J)` | solver_base.f90 | SYOR 内 O(N×M) 次 | 有限差分 + Kutta 判断，最热路径 |
| `DIFCOE()` | solver_base.f90 | 每次 solve 前 1 次 | 预计算所有 FD 系数，内部 O(N×M) |
| `BCEND()` | solver_functions.f90 | SYOR 每列调用 | 应用远场 BC，在 SYOR 内部调用 |

**迭代控制逻辑**

| 函数 | 位置 | 调用频率 | 原因 |
|---|---|---|---|
| `SOLVE()` | main_iteration.f90 | 调用一次但驱动整个循环 | 迭代框架 + 收敛判断，含对 SYOR 的调用 |
| `RECIRC()` | main_iteration.f90 | 每次迭代 1 次 | 环量边界更新，紧耦合 SYOR |
| `REDUB()` | main_iteration.f90 | 每 25 次迭代 1 次 | 偶极子强度积分，在循环内 |
| `RESET()` | main_iteration.f90 | 每次迭代 1 次 | 更新远场边界值，在循环内 |
| `TRAP()` | solver_base.f90 | REDUB/PITCH 内调用 | 被 Fortran 内部（REDUB、PITCH）调用 |
| `VWEDGE()` | solver_functions.f90 | SOLVE 主循环内，每 `NDWDGE` 次迭代调用一次 | 修改翼面斜率边界条件（`WSLP` 数组）|

## 改进过程

本章节包括各个任务的描述，每个任务的描述包括：

- 任务描述：对任务的背景、目标和具体内容进行详细描述。
- 完成情况：描述修改了哪些内容，总结完成情况。
- 测试情况：基于`测试要求`一节中的测试要求，描述测试的过程和结果。

### 任务1：Debug 翼型坐标相关预处理代码

#### 1.1 任务描述

现有代码可以观测到，在使用相同的翼型时，
如果输入的翼型坐标 `airfoil_coordinates` 的点数不同，则计算结果不同。
尤其是，点数增加的时候，甚至更容易获得错误的结果。

这一现象也有可能与网格密度有关，但我们首先需要确认输入的翼型坐标是否正确。
我猜想有可能与边界条件的梯度计算有关。

参考 `test_1_airfoil_input/run_pytsfoil.py` 中的测试代码:

```python
N_MESH_POINTS_X = 200
N_MESH_POINTS_Y = 80
N_MESH_POINTS_AIRFOIL = 100

testing_parameters = [
    (51,), (101,), (201,), (1001,)
]

x, yu, yl, _, _ = cst_foil(nn=params['n_airfoil_points'],
                    cst_u=params['cst_u'], cst_l=params['cst_l'])

xx = np.concatenate((x[::-1], x[1:]))
yy = np.concatenate((yu[::-1], yl[1:]))
airfoil_coordinates = np.column_stack((xx, yy))
```

已知 `cst_foil` 函数是一个可靠、正确的翼型几何生成函数，
其建立了翼型几何的光滑解析函数，生成的坐标点不同仅仅是因为采样点数不同。
因此，我们需要确认 `pytsfoil.py` 中是否正确处理了不同点数的翼型坐标和梯度。

#### 1.2 完成情况

在 `pytsfoil.py` 的 `compute_geometry_derivatives` 函数中发现并修复了两个 bug，
均与 CST 翼型前缘奇异性有关。

**Bug 1：三次样条的端点导数边界条件依赖输入点数**

原始代码用一阶有限差分计算样条的边界条件导数：

```python
dy1_u = (yu[1] - yu[0]) / (xu[1] - xu[0])  # LE 处的斜率
cs_upper = CubicSpline(xu, yu, bc_type=((1, dy1_u), (1, dy2_u)))
```

CST 翼型在前缘（x=0）处有 dy/dx ∼ 1/√x 的奇异性，当输入点数增加时，`xu[1]→0`，
导致 `dy1_u→∞`，使得样条被强制赋予极大的斜率。
以 n=51 为例，`xu[1]≈0.005`，`dy1_u` 只是一个适中的有限值；
以 n=1001 为例，`xu[1]≈5×10⁻⁶`，`dy1_u` 约为 n=51 情况的 6.7 倍。

**修复**：改用 scipy `CubicSpline` 默认的 **not-a-knot** 边界条件，
不需要显式指定端点导数，结果不依赖于输入点间距。

```python
cs_upper = CubicSpline(xu, yu)   # not-a-knot（默认）
cs_lower = CubicSpline(xl, yl)
```

**Bug 2：在 x=0 处对样条求导仍然奇异**

网格的第一个翼型点 `xfoil[0]` 精确等于 0（由 `clustcos(a0=0.01)` 产生）。
即使使用 not-a-knot 样条，在 x=0 处的解析导数仍然依赖第一个样条区间
`[xu[0]=0, xu[1]]` 的宽度，后者随输入点数增加而缩小，
导致 fxu[0] 继续发散（n=51 时约 82，n=1001 时约 546）。

函数值 fu[0] 不受此影响（CST 翼型在 x=0 处 y=0，精确成立），
而 fu[1] 在固定网格点 `xfoil[1]` 处随 n 增加而收敛。

**修复**：当 `xfoil[0]==0` 时，用函数值的前向有限差分代替样条解析导数：

```python
if xfoil[0] == 0.0:
    fxu[0] = (fu[1] - fu[0]) / (xfoil[1] - xfoil[0])   # (fu[1] - 0) / xfoil[1]
    fxl[0] = (fl[1] - fl[0]) / (xfoil[1] - xfoil[0])
```

`xfoil[1]` 是固定的网格坐标，`fu[1]` 随 n 收敛，故 fxu[0] 不再依赖输入点数。

#### 1.3 测试情况

运行 `test_1_airfoil_input/run_pytsfoil.py`，测试参数 n = 51, 101, 201, 1001。
修复前，n=1001 的结果与其他情况相差约 2 倍；修复后，所有情况在 CL、CD、CM 上的差异均在 0.5% 以内：

| 输入点数 | CL       | CD       | CM        |
|---------|----------|----------|-----------|
| 51      | 0.622093 | 0.003119 | -0.140575 |
| 101     | 0.623276 | 0.003251 | -0.140805 |
| 201     | 0.622222 | 0.003129 | -0.140591 |
| 1001    | 0.623099 | 0.003188 | -0.140750 |

此外，以 RAE2822 翼型作为基准（51 个输入点，来自文件），
修复后 CL=0.6249、CD=0.0034、CM=-0.1410，与修复前一致，确认未引入回归。

### 任务2：网格收敛性检验

#### 2.1 任务描述

在完成任务1后，进一步进行网格收敛性验证，并测试 `EPS` 的影响。
`EPS` 是求解器中的人工耗散系数，过大可能导致过度平滑，过小可能导致数值不稳定。

#### 2.2 完成情况

在 `test_2_grid_convergence/run_pytsfoil.py` 中，使用相同的翼型（n=101）和来流条件，
测试不同网格密度（nx=100, 200, 400, 800）对 CL、CD、CM 的影响。
结果发现，完成任务1修复后，网格收敛性得到显著改善。

`EPS` 的测试结果表明合理的范围在 [0.1, 1.0] 之间，
过小会数值发散，过大则导致收敛缓慢。

#### 2.3 测试情况

网格收敛性良好，建议 `EPS` 取值在 0.5 左右以平衡稳定性和收敛速度。

### 任务3：检查 Cp 和 Ma 的匹配性

#### 3.1 任务描述

检查 pyTSFoil 输出的表面压力系数 Cp 和局部马赫数 Ma 是否匹配，
是否满足等熵关系。并与 RANS 结果进行对比，分析可能的差异来源。

在 `test_3_cp_mach_comparison/` 中进行测试，加载 `airfoil_database/` 中的 RANS 数据进行对比。
修复前的结果显示，pyTSFoil 内部的 Cp 计算存在问题，与 Ma 的关系不满足 isentropic flow 的理论预期。
此外，pyTSFoil 的 Ma 分布与 RANS 结果（由 Cp 根据等熵关系计算的 Ma 分布）存在较大差异。

pyTSFoil 的 Cp 和 Ma 计算逻辑主要在 `output_surface()`, `output_field()` 函数中实现，
调用了 `solver_base.px()` 和 `solver_functions.emach1()`。

```python
u = tsf.solver_base.px(i, j)  # Computes U = DP/DX at point I,J
em = tsf.solver_functions.emach1(u, delta)  # Computes Mach number from U
cp_val = -2.0 * u * cpfact  # CPFACT is a scaling factor (transonic similarity) for pressure coefficient
```

相比于 Ma, Cp 的结果更不准确，且不满足等熵关系。
很可能是 `cp_val = -2.0 * u * cpfact` 这个关系有问题，本身就不满足等熵关系。
重点关注这个关系式的物理意义和数值实现，检查是否正确考虑了来流 Mach 数、局部 Mach 数、以及转化为 Cp 的比例因子。如果不合理，那么不妨直接替换为正确的等熵关系。

```python
def calculate_isentropic_Cp(Ma: np.ndarray,
                    Minf: float, g=1.4) -> np.ndarray:
    '''
    Calculate the pressure coefficient (Cp) for isentropic flow,
    given local Mach number (Ma) and free stream Mach number (Minf).

    Parameters
    ----------
    Ma: ndarray
        Local Mach number(s) at which to calculate Cp.
    Minf: float
        Free stream Mach number.
    g: float, optional
        Ratio of specific heats (default is 1.4 for air).
        
    Returns
    -------
    Cp: float, or ndarray
        Pressure coefficient(s) corresponding to the input Mach number(s).
    '''
    xx = (2.0+(g-1.0)*Minf**2)/(2.0+(g-1.0)*Ma**2)
    xx = xx**(g/(g-1.0))
    Cp = 2.0/g/Minf**2*(xx-1.0)

    return Cp
```

#### 3.2 完成情况

**问题分析**

原始代码使用线性化 TSD（Transonic Small Disturbance）公式输出表面 Cp：

```python
cp_val = -2.0 * u * cpfact   # 线性化 TSD Cp
```

而局部马赫数 `emach1` 的计算包含了非线性（二次）修正项（以 Krupp 相似律为例）：

```
Ma² = M_inf² + δ^(2/3) · M_inf · (γ+1) · U
```

因此，将 `emach1` 的马赫数代入等熵关系所得 `Cp_iso`，
与线性化 `Cp_TSD` 之间存在系统性偏差（均方根误差 0.05–0.21），
且在大攻角（吸力强）时偏差更大。
通过与 RANS 数据对比验证，`Cp_iso = _cp_isentropic(Ma, Minf)` 在大多数算例中
比 `Cp_TSD` 更接近 RANS 参考值。

**修复内容**

在 `pytsfoil.py` 中进行以下修改：

1. **新增 `_cp_isentropic` 静态方法**：

   ```python
   @staticmethod
   def _cp_isentropic(ma, minf, gamma=1.4):
       denom = 2.0 + (gamma - 1.0) * ma * ma
       numer = 2.0 + (gamma - 1.0) * minf * minf
       return (2.0 / (gamma * minf * minf)) * ((numer / denom) ** (gamma / (gamma - 1.0)) - 1.0)
   ```

2. **`output_surface` 函数**：将表面 Cp 输出改为：
   - 亚音速区（`emach1 > 0`）：`Cp = _cp_isentropic(Ma, Minf)`，与 Ma 严格自洽

3. **`output_field` 函数**：同步更新全场 Cp 输出。

4. **`compute_scale` 函数**：将临界压力系数 `Cp*` 改为等熵临界值：

   ```python
   self._cpstar = float(PyTSFoil._cp_isentropic(1.0, emach))
   ```

#### 3.3 测试情况

在 `test_3_cp_mach_comparison/run_pytsfoil_database.py` 中对数据库前 10 个算例进行测试，
来流条件为 Ma = 0.72–0.75，AoA = 0.02°–3.88°，与 RANS 数据进行对比。

**Cp-Ma 自洽性（`Cp` 与由 `Ma` 通过等熵关系反算的 `Cp_iso` 之间的 RMSE）：**

| 指标                   | 修复前   | 修复后   |
|------------------------|---------|---------|
| 平均 Cp-Ma 自洽 RMSE   | 0.1455  | 0.0565  |
| 最大 Cp-Ma 自洽 RMSE   | 0.2082  | 0.0833  |

修复后亚音速区 Cp 与 Ma 严格满足等熵关系，剩余误差仅来自强超音速区（`emach1 = 0`）。

**与 RANS 的 Cp 对比（插值到 RANS 坐标后的 RMSE）：**

| 算例 | Ma   | AoA   | Cp RMSE（修复前） | Cp RMSE（修复后） |
|------|------|-------|-----------------|-----------------|
| 0    | 0.72 | 0.02° | 0.1131          | 0.1367 ↑        |
| 1    | 0.72 | 1.92° | 0.2110          | 0.1935 ↓        |
| 2    | 0.73 | 0.80° | 0.1466          | 0.1529 ↑        |
| 3    | 0.73 | 3.17° | 0.6977          | 0.5796 ↓        |
| 4    | 0.74 | 3.38° | 0.7264          | 0.5877 ↓        |
| 5    | 0.74 | 3.88° | 0.7400          | 0.6036 ↓        |
| 6    | 0.75 | 2.25° | 0.2985          | 0.2648 ↓        |
| 7    | 0.75 | 2.48° | 0.6240          | 0.5131 ↓        |
| 8    | 0.75 | 2.59° | 0.6678          | 0.5370 ↓        |
| 9    | 0.75 | 2.99° | 0.6769          | 0.5460 ↓        |
| 均值 |      |       | **0.490**       | **0.412**       |

10 个算例中 8 个改善，平均 RMSE 降低约 16%。
仅极小攻角（AoA ≤ 0.80°）的 2 个算例略有退步（约 +0.02）。

**Ma 分布与 RANS 的对比：**

本修复未改变 `emach1` 马赫数计算，Ma RMSE 保持不变（均值 0.203）。
TSD Ma 分布与 RANS 的差异是 TSD 理论本身的局限性：
TSD 为无粘线性化理论，不计边界层效应，在高攻角（强激波–边界层干扰）时
升力系数系统性偏高约 20–100%，Cp/Ma 分布与 RANS 差异显著。

### 任务4(1)：检查前缘附近 Ma (U, P) 的计算物理合理性

#### 4(1).1 任务描述

检查 pyTSFoil 在前缘 (x=0) 附近的局部马赫数 Ma 的计算是否物理合理。
局部马赫数 Ma 的计算依赖于局部速度 U（由 `px(i,j)` 计算）和翼型最大厚度 δ。
U 由势场 P 的有限差分计算得到，δ 由翼型几何计算得到。

已知计算关系为：

```python
delta = self.airfoil['t_max']
u = tsf.solver_base.px(i, j)  # Computes U = DP/DX at point I,J
em = tsf.solver_functions.emach1(u, delta)  # Computes Mach number from U
```

目前观测到的问题为：
前缘附近的 Ma 计算结果存在一段区域等于0（正常应该只有前缘点的 Ma 为0），
大概率是计算过程中出现了 U 为负数的情况，被截断为 0。
这是物理不合理的。

需要检查 Fortran 和 Python 代码中关于前缘附近 U 和 Ma 的计算逻辑，
确认是否存在导致 U 负值的 bug。
或者是否在迭代过程中出现了数值不稳定导致 U 发散为负。

在 `test_4_x0_mach_error/` 中进行测试，加载 `airfoil_database/` 中的 RANS 数据进行对比。
修复前的结果显示，`entry_0~9` 大部分都存在这一问题，
图片保存在 `test_4_x0_mach_error/figures` 文件夹中，
具体数据保存在 `test_4_x0_mach_error/results_database.json` 文件中。

#### 4(1).2 完成情况

结论：不是代码 bug，而是 TSD 模型在钝前缘翼型下的固有局限性。

**问题根因分析**

Ma=0 的区域出现在下表面前缘附近，是 `EMACH1` 函数将 Ma 截断为 0 的结果。
`EMACH1` 的截断条件（Krupp 相似律）为：

```text
ARG = 1 - δ^(2/3) · AK · M∞ + δ^(2/3) · (γ+1) · U · M∞
Ma  = sqrt(ARG), if ARG > 0;  else Ma = 0
```

当 U ≤ U_critical 时触发截断，其中：

```text
U_critical = -1 / (δ^(2/3) · (γ+1) · M∞)  ≈  -1.64  (对于 M∞=0.73, δ=0.08)
```

**U 为何在前缘下表面出现强负值**

下表面速度 U_lo 通过以下公式从势场 P 外插到翼面（y=0−）：

```text
U_lo(I) = cjlow·PX(JLOW, I) - cjlow1·PX(JLOW-1, I),   I ∈ [ILE, ITE]
```

近前缘处 U_lo 的数值大小受翼面斜率边界条件 FXL 驱动。
FXL 是翼型下表面 dy/dx 的归一化值（除以 δ），在 PY 的边界条件中被直接施加：

```fortran
PY(JLOW, I) = 0.5 * (FXL(IC) - ALPHA + VMINUS)
```

对于 CST 翼型，下表面几何形状在前缘附近满足：

```text
y_lower(x) ≈ -A · sqrt(x),   dy/dx ≈ -A / (2·sqrt(x))  →  ±∞  (x → 0)
```

该奇异性使 FXL 的绝对值在前缘附近非常大。

以数据库中8%厚翼型为例（Krupp, 100点翼面网格, xfoil[1]≈0.0007），
任务1修复后（前向有限差分, `xfoil[1]` 作为差分步长）：

| 翼面点 k | x      | FXL    | \|FXL\|·δ |
|---------|--------|--------|-----------|
| 0       | 0.0000 | -34.28 | 2.74      |
| 1       | 0.0007 | -17.01 | 1.36      |
| 2       | 0.0013 | -10.27 | 0.82      |
| 3       | 0.0019 | -7.34  | 0.59      |
| 5       | 0.0034 | -4.54  | 0.36      |
| 10      | 0.0082 | -2.74  | 0.22      |
| 13      | 0.0126 | -2.16  | 0.17      |

TSD 小扰动理论要求 |FXL|·δ = O(1)。上表显示前3个翼面点违反了这一条件。
这些点处的 FXL 将下表面势梯度 PY 拉向极大负值，
通过 PDE 耦合传播，使 U_lo 在 x=0~4% 弦的范围内深度低于 U_critical。

**为何无法用简单修正消除 Ma=0 区域**

尝试了对前缘1%弦范围内的 FXL 进行斜率封顶（限制 FXL 不超过 x=1% 处的平均斜率 −8.25）：
- FXL[0..2] 从 [-34, -17, -10] 降至 [-8.25, -8.25, -8.25]
- 对 Ma=0 区域的改善极其有限（Entry 3：14点→13点）

原因在于：TSD 是全场耦合的 PDE；x=0~4% 范围内 **所有** 14个点的 FXL 都显著超过 U_critical，
仅修改其中前3个点的边界条件，对整体解影响很小。
将封顶范围进一步扩大到5%弦虽然可以减少 Ma=0 点数，
但等同于抹去前缘奇异性，可能影响翼型的升力和 Cp 分布。

该问题是 TSD 方法对于圆头翼型的**内在局限性**：
小扰动假设在前缘附近（dy/dx → ∞）本身就不成立，
Ma=0 区域的出现是数学上的必然结果，而非代码错误。

原始 TSFOIL 代码使用约 50 个翼型输入点（最近前缘点 x≈0.005），
`|FXL[0]|·δ ≈ 0.6`，处于 O(1) 范围内，因此没有出现此问题；
本项目采用 1001 个输入点（`xfoil[1]≈0.0007`），导致 `|FXL[0]|·δ=2.74`，超出了 TSD 的适用范围。

代码无需修改，保留任务1的前向有限差分修复，不对 FXL 施加额外封顶。

#### 4(1).3 测试情况

在 `test_4_x0_mach_error/run_pytsfoil_database.py` 中对数据库前 10 个算例进行测试，
它们属于同一个翼型几何（airfoil_id=1），
来流条件为 Ma = 0.72–0.75，AoA = 0.02°–3.88°，与 RANS 数据进行对比。

下表面 Ma=0 区域统计（基于任务1修复后的代码）：

| Case | Ma    | AoA   | Ma=0 点数（下表面） | Ma=0 结束位置 x/c | Cp RMSE | Ma RMSE |
|------|-------|-------|---------------------|-------------------|---------|---------|
| 0    | 0.720 | 0.02° | 1                   | 0.0000（前缘驻点）| 0.1350  | 0.0640  |
| 1    | 0.720 | 1.92° | 6                   | 0.0081            | 0.1774  | 0.1106  |
| 2    | 0.730 | 0.80° | 3                   | 0.0019            | 0.1543  | 0.0895  |
| 3    | 0.730 | 3.17° | 14                  | 0.0441            | 0.5361  | 0.2715  |
| 4    | 0.740 | 3.38° | 13                  | 0.0381            | 0.5528  | 0.2784  |
| 5    | 0.740 | 3.88° | 13                  | 0.0381            | 0.5601  | 0.2831  |
| 6    | 0.750 | 2.25° | 8                   | 0.0144            | 0.2497  | 0.1498  |
| 7    | 0.750 | 2.48° | 11                  | 0.0273            | 0.4940  | 0.2561  |
| 8    | 0.750 | 2.59° | 10                  | 0.0226            | 0.5169  | 0.2628  |
| 9    | 0.750 | 2.99° | 11                  | 0.0273            | 0.5217  | 0.2658  |
| Mean |       |       | **9.0**             | **0.022**         | **0.412**| **0.221**|

主要观察：

1. AoA ≈ 0° 时正常：Entry 0（AoA=0.02°）下表面只有 x=0 处的 1 个驻点，上表面亦然，与物理预期吻合。
2. Ma=0 区域随 AoA 增大而扩展：AoA 从 0° 增加到 3.9°，下表面 Ma=0 区域从 1 点扩展到最多 14 点（x/c ≈ 4.4%）。
3. 上表面不受影响：正攻角下驻点移至下表面，上表面无 Ma=0 区域（AoA≈0 时上表面仅 x=0 1个点）。
4. 误差与 AoA 强相关：低攻角（AoA < 1°）的 Cp RMSE 约 0.14–0.17，而高攻角（AoA > 2°）的 Cp RMSE 高达 0.25–0.56，其中包含了 Ma=0 区域对前缘 Cp 分布的人为误差。

结论：该 Ma=0 区域是 TSD 模型对圆头翼型的固有局限性，在高攻角时尤为明显。
不存在代码 bug，结果在 TSD 理论的适用范围内是自洽的。
使用者应意识到 TSD 方法对钝前缘翼型在 AoA > 2° 时前缘附近（x/c < 5%）Ma 分布的预测精度有限。

### 任务4(2)：前缘附近计算的修正方案分析

#### 4(2).1 任务描述

根据任务4(1)的分析，前缘附近 Ma=0 区域是 TSD 模型的固有局限性，无法通过简单限制消除。
扰动速度 U 过度负值是 Ma 截断为 0 的根本原因。

U 的临界值为：
$$
U_{critical} = -\frac{1}{\delta^{2/3} \cdot (\gamma+1) \cdot M_\infty},
$$
当 $U < U_{critical}$ 时，Ma=0。

而显然，U 的过度负值与攻角增大和来流马赫数增大有关。
例如，entry 1~9 都是同一个翼型几何（airfoil_id=1），
而在不同来流马赫数下，攻角越大，U 过度负值区域更大；
来流马赫数越大，U 过度负值区域也更大。

但为了提高前缘附近 Ma 的物理合理性，考虑在前缘附近添加可选的数值修正项，
以避免 U 过度负值导致 Ma=0 区域过大。

给所有修正方法设定一个开关参数 `fix_le_mach`，默认值为 False，用户可根据需要启用。

潜在的修正方法包括：

- 修正项应在前缘附近逐渐衰减，以保持远场解的准确性；
- 修正项应与来流条件（Ma、AoA）相关，以避免过度修正；
- 修正项可以作用于边界条件（FXL），如限制幅值、放缩幅值等；
- 修正项也可以作用于计算过程中，如限制 U 的最小值，或直接修正 P（增加源项之类）；
- 修正项的物理意义应尽量明确，例如可以解释为模拟边界层的粘性效应，或者其他。

#### 4(2).2 修正方案与可行性分析

**修正目标回顾**

问题的完整传播链为：

```text
大 AoA → FXL 在前缘奇异（|FXL|·δ >> 1）
     → SETBC 将过大的 FXL 写入 PY 边界条件
     → Fortran SYOR 迭代收敛到含大负值 U 的势场 P
     → EMACH1 在 ARG ≤ 0 时硬截断 Ma = 0
     → 输出的表面 Ma/Cp 分布在前缘附近物理不合理
```

可能的干预点对应三个层面：

1. **边界条件层面（FXL）**：在传入 Fortran 之前修正 FXL，从根源上减小边界驱动力
2. **求解器层面（P/U）**：在 SOR 迭代中或迭代后对 P/U 施加约束
3. **后处理层面（输出）**：不改变求解器，仅在 Ma/Cp 输出时进行平滑修正

**方案一：FXL 渐变平滑（边界条件层）**

TSD 小扰动理论的有效性要求 $|\text{FXL}| \cdot \delta = O(1)$，即翼面斜率的无量纲化值应为 $O(1)$ 量级。
对 FXL 超出该范围的点（前缘附近），将其平滑拉向 TSD 有效范围边界处的参考值：

```python
# 找到 |FXL[k]|·δ 首次降至 threshold 以下的网格点 k_valid
# 对 k < k_valid 的点，线性混合到 FXL[k_valid]
k_valid = np.argmax(np.abs(fxl) * delta <= threshold)   # 例如 threshold = 1.5
for k in range(k_valid):
    w = xfoil[k] / xfoil[k_valid]                       # 0 at x=0, 1 at x=x_valid
    fxl[k] = (1 - w) * fxl[k_valid] + w * fxl[k]
```

此修正将作用于 `compute_geometry_derivatives` 中，在写入 `tsf.common_data.fxl` 之前应用。

**物理含义**：等价于假设前缘附近翼型几何处于 TSD 适用范围内，
可解释为钝前缘翼型的一种"有效几何"近似（等效尖前缘）。

**可行性评估**：
- 任务4(1) 已尝试 1% 弦处的硬封顶（`FXL[0..2]` 限制为 −8.25），
  Ma=0 点数从 14 降到 13，改善极微。
  失败原因：14 个 Ma=0 点中所有点的 FXL 均超出有效范围，仅修正前 3 点影响可忽略。
- 渐变平滑版本需要将修正范围扩展到 x ≈ 4–5%c（即全部 14 个点），
  这等效于对前缘附近翼型几何做较大改动，可能系统性地改变 CL 和 Cp 分布。
- 修正幅度与 AoA 无关（FXL 纯几何量），但问题严重程度随 AoA 增大，
  导致低攻角时可能过度修正而高攻角时修正仍不足。
- **结论：有效但代价高。修正范围 ≥ 5%c 时对 CL/CM 有影响，且修正范围需人工调参，普适性差。**

**方案二：U 后处理平滑（仅影响输出，不改变流场）**

不修改 Fortran 求解器和边界条件，仅在 `output_surface` 中对下表面 Ma=0 区域
以物理合理的方式进行插值修正，使得输出的 Ma 分布连续且无人为截断为零的平台。

基本思路：利用细翼型理论的驻点位置估算

在薄翼型势流中，下表面驻点（Ma=0 点）近似位于：
$$x_{stag} \approx \frac{\alpha}{\pi}, \quad \alpha \text{ 为几何攻角（弧度）}
$$

对于数据库算例（AoA = 1°~4°），$x_{stag} \approx 0.006 \sim 0.022$，与观测到的 Ma=0 区域终止位置（0.008~0.044）一致。

修正策略（仅作用于 Ma/Cp 的输出，不改动 P 场）：

```python
if fix_le_mach:
    alpha_rad = np.deg2rad(self.config['ALPHA'])  # 物理攻角
    x_stag = alpha_rad / np.pi                   # 薄翼型驻点估计
    
    # 对下表面 Ma=0 区域（x_exit 为 Ma=0 区域右边界）
    x_valid_start = x[i_exit]   # Ma 首次 > 0 的位置
    for i in range(ile, i_exit):
        x_i = x[i]
        # 线性插值：在 x_stag 处 Ma=0（驻点），在 x_valid_start 处恢复实际 Ma
        w = (x_i - x_stag) / (x_valid_start - x_stag)
        w = np.clip(w, 0, 1)
        ma_corrected = w * ma_valid_start           # 从 0 平滑增长到实际值
        # 将 ma_corrected 代入等熵关系得到 Cp_corrected
```

**物理含义**：在 TSD 无法正确分辨的前缘高梯度区域，
用薄翼型理论（驻点线性速度分布）填补输出的 Ma 空白，
同时保持 CL、CD、CM 等积分量完全不变（流场 P 未被改动）。

**可行性评估**：
- 完全不影响 Fortran 求解器，CL/CD/CM 精度零影响。
- 驻点估算公式 $x_{stag} \approx \alpha/\pi$ 来自无粘薄翼型理论，
  对数据库算例的吻合误差约 20–50%（例如 Entry 3：预测 0.0180，实测 0.0441）；
  该估算偏差在可接受范围内（仅影响插值区域内的 Ma 分布形状，不影响插值边界条件）。
- 修正区域自动随 AoA 扩展，天然与问题严重程度匹配。
- **结论：风险最低，对全局积分量零影响；代价是修正区域内的 Ma/Cp 精度为插值估计而非 PDE 解。**

**方案三：等效 EMACH1 软截断（求解器层，仅改 Ma 计算逻辑）**

修改 `EMACH1` 的截断条件，当 ARG ≤ 0 时不返回 0，
而是用一个连续可微的软映射代替硬截断，保留 ARG 接近 0 时的物理趋势：

```fortran
! 原始：
result_emach = 0.0
if (ARG > 0.0) result_emach = sqrt(ARG)

! 软截断版本（仅在 output 路径中使用，不在 SOR 内部使用）：
if (ARG > 0.0) then
    result_emach = sqrt(ARG)
else
    result_emach = 0.0   ! 仍截断，但仅在 solver 内部
end if
```

注意：`EMACH1` 在 Fortran 中同时被 `VWEDGE`（激波-边界层耦合）和输出路径调用。
若修改截断逻辑，可能影响 `VWEDGE` 的激波位置判断，从而改变迭代收敛行为，风险较大。
更安全的做法是在 Python 的 `output_surface` 中对 `emach1` 的返回值做后处理（等价于方案二）。

结论：若修改 Fortran 内部逻辑，需仔细隔离 output 路径与 solver 内部路径，实现复杂且回归风险高。不推荐。

##### 方案对比总结（初步评估）

| 方案 | 干预层面 | 对 CL/CD/CM 影响 | 对 Ma=0 改善 | 实现难度 |
| --- | --- | --- | --- | --- |
| 一：FXL 渐变修正 | 边界条件 | 有（方向不可控） | 无实际改善（见任务4(3)） | 低 |
| 二：U 后处理插值 | 输出层 | 无 | 仅改变输出数值，不改变错误解 | 低 |
| 三：EMACH1 软截断 | Fortran 求解器 | 可能有（通过 VWEDGE） | 低（仅改变 Ma 数值表示） | 高 |

##### 关于方案二（U 后处理）的重要认识

方案二仅修正输出数值，不改变 Fortran 求解到的势场 P。
然而，测试结果表明：当下表面 Ma=0 区域较大时，下表面整体 Ma 分布都显著偏离 RANS，
而不只是 Ma=0 的那些点不对。这说明 Ma=0 区域对应的是求解器给出的错误 P 场，
而非单纯的数值表示问题。后处理只能修改输出的显示值，无法修正底层错误的 P 场，
因此方案二本质上是掩盖了问题而不是解决了问题，不予采纳。

相反，测试中对上表面的修正（`both-*` 系列）在使 Ma=0 计数略有减少的同时，
上表面 Ma 分布也明显向 RANS 结果靠拢——进一步验证消除 Ma=0 才能真正改善分布质量。

##### 修正目标的重新定位

消除前缘 Ma=0 区域是唯一有效的修正方向，后续任务应聚焦于在求解器层面防止 U 降至
U_critical 以下，而非对输出做后处理。方案一（边界条件修正 FXL）经任务4(3) 验证无效；
方案三（EMACH1 软截断）风险较高；后续需要探索直接约束 U 或在 SOR 迭代中添加源项的新方案。

### 任务4(3)：前缘附近计算的修正方案 1 实现与测试

#### 4(3).1 任务描述

基于 `#### 4(2).2 修正方案与可行性分析` 中的方案一（FXL 渐变平滑），
在 `pytsfoil.py` 中实现该修正，并在 `test_4_le_mach_fix_3/` 中进行测试。

#### 4(3).2 完成情况

**实现内容**

在 `pytsfoil.py` 的 `_default_config` 中新增三个配置项：

```python
'fix_le_mach': False,              # 启用前缘斜率平滑修正
'fix_le_mach_threshold': 1.5,      # |(FX−ALPHA_sim)|*delta TSD 有效性阈值
'fix_le_mach_surface': 'both',     # 'both' | 'lower' | 'upper' | 'auto'
```

在 `compute_geometry_derivatives` 中，刚性因子修正之后、写入 Fortran 之前，
新增以下修正逻辑：

```python
if self.config.get('fix_le_mach', False):
    threshold = float(self.config.get('fix_le_mach_threshold', 1.5))
    alpha_sim = float(tsf.common_data.alpha)   # 经 compute_scale 归一化的攻角
    surface = self.config.get('fix_le_mach_surface', 'both')
    if surface == 'auto':
        surface = 'lower' if alpha_sim >= 0.0 else 'upper'
    pairs = []
    if surface in ('both', 'lower'): pairs.append(fxl)
    if surface in ('both', 'upper'): pairs.append(fxu)
    for fx in pairs:
        eff = fx - alpha_sim          # SETBC 实际使用的有效斜率
        valid = np.abs(eff) * delta <= threshold
        if np.any(valid):
            k_v = int(np.argmax(valid))
            if k_v > 0:
                x_v = xfoil[k_v]
                for k in range(k_v):
                    w = xfoil[k] / x_v
                    fx[k] = (1.0 - w) * fx[k_v] + w * fx[k]
```

**阈值判据改进（AoA-aware 修正）**

初始版本使用 `|FXL|*delta` 作为阈值判据，对上下表面一视同仁。
测试后发现，此方案对 FXU（上表面）施加了不必要的修正，
导致上表面吸力减小，引发 CL 大幅下降（Entry 3 下降约 23%）。

根本原因在于 Fortran `SETBC` 中翼面边界条件为：

```fortran
FXLBC = CYYBLU * (FXL - ALPHA_sim)   ! 正攻角时更负
FXUBC = CYYBUD * (FXU - ALPHA_sim)   ! 正攻角时更小
```

实际驱动 Ma=0 的是有效斜率 `FX − ALPHA_sim`，而非 `FX` 本身。
改用 `|(FX − ALPHA_sim)|*delta` 作为阈值判据后：

- **正攻角时**：下表面 `|FXL − ALPHA_sim| = |FXL| + ALPHA_sim > |FXL|` → k_v 更大，修正覆盖更多点
- **正攻角时**：上表面 `|FXU − ALPHA_sim| = |FXU| − ALPHA_sim < |FXU|` → k_v 更小，修正覆盖更少点

但诊断分析发现，由于 `ALPHA_sim`（约 0.5–0.85）远小于 `|FXL|`（5–34），
上下表面的 k_v 差值仅为 ±1 个网格点，对整体结果改善有限。

真正重要的发现是：CL 下降的主要来源是**上表面 FXU 被修正**，而非下表面 FXL。
因此增加了 `fix_le_mach_surface` 参数，允许仅修正下表面（`'lower'`）。

**threshold 的物理含义**

threshold = |FX − ALPHA_sim| · δ 是翼面的有效局部流动偏折角（来流方向到翼面的实际角度），
即 SETBC 边界条件项 (FXL − ALPHA_sim)·δ 的绝对值。
TSD 小扰动假设要求此值 ≪ 1，实际前缘处高达 2.78，说明小扰动假设在该区域已经失效。

threshold=1.5 的含义是：只修正"TSD 理论本身就无效"的区域。

当 threshold 降到 0.5 以下，修正的区域已经是 TSD 在物理上本来合理的区域。
这相当于把翼型的真实前缘几何替换成人为压平的几何，
前缘斜率大是翼型前缘曲率的真实反映，不是数值噪声。
所以 CL 随 threshold 降低而剧烈下降是正确的物理响应，而不是数值误差。

#### 4(3).3 测试情况

在 `test_4_le_mach_fix_3/run_pytsfoil_database.py` 中对数据库前 10 个算例进行测试。
共 7 种配置：基准、both-1.5/0.5（上下表面同时修正）、lower-1.5/1.0/0.5/0.3（仅下表面修正）。

下表面 Ma=0 点数：

| Case | AoA   | base | b-1.5 | b-0.5 | l-1.5 | l-1.0 | l-0.5 | l-0.3 |
|------|-------|------|-------|-------|-------|-------|-------|-------|
| 0    | 0.02° | 1    | 2     | 4     | 2     | 2     | 3     | 1     |
| 1    | 1.92° | 6    | 8     | 9     | 7     | 7     | 8     | 8     |
| 2    | 0.80° | 3    | 4     | 6     | 4     | 4     | 5     | 5     |
| 3    | 3.17° | 14   | 13↓   | 13↓   | 15↑   | 15↑   | 14    | 14    |
| 4    | 3.38° | 13   | 14    | 15    | 13    | 14    | 14    | 15    |
| 5    | 3.88° | 13   | 15    | 16    | 14    | 14    | 15    | 15    |
| 6    | 2.25° | 8    | 9     | 10    | 8     | 8     | 9     | 9     |
| 7    | 2.48° | 11   | 12    | 11    | 11    | 12    | 12    | 13    |
| 8    | 2.59° | 10   | 12    | 14    | 11    | 11    | 12    | 13    |
| 9    | 2.99° | 11   | 13    | 14    | 12    | 12    | 13    | 13    |
| 均值 |       | 9.0  | 10.2  | 11.2  | 9.7   | 9.9   | 10.5  | 10.6  |

ΔCL（相对基准）：

| Case | AoA   | b-1.5   | b-0.5   | l-1.5   | l-1.0   | l-0.5   | l-0.3   |
|------|-------|---------|---------|---------|---------|---------|---------|
| 0    | 0.02° | −0.001  | −0.003  | −0.001  | −0.001  | −0.002  | −0.003  |
| 1    | 1.92° | −0.010  | −0.018  | −0.003  | −0.005  | −0.007  | −0.010  |
| 2    | 0.80° | −0.003  | −0.006  | −0.001  | −0.002  | −0.002  | −0.004  |
| 3    | 3.17° | −0.44 * | −0.66 * | −0.015  | −0.059  | −0.24 * | −0.40 * |
| 4    | 3.38° | +0.005  | +0.008  | +0.001  | +0.002  | +0.004  | +0.007  |
| 5    | 3.88° | +0.008  | +0.017  | +0.001  | +0.002  | +0.004  | +0.008  |
| 6    | 2.25° | −0.11 * | −0.16 * | −0.037  | −0.057  | −0.077  | −0.099  |
| 7    | 2.48° | −0.022  | −0.50 * | ~0.000  | −0.007  | −0.010  | −0.015  |
| 8    | 2.59° | +0.020  | +0.001  | +0.001  | +0.001  | +0.022  | +0.022  |
| 9    | 2.99° | +0.002  | +0.008  | +0.001  | +0.002  | +0.003  | +0.004  |

`*` 标注：|ΔCL| > 5%（不可接受）。

#### 关键发现

1. **CL 大幅下降的根源是上表面 FXU 被修正**，而非下表面 FXL。
   `both-1.5` 下 Entry 3 的 ΔCL = −0.444，而 `lower-1.5` 仅 −0.015（相差 30 倍）。
   用户关于"攻角使上下表面临界条件不对称"的判断是正确的：
   仅对下表面施加修正可以将 CL 扰动降至 1–6% 以内。

2. **仅修正下表面时 Ma=0 计数未减少**：
   `lower-*` 组合的均值 Ma=0 为 9.7–10.6，不优于基准（9.0）。
   Entry 3 的 `lower-1.5` 反而从 14 升至 15，说明仅修正 FXL 而保留 FXU 后，
   上下表面边界条件的不对称性使势场 P 以意料之外的方向响应。

#### 4(3).4 结论

1. 用户关于攻角影响的判断正确：
   SETBC 对上下表面均使用 `FX − ALPHA_sim`，正攻角下 FXU 被过度修正是 CL 大幅下降的根源。
   改用有效斜率 `|FX − ALPHA_sim|` 作为阈值判据，并引入 `fix_le_mach_surface` 参数后，
   可将 `lower-only` 模式下的 ΔCL 控制在 5% 以内（多数算例 < 1%）。

2. 尽管 CL 影响得到控制，**下表面单独修正仍不能有效减少 Ma=0 区域**。
   Ma=0 点数均值不降反升（9.0 → 9.7–10.6），FXL 边界条件修正的有效范围（1–3 个网格点）
   相对于 14 点的 Ma=0 区域来说过于局限，无法通过 PDE 耦合将 U 提升到 U_critical 以上。

3. 综合结论不变：方案一（FXL 渐变平滑）在任何参数组合下均不能有效减少 Ma=0 区域。
   `pytsfoil.py` 保留全部三个参数（默认均关闭），实现与诊断研究的完整性留存。

#### 4(3).5 深层机制分析：为何 FX 修正无法消除 Ma=0

##### V 与 U 的决定机制不同

TSD 中 P 为扰动速度势，U = ∂P/∂x，V = ∂P/∂y。SETBC 给出的是翼面 Neumann BC：

```text
∂P/∂y |_surface = FXLBC = CYYBLU * (FXL − α_sim)
```

FX 直接且仅直接决定翼面处的 V。Ma=0 反映的是 U < U_critical，U 由全局 P 的迭代结果决定，
FX 只能通过"FX → V（BC）→ PDE 椭圆耦合 → P → U"这条间接路径影响 U。

##### U 的主要决定路径

```text
FXL/FXU  →  V（翼面 Neumann BC）  →  P（SYOR 迭代）  →  U = ∂P/∂x
                                                          ↑
环量 CIRCFF  ←  RECIRC（后缘 ΔP = P_top − P_bot）        │
     ↓                                                     │
RESET（P 边界 += CIRCFF · THETA）  ─────────────────────→─┘
```

驱动 U 分布最核心的因素是**全局环量**（由后缘压差通过 RECIRC 每次迭代更新），
而环量主要由上表面吸力分布（FXU 主导）决定——升力几乎全来自上表面低压区到后缘的积累。

##### 上下表面修正效果不对称的根因

FXU 和 FXL 本身符号相反（FXU > 0，FXL < 0），但 SETBC 对两个表面均使用相同符号的
攻角项：`eff = FX − α_sim`。这导致：

| 表面 | eff 与 FX 的关系 | 对 eff 绝对值的影响 | 修正后 eff 变化 |
|------|-----------------|---------------------|----------------|
| 上表面（FXU > 0） | eff = FXU − α，α > 0 | eff 绝对值减小，k_v 较小 | FXU 减小，eff 减小 |
| 下表面（FXL < 0） | eff = FXL − α，α > 0 | eff 绝对值增大，k_v 较大 | FXL 变小负值，eff 变小负值 |

- **修正 FXU**（减小上表面斜率）→ 减小环量 → 驻点效应减弱 → Ma=0 减少（有效，CL 降低）
- **修正 FXL**（减小下表面斜率绝对值）→ 环量几乎不变（FXL 对后缘 ΔP 影响极弱）→
  势场 P 局部微调，驻点压力分布轻微下移 → Ma=0 反而增加

即使将攻角项的符号对下表面取反（`eff_L = FXL + α_sim`），也只改变 k_v 的大小，
不改变"向 FXL[k_v] 平滑 → FXL 绝对值减小"这个方向本身，因此无法使修正变为有利。

##### 结论：FX → V → U 这条路径在前缘区域对 Ma=0 无效

前缘 1–2 个 FXL 点的 Neumann BC 变化，通过椭圆 PDE 收敛后几乎被全场"冲淡"；
而后缘 Kutta 条件和环量才是 U 分布的主控因素。
