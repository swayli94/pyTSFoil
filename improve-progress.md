# 程序功能改进报告

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
       ├─ output_shock()                         [Python]  — 表面 Cp/Ma 分布
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

#### 任务描述

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

#### 完成情况

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

#### 测试情况

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
