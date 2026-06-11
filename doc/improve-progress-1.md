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
- `improve-progress-1.md`: 本报告文件，记录功能改进的过程和结果。

### 测试要求

在修改 Fortran 代码的过程中，我们需要不断测试修改后的代码是否能够正确运行，并且输出的结果是否正确。
由于本项目是 Fortran-Python 混合编程，因此我们需要在 Python 中调用 Fortran 代码来测试修改后的 Fortran 代码是否能够正确运行，并且输出的结果是否正确。

注意，原始代码是正确的。因此，在每项任务完成后，需要使用 `compile_f2py.py` 来编译 Fortran 代码，测试修改后仍然可以 Python 正常调用，并且输出的结果与原始代码相同/相近。

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

## 上阶段工作总结

### 修正方法框架的总结

在上一阶段的任务中，构建了 pyTSFoil 的修正方法框架：匹配渐进展开（MAE）+奇性扣除（singularity subtraction）。

TSD 方程在钝前缘处的奇异性（$x^{-1/2}$ 边界斜率、$x^{-1/3}$ 面速）是其固有特性。
基于奇性扣除（singularity subtraction）思路，将迭代求解的扰动速度势被拆分为两个部分：$\phi = \phi_s + \phi_r$。
其中 $\phi_s$ 是用于解析描述前缘附近奇异性的项（singular term）；
从而使迭代量 $\phi_r = \phi - \phi_s$ 变得光滑有界（remainder），用于迭代求解。
求解完成后，基于匹配渐进展开（MAE）的内区表和总扰动速度势 $\phi$，
计算修正的扰动速度分布、马赫数分布、压力系数分布等结果。

求解步骤：

**Step 1**: 翼型几何处理。求解翼型的曲线、几何参数，以及前缘修正所需的几何信息。

**Step 2**: 预分析。求解 MAE 的内区表，奇性扣除的奇异项 $\phi_s$；以及用于计算迭代右端项 `RHS` 修正项的相关数组等）。

**Step 3**: 更新边界条件。基于奇异项 $\phi_s$ 更新物面斜率边界条件，获得用于迭代光滑项 $\phi_r$。

**Step 4**: 迭代求解光滑项 $\phi_r$。基于更新后的边界条件和右端项，迭代求解 $\phi_r$，直到满足收敛条件。
要注意 Murman-Cole 判别项的计算是基于 $\phi$ 的，而不是 $\phi_r$ 的。

**Step 5**: 后处理。组装 $\phi = \phi_s + \phi_r$，计算 TSD（外区）扰动速度分布、马赫数分布、压力系数分布等结果。
然后，基于 MAE 修正物面马赫数分布、压力系数分布。

奇性扣除修正项在 TSD 中的作用：

| 修正项 | 说明 |
|-----|----|
| A | 替换 SYOR 迭代量 $\phi_r = \phi - \phi_s$ |
| B | Murman-Cole 判据的计算，确保使用 $\phi_x$ 而不是迭代量 $\phi_{r,x}$ |
| C | SYOR 迭代中加入 $L[\phi_s]$ 残量强迫，即右端项 |
| D | 更新物面边界条件，从 FXU/FXL 扣除 $\phi_{s,\tilde y}(x,0^\pm)$ |
| E | 后处理中组装 $\phi = \phi_s + \phi_r$ |

MAE 修正的作用：

基于 TSD 求解的结果（无论是否包含奇性扣除），MAE 对 TSD 的 `cp_TSD`, `ma_TSD` 进行修正，
获得 `cp_MAE`, `ma_MAE`，以更准确地描述前缘附近的流动特性。

需要注意的是，奇性扣除中的修正项（A-D）不是可以随意关闭的。
测试情形包括：

（子图1）

1. 基线（baseline）：不使用任何修正项；
2. 仅开启修正项 D；
3. 仅开启修正项 D, E；
4. 全部修正项（A-E）开启；

（子图2）

1. 基线（baseline）：不使用任何修正项；
2. MAE 修正（MAE only）：不使用奇性扣除修正项，仅使用 MAE 修正；
3. 修正项 D, E + MAE 修正；
4. 全部修正项（A-E）开启 + MAE 修正；

### 完成情况

上述测试已经完成，结果表明：

大攻角下 TSD 在前缘的 M=0 区域很小。

MAE 作为后处理有效，可以获得好看的结果。
但如果 TSD 外区的解本身存在较大误差，例如较大的前缘的 M=0 区域，MAE 也无法完全补偿。

奇性扣除修正无效，无法消除大攻角下 TSD 在前缘的 M=0 区域。

测试表明当翼型厚度较小时，即使在大攻角下，前缘的 M=0 区域会明显缩小，甚至消失。

但是，使用 RIGF 减小前缘的斜率赋值，也不能减小前缘的 M=0 区域。

因此，到底什么导致了大攻角下前缘 M=0 区域的存在，仍然是一个未解之谜。

### 成因分析

#### 核心线索：RIGF 无效

RIGF 减小的是翼面法向斜率 FXU/FXL 中的几何奇性（$1/\sqrt{x}$ 分量），但无法减小 M=0 区域。
这意味着 **M=0 区域不是由前缘几何奇性驱动的**。

`improve-progress-2.md` 中的任务 5 流程图给出了关键诊断：

```
FX → BC → PDE 椭圆耦合 → P → U = ∂P/∂x
                              ↑
         全局环量 CIRCFF（后缘 ΔP → RECIRC 每次更新）─┘
```

**U 由全局环量主控，FX 的局部修正通过 PDE 耦合被"冲淡"**。这对 RIGF 和奇性扣除都成立。

#### 两个分量的竞争

前缘附近的 TSD 速度 U 可以粗略拆为两部分：

| 分量 | 来源 | 量级 | 随 δ 的缩放 |
|---|---|---|---|
| $U_\text{geom}$ | 厚度 → 几何 BC → $1/\sqrt{x}$ 奇性 | $\sim \delta^{1/3}/\sqrt{x}$ | 随 δ 减小而减小 |
| $U_\alpha$ | 迎角 → 环量 CIRCFF → 全局势场 | $\sim \alpha/(\delta^{2/3}\sqrt{x})$ | 随 δ 减小略增大 |

M=0 的阈值：$U_\text{crit} = -1/(\delta^{2/3}(\gamma+1)M_\infty)$，随 δ 减小变得更负（更难触发）。

大攻角时 $U_\alpha$ 主导，$U_\text{geom}$ 退为次要项：

- **薄翼型**：$U_\text{geom}$ 更小，两者合力减弱；同时 $U_\text{crit}$ 更负，双重效果使 M=0 区域消失。
- **厚翼型 + 大攻角**：两分量叠加，合力超过 $|U_\text{crit}|$，产生较大的 M=0 区域。

这解释了为什么 RIGF（仅减小 $U_\text{geom}$）在大攻角下无效——此时 $U_\alpha$ 已经是主导分量。

#### 奇性扣除为何也无效

**φ_s 的幅值与 α 无关**（见 `singularity_subtraction.py`）：

$$A = -\frac{3}{4} \cdot \frac{0.635776\,R_c^{1/3}}{(\gamma+1)^{1/3}\,\delta^{2/3}}, \quad \phi_s = A \cdot X^{2/3} \cdot \chi(r)$$

因此 $\phi_s$ 只捕捉了**几何诱导**的、普适的 $x^{-1/3}$ 速度奇性，幅值仅由 $R_c$（前缘半径）决定。
前缘速度实际上有两个来源：

| 来源 | 速度形式 | 与 α 的关系 | 奇性扣除是否处理 |
|---|---|---|---|
| 几何诱导（厚度 BC 的 $1/\sqrt{x}$ → TSD 解） | $\phi_{s,x} \sim x^{-1/3}$，幅值由 $R_c$ 决定 | 无关 | 是（$\phi_s$ 精确捕捉） |
| 环量诱导（CIRCFF → 椭圆 PDE 内部解） | 随 α 增大，前缘附近大负值 | 正比于 α | **否** |

第二个来源的根本原因：迎角对 BC 的贡献是**光滑常数项** $-\alpha$，不含奇点，因此奇性扣除的 BC 修正（Step D）完全忽略了它。
但这个光滑 BC 经椭圆 PDE 求解后，通过全局环量 CIRCFF 在前缘附近积累大负速度——这是内部解特性，不在边界条件中体现。

因此，即使几何奇性被完美扣除，$\phi_r = \phi - \phi_s$ 中仍然含有 α 驱动的前缘大负速度。
大攻角时总速度 $U = \phi_{s,x} + \phi_{r,x}$ 两项均为负值，叠加后超过 $U_\text{crit}$，M=0 区域依然存在。

要真正消除这一问题，$\phi_s$ 需要在每次迭代中动态追踪 CIRCFF 的贡献，
这等价于在迭代内部引入内区解约束，远比当前静态几何奇性扣除复杂。

### 结论与启示

M=0 区域是**大攻角下 TSD 模型固有的结构性缺陷**：
TSD 把边界条件投影到 $y=0$，无法正确表示驻点附近的流动，
而 CIRCFF 在前缘附近积累的速度亏量会随 α 增大而扩展。

对后续工作的启示：

1. **MAE 后处理目前是正确思路**，但前提是在复合公式中使用**未截断的** TSD 速度 $U$（即 `cp_tsd_linear = -2U·cpfact`，绕过 EMACH1 截断），以保证对消机制正确工作。
2. **MAE 的局限性**也是结构性的：它只是后处理，无法改变 CIRCFF 本身的计算。当 TSD 外区的环量（CL）因大 M=0 区域而本身就存在误差时，MAE 也无法完全补偿。
3. 真正的改进方向可能需要在**迭代内部**引入内区解的约束，即用 MAE 内区解修正前缘附近的边界条件（而非仅用于后处理），让 TSD 外区解在大攻角下本身更准确。但这是侵入性修改，复杂度较高。

## 改进过程

本章节包括各个任务的描述，每个任务的描述包括：

- 任务描述：对任务的背景、目标和具体内容进行详细描述。
- 完成情况：描述修改了哪些内容，总结完成情况。
- 测试情况：基于`测试要求`一节中的测试要求，描述测试的过程和结果。

### 任务1：TSD 的厚度、弯度、攻角分解

#### 1.1 任务描述

主要探讨在不同攻角下，TSD 的前缘 M=0 区域是由弯度、厚度、还是迎角驱动的。

- 翼型几何：RAE2822 （厚度可以放缩）
- CST参数：

    ```python
    cst_u = [ 0.12829643, 0.12670863, 0.16065898, 0.14942386, 0.15102884, 0.22416928, 0.16078175, 0.20998555, 0.18608795, 0.21052324]
    cst_l = [-0.12927128,-0.13176061,-0.17044964,-0.07045476,-0.33888064, 0.00991923,-0.20070721,-0.03536713,-0.04397496, 0.06436195]
    ```
- 攻角范围 (degree)：[0, 1, 2]
- 来流马赫数范围：[0.7, 0.75, 0.8]

在每个组合下，分别计算：

- 仅厚度：0 攻角，原始厚度
- 仅弯度：0 攻角，厚度为 0
- 仅攻角：平板，非零攻角
- 全部：原始厚度，非零攻角

看看以上的纯弯度、纯攻角情形下能否收敛，如果不能收敛，那么就缩放厚度，看看能否收敛。

在都获得收敛结果后，分析前缘 M=0 区域的大小，看看是由哪个因素主导的。
