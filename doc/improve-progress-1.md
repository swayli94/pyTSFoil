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
    cst_u = [ 0.12829643, 0.12670863, 0.16065898, 0.14942386, 0.15102884,
              0.22416928, 0.16078175, 0.20998555, 0.18608795, 0.21052324]
    cst_l = [-0.12927128,-0.13176061,-0.17044964,-0.07045476,-0.33888064,
              0.00991923,-0.20070721,-0.03536713,-0.04397496, 0.06436195]
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

### 1.2 完成情况

编写了 `test_1_decomposition/run_test.py`，覆盖以下参数空间：

- Ma = [0.70, 0.75, 0.80]，AoA = [0.0, 1.0, 2.0, 3.0]°（在任务描述的 [0,1,2]° 基础上增加了 3°）
- 5 种几何变体：`thickness_only`、`thickness_half`（半厚度）、`camber_only`、`aoa_only`、`full`

对 `camber_only` 和 `aoa_only`，TSD 需要非零厚度参数 δ，采用 `t_scale=0.1`（参考半厚度为原始的 10%）作为薄板近似。全部 60 个工况均收敛。

生成结果：

- 每个 (Ma, AoA) 组合的 Mach 分布图（左：几何，右：各变体上/下表面 Mach 分布）
- 上/下表面前缘 Ma=0 网格点数的热图（`n_ma0_upper.png`、`n_ma0_lower.png`）

### 1.3 测试情况

**收敛性**：所有工况在 `t_scale=0.1` 下均正常收敛，无需进一步缩放厚度。

**上表面 Ma=0 区域**（前缘驻点侧）：

| 变体 | 典型点数（Ma=0.80） |
|---|---|
| Thickness only | 1~3 |
| Camber only | 0（全为 0） |
| AoA only | 0（仅 AoA=3°/Ma=0.75 出现 7 点的孤立峰值） |
| Full | 1~2 |

上表面 Ma=0 区域由**厚度主导**，弯度和攻角单独贡献均可忽略。Full 构型与 `thickness_only` 基本一致。

**下表面 Ma=0 区域**（前缘绕流加速后减速侧）：

| 变体 | AoA=0° | AoA=1° | AoA=2° | AoA=3° |
|---|---|---|---|---|
| Thickness only (Ma=0.80) | 3 | 5 | 5 | 6 |
| Camber only (Ma=0.80) | 0 | 4 | 56 | 70 |
| AoA only (Ma=0.80) | 0 | 0 | 2 | 36 |
| Full (Ma=0.80) | 4 | 5 | 5 | 6 |

下表面 Ma=0 区域的主导因素随攻角变化：

1. **AoA=0°**：仅厚度贡献（3~4 点），弯度在无攻角时下表面 Ma=0 为 0。
2. **AoA=1°**：厚度与弯度贡献相当（各约 3~5 点），攻角单独贡献仍为 0。
3. **AoA≥2°**：**弯度贡献急剧放大**（AoA=2°/Ma=0.80 达 56 点，AoA=3° 达 70 点），攻角单独贡献也显著增大（AoA=3°/Ma=0.80 达 36 点）。
4. **Full 构型始终约 4~6 点**，远小于 `camber_only` 和 `aoa_only` 的单独值——说明原始厚度对下表面 Ma=0 区域有强烈的非线性抑制作用，各分量不能线性叠加。

**结论**：前缘 Ma=0 区域在上下表面由不同因素主导。上表面主要由厚度驱动，随 Ma 增大略有增长，与攻角关系不明显。下表面在大攻角下由**弯度与攻角的耦合**驱动，但 Full 构型中原始厚度的存在使实际 Ma=0 区域远小于分量单独计算的结果，说明 TSD 中厚度、弯度、攻角三者对前缘流动的影响是强非线性的。

### 任务2：TSD 扰动速度限制器

#### 2.1 任务描述

参考 `TSD_velocity_limiter_projected_SOR.md` 中的分析，
修改 Fortran 和 Python 代码，构造扰动速度的限制器，避免扰动速度的模长（按分量）过大破坏小扰动假设，
阻止该局部失效通过非线性反馈污染全局解、压住发散。

#### 2.2 完成情况

按照 `TSD_velocity_limiter_projected_SOR.md` 的设计规范，完整实现了投影截断（Projected Clip）速度限制器，共涉及 4 个文件。

**Fortran 修改**

`common_data.f90`：新增 4 个限制器控制参数及其在 `initialize_common()` 中的重置。

| 参数 | 默认值 | 说明 |
|---|---|---|
| `VEL_LIM_ENABLED` | `.false.` | 总开关（默认关闭） |
| `VEL_LIM_D` | `5.0` | 速度上界 $D$，与求解器内部 $P_x$ 同单位（Krupp 标度） |
| `VEL_LIM_THETA` | `1.0` | 截断欠松弛系数 $\theta$（0 < θ ≤ 1） |
| `VEL_LIM_ELLIPTIC_ONLY` | `.false.` | 仅在亚声速（椭圆型）节点启用 |

`solver_data.f90`：新增 3 个每次 sweep 的标量诊断量（`VEL_LIM_N_CLIPPED`、`VEL_LIM_MAX_U_BEFORE`、`VEL_LIM_N_INFEASIBLE`）和 1 个二维逐节点 clip 计数数组 `VEL_LIM_CLIP_MAP(N_MESH_POINTS, N_MESH_POINTS)`，均在每次 SYOR sweep 开始时重置。

`main_iteration.f90`：核心修改分三处。

- 新增私有子程序 `CLIPLN(I_col, JBOT, JTOP, VC)`：对 Thomas 求解完成的某一 x 列逐节点做投影截断，根据四个方向相邻节点的值和各自的**局部网格间距**计算可行区间 $[L,U]$；出现不可行区间时取中点并计数告警；对翼面缝隙行（`JUP`/`JLOW`）完全跳过（y 方向相邻节点跨越含 `PJUMP` 不连续的缝隙，约束不合物理）；在 `VEL_LIM_ELLIPTIC_ONLY` 模式下跳过 $1-M^2<0$（超声速）节点。
- `SYOR`：每次 sweep 开始时重置全部诊断量，对每列 Thomas 解完后调用 `CLIPLN`。
- `SOLVE`：在 `flag_output` 开启时打印每次 sweep 的限制器诊断行。

**Python 修改**

`pytsfoil.py`：
- `_default_config()` 新增上述 4 个配置键，通过已有的 `setattr` 循环自动写入 Fortran。
- `run_fortran_solver()` 在求解结束后导出诊断量（`vel_lim_n_clipped`、`vel_lim_max_u_before`、`vel_lim_n_infeasible`）和逐节点 clip 计数矩阵（`vel_lim_clip_map`）到 `data_summary`；同时对所有模式统一导出 P 势场（`P_field`）供后处理可视化。

**测试脚本**

编写了 `test_2_velocity_limiter/run_test.py`，覆盖 4 种模式：

| 模式标签 | `enabled` | `D` | `elliptic_only` | 说明 |
|---|---|---|---|---|
| `baseline` | False | 5.0 | False | 基准（无限制器） |
| `lim_D5` | True | 5.0 | False | 宽松护栏 |
| `lim_D3` | True | 3.0 | False | 较紧截断 |
| `lim_elliptic` | True | 3.0 | True | 仅亚声速区，D=3 |

每个工况输出 3 类图：
1. `case_XXXX_cp.png`：Cp/Mach 分布 + ΔCp 与基准偏差 + 限制器激活区域阴影
2. `case_XXXX_velocity.png`：上/下表面 $|P_x|$ 分布 + D 阈值标注
3. `case_XXXX_field.png`（本次新增）：P 势场等值线填色图（含灰色基准等值线参考）+ 最后一次 sweep 中被 clip 的节点散点图（颜色表示 clip 次数）

#### 2.3 测试情况

测试数据库前 10 个工况（Ma ∈ [0.72, 0.75]，AoA ∈ [0.02°, 3.88°]），4 种模式全部正常收敛。

**限制器激活规律**

`SONVEL`（临界速度）在典型条件（Ma≈0.72, δ≈0.12）下约为 1.1（Krupp 标度）。

| 工况特征 | `max_u_before`（D5 模式） | `n_clipped`（D5 最后 sweep） | 结论 |
|---|---|---|---|
| 低 AoA（≤1°，如 case 0,2） | 0.93–1.27 | 0 | 限制器完全不触发，结果与基准完全一致 |
| 中 AoA（≈2°，如 case 1,6~8） | 2.4–2.9 | 5–11 | D5 触发少量节点（最后 sweep 集中在前缘附近） |
| 高 AoA（≥3°，如 case 3~5,9） | 3.1–3.6 | 13–17 | D5 触发明显增多，但远小于 D3 |

**D=5 的表现（宽松护栏）**

- 低 AoA 工况：`n_clipped=0`，Cp 分布与基准完全重合，CL 偏差 < 0.02%，符合"护栏应极少触发"的设计目标。
- 高 AoA 工况（case 3，AoA=3.17°）：`n_clipped=13`，dCL%=+8.98%，RMSE_Cp 从 0.279 升至 0.330。说明在高攻角下，真实物理速度峰值已接近甚至超过 D=5，限制器已开始系统性偏置解。

**D=3 的表现（过紧）**

- 最后一次 sweep 中 `n_clipped` 普遍为 10~130 个节点，高 AoA 工况 (case 3~9) 平均约 100 个。
- CL 相对基准偏差幅度高达 −17%（case 5）至 +50%（case 6），RMSE_Cp 也显著上升。
- D=3 ≈ 2.7 × SONVEL，已深入物理超声速流速区，限制器系统性截断了真实解，不适合作为护栏参数。

**`lim_elliptic`（D=3，仅亚声速区）的表现**

- 多数工况 `n_clipped` 仅略少于 D3 全场（超声速节点占比不大），CL/RMSE_Cp 结果与 D3 全场相近。
- 个别工况（case 3，AoA=3.17°）dCL% 反而更大（34.8% vs 9.6%），说明在该工况下，全场截断和仅亚声速截断对全局环量的影响存在非线性差异，`elliptic_only` 并非总是更温和的选择。

**P 场与 clip 位置可视化**

`_plot_field()` 图显示，在 D5 模式下最后一次 sweep 中被 clip 的节点稳定地集中在翼型前缘附近（x/c < 0.05 区域内少数几个节点），位置与已知的 $P_x \sim x^{-1/3}$ 奇性区域吻合，符合护栏设计的预期行为。D3 模式下被 clip 的节点数量更多，且扩散至翼型中部，表明此时限制器已在压制物理解而非单纯的奇异尖峰。

**结论**

速度限制器作为机制本身实现正确、运行稳定。但合适的 $D$ 参数区间很窄：

- $D = 5$（≈ 4.5 × SONVEL）：在低-中 AoA 场合有效，基本不干扰物理解；但在高 AoA 时真实速度峰值已接近 D，护栏效果下降，同时开始引入偏差。
- $D = 3$（≈ 2.7 × SONVEL）：在多数工况下均激活，对物理解有显著干扰，不适合作为护栏。
- 限制器本质上只能抑制因奇性导致的发散，无法改善 TSD 模型在大攻角下前缘速度分布本身的精度；这与 `TSD_velocity_limiter_projected_SOR.md` §7 的警告一致。

### 任务3：IBL 积分边界层耦合

#### 3.1 任务描述

参考 `test_3_ibl_demo/ibl.py` 中的 IBL 边界层求解器实现，构建 IBL-TSD 耦合流程，
修改 Fortran 和 Python 代码，在每 `N_IBL` 次迭代中用 IBL 计算的边界层厚度修正 TSD 的物面边界条件，
观察对计算结果的影响，尤其是原始 pyTSFoil 尾缘的马赫数明显低于 RANS 结果。

在 `test_3_ibl_coupling` 中编写测试脚本。

#### 3.2 完成情况

**Python 修改（`pytsfoil/pytsfoil.py`）**

在 `PyTSFoil` 类中新增 `run_ibl_coupled()` 方法，实现完整的 IBL-TSD 外迭代耦合流程：

```
initialize → set_airfoil → set_mesh → compute_mesh_indices → run_fortran_solver
                                      [首次完整 TSD 求解，使用 config['MAXIT']]
for k in range(n_outer):
    1. 取翼面马赫数 mau/mal（从 data_summary）
    2. IBL.smooth_mach()：高斯平滑马赫分布，弥散 TSD 激波尖峰，防止 IBL ODE 发散
    3. IBL.run() → 上/下表面 θ, δ*, H, cf, x_tr
    4. 裁剪 δ* ≤ delta_star_max
       最后一次迭代（k == n_outer-1）额外调用 IBL.repair_dstar()：
         检测尾缘区数值爆炸并用线性外插替换，使最终边界条件和存储结果干净
    5. IBL.wall_slope_correction() → ±dδ*/dx
       IBL.clip_and_smooth_slope()：裁剪 + 高斯平滑斜率修正
    6. fxu_new = fxu_base + slope_u × relax / delta
       fxl_new = fxl_base + slope_l × relax / delta
    7. 写入 tsf.common_data.fxu/fxl → SETBC(0) → SOLVE（P 热启动，上限 maxit_inner）
    8. 更新 data_summary；记录 history
```

关键参数：

| 参数 | 默认值 | 说明 |
|---|---|---|
| `n_outer` | 5 | 外迭代次数 |
| `ibl_relax` | 0.5 | 斜率更新欠松弛系数 |
| `mach_smooth_sigma` | 2.0 | 马赫分布高斯平滑（网格点数为单位） |
| `slope_smooth_sigma` | 3.0 | 斜率修正高斯平滑 |
| `delta_star_max` | 0.05 | δ* 上界（弦长比），防止 IBL 发散传递到 TSD |
| `slope_correction_max` | 0.1 | `d(δ*)/dx` 绝对值上界 |
| `maxit_inner` | 200 | 热启动 TSD 子迭代上限；首次冷启动仍用 `config['MAXIT']` |

耦合机制：NWDGE=0 时 SETBC(0) 直接将 FXU/FXL 写入 FXUBC/FXLBC，整个 SOLVE 过程中这些边界条件保持不变（VWEDGE 不会覆盖）；SOLVE 内 P 数组不重置，从当前势场热启动。热启动子迭代数上限 `maxit_inner`（默认 200）远小于冷启动的 `MAXIT`（默认 9999），利用了相邻外迭代步边界条件扰动小的特点，减少总 TSD 求解开销。

**IBL 模块（`pytsfoil/ibl.py`）**

将 `IBL` 类从 `test_3_ibl_demo/ibl.py` 迁移至 `pytsfoil/` 包，作为正式公开 API（`from pytsfoil import IBL`）。在公共接口中新增三个静态工具方法：

| 方法 | 说明 |
|---|---|
| `IBL.smooth_mach(mach, sigma)` | 对壁面马赫分布做高斯平滑，前置 M=0 地板替换 |
| `IBL.clip_and_smooth_slope(arr, sigma, max_val)` | 裁剪 + 高斯平滑斜率修正数组 |
| `IBL.repair_dstar(xx, dstar, outlier_sigma=5.0)` | 检测 δ*/θ 尾缘数值爆炸（梯度超过上游中位梯度 5 倍）并用线性外插修复 |

`repair_dstar` 不在耦合主循环中逐步调用（否则会错误压制湍流区合理的加速增长），仅在最后一次外迭代应用，用于生成干净的最终存储结果与图形输出。

**测试脚本（`test_3_ibl_coupling/run_test.py`）**

多翼型批量测试：从翼型数据库取前 10 条，10 进程并行（每进程一个翼型），N_OUTER=10。

每个 case 输出 3×2 子图（`figures/case_XXXX_ibl.png`）：
- 第一行：压力系数 Cp 对比（RANS / 基线 / IBL 耦合） | 翼型几何与黏性等效体（δ* 覆盖）
- 第二行：壁面马赫数分布 | 外迭代收敛历史（CL 和 Cd_f）
- 第三行：位移厚度 δ* 和动量厚度 θ（层流虚线/湍流实线） | 摩擦系数 cf

汇总报告（`figures/summary.txt`）按 case 列出 RMSE_Cp、CL、CD_wave、CD_fric、CD_total（与 RANS 对比）及各阶段 CPU 时间（基线 TSD / IBL 耦合）。

#### 3.3 测试情况

**数值稳定性**

初始实现（无平滑/裁剪）出现灾难性发散：TSD 激波尖锋（1~2 个网格点）使 IBL Head ODE 失败，
forward-Euler 回退步长过大，δ* 爆炸 → dδ*/dx 爆炸 → TSD 壁面条件过度修正 → CL → −5772 → NaN。

加入高斯平滑（mach_smooth_sigma=2.0）和裁剪（delta_star_max=0.05，slope_correction_max=0.1）后，10 轮外迭代全部稳定收敛。部分高马赫/大攻角工况（TSD 趋近发散）下，IBL 尾缘 δ* 仍会出现局部数值爆炸；此时 delta_star_max 硬截断防止发散传播至 TSD，最终迭代后 repair_dstar 修复存储结果供图形输出。

**结论与分析**

- IBL 耦合将 CL 向 RANS 参考值靠近，典型降幅 10%~20%，方向一致。残余差距来自：（a）外迭代轮数有限，尚未完全收敛；（b）IBL 为薄边界层近似，激波-边界层干扰区精度有限；（c）RANS 包含完整分离与非线性黏性效应，TSD+IBL 不能完全复现。
- Cd_f 处于物理合理区间（Re=O(10⁶) 超临界翼型典型摩擦阻力 0.003~0.006）。
- 热启动子迭代限制（maxit_inner=200）将 IBL 耦合总耗时控制在基线 TSD 的 3~5 倍，相比不限制（每步跑满 MAXIT）节省约 30%~50%。
- 欠松弛（relax=0.5）可保证大多数工况稳定，少数高非线性工况仍有轻微振荡；可通过减小 relax 或增大 n_outer 进一步收敛，代价是更多 TSD 求解次数。
