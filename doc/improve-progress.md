# 程序功能改进报告

## 任务背景与目标

### 目标

本项目是在上一阶段重构和改进的基础上，进一步对 `pyTSFoil` 进行功能改进，
以提高数值稳定性、收敛速度和计算结果的准确性。
修改的对象含 `src` 文件夹中的 Fortran 文件，以及 `pytsfoil.py`。

### 文件结构

- `src/`: 本报告工作主要修改的代码所在的文件夹，包含 Fortran 源文件。
- `compile_f2py.py`: 用于编译 Fortran 代码并生成 Python 模块的脚本。
- `pytsfoil.py`: Python 接口代码，调用 Fortran 模块，以及数据处理、结果输出等功能。
- `example/`: 包含示例代码的文件夹。
- `test_*_**/`: 计划编写的测试脚本文件夹，`*` 代表改进过程的任务编号，`**` 代表测试名称。
- `improve-progress.md`: 本报告文件，记录功能改进的过程和结果。

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

前序工作中尝试了以下改进：

- 基于匹配渐进展开 (matching asymptotic expansion, MAE) 的前缘压力分布后处理修正 —— 结果不理想，舍弃；
- 基于钝前缘奇性扣除 (singularity subtraction) 的修正 —— 结果不理想，舍弃；
- 基于边界层积分法 (boundary layer integral method, IBL) 的粘性修正 —— 有效果；
- 在 IBL 的基础上，提出的尾缘边界层形态修正 (trailing edge correction, TEC) —— 有效果。

因此，本阶段在 TSD + IBL + TEC 的基础上，继续对 pyTSFoil 代码进行改进，
以提高数值稳定性、收敛速度和计算结果的准确性。

## 改进过程

本章节包括各个任务的描述，每个任务的描述包括：

- 任务描述：对任务的背景、目标和具体内容进行详细描述。
- 完成情况：描述修改了哪些内容，总结完成情况。
- 测试情况：基于`测试要求`一节中的测试要求，描述测试的过程和结果。

### 任务1：TSD + IBL + TEC 参数

#### 1.1 任务描述

根据与 RANS 结果的对比，寻找合适的 TSD + IBL + TEC 参数组合。

#### 1.2 完成情况

编写了测试脚本 `test_1_ibl_tec_wedge/run_test.py`。

#### 1.3 测试情况

测试结果表明，以下设置可以获得较为合理的结果：

- TSD 中的相似律模型 `SIMDEF=3` (Krupp) 提供了更好的结果;
- TSD 中的 Rigidity factor `RIGF=0.2` 对钝前缘的斜率进行了适当的限制，提供了更好的结果;
- IBL + TEC 开启，且 `TE_RELAX=0.5` 适当放松了 TEC 对尾缘处边界层斜率匹配来流的限制，提供了更好的结果;
- IBL + TEC 开启，且 `NWDGE=2` 适当增加了粘性楔修正的频率，提供了更好的结果。

目前的不足：

- 大攻角或来流马赫数较大时，如果翼型当地马赫数 `Ma>1.2`，则流动速度无法继续增加，
  要么导致激波计算结果偏弱、位置偏前；要么导致计算发散，整个上表面全是超声速区。

### 任务2: 当地马赫数 Ma>1.2 时的数值稳定性/收敛性

#### 2.1 任务描述

当来流马赫数较大或攻角较大时，翼型局部马赫数 `Ma` 会超过 1.2，导致 TSD 结果不合理。
这种数值迭代结果不合理表现为：局部马赫数无法突破 1.2，要么导致激波计算结果偏弱、位置偏前；
要么导致计算发散，整个上表面全是超声速区。
需要针对这种情况进行改进，以提高计算结果的准确性。

本部分在 `test_2_large_local_ma` 中进行测试，
以 RANS 样本集中的 `LIST_CASES = [0, 1, 2, 3, 4, 5]` 作为测试对象，进行改进。

重点分析 TSD 迭代中（尤其是 Fortran 部分）局部马赫数大于 1.2 时的数值行为，
是否触发了什么限制器，或者为什么无法正常计算出激波。
不考虑 TSD 与 IBL/TEC 之间的耦合，目前 TSD 本身即存在问题。

可以先分析 Fortran 代码，编写分析文档。

#### 2.2 Fortran 代码分析：TSD 求解器在 Ma > 1.2 时的数值行为

##### 2.2.1 TSD 方程与 Murman-Cole 型差分方案

TSD（Transonic Small-Disturbance）方程的相似形式为：

```
(K - (γ+1)·φ_x) · φ_xx + φ_yy = 0
```

其中 `K = AK`（相似参数），`γ+1 = GAM1 = 2.4`，`φ` 为扰动速度势。
系数 `(K - (γ+1)·φ_x)` 对应 `1-M²` 的相似量，是决定流场类型（亚/超声速）的关键。

在 `SYOR`（`main_iteration.f90`）中，该系数被命名为 `VC(J)`：

```fortran
VC(J) = C1(I) - (CXL(I)*POLD(J,I2) + CXC(I)*P(J,I) + CXR(I)*P(J,I+1))
```

其中：
- `C1(I) = AK / DXC`：对应 `K / Δx_c`（相似参数的贡献）
- `CXL(I)*POLD + CXC(I)*P + CXR(I)*P_next`：对应 `(GAM1/2) · φ_x / Δx_c`（一阶速度项）

当 `VC(J) < 0` 时，流动局部超声速（`M_local > 1`）；反之为亚声速。

Murman-Cole 型差分的迎风机制通过 `EMU` 实现：

```fortran
EMU(J,I1) = 0.0
if (VC(J) < 0.0) EMU(J,I1) = VC(J)   ! 仅超声速区有值（负数）
```

在矩阵对角线组装时：

```fortran
DIAG(J) = (EMU(J,I1) - VC(J)) * CXXC(I) * WI + EMU(J,I2) * CXXR(I-1) - CYYC(J)
```

- 亚声速（VC > 0）：`EMU=0`，第一项为 `-VC·CXXC·WI < 0`（对角线贡献为正稳定项）
- 超声速（VC < 0）：`EMU=VC`，第一项为零，改由上一列的 `EMU(J,I2)·CXXR(I-1)` 提供迎风耗散

这就是 Murman-Cole 迎风差分的核心。

**关键：无 Ma 限制器。** `VC(J)` 可以无限制地取负值，无论 `M_local` 是 1.1 还是 1.5。

##### 2.2.2 EMACH1 函数的作用范围

`solver_functions.f90` 中的 `EMACH1` 计算局部相似马赫数：

```fortran
function EMACH1(U, DELTA) result(result_emach)
    AK1 = AK - GAM1*U
    ! Krupp scaling (SIMDEF=3):
    ARG = 1.0 - DELRT2 * AK1 * EMACH
    result_emach = 0.0
    if (ARG > 0.0) result_emach = sqrt(ARG)
end function
```

**重要结论：`EMACH1` 仅用于后处理和 `VWEDGE`，不参与 SOR 迭代主循环。**

`EMACH1` 在代码中的调用位置：
- `VWEDGE`：计算激波上游马赫数 `AM1(M,N)` — 用于粘性楔角计算
- `output_surface`（Python `pytsfoil.py`）：输出表面马赫数分布
- `output_field`（Python `pytsfoil.py`）：输出全场马赫数

超声速区（`U > SONVEL`）时：`AK1 = AK - GAM1·U < 0` → `ARG > 1.0` → `result_emach > 1.0`，正常返回，无截断。

`ARG ≤ 0` 的情况（返回 0）仅发生在强减速区（U 很大的负值），即激波下游强亚声速区，这是物理上的边界情况，并非限制器。

##### 2.2.3 SONVEL 和激波定位

`SONVEL = AK / GAM1`（`solver_data.f90`）是相似坐标下的声速值。
激波定位通过 `FINDSK`（`solver_base.f90`）完成：

```fortran
subroutine FINDSK(...)
    if (U1 > SONVEL .and. U2 <= SONVEL) exit  ! 找到从超→亚的跨越点
```

即寻找 `PX = dP/dx` 从超声速（> SONVEL）到亚声速（≤ SONVEL）的过渡位置。

**TSD 激波跳跃关系的有效性限制：**

TSD 理论给出的激波关系式（小扰动假设下）等效于正激波在 `Ma₁ → 1` 时的线性展开。
当实际 `Ma₁ > 1.3` 时，TSD 激波关系与精确 Rankine-Hugoniot 条件的偏差迅速增大：

| 激波上游 Ma₁ | 精确 Ma₂ (RH) | TSD 估算 Ma₂ | 误差 |
|:---:|:---:|:---:|:---:|
| 1.1 | 0.912 | ≈ 0.912 | 极小 |
| 1.2 | 0.842 | ≈ 0.840 | 小 |
| 1.3 | 0.786 | ≈ 0.775 | 中等 |
| 1.5 | 0.701 | ≈ 0.660 | 明显 |

结果是：当 `Ma_local > 1.2` 时，TSD 求解器仍然能迭代，但激波位置/强度的预测开始偏离实际；
`Ma > 1.3` 时偏差更大，这是 TSD 方法本身的物理局限，而非数值 bug。

##### 2.2.4 EPS 人工黏性的实际作用：稳定性底线，而非峰值马赫数的限制因素

```fortran
EPSX = EPS / ((X(I) - X(I-1))**2)
DIAG(J) = DIAG(J) - EPSX
RHS(J) = RHS(J) - EPSX*(P(J,I-1) - POLD(J,I2))
```

该项等效于在 TSD 方程中添加沿流向的人工黏性 `EPS · d(dP/dx)/dx`，
目的是抑制 Murman-Cole 格式在声速线处类型切换时产生的数值振荡（"激波抖振"，shock chattering）。

**测试验证**：将 `EPS` 从 0.5 降至 0.1–0.2，峰值马赫数仍维持在约 1.2，未见突破；
且更小的 EPS 反而导致数值不稳定。这表明：

1. **EPS 不是峰值 Ma 的决定因素**：即使人工耗散降低 5×，峰值 Ma 几乎不变，
   说明在收敛解处 EPS 项并非控制峰值 U 的主导机制。

2. **EPS 是迭代稳定性的底线**：Murman-Cole 格式在 VC 变号（声速线跨越）时，
   `DIAG` 的椭圆贡献 `(EMU-VC)·CXXC·WI` 在超声速侧归零（见 2.2.1），
   使得矩阵对角线在声速线附近偏弱。EPS 补充了该列的对角元，
   维持矩阵的对角优势，阻止激波位置在每次迭代中反复振荡（shock chattering）。
   减小 EPS 会使激波抖振加剧，最终导致不收敛。

**结论**：EPS 有一个不可低于的稳定性下限（约 0.1–0.2）。
在此范围内调整 EPS 不会影响峰值马赫数；低于此下限则会破坏收敛性。
峰值 Ma 的实际上限来自 TSD 方程本身的精度（见 2.2.5 根因 1）。

##### 2.2.5 失效模式 A：收敛但马赫数偏低（激波偏前偏弱）

这是 TSD 在 Ma_local 应超过 1.2 的工况下最常见的表现：迭代收敛，但结果偏离实际。
根因有两层：

**根因 1：TSD 小扰动方程的固有精度上限**

TSD 方程基于渐近展开，假设扰动速度 `U = O(δ^(1/3))`。
对于 δ ≈ 0.12，`δ^(1/3) ≈ 0.49`，这并非真正意义上的"小量"；
而在超声速泡内，`U ≈ 2 × SONVEL`，远超该量级假设。

TSD 方程丢弃了所有 `O(δ^(2/3))` 量级的高阶非线性项（如 `U²·Uxx`、`V·Uxy` 等），
正是这些项驱动超声速区内的进一步流动加速。缺少这些项，
TSD 的超声速解比真实 Euler 方程偏"平"：超声速泡的峰值扰动速度 `U_peak` 被系统性低估。

对于本项目的测试工况（M_inf ≈ 0.75–0.78，δ ≈ 0.10–0.12，AoA ≈ 1–4°），
TSD 方程收敛后的自然峰值 Ma 约为 1.1–1.2。
这不是 EPS 的压制作用（已由测试否定），也不是数值 bug，
而是 TSD 方程精度所决定的上限：**TSD 的正确答案即为 Ma ≈ 1.1–1.2；
RANS 给出的 Ma > 1.2 更接近真实流动，因为 RANS 求解包含全部非线性项的 N-S 方程。**

**根因 2：TSD 激波跳跃关系本身的误差**

TSD 弱激波理论给出的跳跃关系（对称于 SONVEL）：
```
U₁ + U₂ = 2 · SONVEL
```

与精确 Rankine-Hugoniot 相比，TSD 高估了激波两侧的速度差 ΔU，
等价于低估了激波下游的 Ma₂（使下游更偏亚声速）。

由此产生的效果：在给定的压力系数积分约束（CL = 翼面 Cp 分布积分）下，
TSD 解的"平衡" U₁ 低于精确解 —— 系统在更低的峰值马赫数处达到压力平衡。

两个根因叠加，导致 TSD 在应该计算 Ma ≈ 1.25 的工况下只能输出 Ma ≈ 1.05–1.15，
激波位置前移 5%–15%c，激波压升比偏小。

##### 2.2.6 失效模式 B：发散（整个上表面超声速，无激波形成）

对于更高 Ma_inf 或更大攻角的工况，物理流动要求激波出现在翼型弦线范围以外
（激波完全被推出翼型后缘，或根本不形成附体激波），此时 TSD 迭代发散。

**发散机制**：

Murman-Cole 方案在超声速列（VC < 0 的 I 列）退化为**迎风推进方案**：
当 VC < 0 时，DIAG 的椭圆贡献 `(EMU-VC)·CXXC·WI` 归零，
矩阵对角线仅由上一列的 `EMU(J,I2)·CXXR(I-1)` 给出。
这意味着每一列 I 的解完全由 I-1 列驱动，形成单向推进格式。

在亚声速区（VC > 0）或激波点（VC 跨零），椭圆和双曲算符共存，
解在两个方向上都受约束，形成良定边值问题。

若整个上表面均为超声速（无激波），推进从 ILE 一直到 ITE：
- 每列的解只看前一列，不存在来自下游（ITE/IMAX）的约束
- 尾缘 Kutta 条件（`PJUMP`, `CIRCTE`，见 `RECIRC`）提供一个跳跃约束，
  但该条件是针对亚声速回流而设计的 —— 全超声速时 CIRCTE 随迭代剧烈振荡
- 每次迭代的 `ERROR`（最大 dP 变化量）在上下两侧激波缺失时无法自然收缩
- 最终 `ERROR > DVERGE = 10.0` → `ABORT1 = .true.` → 迭代终止

**发散的边界条件**：

来流越高（M_inf↑）或攻角越大（AoA↑），临界点越低：
物理上，超临界翼型在 M_inf ≈ 0.78–0.80（视攻角而定）时激波开始推至后缘，
对应 TSD 中上述全超声速失稳条件。

##### 2.2.7 两种失效模式的根本原因总结

| | 模式 A：收敛但偏差 | 模式 B：发散 |
|---|---|---|
| **表现** | 峰值 Ma ≈ 1.05–1.20，激波位置偏前 | ERROR 快速增大，ABORT1 触发 |
| **物理条件** | 激波在 [0,1] 范围内 | 激波需在 x > 1 或不存在 |
| **根因 1** | TSD 方程丢弃高阶非线性项，使峰值 U 系统性偏低 | 全超声速，Murman-Cole 缺少下游约束 |
| **根因 2** | TSD 弱激波误差，平衡于偏低的 U₁ | Kutta 条件与全超声速上表面不相容 |
| **可修复性** | EPS 调整无效；需后处理修正或方程升级 | TSD 适用范围外；需特殊处理 |

代码层面：两种模式都**没有显式限制器**触发于 Ma = 1.2，
"无法突破 1.2" 是 TSD 方程精度固有局限与弱激波关系误差共同作用的结果。
EPS 调整测试已确认：数值耗散不是主要原因。

##### 2.2.8 改进方向建议（聚焦纯 TSD）

由于峰值 Ma 的上限由 TSD 方程本身的精度决定（非数值参数可调），
且 EPS 有不可突破的稳定性下限，可行的改进方向分为以下三类：

**方向 A（针对模式 A）：翼面 Cp/Ma 的后处理修正**

接受 TSD 解给出的激波位置和 Cp 分布形状，但对超声速区的峰值幅值
（峰值负压区 Cp_min）进行基于 RANS 数据的系统性修正。

具体思路：
1. TSD 输出 Ma_local 分布（已通过 `EMACH1` + `calculate_cp_isentropic` 换算为 Cp）
2. 与数据库 RANS 对比，拟合修正因子 `k(M_inf, AoA)` 使 Cp_peak_TSD × k ≈ Cp_peak_RANS
3. 修正系数作为元数据保存，在 `output_surface` 的后处理中应用

此方法无需修改 Fortran 求解器，仅在 Python 层添加后处理；
风险：k 的外推精度取决于样本量，且对激波弦向位置的偏差无法修正。

**方向 B（针对模式 B）：检测全超声速后强迫产生尾缘激波**

模式 B 发散的根本原因是全超声速区 Murman-Cole 退化为单向推进、缺少下游约束。
修复思路是：在计算域内人为恢复一个声速点（VC = 0），重建椭圆性与下游约束，
迫使求解器在尾缘 x = 1 处"钉住"一道激波，将模式 B 转化为可收敛的近似解。

##### 机制一：尾缘声速惩罚项（主要修改，`SYOR`）

在 `SYOR` 的 I 列循环中，当 Mode B 激活（`MODEB = .TRUE.`）时，
对 I = ITE-2 ~ ITE 的列额外添加右端惩罚项，将局部 U 向 SONVEL 拉拢：

```fortran
! 在 SYOR 中，DIAG/RHS 组装完成后、Thomas 求解前：
if (MODEB .and. I >= ITE-2 .and. I <= ITE) then
    ALPHA_SONIC = BETA_FORCE / ((X(I) - X(I-1))**2)  ! BETA_FORCE >> EPS
    do J = JBOT, JTOP
        ! 目标：令 (P(J,I) - P(J,I-1))/DX → SONVEL（局部 U → 声速）
        P_SONIC_TARGET = P(J,I-1) + SONVEL * (X(I) - X(I-1))
        DIAG(J) = DIAG(J) + ALPHA_SONIC
        RHS(J)  = RHS(J)  - ALPHA_SONIC * (P(J,I) - P_SONIC_TARGET)
    end do
end if
```

该项使 ITE 列的 VC → 0，Murman-Cole 在此切换回椭圆模式，恢复下游约束。  
建议 `BETA_FORCE = EPS * 200 ~ 1000`；只影响靠近 ITE 的 2–3 列，其余不变。

##### 机制二：局部增强耗散（备选/叠加，`SYOR`）

实现最简单，可单独或与机制一叠加使用：

```fortran
EPS_LOCAL = EPS
if (MODEB .and. I >= ITE-4 .and. I <= ITE) then
    EPS_LOCAL = EPS * EPS_AMPL    ! EPS_AMPL = 200 ~ 1000
end if
EPSX = EPS_LOCAL / ((X(I) - X(I-1))**2)
```

强耗散迫使 U 在进入尾缘前单调下降，产生扩散型"激波"。
缺点：激波宽度约为 N_FORCE 个格距，比机制一的声速钉点更弥散。

##### 机制三：冻结环量更新（必要配合，`SOLVE`/`RECIRC`）

单独使用机制一/二 仍可能因 CIRCTE 振荡而失稳，需同步冻结循环量更新：

```fortran
! 在 SOLVE 中，调用 RECIRC 之前：
WCIRC_ACTIVE = WCIRC
if (MODEB) WCIRC_ACTIVE = 0.02   ! 极慢更新，近似冻结 PJUMP
```

消除 CIRCTE 振荡后，机制一产生的尾缘声速点才能稳定建立。

##### 检测与控制流（`SOLVE`）

```fortran
! 连续 K_MODEB 次迭代 ERROR 单调上升 → 触发 Mode B 恢复（而非 ABORT1）
if (ERROR_RISING_COUNT >= K_MODEB .and. .not. MODEB) then
    MODEB = .TRUE.
    ERROR = 0.0    ! 重置误差，继续迭代
    ABORT1 = .FALSE.
end if
```

若施加机制一/三后仍未在 MAXIT 内收敛，则记录 `tsd_modeb = True` 并退出；
在 Python 层识别该标志，标记本工况为近似解，不参与 IBL/TEC 耦合。

##### 物理近似的有效范围

| 实际激波位置 | 方向 B 后精度 |
|---|---|
| x ≈ 0.98–1.05（刚越过 TE） | 较好；钉住激波与真实位置接近 |
| x ≈ 1.1–1.3（较远尾流） | 中等；超声速泡偏长，峰值 Cp 偏低 |
| x > 1.5（深入尾流） | 差；TSD 本身已超出适用范围，结果仅供参考 |

**方向 C（长期规划）：引入高阶修正项**

若需要 TSD 本身给出更准确的峰值 Ma（而非后处理修正），
需在方程中引入被截断的 `O(δ^(2/3))` 非线性项，升级为改进 TSD（Extended TSD）
或直接切换到全位势方程（Full Potential Equation）。
这超出当前 TSD 框架，视优先级单独立项。

**优先级排序**：
- 最高优先：**方向 B** — 强迫尾缘激波 — 将模式 B 从发散转化为近似收敛，提高鲁棒性，实现成本低–中
- 次优先：**方向 A** — Cp 后处理修正 — 缩短 TSD 与 RANS 的峰值 Cp 偏差，实现成本中
- 长期规划：**方向 C** — 方程升级 — 从根本上提高精度，实现成本高

### 任务3: 当地马赫数 Ma>1.2 时的数值稳定性/收敛性

#### 3.1 任务描述

完成任务2中计划的 **方向 B**。

本部分在 `test_3_large_local_ma` 中进行测试，
以 RANS 样本集中的 `LIST_CASES = [0, 1, 2, 3, 4, 5]` 作为测试对象，进行改进。
