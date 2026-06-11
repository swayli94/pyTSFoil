# 程序功能改进报告（3）

## 任务背景与目标

### 目标

本项目是在上一阶段重构（`refactor-progress.md`）和程序功能改进 (1,2)
（`improve-progress-1.md`, `improve-progress-2.md`）的基础上，
进一步对 `pyTSFoil` 进行功能改进，以提高数值稳定性、收敛速度和计算结果的准确性。
修改的对象含 `src` 文件夹中的 Fortran 文件，以及 `pytsfoil.py`。

### 文件结构

- `src/`: 本报告工作主要修改的代码所在的文件夹，包含 Fortran 源文件。
- `refactored_src/`: 上一阶段重构后的 Fortran 代码备份，是 `src/` 的初始版本。
- `original_src/`: 原始 Fortran 代码备份，重构前的版本。
- `compile_f2py.py`: 用于编译 Fortran 代码并生成 Python 模块的脚本。
- `pytsfoil.py`: Python 接口代码，调用 Fortran 模块，以及数据处理、结果输出等功能。
- `example/`: 包含示例代码的文件夹。
- `test_*_**/`: 计划编写的测试脚本文件夹，`*` 代表改进过程的任务编号，`**` 代表测试名称。
- `refactor-progress.md`: 上一阶段的重构报告，记录了重构的内容和结果。
- `improve-progress-1.md`: 上一阶段的功能改进报告，记录了第一阶段的改进内容和结果。
- `improve-progress-2.md`: 上一阶段的功能改进报告，记录了第二阶段的改进内容和结果。
- `improve-progress-3.md`: 本报告文件，记录功能改进的过程和结果。

### 测试要求

在修改 Fortran 代码的过程中，我们需要不断测试修改后的代码是否能够正确运行，并且输出的结果是否正确。
由于本项目是 Fortran-Python 混合编程，因此我们需要在 Python 中调用 Fortran 代码来测试修改后的 Fortran 代码是否能够正确运行，并且输出的结果是否正确。

注意，原始代码是正确的。因此，在每项任务完成后，需要使用 `compile_f2py.py` 来编译 Fortran 代码，测试修改后仍然可以 Python 正常调用，并且输出的结果与原始代码相同/相近。

目前没有写测试代码，但是可以参考 `example` 文件夹中的示例代码来构建新的测试脚本。

## 改进过程

本章节包括各个任务的描述，每个任务的描述包括：

- 任务描述：对任务的背景、目标和具体内容进行详细描述。
- 完成情况：描述修改了哪些内容，总结完成情况。
- 测试情况：基于`测试要求`一节中的测试要求，描述测试的过程和结果。

### 修正方法框架的总结

综合前序任务的工作和分析，pyTSFoil 的修正方法框架已经基本成型。
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

### 任务12：前缘修正的改进

#### 12.1 任务描述

前序任务已经实现了上述功能，但效果仍不让人满意，存在一些问题。
因此，本任务对奇性扣除修正和 MAE 修正的各个环节，进行消融测试，以分析各个环节的作用和效果，
并基于分析结果进行针对性的改进。

测试文件夹为 `test_12_correction_components/`。

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

另外，修正项 D 中的修正，全部采取 'linear' 的方式。

#### 12.2 完成情况

**代码改动（`pytsfoil/pytsfoil.py`）：**

新增配置项 `apply_step_e: False`，允许步骤 E（后处理恢复 $\phi_{s,x}$）独立于步骤 A/C 激活。
相关改动共 4 处：

1. `_default_config`：新增 `'apply_step_e': False`。
2. `run_fortran_solver`：触发 `_apply_singularity_subtraction_bc` 的条件扩展为
   `apply_singularity_subtraction=True` 或 `apply_step_d=True`，支持仅 D 的路线。
3. `_apply_singularity_subtraction_bc`：将 2D PHI_S 填充（步骤 A/C）用条件保护，
   仅 `apply_singularity_subtraction=True` 时填充；仅开 D 时 PHI_S 保持零，迭代量仍为 $\phi$。
   FXU/FXL 恢复逻辑改为仅检测 `_fxu_orig` 是否存在，不再依赖 `apply_singularity_subtraction`。
4. `output_surface`：步骤 E 激活条件改为 `apply_singularity_subtraction OR apply_step_e`。

七个测试模式对应配置（`step_d_method='linear'`）：

| 模式 | `apply_singularity_subtraction` | `apply_step_d` | `apply_step_e` | `apply_le_correction` |
|------|:---:|:---:|:---:|:---:|
| baseline    | F | F | F | F |
| only_D      | F | T | F | F |
| only_DE     | F | T | T | F |
| all_AE      | T | T | F* | F |
| MAE_only    | F | F | F | T |
| DE_MAE      | F | T | T | T |
| all_AE_MAE  | T | T | F* | T |

\* `apply_singularity_subtraction=True` 时步骤 E 自动激活。

#### 12.3 测试情况

大攻角下 TSD 在前缘的 M=0 区域很小。

MAE 作为后处理有效，可以获得好看的结果。
但如果 TSD 外区的解本身存在较大误差，例如较大的前缘的 M=0 区域，MAE 也无法完全补偿。

奇性扣除修正无效，无法消除大攻角下 TSD 在前缘的 M=0 区域。

测试表明当翼型厚度较小时，即使在大攻角下，前缘的 M=0 区域会明显缩小，甚至消失。

但是，使用 RIGF 减小前缘的斜率赋值，也不能减小前缘的 M=0 区域。

因此，到底什么导致了大攻角下前缘 M=0 区域的存在，仍然是一个未解之谜。

#### 12.4 成因分析

##### 核心线索：RIGF 无效

RIGF 减小的是翼面法向斜率 FXU/FXL 中的几何奇性（$1/\sqrt{x}$ 分量），但无法减小 M=0 区域。
这意味着 **M=0 区域不是由前缘几何奇性驱动的**。

`improve-progress-2.md` 中的任务 5 流程图给出了关键诊断：

```
FX → BC → PDE 椭圆耦合 → P → U = ∂P/∂x
                              ↑
         全局环量 CIRCFF（后缘 ΔP → RECIRC 每次更新）─┘
```

**U 由全局环量主控，FX 的局部修正通过 PDE 耦合被"冲淡"**。这对 RIGF 和奇性扣除都成立。

##### 两个分量的竞争

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

##### 奇性扣除为何也无效

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

##### 结论与启示

M=0 区域是**大攻角下 TSD 模型固有的结构性缺陷**：
TSD 把边界条件投影到 $y=0$，无法正确表示驻点附近的流动，
而 CIRCFF 在前缘附近积累的速度亏量会随 α 增大而扩展。

对后续工作的启示：

1. **MAE 后处理目前是正确思路**，但前提是在复合公式中使用**未截断的** TSD 速度 $U$（即 `cp_tsd_linear = -2U·cpfact`，绕过 EMACH1 截断），以保证对消机制正确工作。
2. **MAE 的局限性**也是结构性的：它只是后处理，无法改变 CIRCFF 本身的计算。当 TSD 外区的环量（CL）因大 M=0 区域而本身就存在误差时，MAE 也无法完全补偿。
3. 真正的改进方向可能需要在**迭代内部**引入内区解的约束，即用 MAE 内区解修正前缘附近的边界条件（而非仅用于后处理），让 TSD 外区解在大攻角下本身更准确。但这是侵入性修改，复杂度较高。
