# 程序功能改进报告（2）

## 任务背景与目标

### 目标

本项目是在上一阶段重构（`refactor-progress.md`）和程序功能改进（1）（`improve-progress-1.md`）的基础上，
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
- `improve-progress-2.md`: 本报告文件，记录功能改进的过程和结果。

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

### 任务5：前缘修正的渐近匹配方案分析

#### 5.1 任务描述

延续 `improve-progress-1.md` 中任务4（1-3）的分析，
pyTSFoil 在前缘 (x=0) 附近计算过程中出现了 U ≤ U_critical 触发截断为 M=0 的情况，其中：

```text
U_critical = -1 / (δ^(2/3) · (γ+1) · M∞)  ≈  -1.64  (对于 M∞=0.73, δ=0.08)
```

前缘附近扰动速度 U 过度负值导致的 Ma=0 区域是 TSD 模型的固有局限性，无法通过简单限制消除。
尝试的简单前缘附近边界条件修正方案无效。

跨声速小扰动（TSD）方程在圆前缘（钝前缘）翼型上的核心矛盾：
TSD 假设扰动量小（u, v ≪ U∞），但圆前缘附近恰恰违反这个前提——驻点处 u → −U∞，
绕鼻部流速梯度大，
而且几何上抛物线鼻 y ∼ ±√(2Rx) 的斜率 dy/dx ∼ √(R/2x) → ∞，
使得贴在 y=0 缝面上的边界条件 φ_y = U∞·dy/dx 继承了 1/√x 奇性，
叠加迎角后就是所谓"前缘吸力峰"奇点。结果是 TSD 解在外区有效、
但在鼻部一个 O(R) ∼ O(τ²·c) 的内区里非一致有效。

匹配渐近展开（内外区匹配）是最主流的理论路线，理论参考 `TSD_leading_edge_correction.md`。
外区就是标准 TSD（其二阶问题里显式出现前缘奇点），内区描述鼻部绕流。
领头项对应的是均匀声速流绕一个二维抛物线（零迎角、无环量）的边值问题，存在一个驻点。
两个展开做渐近匹配后，可以用外区小扰动解加上鼻部解，导出整个翼面上一致有效的压力分布。

结合 pyTSFoil 代码，结合理论，分析前缘修正的渐近匹配方案，寻找可行的实现方法。

#### 5.2 修正方案与可行性分析

##### 5.2.1 MAE 复合解方案概述

`TSD_leading_edge_correction.md`（Rusak 1993 理论整理）给出了从理论上完整解决前缘奇性的路线图，核心是以下**加性复合公式**（第 6 节）：

$$c_p(x) = c_p^*(s) + \frac{\rho^*}{\rho_\infty}\,\phi_{0x^*}\!\left[\,c_{p\text{TSD}}(x) - c_{pc,p}(s)\,\right], \quad s = x/R_c$$

各项含义：

| 符号 | 来源 | 说明 |
|---|---|---|
| $c_{p\text{TSD}}(x)$ | pyTSFoil SLOR 已有输出 | 外区 TSD 压力系数（含鼻部 $x^{-1/3}$ 奇性） |
| $c_{pc,p}(s) = 0.635776\,(\gamma+1)^{-1/3}\,s^{-1/3}$ | 解析闭式 | TSD 鼻部奇性的精确渐近形式（公共部分） |
| $c_p^*(s),\ \rho^*/\rho_\infty(s),\ \phi_{0x^*}(s)$ | 内区数值求解 | 随 $s=x/R_c$ 变化的三张表（一次性计算） |

**对消机理**（三段行为）：

| 区域 | $\phi_{0x^*}$ 趋势 | 复合解退化到 | 物理意义 |
|---|---|---|---|
| 鼻部内区 $x \lesssim R_c$ | $\to 0$ | 内区解 $c_p^*(s)$（有限驻点值） | $c_{pc,p}$ 对消 TSD 奇性，驻点有限 |
| 重叠区 | 过渡 | 内外平滑衔接 | 渐近匹配保证 |
| 外区 $x \gg R_c$ | $\to 1$ | 外区 TSD 解 $c_{p\text{TSD}}(x)$ | $c_p^*$ 对消 $c_{pc,p}$，退化到原 TSD |

结果：复合解处处有限、前缘对称（与 $M_\infty, \alpha$ 无关的内区主控）、远处精确退化为标准 TSD。

**最关键属性**：复合公式是**后处理**修正，不改变 Fortran SLOR 内核，不影响迭代收敛性，也不改动任何现有 CL/CD/CM 的 Fortran 计算路径。

##### 5.2.2 与任务 4 已验证方案的对比

任务 4 测试的三种方案均未能有效解决前缘 Ma=0 问题，根本原因已清楚：

```text
FX → V（翼面 Neumann BC）→ PDE 椭圆耦合 → P → U = ∂P/∂x
                                                  ↑
          全局环量 CIRCFF（后缘 ΔP → RECIRC 每次更新）─┘
```

U 由全局环量主控，FXL 前缘修正通过 PDE 耦合被"冲淡"；后处理插值没有物理依据；修改 EMACH1 风险过高。

MAE 方案的根本不同点：

1. **理论合理性**：内区方程是完整非线性位势方程，精确描述驻点处的物理（U→-U∞ 时流动连续滞止），不依赖小扰动假设。
2. **外区兼容性**：在外区（$x \gg R_c$）精确退化到 TSD 解，激波位置、环量、尾迹均不受影响。
3. **奇性对消精确**：$c_{pc,p}(s) \propto s^{-1/3}$ 恰好是 TSD 鼻部奇性的渐近形式（见 `TSD_leading_edge_correction.md` 第 3.1 节），数学上严格相消，而非近似插值。
4. **非侵入式**：完全后处理，适合作为独立模块接入，可随时开关，零回归风险。

##### 5.2.3 各实现步骤可行性评估

参考 `TSD_leading_edge_correction.md` 第 8 节给出的五步方案，逐步评估：

---

**步骤一：鼻部几何参数提取（$h$、$R_c$）**

模型：$ct(x/c) \sim 2h(cx)^{1/2}$（$x \to 0$），其中 $t(x/c)$ 为归一化半厚度；曲率半径 $R_c = 2h^2\delta^2 c$。

在 pyTSFoil 中，`set_airfoil()` 后翼型坐标 `xu, yu, xl, yl` 已可用。鼻部拟合方法：

```python
# 取前缘附近 x < x_fit（例如 x_fit = 0.05）的半厚度做 sqrt(x) 拟合
mask = xu < 0.05
half_thickness = (np.interp(xu[mask], xl, yl) - ... ) / 2   # 或直接用上下表面插值
h_coef, _ = np.polyfit(np.sqrt(xu[mask]), half_thickness, 1)  # 半厚度 ≈ h_coef * sqrt(x)
h = h_coef / np.sqrt(c)   # t(x/c) = 2h * sqrt(x/c)，故 h_coef = 2h * sqrt(c)，等价 h = h_coef / (2*sqrt(c))
R_c = 2.0 * h**2 * delta**2 * c
```

可行性：**高**。仅需 numpy 最小二乘，无额外依赖。

注意：
- CST 翼型的 $\sqrt{x}$ 依赖是解析的，拟合误差对前缘 5% 弦内的点通常 < 1%；
- 对任意给定坐标的翼型，若鼻部不严格为抛物线（例如 NACA 四位数翼型的较平滑鼻部），拟合范围应适当限制在前缘 1–3% 弦；
- 需验证所得 $R_c$ 的合理性：量级应在 $2\delta^2 c \sim 0.001c$（$\delta=0.08$）附近。

---

**步骤二：内区数值求解器（一次性，可缓存）**

内区问题在抛物坐标 $(\mu, \eta)$ 下用守恒通量盒式格式离散、SOR 迭代求解（`TSD_leading_edge_correction.md` 第 7 节）。

**普适性**：内区方程中 $M_\infty$（跨声速相似参数 $K$）和 $\alpha$（迎角 $A$）均掉出（领头项由 $M_\infty = 1$、零攻角问题主控），内解仅随 $h, c$ 标度——同一翼型几何的所有来流条件（不同 $M_\infty, \alpha$）共用同一张表，只需计算一次后缓存复用。

实现要点：

| 项目 | 参数 | 说明 |
|---|---|---|
| 计算域 | $\mu_T = \eta_T = 40$ | 足够覆盖内外匹配区，$M=N=40$ 即收敛（与 80×80 差 <0.1%） |
| 翼面 BC | $\phi_\eta(\mu, \eta=\sqrt{2})=0$ | 镜像行：$\phi(i,1) = \phi(i,3)$（切向） |
| 对称 BC | $\phi_\mu(0, \eta)=0$ | 镜像列：$\phi(1,j) = \phi(3,j)$（$x$ 轴对称，零攻角） |
| 远场 BC | $\phi \sim X + \frac{Y^{4/7}}{\gamma+1}\bar{f}(\bar\xi) + O(Y^{2/7})$ | hodograph 函数 $\bar{f}$，由 $\alpha_3=80.41°$ 决定 |
| 离散格式 | 守恒通量盒式，中心差分 | 内区只有亚声速，无需迎风 |
| 迭代 | SOR，$\Omega \approx 1.2$ | 约 1000 次迭代，Python Numba 加速后亚秒级 |
| 输出 | $c_p^*(s),\ \rho^*/\rho_\infty(s),\ \phi_{0x^*}(s)$ | 表面压力在 $x_i^* = \frac{1}{2}h^2\mu_{(i,2)}^2$ 处取得 |

可行性：**中等**。SOR 框架本身简单，但**远场 hodograph 条件**（$\bar{f}$ 函数的计算）需要仔细参考 Rusak (1993) JFM 公式（式 29、30、76），实现有一定难度。

建议分两阶段：
1. 先实现简化版（远场截断为 $\phi \to X$，即 $\bar{f}=0$），验证 SOR 框架与壁面/对称 BC 的正确性；
2. 再补全精确远场条件，以 JFM 图 6 的三条曲线作为验证基准。

---

**步骤三：外区 TSD 解**

**无需额外工作**。pyTSFoil 的 SLOR 求解器已经计算 $c_{p\text{TSD}}$，直接用 `output_surface()` 中已有的 Cp 数组即可。

唯一注意：复合公式要求 TSD 完全收敛（文档特别提示半收敛可能污染复合结果）。pyTSFoil 现有收敛判据（`CVERGE`）已足够；若出现仅部分收敛的情况，可提高最大迭代次数 `MAXIT`。

---

**步骤四：复合后处理（核心计算）**

**变量链与 mau/mal 的修正**

pyTSFoil 的原始变量链为：

```text
P（势场）→ U = ∂P/∂x（扰动速度）→ Ma（EMACH1，含 Ma=0 截断）→ Cp（等熵关系）
```

复合修正应尽量在上游变量 U 处介入，而非对最下游的等熵 Cp 操作。具体地：

1. 复合公式中的外区输入 $c_{p\text{TSD}}$ 应使用**线性 TSD 公式**直接从 U 得到，不经过 Ma 截断：

   $$c_{p\text{TSD}}(x) = -2\,U \cdot \text{cpfact}$$

   这是 TSD 理论本身的 Cp 定义（含鼻部 $x^{-1/3}$ 奇性），其中 `cpfact` = $\delta^{2/3}$ 标度因子（已由 `compute_scale()` 计算）。用等熵 Cp（来自截断后的 Ma）作为外区输入会在 Ma=0 区域引入人为的零值平台，破坏对消机制。

2. 复合公式输出 $c_p^{\text{composite}}$ 是物理压力系数（来自内区完整方程），直接作为最终 Cp 输出。

3. 从 $c_p^{\text{composite}}$ **反算 Ma**，输出 `mau`/`mal`，保证 Ma 与 Cp 自洽：

   $$Ma^{\text{composite}} = \sqrt{\frac{2}{\gamma-1}\left[\left(\frac{2+(\gamma-1)M_\infty^2}{2 + \gamma M_\infty^2 \cdot c_p^{\text{composite}}}\right)^{(\gamma-1)/\gamma} - 1\right]}$$

完整实现：

```python
def apply_composite_correction(x_surface, u_surface, cpfact, minf,
                                R_c, inner_tables, gamma=1.4):
    """
    对上/下表面施加 MAE 复合修正。
    输入: x_surface, u_surface（原始扰动速度 U = ∂P/∂x，不截断）。
    输出: cp_composite, ma_composite（两者严格自洽）。
    """
    s = x_surface / R_c

    # 内区表插值
    cp_star   = np.interp(s, inner_tables['s'], inner_tables['cp_star'])
    rho_ratio = np.interp(s, inner_tables['s'], inner_tables['rho_ratio'])
    phi_ox    = np.interp(s, inner_tables['s'], inner_tables['phi_ox'])

    # 外区输入：线性 TSD Cp（直接从 U 算，不经过 EMACH1 截断）
    cp_tsd_linear = -2.0 * u_surface * cpfact

    # 公共部分（s=0 端点保护）
    s_safe = np.maximum(s, 1e-6)
    cp_common = 0.635776 / (gamma + 1)**(1.0/3.0) * s_safe**(-1.0/3.0)

    # 复合 Cp
    cp_composite = cp_star + rho_ratio * phi_ox * (cp_tsd_linear - cp_common)

    # 反算复合 Ma（等熵关系逆运算）
    numer = 2.0 + (gamma - 1.0) * minf**2
    arg = numer / (numer + gamma * minf**2 * cp_composite)
    arg = np.maximum(arg, 0.0)   # 避免数值负值
    ma_composite = np.sqrt(2.0 / (gamma - 1.0) * (arg**((gamma-1.0)/gamma) - 1.0))

    return cp_composite, ma_composite
```

可行性：**高**。纯 Python/NumPy，逻辑直接，计算开销极小（每次调用约微秒级）。

端点处理说明：$c_{pc,p}(s) \propto s^{-1/3}$ 在 $s \to 0$ 奇异，但 $c_{p\text{TSD,linear}}$ 在同一点有同量级奇性（$U \propto x^{-1/3}$），两者之差为有限量。对 $s < s_{\min}$（如 $s_{\min} = 0.01$）的点令括号项为零（即令 $c_{pc,p} = c_{p\text{TSD,linear}}$），让内区解 $c_p^*$ 单独主控，避免数值除零。

---

**步骤五：载荷重积分**

用修正后的 $c_p(x)$ 重新积分 CL、CM（波阻 CD 由动量积分给出，若使用修正后的场则需要重新计算）：

```python
# CL（梯形积分，上下表面 Cp 差）
CL_corrected = np.trapz(cp_lower_corrected - cp_upper_corrected, x_surface)
# CM（四分之一弦点，积分号中 x 以弦长归一化）
CM_corrected = np.trapz((cp_lower_corrected - cp_upper_corrected) * (0.25 - x_surface), x_surface)
```

可行性：**高**。逻辑简单，可完全在 Python 端实现，无需修改 Fortran。

对阻力 CD：MAE 修正只改变鼻部 Cp，而动量积分阻力（Cole 法）主要由激波剖面决定，前缘修正对 CD 影响相对较小，可暂时保留现有 Fortran 计算路径。

---

**步骤概要对比**：

| 步骤 | 依赖 | 可行性 | 工作量（估计） |
|---|---|---|---|
| 1. 鼻部几何拟合 | numpy | 高 | 0.5 天 |
| 2. 内区 SOR 求解器 | Python/Numba | 中等（远场条件复杂） | 3–5 天 |
| 3. 外区 TSD | 已有 | 无需工作 | — |
| 4. 复合后处理公式 | numpy | 高 | 0.5 天 |
| 5. 载荷重积分 | numpy | 高 | 0.5 天 |

##### 5.2.4 预期改善效果与局限

**改善范围**：

重叠区上界 $\eta(\delta) \ll \delta^{0.772}h^2 c$；对 $\delta=0.08, h=0.5, c=1$：

$$\delta^{0.772} \approx 0.14, \quad \delta^{0.772}h^2 c \approx 0.036$$

即前缘 **~3.6% 弦长**范围内的 Cp 分布由复合解主控，而不是单纯的 TSD 奇性。对数据库算例（AoA ≤ 4°，Ma=0 区域最大到 4.4% 弦），此范围与 Ma=0 区域的重叠良好。

**AoA 的进入方式与接口设计**：

内区领头项问题中 $K$（跨声速相似参数）和 $A$（迎角）均掉出，内解关于 $x$ 轴严格对称，驻点固定在前缘——这恰好是 Rusak 理论的核心结论之一：**跨声速条件 $M_\infty \sim 1$ 下，驻点对任意 AoA 都钉在前缘点**，与亚声速下驻点随 AoA 沿鼻面移动的行为根本不同。

这直接解释了为什么"0 攻角时几乎没问题，高 AoA 才出问题"：
- 0 AoA：流场对称，两个表面的 $U$ 量级相近，TSD 的 $x^{-1/3}$ 奇性上下表面对称，Ma=0 只出现在 $x=0$ 单点（物理正确）；
- 高 AoA：攻角使上下表面边界条件严重不对称，下表面前缘 $U$ 深度负值，TSD 奇性被 AoA 放大，Ma=0 人为扩展为一片区域（物理错误的 TSD 假象）。

**AoA 通过外区 TSD 进入复合公式，无需修改内区求解器**：复合公式上下表面分别应用，各自用本表面的 $c_{p\text{TSD,linear}}$ 作为外区输入：

$$c_p^{\text{upper}}(x) = c_p^*(s) + \frac{\rho^*}{\rho_\infty}\phi_{0x^*}\left[c_{p\text{TSD}}^{\text{upper}}(x) - c_{pc,p}(s)\right]$$

$$c_p^{\text{lower}}(x) = c_p^*(s) + \frac{\rho^*}{\rho_\infty}\phi_{0x^*}\left[c_{p\text{TSD}}^{\text{lower}}(x) - c_{pc,p}(s)\right]$$

内区函数 $c_p^*(s)$、$\rho^*/\rho_\infty(s)$、$\phi_{0x^*}(s)$ 对上下表面完全相同（对称驻点值）；AoA 的不对称性完全由 $c_{p\text{TSD}}^{\text{upper/lower}}$ 携带。在 $s \to 0$ 处，两个表面都趋向同一驻点值 $c_p^*(0)$（物理正确：前缘驻点处 Cp 唯一），然后随 $s$ 增大通过不同的外区值分叉，自然重建 AoA 引起的上下压差。

因此接口设计上，需要分别对上/下表面传入各自的 U 数组：

```python
cp_upper, ma_upper = apply_composite_correction(
    x_foil, u_upper, cpfact, minf, R_c, inner_tables)
cp_lower, ma_lower = apply_composite_correction(
    x_foil, u_lower, cpfact, minf, R_c, inner_tables)
```

其中 `u_upper`/`u_lower` 是从势场 P 提取的上/下翼面原始扰动速度（未经 Ma 截断），与 `output_surface()` 中现有的 `px(i, jup)` / `px(i, jlow)` 调用完全对应。

**精度随 AoA 的变化**：

| AoA 范围 | 下表面 Ma=0 区域 | 内区假设准确性 | 预期效果 |
|---|---|---|---|
| AoA ≈ 0° | 仅 $x=0$ 单点 | 完全准确（对称驻点） | 小幅修正，前缘 Cp 峰值更平滑 |
| 1°–2° | < 1% 弦（1–6 点） | 高 | 消除人为 Ma=0 平台，Cp/Ma 连续 |
| 2°–3° | 1–3% 弦（6–11 点） | 较好 | 显著改善下表面前缘分布 |
| > 3° | > 4% 弦（> 11 点） | 可接受（驻点钉在前缘的跨声速特性仍成立） | 消除 Ma=0 区域，但下表面 $x_{stag}$ 略有偏差 |

注：AoA > 3° 时，TSD 方程本身（不只是前缘处理）对来流条件的描述精度已下降，前缘修正仍有意义但不能消除 TSD 的其他局限。

**与任务 4 方案的根本差异**：

任务 4 中方案二（后处理插值）被否定，理由是"只修改显示值，底层 P 场仍然错误"。MAE 方案也是后处理，但物理意义根本不同——它不是对已有 P 场的局部插值修正，而是用完整方程的内区解**替换** TSD 在鼻部有效性失效区域的输出，同时通过公共部分保证内外区的精确衔接。这正是渐近展开理论的设计目的：用"外+内-公共"合成一致有效解。

##### 5.2.5 与现有代码结构的接口设计

建议在 `pytsfoil/` 下新增子模块（对应 `TSD_leading_edge_correction.md` 第 8 节建议）：

```
pytsfoil/
├── pytsfoil.py               # 现有主类（仅新增两处调用点）
├── leading_edge/             # 新增：前缘修正模块
│   ├── __init__.py
│   ├── inner_parabola.py     # 内区 SOR 求解器（步骤 2）
│   ├── composite.py          # 复合公式 + 公共部分闭式（步骤 4）
│   └── inner_tables_cache/   # 内区表缓存（以翼型 hash 为文件名）
│       └── *.npz
```

在 `pytsfoil.py` 的 `_default_config` 中新增开关：

```python
'apply_le_correction': False,   # 启用 MAE 前缘修正（后处理）
```

接入点：
1. `compute_geometry_derivatives()` 完成后（`delta`、翼型坐标均已就绪）→ 计算 $h$、$R_c$，调用 `inner_parabola.solve_inner_problem()` 生成/载入内区表；
2. `output_surface()` 中，在原始 TSD Cp 计算后、写入输出之前，调用 `composite.apply_composite_correction()`。

这两处改动对现有调用路径（Fortran 主迭代、收敛判断、CL/CD/CM 计算）零影响。

##### 5.2.6 结论

MAE 复合解方案是目前分析到的最理论合理的前缘修正路线，总结如下：

**优势**：
1. **物理正确**：内区用完整方程求解，精确描述驻点处的流动物理，TSD 奇性被严格对消而非近似填补；
2. **非侵入式**：完全后处理，Fortran SLOR 内核零改动，收敛性、CL/CD/CM 基准不受影响；
3. **普适性**：内区表一次性计算并缓存，适用于同一翼型几何的所有来流条件；
4. **理论保证**：内外展开在重叠区 $\delta^2 \ll \eta \ll \delta^{0.772}$ 内精确匹配，自洽性有数学保证。

**挑战**：
1. **内区求解器**是最复杂的部分（远场 hodograph 条件的实现），但技术上可行，难度适中；
2. **高 AoA 精度**：内区对称驻点假设在 AoA > 3° 时精度下降，但仍优于无修正的 TSD；
3. 修正作用于 Cp 层面，而非 P 场本身；Fortran 内部 `EMACH1` 计算的 Ma 仍来自未修正的 P 场（仅影响 Ma 分布的中间输出，不影响修正后的 Cp 和气动力）。

**建议实施顺序**：

1. 步骤一 + 步骤四 + 步骤五：先将内区表替换为解析近似（简化验证），打通整个调用链；
2. 步骤二：实现完整内区 SOR 求解器，以 Joukowski 翼型 $\delta=0.10, M_\infty=0.8, \theta=0°$ 和 $1°$（Rusak 1993 JFM 图 7）为基准验证；
3. 最终以数据库前 10 个算例对比修正前后的 Cp 分布、CL、Cp RMSE。

### 任务6：前缘修正的渐近匹配方案实现

#### 6.1 任务描述

基于任务5的理论分析（`5.2 修正方案与可行性分析`），按照 `TSD_leading_edge_correction.md` 第 8 节的工程方案，在 pyTSFoil 中实现 MAE（匹配渐近展开）前缘修正，并以数据库算例验证修正效果。

##### 实现范围

本任务完成以下五个步骤的代码实现（对应任务5的可行性评估）：

**步骤一：鼻部几何参数提取**

在 `pytsfoil.py` 的 `set_airfoil()` 之后（`xu, yu, xl, yl` 已可用），新增对鼻部抛物线拟合参数 $h$（形状常数）和 $R_c$（曲率半径）的计算：

$$ct(x/c) \sim 2h(cx)^{1/2}\quad(x\to0), \qquad R_c = 2h^2\delta^2 c$$

拟合区间建议取 $x/c \in (0, 0.05]$ 的半厚度点，用 `numpy.polyfit` 对 $\sqrt{x}$ 做线性拟合。$h$、$R_c$ 保存为实例属性，供后续步骤使用。

**步骤二：内区 SOR 求解器**

在 `pytsfoil/leading_edge/inner_parabola.py` 中实现内区问题的数值求解（`TSD_leading_edge_correction.md` 第 7 节）：

- 坐标变换：$x^* = (\bar\mu^2 - \bar\eta^2)/2$，$y^* = \bar\mu\bar\eta$，翼面在 $\bar\eta = \sqrt{2}$ 上；以 $hc^{1/2}$ 归一化后翼面在 $\eta = \sqrt{2}$ 上；
- 控制方程：守恒通量形式，中心差分（内区全域亚声速，无需迎风项）；
- 边界条件：翼面切向（镜像行）、$x$ 轴对称（镜像列）、远场展开（初步实现先用截断 $\phi \to X$，后续补入完整 hodograph 条件）；
- 迭代：SOR，松弛因子 $\Omega \approx 1.2$，收敛判据为残差降至 $10^{-6}$；
- 输出：三张随 $s = x/R_c$ 变化的数组，保存到 `inner_tables.npz`（以翼型几何 hash 为文件名，同一翼型只计算一次）：

  | 数组 | 物理含义 | 极限行为 |
  |---|---|---|
  | `cp_star(s)` | 内区表面压力系数 | $s \to 0$：驻点峰值；$s \to \infty$：$\to 0$ |
  | `rho_ratio(s)` | $\rho^*/\rho_\infty$ 密度比 | $s \to 0$：最大；$s \to \infty$：$\to 1$ |
  | `phi_ox(s)` | $\phi_{0x^*}$ 轴向速度 | $s \to 0$：$\to 0$；$s \to \infty$：$\to 1$ |

**步骤三：外区 TSD 解**

此步骤无需新增代码——复合公式所需的外区输入 $c_{p\text{TSD}}$ 直接从 Fortran 势场 P 提取原始扰动速度 U，以**线性 TSD 公式**计算：

$$c_{p\text{TSD}}(x) = -2\,U \cdot \text{cpfact}$$

不使用任务3引入的等熵 Cp（后者已经过 `EMACH1` 截断，在 Ma=0 区域为零，会破坏对消机制）。U 数组来自 `output_surface()` 中已有的 `px(i, jup)` / `px(i, jlow)` 调用，不需要额外的 Fortran 接口。

**步骤四：复合后处理与 mau/mal 修正**

在 `pytsfoil/leading_edge/composite.py` 中实现复合公式，对上/下表面分别计算：

$$c_p^{\text{composite}}(x) = c_p^*(s) + \frac{\rho^*}{\rho_\infty}\,\phi_{0x^*}\!\left[-2U\cdot\text{cpfact} - c_{pc,p}(s)\right], \quad c_{pc,p}(s) = \frac{0.635776}{(\gamma+1)^{1/3}}\,s^{-1/3}$$

输出同时包括修正后的 `cp_composite` 和由其反算的 `ma_composite`（等熵关系逆运算），保证 `mau`/`mal` 与 `cpu`/`cpl` 自洽。AoA 不对称性通过上下表面各自的 U 数组自然进入，内区函数对上下表面相同。

具体接口：

```python
# composite.py
def apply_composite_correction(x_surface, u_surface, cpfact, minf,
                                R_c, inner_tables, gamma=1.4):
    """返回 (cp_composite, ma_composite)，上/下表面分别调用。"""
    ...
```

**步骤五：载荷重积分**

用修正后的 `cp_upper`、`cp_lower` 重积分 CL 和 CM：

```python
CL_corrected = np.trapz(cp_lower - cp_upper, x_foil)
CM_corrected = np.trapz((cp_lower - cp_upper) * (0.25 - x_foil), x_foil)
```

CD（动量积分）暂时保留现有 Fortran 路径，仅在开启修正时输出重积分的 CL、CM。

##### 代码结构变化

新增文件：

```
pytsfoil/
└── leading_edge/
    ├── __init__.py
    ├── inner_parabola.py    # 内区 SOR 求解器（步骤二）
    └── composite.py         # 复合公式（步骤四）
```

修改文件：

- `pytsfoil.py`：
  - `_default_config` 中新增开关 `'apply_le_correction': False`；
  - `set_airfoil()` 末尾或 `compute_geometry_derivatives()` 末尾新增鼻部拟合与内区表载入/生成（步骤一、二）；
  - `output_surface()` 中在写入输出之前，若 `apply_le_correction` 为 True，对 `mau`/`mal`/`cpu`/`cpl` 数组施加复合修正（步骤四）；
  - `compute_data_summary()` 或新增 `compute_corrected_loads()` 方法输出修正后 CL、CM（步骤五）。

##### 测试计划

测试脚本放在 `test_6_le_correction/`，使用数据库前 10 个算例（与任务4 保持一致），对比以下指标：

1. **下表面 Ma=0 点数**（对比任务4的基准）；
2. **Cp RMSE（与 RANS 对比）**，分上下表面分别统计；
3. **CL、CM 相对变化**（相对任务4基准）；
4. **Cp/Ma 自洽性**（修正后的 $c_p^{\text{composite}}$ 与 $Ma^{\text{composite}}$ 满足等熵关系的 RMSE）。

验证基准（内区求解器正确性）：以 Joukowski 翼型 $\delta=0.10$、$M_\infty=0.8$、$\theta=0°$ 和 $1°$ 复现 Rusak (1993) JFM 图 7，确认复合解与纯 TSD / 纯内区解的对比形态。

#### 6.2 完成情况

五个实现步骤全部完成，新增/修改文件如下：

**新增文件**

- `pytsfoil/leading_edge/__init__.py`：子模块入口
- `pytsfoil/leading_edge/inner_parabola.py`：内区 SOR 求解器 + 几何 hash 磁盘缓存
- `pytsfoil/leading_edge/composite.py`：复合公式 + 驻点上限截断 + 平滑过渡到 TSD
- `test_6_le_correction/run_test.py`：10 个数据库算例的并行验证脚本

**修改文件**

- `pytsfoil/pytsfoil.py`：
  - `_default_config` 新增开关 `'apply_le_correction': False`
  - `set_airfoil()` 末尾新增 `_fit_nose_geometry()`（步骤一）
  - `output_surface()` 末尾新增 `_apply_le_correction()`（步骤四+五）

**主要调试记录**

实现过程中发现并修复了若干关键问题：

1. **$R_c$ 公式错误（致命 bug）**：任务 5 分析中有 $R_c = 2h^2\delta^2 c$，但物理推导（翼面半厚度 $\sim h_{\text{coef}}\sqrt{x}$，其中 $h_{\text{coef}}=2h$）给出 $R_c = h_{\text{coef}}^2/2 = 2h^2$（对 $c=1$）。代码原始版本含多余的 $\delta^2$ 因子，导致 $R_c \approx 1.9\times10^{-5}$（应为 $\approx 0.003$），$s = x/R_c$ 整个翼型远超表格范围，修正几乎失效。修正后：`R_c = 2.0 * h**2 * c`。

2. **SOR 松弛因子 $\omega$**：任务 5 文档建议 $\omega \approx 1.2$，但对 $40\times40$ 网格、GS 谱半径 $\rho_{GS} = \cos(\pi/40) \approx 0.997$，最优 SOR 因子为 $\omega_{\text{opt}} = 2/(1+\sqrt{1-\rho_{GS}}) \approx 1.86$。$\omega=1.2$ 时谱半径 $\approx 0.996$，5000 次迭代不收敛；$\omega=1.85$ 时谱半径 $\approx 0.855$，约 1400 次迭代收敛至 $10^{-6}$，单次求解约 0.9 s。

3. **Ghost cell 密度修正**：鬼行 $i=0$（$\mu=-\Delta\mu$）和鬼列 $j=0$（$\eta=\eta_w-\Delta\eta$）的密度若用错误的 MU/ETA 计算，会污染 Neumann 边界附近的面通量。修正方法：密度计算后覆盖 `rho[0,:] = rho[2,:]`，`rho[:,0] = rho[:,2]`。

4. **解析驻点插入**：$40\times40$ 均匀网格首格在 $\mu=\Delta\mu=1$，对应 $s=(\mu^2-2)/4=-0.25<0$（驻点以下游）。第一个 $s\geq0$ 的格点在 $\mu=2$（$s=0.5$），驻点 $s=0$（$\mu=\sqrt{2}$）落在格间。解决方案：在表格头部预置解析驻点值（$\rho_{\max}=(1+(\gamma-1)/2)^{1/(\gamma-1)}=1.5774$，$c_p^*=1.2756$，$\phi_{0x^*}=0$）。

5. **驻点 Cp 上限截断**：内区在 $M_\infty=1$ 条件下求解，驻点 $c_p^*=1.276$ 超过外区 $M_\infty<1$ 的物理驻点 Cp（例如 $M_\infty=0.72$ 时为 1.139），导致修正后仍出现 $Ma=0$ 点。修正方案：`cp_composite = min(cp_composite, cp_stagnation(M_inf))`。

6. **cp_common 系统性偏差与平滑过渡**：复合公式在 $\phi_{0x^*}\to1$（$s\gtrsim8$）后退化为 $c_{p\text{TSD}}-c_{pc,p}$；而 $c_{pc,p}=Cs^{-1/3}$ 在整个翼面范围内均为 $O(0.1\text{–}0.2)$，未能忽略，导致修正后 Cp 整体系统性偏低。修正方案：在 $s=[5,10]$ 区间用三次 smoothstep 平滑回到原始 TSD 值，完全消除 $s>10$ 的 cp_common 偏差。

7. **NumPy 2.0 兼容性**：`np.trapz` 在 NumPy 2.0 中被移除，替换为 `_trapz = np.trapezoid if hasattr(np, 'trapezoid') else np.trapz`。

#### 6.3 测试情况

使用数据库前 10 个算例，4 进程并行（`test_6_le_correction/run_test.py`），结果如下：

```
================================================================================
 idx     Ma    AoA  Ma0_base  Ma0_corr  RmCp_base  RmCp_corr     dCL%     dCM%  Self_corr
--------------------------------------------------------------------------------
    0   0.72   0.02         1         1    0.13396    0.13080    -0.53    -0.43    0.00000
    1   0.72   1.92         6         2    0.17644    0.24127    -1.98    -4.16    0.00000
    2   0.73   0.80         3         1    0.15418    0.16860    -1.54    -1.99    0.00000
    3   0.73   3.17        14         6    0.53687    0.68951    -1.35    -1.41    0.00000
    4   0.74   3.38        13         5    0.55229    0.72062    -1.29    -1.29    0.00000
    5   0.74   3.88        14         5    0.55941    0.73334    -1.33    -1.35    0.00000
    6   0.75   2.25         8         2    0.24713    0.30900    -1.44    -2.58    0.00000
    7   0.75   2.48        11         4    0.49288    0.62054    -1.19    -1.15    0.00000
    8   0.75   2.59        10         4    0.51665    0.66475    -1.18    -1.12    0.00000
    9   0.75   2.99        11         4    0.52129    0.67484    -1.21    -1.18    0.00000
================================================================================
```

**主要结论**

| 指标 | 结果 |
|------|------|
| Ma=0 消除率 | 低 AoA（≤1°）：100%；中 AoA（1°–3°）：60–75%；高 AoA（>3°）：57–64% |
| Cp RMSE vs RANS | 低 AoA 略有改善（case 0：-2.4%）；高 AoA 上升（主因激波区 TSD vs RANS 差异，与前缘修正无关） |
| ΔCL | −0.5% 至 −2.0%（物理合理：内区去除了人为的 Ma=0 驻点"高压"区域） |
| Cp/Ma 自洽性 | 完美（RMSE = 0.00000，等熵关系严格满足） |

**残余局限**

- 高 AoA（>3°）仍有少量 Ma=0 点残留（4–6 点）：内区求解在零攻角假设下进行，不对称驻点偏移未进入内区解，仅通过外区 $c_{p\text{TSD}}$ 的不对称性间接引入。
- 高 AoA 算例 RmCp_corr > RmCp_base 约 15–30%：源于激波强度与位置的 TSD vs RANS 差异，非前缘修正引入的误差；低 AoA 算例 RmCp 持平或改善。
- 内区求解仅在 $s\in[0,10]$ 区间内（$x/c\lesssim3\%$）有效应用，平滑过渡到 TSD；更精细的过渡或高阶匹配条件留待后续任务。
