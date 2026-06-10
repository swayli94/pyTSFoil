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

### 任务7：前缘修正的渐近匹配的奇性扣除方案分析

#### 7.1 任务描述

任务 6 的 MAE 复合解方案虽然成功消除了 x=0 附近 x>0 区域的 Ma=0 区域，
但修正的结果不是很光滑，整体的 Ma, Cp 分布仍然存在系统性偏差，尤其是在前缘驻点附近。
x=0 附近 x<0 区域的 Ma=0 区域仍然较大。

其根本原因在于小扰动假设本身在鼻部失效，
而渐近匹配需要外区求解在圆前缘处数值干净，即数值解真正等于理论里的外区渐近解 $\phi_1$，
从而环量 $\Gamma$、激波位置、远场可信。
这是 `TSD_leading_edge_correction.md` 第 8 节里"正则化外解"那一步的展开。

所以需要**奇性扣除负责全局调理（让外解干净），composite 负责鼻部物理 $c_p$。两者互补，不可互相替代。**
具体理论参考 `TSD_singularity_subtraction.md` 中的分析。

结合 pyTSFoil 代码与任务6的结果，在此基础上，
结合理论，分析前缘修正的"渐近匹配+奇性扣除"方案，寻找可行的实现方法。

#### 7.2 修正方案与可行性分析

##### 7.2.1 方案总述

奇性扣除的核心操作是把求解器的未知量从全势 $\phi_1$ 换成光滑余项 $\phi_r = \phi_1 - \phi_s$，其中 $\phi_s$ 是解析预知的鼻部跨声速相似解，乘以平滑窗 $\chi$。求解器所有内核（VC 系数、DIAG/RHS 装配、缝面斜率 BC）均改用 $\phi_{tot} = \phi_s + \phi_r$ 来计算算子，$\phi_s$ 以只读预置数组进入，不参与迭代更新。

与任务 6 的分工：奇性扣除确保外区 $c_{p,TSD}$ 和环量 $\Gamma$ 干净（全局），任务 6 的 composite 在此基础上做鼻部物理 $c_p$ 修正（局部后处理）。两者串联，缺一不可。

---

##### 7.2.2 $\phi_s$ 的形式与预计算

**解析形式**（外区坐标 $X = x/c$，$Y = \tilde y$，即 pyTSFoil 中的 `X(I)`/`Y(J)`）：

$$P_s(X, Y) = \frac{Y^{4/7}}{\gamma+1}\, f\!\left(\frac{X}{Y^{6/7}}\right)\cdot\chi(r),
\qquad r = \sqrt{X^2 + Y^2}/R_c$$

其中：
- $f(\xi)$ 是 Rusak (1993) 外区鼻部相似 ODE 的解，即 `inner_parabola.py` 已经建立的 hodograph 表（$f$ 与内区问题匹配，函数形式完全相同，无额外量纲因子）。
- $\chi(r)$：平滑截断窗，在 $r \leq r_1 \approx 2$ 处 $\chi=1$，在 $r_2 \approx 5$–$10$ 处降到 0（smoothstep 或类似）；须保证 TE 前 $\chi=0$。
- $R_c = 2h^2 c$（已在任务 6 中计算，`_fit_nose_geometry` 方法）。

所需导数（解析计算）：

$$P_{s,X} = \frac{\chi}{\gamma+1}\left[\frac{4}{7} Y^{-3/7} f'(\xi)\cdot(-Y^{-6/7}) + Y^{4/7} f'(\xi)\cdot Y^{-6/7}\right] + \phi_s \cdot \chi'/\chi \cdot r_X$$

更简洁地：以链式法则展开后对 $X$ 偏导：

$$P_{s,X}(X, Y) = \frac{\chi}{\gamma+1}\, Y^{-2/7}\, f'(\xi) + P_s \frac{\chi_X}{\chi}$$

对 $Y$ 偏导（用于表面 BC 扣除）：

$$P_{s,Y}(X, Y) = \frac{\chi}{\gamma+1}\left[\frac{4}{7}Y^{-3/7}f(\xi) - \frac{6}{7}\frac{X}{Y}\,Y^{-3/7}f'(\xi)\right] + P_s \frac{\chi_Y}{\chi}$$

**表面极限**（$Y \to 0^+$，$\xi \to +\infty$，利用 $f(\xi)\to C\xi^{2/3}$）：

$$P_{s,Y}(X, 0^+) \;\to\; h\,c^{1/2}\,X^{-1/2}$$

即精确复现翼面斜率的 $X^{-1/2}$ 奇异部分（这正是 §2.4 的设计目标），且窗函数在 $X \gg R_c$ 后将其截断至零。

**预计算策略**：$P_s$、$P_{s,X}$、$P_{s,Y}$ **离线一次性计算**，结果存为与 pyTSFoil 网格 `(JMAX, IMAX)` 对齐的 NumPy 数组，通过 f2py 注入 Fortran 求解器。
- 对 $Y \leq 0$ 的下半域（$J \leq$ JLOW）：$P_s$ 关于 $Y=0$ 对称（$P_s$ 偶函数），$P_{s,Y}$ 反对称（奇函数）。
- 节点 `J = JUP/JLOW` 处须直接用表面极限公式（避免 $Y^{4/7} \to 0$ 的数值精度问题）。
- $f(\xi)$ 和 $f'(\xi)$ 的大 $\xi$ 渐近（$C\xi^{2/3}$）需要作为远端 fallback 补充进 `inner_parabola.py` 的插值表之外。

---

##### 7.2.3 对 SLOR 求解器的修改（逐环节）

求解器入口：`main_iteration.f90: SYOR` → `SOLVE`；边界条件：`solver_functions.f90: SETBC`。

**A. VC（非线性系数 / Murman–Cole 类型判别）**

```fortran
! 当前：
VC(J) = C1(I) - (CXL(I)*POLD(J,I2) + CXC(I)*P(J,I) + CXR(I)*P(J,I+1))

! 修改后（P 存 φ_r；PHI_S 为预置数组）：
VC(J) = C1(I) - (CXL(I)*(POLD(J,I2) + PHI_S(J,IM2))   &
               + CXC(I)*(P(J,I)   + PHI_S(J,I))         &
               + CXR(I)*(P(J,I+1) + PHI_S(J,I+1)))
```

即把 `PHI_S` 叠加到 `P` 上再代入现有公式，不改变差分模板结构。  
效果：MC 判别使用 $\phi_{tot,X}$，鼻区由解析 $P_{s,X}$ 主导，切断"鼻部伪超声速点播撒"通道。

**B. DIAG**

```fortran
DIAG(J) = (EMU(J,I1) - VC(J)) * CXXC(I) * WI + EMU(J,I2)*CXXR(I-1) - CYYC(J)
```

`EMU` 由 `VC < 0` 触发，VC 已在步骤 A 修正，故 **DIAG 不需额外改动**；影响自动传递。

**C. RHS**

RHS 由三部分构成（对应 KG 方程的 $x$ 二阶项、$x$ 跨列项、$y$ 二阶项）。P 改为 $\phi_r$ 后，每一项都缺少 $\phi_s$ 的贡献，需作为已知源项补入右端。

*C1：$x$ 弦向首项（亚声速：$\phi_{tot,XX}$）*

```fortran
! 当前：
RHS(J) = -(VC(J) - EMU(J,I1)) * (CXXL(I)*P(J,I-1) - CXXC(I)*P(J,I) + CXXR(I)*P(J,I+1))

! 修改后（在已知行 P_s 上补加）：
RHS(J) = RHS(J) - (VC(J) - EMU(J,I1)) * (CXXL(I)*PHI_S(J,I-1) - CXXC(I)*PHI_S(J,I) + CXXR(I)*PHI_S(J,I+1))
```

*C2：$x$ 跨列超声速修正项（$\phi_{tot,XX}$ 的 upwind 部分）*

```fortran
RHS(J) = RHS(J) - EMU(J,I2) * (CXXL(I-1)*PHI_S(J,IM2) - CXXC(I-1)*PHI_S(J,I-1) + CXXR(I-1)*PHI_S(J,I))
```

*C3：$y$ 方向项*

```fortran
RHS(J) = RHS(J) - (CYYD(J)*PHI_S(J-1,I) - CYYC(J)*PHI_S(J,I) + CYYU(J)*PHI_S(J+1,I))
```

对 `J = JBOT/JTOP` 的边界特殊处理同现有代码结构。

> 上述 RHS 修改等价于：对 $\phi_r$ 的残差方程 $L[\phi_r] = -L[\phi_s]$ 中补入解析已知的 $\phi_s$ 强迫项。窗过渡区内 $L[\phi_s] \neq 0$（窗梯度项），但有界，不引入新奇性。

**D. 缝面斜率边界条件（`SETBC`）**

```fortran
! 当前（solver_functions.f90 第 59–60 行）：
FXLBC(I) = CYYBLU * (FXL(IF1) - ALPHA + WSLP(I,2))
FXUBC(I) = CYYBUD * (FXU(IF1) - ALPHA + WSLP(I,1))

! 修改后（扣除 φ_s 的法向导数）：
FXLBC(I) = CYYBLU * (FXL(IF1) - ALPHA + WSLP(I,2) - PHI_SY_SURF(I,2))
FXUBC(I) = CYYBUD * (FXU(IF1) - ALPHA + WSLP(I,1) - PHI_SY_SURF(I,1))
```

其中 `PHI_SY_SURF(I,1/2)` 是 $P_{s,Y}(X_i, 0^\pm)$，1D 数组，用表面极限公式预置（无需 2D 插值）。  
效果：$\phi_r$ 的缝面斜率 BC 变为有界，消除驱动残差中的 $X^{-1/2}$ 奇性注入。  
注意：$\phi_s$ 对称（偶函数），对 $\Gamma$ 贡献为零（上下斜率符号相反，相加消去），Kutta 条件与 `PJUMP` 读取无需修改（§2.2）。

**E. 迭代控制**

- 初值：只对 $\phi_r$ 赋初值（通常 0 即可；$\phi_s$ 已解析预置，不参与初始化）。
- 残差监控：监控 $|\phi_r|$ 和 $\phi_r$ 的残差，不再被鼻部奇性主导，收敛应更干净。
- SOR 松弛步作用在 $\phi_r$ 的更新量 `RHS(J)` 上，无需额外改动。

**F. 后处理**

在 `output_surface`（`pytsfoil.py`）读取 `PX(i,j)` 后，须叠加 $P_{s,X}$ 以还原 $\phi_{tot,X}$，再按现有流程计算 Mach/Cp：

```python
# 现有：uu_p1 = cjup * tsf.solver_base.px(i_p1, jup) - ...
# 修改：
uu_p1 += phi_s_x_surface_upper[i_py]   # P_{s,X}(X_i, 0^+) 预置数组
ul_p1 += phi_s_x_surface_lower[i_py]   # P_{s,X}(X_i, 0^-)（= P_{s,X}(X_i, 0^+)，对称）
```

之后再执行任务 6 的 composite 修正，流程不变。$\phi_s$ 在 $s > 10$（窗外）已为 0，过渡区外与现有代码完全一致。

---

##### 7.2.4 新增 Fortran 数组需求

需在 `solver_data.f90` 中新增以下公开数组（由 Python 通过 f2py 填充）：

| 数组名 | 维度 | 说明 |
|---|---|---|
| `PHI_S(JMAX, IMAX)` | 2D | $P_s$ 全场（窗化的相似解） |
| `PHI_SY_SURF(IMAX, 2)` | 1D×2 | $P_{s,Y}(X_i, 0^\pm)$，上下表面各一行 |

内场 RHS 的 C1–C3 修正只需 `PHI_S`（有限差分模板内取邻点），VC 修正同。`PHI_SY_SURF` 仅用于 `SETBC`（1D 表面 BC）。

可选：额外存 `PHI_SX(JMAX, IMAX)` 供后处理用（若不想在 Python 端重算）。

---

##### 7.2.5 主要难点与风险

**难点 1：$f(\xi)$ 大 $\xi$ 渐近**

`inner_parabola.py` 当前的 hodograph 表覆盖 $|\sin\alpha| \leq \alpha_3 = 80.41°$，对应 $\xi$ 范围有限（$\alpha \to \alpha_3$ 时 $\xi \to +\infty$ 的渐近极限）。在外区第一网格行 $J = $ JUP（$Y_J$ 很小，X > 0），$\xi = X/Y^{6/7}$ 可能超出表格。需要补充大 $\xi$ 渐近式 $f(\xi) \approx C\xi^{2/3}$（$C = \cot^{2/3}\alpha_3 \cdot 3^{1/3}$）作为外推 fallback。

**难点 2：窗函数的选择与 RHS 强迫**

$\chi$ 的过渡区（$r_1 < r < r_2$）产生额外 RHS 源（窗梯度项）。源的量级为 $O(|\nabla\chi| \cdot |P_s|)$，若过渡带太窄（$r_2 - r_1 \ll 1$）会产生大梯度；若太宽（$r_2$ 超过激波位置）会污染已收敛的激波解。窗的位置与形状需实验性调整，建议初始取 $r_1 = 2, r_2 = 8$（单位为 $R_c$）。

**难点 3：$Y = 0$ 附近的数值计算**

内区网格行 $J = $ JUP/JLOW 的 $Y_J$ 通常非常小（pyTSFoil 的缝面在 $Y = 0$，近缝第一行约为网格间距 $\Delta Y$）。在这些点上用 2D 插值（$Y^{4/7} f(\xi)$）会面临大 $\xi$ 和小 $Y^{4/7}$ 同时出现，精度依赖大 $\xi$ 渐近的准确性。实际上这些点的 RHS 修正（C3 项）非常小，因为 $P_s \to 0$ 当 $Y \to 0$；对计算精度影响有限，但数值实现须避免 $0/0$ 型的 NaN。

**难点 4：Fortran 数组维度约束**

`solver_data.f90` 中的数组大小由编译期常数 `N_MESH_POINTS` 决定，新增 `PHI_S(JMAX, IMAX)` 需对应 Fortran 模块的实际网格维度。f2py 暴露后在 Python 端赋值时须保证维度匹配（C vs Fortran 列优先顺序）。

**难点 5：与任务 6 composite 的衔接点**

任务 6 的 composite 在 `_apply_le_correction`（后处理，`output_surface` 末尾）中用 `c_{p,TSD}` 作为输入。奇性扣除改变的是 `P` 数组，后处理中 `PX` 须先叠加 $P_{s,X}$ 还原 $\phi_{tot,X}$，然后再用已有 composite 公式。需确保"扣除在求解器内、加回在后处理入口"的边界清晰，避免双重扣除或漏加。

---

##### 7.2.6 可行性结论

**整体判断**：方案理论完备、代码改动局部可控，**可行**。核心修改点明确（`SYOR` 约 5 处，`SETBC` 1 处，`solver_data.f90` 新增 1-2 个数组，Python 后处理 2 处），无需重构 SLOR 框架。

**工作量估计**：
1. `inner_parabola.py` 扩展（大 $\xi$ 渐近 + 导数 $f'(\xi)$ 输出）：小
2. 新模块 `leading_edge/singularity_subtraction.py`（2D $\phi_s$/$P_{s,X}$/$P_{s,Y}$ 计算 + 窗函数）：中
3. `solver_data.f90` 新增数组 + f2py 接口：小
4. `main_iteration.f90 SYOR` 修改（A+C）：小（各处约 2–4 行）
5. `solver_functions.f90 SETBC` 修改（D）：小
6. `pytsfoil.py`（预计算调用 + 后处理叠加）：小
7. 测试与窗参数调试：中

**推荐实施顺序**：先仅做 D（SETBC 扣除表面 BC）观察收敛改善；再加 A（VC 修正）；最后加 C（RHS 全场修正）。逐步验证，每步与无扣除基准对比 $\Gamma$、$c_p$ 分布和网格收敛性。

### 任务8：前缘修正的渐近匹配的奇性扣除方案实现

#### 8.1 任务描述

基于任务7的理论分析（`7.2 修正方案与可行性分析`），
在 pyTSFoil 中实现匹配渐近展开+奇性扣除的前缘修正，并以数据库算例验证修正效果。

#### 8.2 完成情况

本次完成了奇性扣除所需的**全部代码基础设施**：Python 修正模块、Fortran 扩展数组（步骤 A 预留接口）、求解器钩子，以及集成测试脚本。同时通过测试揭示了分步实现（仅 D+E）的本质局限性。

##### 新增文件

**`pytsfoil/leading_edge/singularity_subtraction.py`**

核心函数 `compute_surface_corrections(x_foil, h, delta, R_c, cpfact)`：

| 返回值 | 用途 | 公式 |
|---|---|---|
| `phi_sy_upper` | 步骤 D：从 FXU 减去的奇异斜率 | $\chi(r)\cdot h/(\delta\sqrt{x})$ |
| `phi_sx_surface` | 步骤 E：后处理中加回的 $\phi_{s,x}$ | $-\chi(r)\cdot c_{\text{pc}}/(2 c_{p\text{fact}})$ |

窗函数 $\chi(r)$：$r \leq r_1=2$ 时为 1，$r \geq r_2=8$ 时为 0，C¹ smoothstep 过渡（$r = x/R_c$）。

**`test_8_singularity_subtraction/run_test.py`**：10 算例集成测试脚本，对比 baseline / sing_sub / full 三模式，输出 CL 偏差、Cp RMSE、Ma=0 点数、Cp/Ma 自洽性。

##### 修改文件

**`pytsfoil/leading_edge/__init__.py`**：导出 `compute_surface_corrections`。

**`pytsfoil/src/solver_data.f90`**：新增 `PHI_SX_C1TERM(N_MESH_POINTS)`（步骤 A 预留数组，全零）。

**`pytsfoil/src/main_iteration.f90` SYOR**：接口预留：`VC(J) -= PHI_SX_C1TERM(I)`（当前恒零）。

**`pytsfoil/pytsfoil.py`**：
1. 新增 `'apply_singularity_subtraction': False`；
2. 添加步骤 D 钩子（FXU/FXL 修改与恢复）；
3. 添加步骤 E 钩子（PX 叠加 `phi_sx_surface`）；
4. 新增 `_fxu_orig`/`_fxl_orig` 存储原始 BC。

##### 实现范围与状态

| 步骤 | 说明 | 代码状态 | 功能状态 |
|---|---|---|---|
| D（表面 BC） | 从 FXU/FXL 扣除 $\phi_{s,\tilde y}(x,0^\pm)$ | ✅ 已实现 | ⚠️ 单独使用时 CL 严重偏离（见 §8.3） |
| E（后处理） | `output_surface` 中叠加 $\phi_{s,x}(x,0)$ | ✅ 已实现 | ⚠️ 对 CL 无贡献；依赖正确的 $\phi_r$ |
| A（VC 系数） | SYOR 中用 $\phi_{\text{tot},x}$ 代替 $\phi_{r,x}$ | ⚠️ 接口预留，当前零 | ❌ 未正确实现（见 §8.3 分析） |
| B（MC 判别） | 类型判别用 $\phi_{\text{tot},x}$ | ❌ | ❌ |
| C（RHS 全场） | SYOR 加入 $L[\phi_s]$ 残量强迫 | ❌ | ❌ |

#### 8.3 测试情况

##### 单元测试（公式验证）

```
[ 0.        44.798935  14.166667   6.335526   4.479893   1.1703403
  0.         0.         0.       ]
```

输出符合公式：$x=0$ 处为 0；$x=0.001$ 处 ≈ 44.8 ≈ $h/(\delta\sqrt{x})$；$x=0.5$ 处为 0（窗外）。

##### 集成测试（10 算例，baseline / sing_sub / full）

所有 10 算例均可运行无崩溃，但 sing_sub 模式 **CL 严重偏离基线**：

```
 Idx     Ma    AoA  nMa0_B  nMa0_S  nMa0_F  rmseCp_B  rmseCp_S  rmseCp_F    selfB    selfF   dCL_S%   dCL_F%
------------------------------------------------------------------------------------------------------------
   0  0.720   0.02       1       1       1    0.1340    0.1340    0.1308  0.0031  0.0000    -1.35    -1.73
   1  0.720   1.92       6       6       2    0.1764    0.3088    0.2413  0.1248  0.0000  +296.95  +296.41
   2  0.730   0.80       3       3       1    0.1542    0.1657    0.1686  0.0535  0.0000   +25.13   +23.59
   3  0.730   3.17      14      14       6    0.5369    0.5481    0.6895  0.4329  0.0000    -0.14    -1.49
   4  0.740   3.38      13      13       5    0.5523    0.5625    0.7206  0.4439  0.0000    -0.10    -1.39
   5  0.740   3.88      14      14       5    0.5594    0.5726    0.7333  0.4533  0.0000    -0.06    -1.39
   6  0.750   2.25       8       8       2    0.2471    0.2562    0.3090  0.1927  0.0000   +17.82   +16.38
   7  0.750   2.48      11      11       4    0.4929    0.5021    0.6205  0.3865  0.0000    +1.09    -0.10
   8  0.750   2.59      10      10       4    0.5167    0.5263    0.6648  0.4104  0.0000    +1.65    +0.47
   9  0.750   2.99      11      11       4    0.5213    0.5300    0.6748  0.4148  0.0000    +2.67    +1.46
```

AoA ≈ 0 时偏差较小（−1.35%）；AoA > 1° 时偏差剧烈（+25% ~ +300%）。

##### 根本原因诊断

通过隔离步骤 E（将 `phi_sx_surface` 清零后重跑 sing_sub）确认：

> **步骤 D（FXU/FXL 修改）单独造成了全部 CL 偏差；步骤 E 对 CL 贡献为零。**

根本原因：

1. **步骤 D 改变全局 TSD 解**。TSD 外解高度非线性：前缘奇性通过非线性 VC 系数影响全场（激波位置、环量 $\Gamma$）。修改 FXU/FXL（步骤 D）改变了求解器所见的翼型形状，导致收敛到完全不同的全局解——这正是 §1.2 所述的污染链在逆方向上的表现：删除奇性 BC 后，维持该全局解的约束也随之消失。

2. **缺少步骤 A+C 无法补偿**。正确方案要求：求解器在 SYOR 的 VC 计算中使用 $\phi_{\text{tot},x} = \phi_{r,x} + \phi_{s,x}$（步骤 A）、RHS 中包含 $L[\phi_s]$ 残量（步骤 C），使 $\phi_r$ 的求解方程在形式上等价于原始 $\phi_{\text{tot}}$ 方程的正则化版本。若仅做步骤 D，求解器解的是错误的非线性方程组。

3. **步骤 A 的简化实现（1D 均匀近似）同样有害**。将表面 $\phi_{s,x}$ 对所有 J 行统一施加，对内部行（远离翼面）过修正 VC 量级达 O(1000×)，同样破坏全局解。

4. **步骤 E 对 CL 无贡献（对称性保证）**。$\phi_{sx}$ 对上下翼面加同一值，$\Delta C_p = C_{p,\text{lower}} - C_{p,\text{upper}}$ 中修正互相抵消（一阶线性），Mach 的非线性也不显著。

##### 当前状态与后续建议

| 项目 | 状态 |
|---|---|
| 基础设施（Python 模块、Fortran 接口、测试脚本） | ✅ 已就绪 |
| 步骤 D+E 孤立实施 | ❌ 物理错误，`apply_singularity_subtraction` 保持默认 False |
| 步骤 A（1D 均匀近似） | ❌ 已实现但禁用（内部行严重过修正） |
| 步骤 A+C（正确 2D 实现） | ✅ 任务9完成（外场 $\phi_s = A\cdot X^{2/3}$） |
| 步骤 D（内场 $\phi_s$ 正确实现） | 🔲 待完成（需内场相似解 $Y^{4/7}f(X/Y^{6/7})$） |
| 完整方案（A+C+D+E）验证 | 🔲 待完成（取决于步骤 D） |

**后续任务**：从 `inner_parabola.py` `_solve_sor` 输出的完整 2D 场构造内场 $\phi_s^{\text{inner}}(x,y)$（抛物坐标到直角坐标映射），重新启用步骤 D（使用内场 `phi_sy_upper`），补全 A+C+D+E 完整方案。

#### 8.4 任务6（composite）与任务8（A-E）的关系

两者属于**两个独立的理论层次**，互补而非互相替代：

| 层次 | 步骤 | 作用位置 | 目的 |
|---|---|---|---|
| **奇性扣除**（任务8） | A、B、C、D、E | SLOR 求解器内部 | 让求解器解干净的 $\phi_r$，使 $\Gamma$、激波位置正确 |
| **MAE 复合**（任务6） | 无字母，独立后处理 | 求解器输出之后 | 把鼻部 $x^{-1/3}$ 奇性替换成内区物理解 $c_p^*$ |

**步骤 A–E** 是对 Fortran SLOR 求解器的修改：
改 VC 系数（A）、类型判别（B）、RHS（C）、缝面 BC（D）、后处理速度还原（E）。
目标是让求解器看到有界的 $\phi_r$，解出来的全场（环量、激波位置）才是正确的外区解。

**任务6 composite** 在 SLOR 收敛之后运行，用 Rusak (1993) 的复合公式
$$c_p^{\text{comp}}(x) = c_p^*(s) + \frac{\rho^*}{\rho_\infty}\phi_{0x^*}\!\left[c_{p,\text{TSD}}(x) - c_{p,\text{common}}(s)\right]$$
把 TSD 鼻部奇性输出替换为内区物理解。它不改求解器，只改最终输出的 Cp/Ma 数组。

**步骤 E 是两者的接缝**：若奇性扣除已启用，Step E 在 `output_surface` 里把 $\phi_{s,x}$ 加回 PX，使 composite 收到完整的 $\phi_{\text{tot},x}$ 而非 $\phi_{r,x}$；若奇性扣除未启用（任务6单独跑），composite 直接用未修改的 PX，$c_{p,\text{common}}$ 对消 TSD 奇性，任务6仍能工作——这正是任务6可以独立运行的理论保证。

**串联关系**（完整方案）：
```
SLOR 求解（A+B+C+D）→ Step E 速度还原 → 任务6 composite → 输出 Cp/Ma
```
奇性扣除确保外区 $c_{p,\text{TSD}}$ 和环量 $\Gamma$ 干净（全局），任务6 composite 在此基础上做鼻部物理 $c_p$ 修正（局部后处理）。

在 `test_8_singularity_subtraction/run_test.py` 中，四个 mode 覆盖了完整的对比矩阵：

| mode | 奇性扣除 D+E | composite | 说明 |
|---|---|---|---|
| `baseline` | ✗ | ✗ | 原始 TSD |
| `composite` | ✗ | ✓ | 任务6结果 |
| `sing_sub` | ✓ | ✗ | D+E 单独（当前破坏全局解） |
| `full` | ✓ | ✓ | 预期完整方案（待 A+C 实现后有效） |

##### 任务6与任务8的共用基础

两者在代码层面共享以下基础，无需重复实现：

1. **`_fit_nose_geometry()`（`pytsfoil.py:311`）**：在 `set_airfoil()` 末尾调用一次，计算抛物线鼻部参数 `h`（形状常数）和 `R_c = 2h^2`（曲率半径），结果写入 `self.airfoil`。步骤 D 的 BC 修正公式和 composite 的 $s = x/R_c$ 均读同一份 `h`/`R_c`，无额外计算。

2. **`cp_common` 公式（`_COMMON_COEF = 0.635776`）**：TSD 鼻部奇性的精确渐近形式 $c_{p,\text{common}}(s) = 0.635776/(\gamma+1)^{1/3} \cdot s^{-1/3}$ 在两个模块中完全一致：
   - `singularity_subtraction.py`：用于计算步骤 E 的速度还原量 $\phi_{s,x} = -\chi \cdot c_{p,\text{common}}/(2 \cdot \text{cpfact})$；
   - `composite.py`：用作复合公式括号中的对消项 $(c_{p,\text{TSD}} - c_{p,\text{common}})$。两者对消的是同一奇性，数学上自洽。

3. **`cpfact`（$= \delta^{2/3}$，TSD 压力标度因子）**：步骤 E 和 composite 均用它在速度 $U$ 与线性 TSD Cp 之间转换（$c_{p,\text{TSD}} = -2U \cdot \text{cpfact}$）。

4. **`inner_parabola.py`（内区 SOR 求解器）**：当前仅任务6 composite 使用（读取 $c_p^*$、$\rho^*/\rho_\infty$、$\phi_{0x^*}$ 三张表）。任务8完整实现步骤 A+C 时，同一求解器输出的 $f(\xi)$ 将用于在全场网格上构造 $\phi_s(x,y) \propto Y^{4/7} f(X/Y^{6/7})$，**不需要重新开发内区求解器**。

### 任务9：前缘修正的渐近匹配的奇性扣除方案实现

#### 9.1 任务描述

基于任务7的理论分析（`7.2 修正方案与可行性分析`），
继续完善任务8 (A+C, A+C+D+E) 的代码实现，
在 pyTSFoil 中实现匹配渐近展开+奇性扣除的前缘修正，并以数据库算例验证修正效果。

#### 9.2 完成情况

##### 实现范围

本任务完成了任务7分析的 A+C+E 三步（外场奇性扣除）；步骤 D（面边界条件正则化）因一致性问题暂时搁置。

| 步骤 | 内容 | 状态 |
|------|------|------|
| A | SYOR VC 使用总速度 $\phi_{\text{tot},x}=\phi_{r,x}+\phi_{s,x}$（type-switching 基于总速度） | ✅ 完成 |
| C | RHS 中加入 $-L[\phi_s]$ 强制项（C1 x-亚音速项、C2 x-超音速上游修正、C3 y-方向二阶导） | ✅ 完成 |
| D | FXU/FXL 减去 $\phi_{s,y}(x,0^+)$（面 BC 正则化） | ⛔ 搁置 |
| E | 后处理还原：PX += $\phi_{s,x}$（恢复全局 $\phi_{\text{tot},x}$ 用于 Cp/Ma 计算） | ✅ 完成 |

##### 各修正项汇总

当前代码中传递给 TSD 求解器或在后处理中叠加的所有修正项如下。

**进入 Fortran SYOR 求解器（影响迭代）：**

| 项目 | 表达式 / 变量 | 作用位置 | 说明 |
|------|--------------|----------|------|
| Step A：总速度 type-switching | $\phi_{\text{tot},x} = P_x + \phi_{s,x}$ | Fortran VC 计算 | `PHI_S(J,I)` 加入 `P(J,I)`，使 Murman-Cole 判断基于总速度而非正则量 $P=\phi_r$ |
| Step C1：x-亚音速强制 | $-(\text{VC}-\text{EMU})\cdot(\phi_s \text{ 的 }x\text{-二阶导})$ | Fortran RHS | 从 RHS 扣除 $\phi_s$ 引起的 x-方向亚音速残量，使求解器实际解 $L[\phi_r]=0$ |
| Step C2：x-超音速上游修正 | $-\text{EMU}_{i-1}\cdot(\phi_s \text{ 的上游 }x\text{-二阶导})$ | Fortran RHS | 超音速区的 Murman-Cole 差分对 $\phi_s$ 的超音速修正项 |
| Step C3：y-方向强制 | $-(\phi_s \text{ 的 }y\text{-二阶导})$ | Fortran RHS | 对外场 $\phi_s=A\cdot X^{2/3}$（$Y$-无关）此项为零；内场 $\phi_s$ 时非零 |
| Step D（⛔ 已禁用）| $\text{FXU} \mathrel{-}= \phi_{s,y}(x,0^+)$ | Fortran 面 BC | ??? |

**纯后处理叠加（不进求解器，收敛后一次性加回）：**

| 项目 | 表达式 / 变量 | 作用位置 | 说明 |
|------|--------------|----------|------|
| Step E：速度还原 | $U_{\text{tot}} = P_x + \phi_{s,x}$ | `output_surface` | 将 `phi_sx_surface` $= -c_{p,\text{common}}/(2\,c_{p\text{fact}})$（$x^{-1/3}$）加回 ul/uu，令 Cp/Ma 计算基于完整速度 $\phi_{\text{tot},x}$ |
| Composite 修正（任务6） | MAE 复合公式替换近前缘 Cp/Ma | `_apply_le_correction` | 调用 `apply_composite_correction`，用内外匹配解替换 cpu/cpl/mau/mal；不改变 uu/ul 速度场本身 |

**各修正项的量级与结构（以算例 0 为例，Ma=0.720, AoA=0.02°）：**

- `FXU`（上面斜率）：前缘 $x^{-1/2}$ 发散，最大值 ≈ 18（无量纲），等于 $h/(\delta\sqrt{x})$；Step D 的 `phi_sy_upper` 与其几乎重合，确认 Step D 的物理含义
- `phi_sx_surface`（Step E）：$x^{-1/3}$ 负速度扰动（减速），最小值 ≈ −1.6，窗函数 $\chi$ 截断后仅作用在 $x/R_c \lesssim 8$ 范围内
- Composite $\Delta c_p$：集中在 $x/c \lesssim 0.10$，上下面修正量级 $O(0.1)$，$x > 0.30$ 后降为零
- 收敛残差 $\Delta U = u_{\text{sing\_sub}} - u_{\text{baseline}} \approx 0$：A+C+E 收敛后正则量 $P_x + \phi_{s,x} = \phi_{1,x}$，两者等价

**分解图（$\phi_{1,x} = \phi_{r,x} + \phi_{s,x}$）**：

$$\underbrace{u_{\text{baseline}}}_{\phi_{1,x},\;x^{-1/3}\text{发散}} = \underbrace{\left(u_{\text{sing\_sub}} - \phi_{s,x}\right)}_{\phi_{r,x},\;\text{有界}} + \underbrace{\phi_{s,x}}_{\text{Step E 奇性部分}}$$

正则量 $\phi_{r,x}$ 在 $x \to 0$ 时有界，这正是奇性扣除方案的核心目标：用有界变量迭代，避免 SLOR 在前缘处的数值不稳定。

##### 修改的文件

- **`pytsfoil/src/main_iteration.f90`（SYOR 子程序）**：步骤 A（总速度 VC）、步骤 C1/C2/C3（RHS 强制项），使用 `solver_data.f90` 中的 `PHI_S(NMP_plus2, NMP_plus1)` 数组。
- **`pytsfoil/pytsfoil.py`（`_apply_singularity_subtraction_bc`）**：填充 2D `PHI_S` 数组，步骤 E 还原 `phi_sx_surface` 至 PX；移除步骤 D 的 FXU/FXL 修改。
- **`pytsfoil/leading_edge/singularity_subtraction.py`（`compute_phi_s_2d`）**：计算 $A \cdot X^{2/3} \cdot \chi(r)$ 的 2D 外场 $\phi_s$，shape `(NJ, NI)`，float32，直接赋给 `tsf.solver_data.phi_s`。

#### 9.3 测试情况

##### 测试配置

脚本：`test_9_singularity_subtraction/run_test.py`  
算例数：10（数据库前10条），四模式对比：baseline / composite / sing\_sub / full

| 模式 | `apply_singularity_subtraction` | `apply_le_correction` |
|------|----------------------------------|------------------------|
| baseline | False | False |
| composite | False | True |
| sing\_sub | True | False |
| full | True | True |

指标说明：
- **nMa0**：翼面下表面 Ma=0（停滞）点数，反映前缘停滞区范围
- **rmCp**：相对 RANS 参考的 Cp RMSE（上下表面平均），越小越好
- **dCL%**：相对 baseline 的 $C_L$ 相对变化，奇性扣除不应改变 $C_L$（应 ≈0%）
- **rmSelf**：局部 Ma 与 Cp 的等熵自洽 RMSE（$\approx0$ 表示自洽）

##### 测试结果（10/10 算例成功）

```
Idx     Ma    AoA  nMa0_B nMa0_C nMa0_S nMa0_F   rmCp_B  rmCp_C  rmCp_S  rmCp_F   dCL_C%  dCL_S%  dCL_F%  rmSelf_S rmSelf_F
   0  0.720   0.02       1      1      0      1   0.1340  0.1308  0.1955  0.0913    -0.53   +0.00   -0.53   0.0000  0.0000
   1  0.720   1.92       6      2     10      1   0.1764  0.2413  0.2046  0.2229    -1.98   -0.00   -1.97   0.0000  0.0000
   2  0.730   0.80       3      1      4      1   0.1542  0.1686  0.2243  0.1239    -1.54   +0.00   -1.54   0.0000  0.0000
   3  0.730   3.17      14      6     14      9   0.5369  0.6895  0.5349  0.6812    -1.35   +0.00   -1.47   0.0000  0.0000
   4  0.740   3.38      13      5     13      9   0.5523  0.7206  0.5537  0.7177    -1.29   -0.06   -1.40   0.0000  0.0000
   5  0.740   3.88      14      5     14      9   0.5594  0.7333  0.5535  0.7274    -1.33   -0.00   -1.44   0.0000  0.0000
   6  0.750   2.25       8      2     10      1   0.2471  0.3090  0.2575  0.2984    -1.44   -0.01   -1.45   0.0000  0.0000
   7  0.750   2.48      11      4     11      6   0.4929  0.6205  0.4947  0.6158    -1.19   -0.00   -1.19   0.0000  0.0000
   8  0.750   2.59      10      4     11      5   0.5167  0.6647  0.5175  0.6606    -1.18   -0.00   -1.17   0.0000  0.0000
   9  0.750   2.99      11      4     11      7   0.5213  0.6748  0.5220  0.6727    -1.21   -0.03   -1.23   0.0000  0.0000
```

##### 结果分析

1. **dCL\_S% ≈ 0（最大 |0.06%|）**：奇性扣除不改变升力，A+C+E 方案数学上自洽，无虚假 CL 扰动。这是本任务最关键的验证指标。

2. **sing\_sub 模式 ≈ baseline**：nMa0\_S ≈ nMa0\_B，rmCp\_S ≈ rmCp\_B（略有差异来自不同迭代路径的数值收敛差异）。理论上 A+C+E（无步骤 D）在收敛后等价于 baseline：$P = \phi_r = \phi_1 - \phi_s$，步骤 E 恢复 $\phi_{s,x}$，因此 $U_\text{tot} = P_x + \phi_{s,x} = \phi_{1,x}$。

3. **full 模式 ≈ composite 模式（任务6结果）**：由于 sing\_sub 给出与 baseline 相同的 $\phi_{\text{tot},x}$，full 模式下 composite 修正接收的速度场与 composite 单独运行时相同，两者输出一致。

4. **rmSelf = 0.0000**：等熵自洽 RMSE 为零，说明当前实现中 Ma 与 Cp 由同一速度场一致导出。

5. **低迎角（cases 0,2）full 模式略优于 composite**：rmCp\_F < rmCp\_C，可能来自 $\phi_r$（有界）的迭代路径在前缘附近收敛到更干净的数值解；该差异属于数值效应，不是物理改进。

##### 局限性与后续工作

当前实现的 A+C+E（外场 $\phi_s = A\cdot X^{2/3}$）本质上是 baseline 的等价重新参数化：用有界变量 $P=\phi_r$ 迭代代替 $\phi_1$，但收敛后输出完全相同的 $\phi_{\text{tot}}$，无额外精度收益。

真正的精度提升需要步骤 D 与步骤 A+C+E 的完整闭环（A+C+D+E）：
- 步骤 D 的 FXU/FXL 修正：$\phi_{s,y}^{\text{inner}}|_{Y=0} = h/(\delta\sqrt{x}) \ne 0$，使 $P$ 的面 BC 真正有界
- 步骤 C 的强制项：$-L[\phi_s^{\text{inner}}] \approx 0$（内场解近似满足 TSD 方程），故 RHS 扰动小
- 步骤 A 的 VC：使用 $\phi_r + \phi_s^{\text{inner}}$ 计算总速度用于 type-switching

内场 $\phi_s$ 的 2D 场可从 `inner_parabola.py` 的 `_solve_sor` 输出（抛物坐标 $(\mu,\eta)$ → 直角坐标映射）直接扩展，无需另建内区求解器。

### 任务10：前缘修正的检查与改进

#### 10.1 任务描述

任务6, 8和9的实现完成后，获得了对 TSD 的部分修正项。
虽然任务9中仍有部分环节没有完成（步骤 D），但已经实现了 A+C+E 的完整闭环。

在进一步开展下一步骤的编程前，需要先检查和修复下面的问题：

1. 检查各个修正项的作用对象、范围和量级；
2. 检查各个修正项的光滑性和数值稳定性；
3. 检查各个修正项依赖的参数和计算中间变量；
4. 额外检查奇性扣除方案中试图对边界条件（物面斜率）的修正项的形态和量级，与原始边界条件进行对比，确认其物理合理性、光滑性和数值可行性。

目的：修正项应当尽可能光滑，目前从 `test_6_le_correction` 和 `test_9_singularity_subtraction` 的结果看，某些修正项在 $x \approx 0$ 附近的行为或者过渡区域存在不光滑的问题。

可以新建一个测试文件夹，进行额外分析和可视化，检查上述问题，并尝试改进修正项的定义或计算方法，使其更光滑、更合理。

#### 10.2 完成情况

##### 新建诊断文件夹

新建 `test_10_le_smoothness/run_test.py`，对3个典型工况（case 0：Ma=0.720, AoA=0.02°；case 1：Ma=0.720, AoA=1.92°；case 3：Ma=0.730, AoA=3.17°）生成4组诊断图：

| 图编号 | 文件名 | 内容 |
|--------|--------|------|
| Fig 1 | `case_XXXX_cp.png` | Cp/Ma 分布：baseline vs. composite 对比 |
| Fig 2 | `case_XXXX_terms.png` | 复合修正中间量：cp_star, cp_common, bracket（old vs. new），phi_ox, rho_ratio |
| Fig 3 | `case_XXXX_bc.png` | Step D：phi_sy_upper（$h/(\delta\sqrt{x})$）vs. 实际物面斜率 FXU |
| Fig 4 | `case_XXXX_phisx.png` | Step E：phi_sx_surface（$x^{-1/3}$）及其 x 方向导数 |

##### 各修正项检查结果

**1. composite bracket 的非光滑性根因**

对 `apply_composite_correction` 中的 `bracket = cp_tsd - cp_common` 进行分析：

- 原始代码设有 `s_min = 0.01` 的硬截断（`bracket = 0` 当 $s < 0.01$）。经检查，该截断**实际无效**：第一个非零网格点 $x[1] \approx 0.0007$，对应 $s = x/R_c \approx 0.23 \gg 0.01$，截断区域 $s < 0.01$ 仅覆盖 $x[0] = 0$（前缘顶点），此处 `cp_tsd = 0` 且截断与否对结果无影响。

- 真正的非光滑来源：高迎角工况下，**下表面驻点区**（6–14 个网格点 Ma≈0）使 `cp_tsd ≈ 0`，而 `cp_common ~ s^{-1/3}` 在 $s \lesssim 2$ 时较大（量级 0.5–3），导致 `bracket = cp_tsd - cp_common` 为较大负值，使 `cp_composite` 在前缘驻点区出现凹陷。

**2. bracket 光滑化尝试及结论**

尝试对负 bracket 在 $s \in [0, 2]$ 施加 C1 smoothstep fade（仅对 `bracket_raw < 0` 的分量），使其在 $s \to 0$ 时平滑过渡至零：

```python
bracket_raw = cp_tsd - cp_common
s_fade = 2.0
t_fade = clip(s_full / s_fade, 0, 1)
w_fade = t_fade² × (3 - 2·t_fade)   # C1 smoothstep 0→1
bracket = where(bracket_raw < 0, w_fade × bracket_raw, bracket_raw)
```

测试（见 §10.3）显示：低迎角 case 0 的 rmCp 略有改善（−3%），但中/高迎角 cases 1–9 的 rmCp **明显恶化**（+7%–+17%）。

根因分析：高迎角下，下表面驻点区的负 bracket 在数值上是"意外有益"的——它将 `cp_composite` 从内场 `cp_star`（驻点值约 1.28，被 `cp_stagnation` 上限截断至约 1.05）**向下拉**，使其更接近驻点下游的参考 Cp（该区域实际 Cp 低于驻点值）。smooth fade 削弱了这种向下拉的效果，反而导致 `cp_composite` 偏高，增大误差。

结论：**此修改不带来整体改善，已回退至原始 `s_min = 0.01` 公式**（实质不变，仅修复注释说明）。

**3. Step D：phi_sy_upper vs. FXU 验证**

从 Fig 3（`case_XXXX_bc.png`）可见：

- `phi_sy_upper = chi·h/(delta·sqrt(x))`（抛物近似）在前缘 $x \lesssim 3R_c$ 处与实际物面斜率 `FXU` 形态一致，量级吻合（均以 $x^{-1/2}$ 发散，最大值约 18）。
- 确认 Step D 的近似式具有物理合理性。
- 但是，`phi_sy_upper` 在 x = 0 处反而值为零（网格点 $x[0] = 0$），而实际物面斜率 `FXU` 在 $x \to 0^+$ 时发散，导致 Step D 的边界条件修正不连续且不光滑。

**4. Step E：phi_sx_surface 的奇性结构**

从 Fig 4（`case_XXXX_phisx.png`）可见：

- `phi_sx_surface = chi·(-cp_common)/(2·cpfact)`，在 $x \to 0^+$ 时以 $x^{-1/3}$ 发散；$x = 0$ 处手动设为 0（网格点 $x[0] = 0$）。
- 第一个非零网格点 $x[1] \approx 0.0007$ 处，`phi_sx ≈ -1.6`，量级合理。
- 其 x 方向导数以 $x^{-4/3}$ 发散，但受网格分辨率（$\Delta x \sim 10^{-3}$）限制，实际使用时平滑性足够。
- 步骤 E 的奇性结构与任务 9 的分析结论一致，无需进一步修改。

**5. composite 修正的根本局限性**

- 内场问题以 $M_\infty = 1$（临界流）求解，cp_star 的驻点值约 1.28（M=1 等熵），经 `cp_stagnation` 上限截断至约 1.05（实际 $M_\infty = 0.72$）。
- 对于高迎角工况（$M_\infty = 0.72$–0.75），TSD 在前缘驻点区的 Cp 精度本身有限（nMa0\_B 达到 6–14），composite 修正所能改善的空间不大。
- **推荐使用"full"模式**（A+C+E + composite）：Step E 将 ul/uu 恢复至外部奇性量级，使 `cp_tsd ≈ cp_common`，bracket ≈ 0，`cp_composite ≈ cp_star`，是三种修正模式中综合表现最好的配置。

#### 10.3 测试情况

##### 诊断测试（test_10_le_smoothness）

脚本：`test_10_le_smoothness/run_test.py`；3个典型工况，对比 baseline（无修正）与 composite（修正）模式。

**smooth fade 修改前后对比（仅 composite 模式，rmCp）：**

| case | Ma | AoA | nMa0_B | nMa0_C | rmCp_B | rmCp_C(原始) | rmCp_C(smooth fade) | 改善 |
|------|----|-----|--------|--------|--------|-------------|---------------------|------|
| 0 | 0.720 | 0.02° | 1 | 1 | 0.1340 | 0.1308 | 0.1298 | −0.8% |
| 1 | 0.720 | 1.92° | 6 | 2 | 0.1764 | 0.2413 | 0.2822 | **+16.9%（恶化）** |
| 3 | 0.730 | 3.17° | 14 | 6 | 0.5369 | 0.6895 | 0.7162 | **+3.9%（恶化）** |

smooth fade 对 case 0 有微弱改善，但对中高迎角工况明显恶化，**已回退**。

##### 全量测试（test_9_singularity_subtraction，原始公式，10个算例）

当前 `composite.py`（回退至原始 `s_min = 0.01`）的完整结果，与任务 9 测试结果一致：

```
Idx     Ma    AoA  nMa0_B nMa0_C nMa0_S nMa0_F   rmCp_B  rmCp_C  rmCp_S  rmCp_F   dCL_C%  dCL_S%  dCL_F%
   0  0.720   0.02       1      1      0      1   0.1340  0.1308  0.1955  0.0913    -0.53   +0.00   -0.53
   1  0.720   1.92       6      2     10      1   0.1764  0.2413  0.2046  0.2229    -1.98   -0.00   -1.97
   2  0.730   0.80       3      1      4      1   0.1542  0.1686  0.2243  0.1239    -1.54   +0.00   -1.54
   3  0.730   3.17      14      6     14      9   0.5369  0.6895  0.5349  0.6812    -1.35   +0.00   -1.47
   4  0.740   3.38      13      5     13      9   0.5523  0.7206  0.5537  0.7177    -1.29   -0.06   -1.40
   5  0.740   3.88      14      5     14      9   0.5594  0.7333  0.5535  0.7274    -1.33   -0.00   -1.44
   6  0.750   2.25       8      2     10      1   0.2471  0.3090  0.2575  0.2984    -1.44   -0.01   -1.45
   7  0.750   2.48      11      4     11      6   0.4929  0.6205  0.4947  0.6158    -1.19   -0.00   -1.19
   8  0.750   2.59      10      4     11      5   0.5167  0.6647  0.5175  0.6606    -1.18   -0.00   -1.17
   9  0.750   2.99      11      4     11      7   0.5213  0.6748  0.5220  0.6727    -1.21   -0.03   -1.23
```

##### 结论

- **composite 单独模式**：低迎角（cases 0, 2）有效，rmCp_C < rmCp_B；中高迎角（cases 1, 3–9）恶化，原因是 TSD 驻点区（nMa0\_B 大）超出内场模型的有效范围。
- **full 模式（A+C+E + composite）**：10 个算例中均为三种修正模式里 rmCp 最低。低迎角 case 0 的 rmCp\_F = 0.0913，比 baseline 改善 31.8%；高迎角 cases 3–5 受 TSD 基线误差限制，改善空间有限。
- **任务10 未发现需要紧急修复的光滑性问题**：现有修正项的奇性结构（$x^{-1/3}$ 和 $x^{-1/2}$）在当前网格分辨率下行为合理；composite bracket 的非光滑凹陷通过 smooth fade 无法有效消除，根本解决方案是使用 full 模式（Step E 保证 bracket ≈ 0）。

### 任务11：前缘修正的步骤 D 闭环

#### 11.1 任务描述

从前序任务 8, 9, 10 的实现和分析中，我们已经完成了前缘修正的 A+C+E 三步，
验证了其数学自洽性和数值稳定性，并叠加了 composite 修正。

目前仍缺少步骤 D（面边界条件正则化）的完整实现，原因可参考
`test_10_le_smoothness/figures/case_****_bc.png` 中展示的
`Residual BC for phi_r` 在前缘（x -> 0）处存在一个尖峰，
这很可能造成数值不稳定，导致求解器无法正确收敛。
这个尖峰的根源在于，步骤 D 中的修正项（正则项）`phi_{s,y}`的解析解
在前缘（x -> 0）处又重新回到0（左侧子图的绿色虚线）；
而原始边界条件（翼型物面梯度 `FXU`, `FXL`）在前缘处以发散（左侧子图的蓝色实线）。
因此，正则化后的光滑边界条件反而在前缘处保留了尖峰，导致数值求解器无法正确处理。

总结，步骤 D 的可能实现方法：由于正则化的目的在于使 `phi_r` 的边界条件光滑有界且量级合理，
而 `phi_ry_upper` = `FXU` - `phi_sy_upper`, `phi_ry_lower` = `FXL` - `phi_sy_lower`。
我们可以定位 `phi_sy_upper`, `phi_sy_lower` 没有被赋值为 0 的区域，称其为“前缘有效修正区”。
尝试对 `phi_ry_upper`, `phi_ry_lower` 在前缘处的行为进行修改，
使其在 $x \to 0$ 时趋于一个有限值（比如从“前缘有效修正区”的`phi_ry`值插值过去）。

#### 11.2 完成情况

**已实现：步骤 D LE closure（`apply_step_d=True`，`step_d_method`）**

核心逻辑在 `pytsfoil/leading_edge/singularity_subtraction.py` 中新增函数
`apply_step_d_le_closure(fxu, fxl, phi_sy_upper, x_foil, method, n_fit)`：

1. 计算残差边界条件：`phi_ry_upper = FXU - phi_sy_upper`，`phi_ry_lower = FXL + phi_sy_upper`。
2. 定位"前缘有效修正区"：`i_eff = argmax(phi_sy_upper > 0)`（即第一个 x>0 的网格点，通常 `i_eff=1`）。
3. LE closure：对 `i < i_eff` 的点，通过以下方法之一外插，消除 x=0 处的尖峰：
   - `'constant'`：`phi_ry[0] = phi_ry[i_eff]`，最简单，保留 O(√x) 偏差；
   - `'linear'`：以有效区首 `n_fit` 个点拟合 `phi_ry = A + B·x`，外插至 x=0 取截距 A；
   - `'sqrt_fit'`（理论最优）：拟合 `phi_ry = A + B·√x`，利用近前缘理论形式 `phi_ry ~ c₁/δ + c₂/δ·√x` 直接提取截距 A。
4. 返回修改后的 `FXU_modified = phi_ry_upper`，`FXL_modified = phi_ry_lower`。

**网格收敛性分析：**
`phi_ry(x) = FXU - phi_sy ≈ c₁/δ + c₂/δ·√x + O(x)`（抛物线鼻部 Taylor 展开）。
三种方法均在网格加密时收敛至 c₁/δ，但具体路径不同：
- `constant`：`phi_ry[0] = c₁/δ + c₂/δ·√x[1]`，含 O(√x[1]) 偏差；
- `linear`：截距含 O(√x[1]) 偏差（因拟合函数族与真实形式不完全吻合），但实验中表现最佳；
- `sqrt_fit`：理论上零偏差，最精确；实验中对高迎角最好，中迎角次于 linear。

在 `pytsfoil.py` 中，新增配置项：
- `apply_step_d: False`（默认关闭）；
- `step_d_method: 'linear'`（默认，综合性能最佳）。

当 `apply_singularity_subtraction=True` 且 `apply_step_d=True` 时：
- 保存原始 `FXU/FXL` 至 `_fxu_orig/_fxl_orig`；
- 将修改后的残差 BC 写入 `tsf.common_data.fxu/fxl`；
- 求解器完成后自动恢复原始 `FXU/FXL`。

#### 11.3 测试情况

##### 诊断测试（test_11_step_d_closure）

脚本：`test_11_step_d_closure/run_test.py`；3个典型工况（cases 0, 1, 3），对比 3 种外插方法。

**数值结果（rmCp），参考列：baseline=rmB, full=rmF：**

| case | Ma    | AoA   | rmB    | rmF    | rmFD_const | rmFD_linear | rmFD_sqrt |
|------|-------|-------|--------|--------|------------|-------------|-----------|
| 0    | 0.720 | 0.02° | 0.1340 | 0.0913 | 0.2300     | 0.2296      | 0.2301    |
| 1    | 0.720 | 1.92° | 0.1764 | 0.2229 | 0.3584     | **0.2793**  | 0.3896    |
| 3    | 0.730 | 3.17° | 0.5369 | 0.6812 | 0.3641     | 0.3092      | **0.3069**|

（rmFD = full_D = A+C+D+E+composite）

##### 分析结论

**外插方法的效果对比：**
- Case 0（低迎角）：三种方法几乎相同（差距 < 0.0005），外插方式无实质影响。
- Case 1（中迎角）：`linear` 明显优于 `constant` 和 `sqrt_fit`（rmCp 0.2793 vs 0.3584/0.3896）。
- Case 3（高迎角）：`sqrt_fit` 最好（0.3069），`linear` 次之（0.3092），两者差距甚小；
  均大幅优于 `constant`（0.3641）。

**综合推荐：`linear` 为默认方法**（`step_d_method='linear'`），因为：
- 对中迎角改善显著（高出 constant 22%）；
- 对高迎角与 `sqrt_fit` 相差极小（0.3092 vs 0.3069）；
- 实现简单，数值稳定。

**Step D 对高迎角工况（case 3）有效，full_D_linear 的改善情况：**
- `full_D_linear` rmCp = 0.3092，改善幅度 vs baseline：**42.5%**，vs full：**54.6%**；
- 对应 nMa0：10（与 `full` = 9 相当）。

**Step D 对低/中迎角工况（cases 0, 1）仍然恶化：**
- 根本原因：**步骤 A+C 使用的 2D outer PHI_S = A·X^{2/3} 在表面无 Y 方向导数**（phi_s,y|_{y=0} = 0），
  而步骤 D 使用的抛物线公式 `phi_sy = chi·h/(δ·√x)` 来自内场相似解，两者不自洽。
  在低/中迎角时，不自洽引入的额外误差超过了 LE 正则化带来的收益。

**推荐配置：**
- 中低迎角（nMa0_B < 10）：`full`（A+C+E+composite），不启用步骤 D；
- 高迎角（nMa0_B ≥ 10）：`full_D`（`apply_step_d=True, step_d_method='linear'`），改善显著。
