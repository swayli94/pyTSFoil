# pyTSFoil 三维扩展指导（3D TSD / vibe coding 参考）

> 目标读者：在已有 2D pyTSFoil（TSD + Murman–Cole + SLOR/Thomas）基础上，
> 把求解器扩展到**三维机翼**（后掠、锥削、扭转、上反）。
> 本文是实现路线图 + 公式速查 + 坑位清单，供后续 vibe coding 直接参考。
>
> 坐标约定（全文统一）：**x = 弦向（来流方向）、y = 法向（竖直）、z = 展向**。
> 翼根/对称面在 `z = 0`，翼尖在 `z = b/2`，展向远场在 `z_max ≫ b/2`。

---

## 0. 一句话总览

3D TSD = 2D TSD 算子 **加一个展向二阶项 `φ_zz`** + **一个平面形贴合的解析剪切变换** + **一张三维尾迹涡面**。
核心内核（type-dependent 判别、Murman–Cole 切换、三对角线扫）**原样复用**，
只是多了一维 `k`、几个度规系数、和交叉导数项的滞后处理。

---

## 1. 控制方程

### 1.1 非守恒形式（推荐起步，type-dependent 最自然）

$$
\big[\,1 - M_\infty^2 - (\gamma+1)M_\infty^2\,\phi_x\,\big]\,\phi_{xx} \;+\; \phi_{yy} \;+\; \phi_{zz} \;=\; 0
$$

- 前两项 = 经典 2D TSD 算子；`φ_zz` = 展向耦合（三维释压 / 3D relief）。
- **非线性只在流向**：混合型（椭圆/双曲）、激波、type 判别全集中在 `φ_xx` 的系数里，由当地 `φ_x` 决定。
- `φ_yy`、`φ_zz` 都是纯线性、恒椭圆。
- `w = φ_z` = 展向/横流扰动速度。
- 令 `φ_zz = 0` 即退化为条带理论（strip theory）；正是 `φ_zz` 把相邻剖面耦合起来，给出有限翼/后掠的等效马赫数下降与激波减弱。

### 1.2 守恒形式（要正确激波位置/强度时改用）

通量散度形式（参考 Kwon–Vepa 的 Batina 通量）：

$$
\frac{\partial f_0^*}{\partial \tau} + \frac{\partial f_1^*}{\partial x} + \frac{\partial f_2^*}{\partial y} + \frac{\partial f_3^*}{\partial z} = 0
$$

稳态去掉时间项即可。守恒 vs 非守恒的取舍见 §6.3。

---

## 2. 几何处理：一个剪切变换吃掉三个特征

小扰动把所有 BC 转移到一张参考面，于是机翼几何分成两类：
**参考面"在哪儿"（进网格的坐标变换）** 与 **参考面上"斜率多少"（进 BC）**。

### 2.1 统一贴合变换

$$
\xi = \frac{x - x_{LE}(z)}{c(z)}, \qquad \eta = z, \qquad \zeta = y - Y(z)
$$

| 几何特征 | 几何数据 | 进入方式 | 对方程的作用 |
|---|---|---|---|
| **后掠** | 前缘线 `x_LE(z)`，斜率 `x_LE'` | 度规 `ξ_z` | 给 `φ_ξξ` 系数加 `ξ_z²`（即 `tan²Λ`）；生成 `φ_ξη` |
| **锥削** | 当地弦 `c(z)`，`c'` | 度规 `1/c`、`ξ_z` | 算子整体按 `1/c²` 缩放；贡献 `ξ_z`、`ξ_zz` |
| **上反** | 竖向偏置 `Y(z)=z·tanΓ`，`Y'` | 度规 `ζ_z = -Y'` | `φ_ζζ` 系数加 `Y'²`；生成 `φ_ξζ`、`φ_ηζ` |
| **扭转** | 当地迎角 `α(z)` | **不进方程，进 BC** | wall 斜率项 `-α(z)` |
| 厚度/弯度 | `f±(x,z)` | 进 BC | wall 斜率 `∂f±/∂x` |

关键度规（后掠藏在 `x_LE'`，锥削藏在 `c'`，二者天然打包在同一个 `ξ_z` 里）：

$$
\xi_z = -\frac{x_{LE}'(z) + \xi\, c'(z)}{c}
$$

链式法则：
`∂_x = (1/c)∂_ξ`， `∂_y = ∂_ζ`， `∂_z = ξ_z ∂_ξ + ∂_η - Y' ∂_ζ`。

### 2.2 变换后的方程骨架

$$
\underbrace{\Big[\tfrac{1}{c^2}\big(1-M_\infty^2-\tfrac{(\gamma+1)M_\infty^2}{c}\phi_\xi\big)+\xi_z^2\Big]}_{\text{仍是 type-dependent 系数}}\phi_{\xi\xi}
+ \phi_{\eta\eta} + (1+Y'^2)\phi_{\zeta\zeta}
+ \underbrace{2\xi_z\phi_{\xi\eta}-2\xi_z Y'\phi_{\xi\zeta}-2Y'\phi_{\eta\zeta}}_{\text{交叉项}}
+ \underbrace{\xi_{zz}\phi_\xi-Y''\phi_\zeta}_{\text{一阶度规项}} = 0
$$

实现要点：
1. **混合型本质没变**：type 判别仍只看 `φ_ξξ` 系数符号，Murman–Cole 切换照搬。
2. **新增的全是交叉项 + 变系数**：滞后显式处理（用上一迭代步的值），不破坏三对角结构。
3. **一阶项来自几何曲率**：`ξ_zz`（变后掠/变锥削）、`Y''`（鸥翼折点）；直梯形等截面机翼这两项 ≈ 0。

### 2.3 进网格 vs 进 BC（关键认知）

- **进网格的只有 O(1) 的平面形几何**：后掠、锥削、上反中线。
- **翼型本身的厚度、弯度、扭转、迎角全不进网格** —— 它们只是 `ζ=0` 平参考面上的斜率 BC。
- 这和 2D pyTSFoil 完全一致：翼型压在 `y=0` 的 slit 上，厚度/弯度只是 `φ_y` BC，网格永远规整。
- 扭转尤其特殊：绕展向轴转小角 `α(z)`，只改 slit 斜率、不移动参考面 → 网格对扭转完全透明。

---

## 3. 网格（结构化剪切笛卡尔，不要纯物理笛卡尔）

### 3.1 本质

- 计算空间 `(ξ, η, ζ)` 是**规整矩形结构网格**（`φ[i,j,k]`）；物理空间里它被剪切成跟着平面形走的样子。
- 这就是 TSD 的 "sheared-stretched Cartesian-like" 网格（Holst Fig 11、Kwon–Vepa shearing transformation）。
- **不需要网格生成器**：给定 `x_LE(z), c(z), Y(z)`，度规解析算出，乘进差分模板即可。

### 3.2 为什么不用纯物理笛卡尔（针对"简单 + 快"）

| 维度 | 剪切结构化笛卡尔（采用） | 纯物理笛卡尔 + cut-cell（不用） |
|---|---|---|
| 生成 | 解析变换，零网格生成器 | 要处理 LE/TE 斜穿的不规则切割单元 + 自适应加密（TRANAIR 那套），更复杂 |
| 点数 | 每展向站固定弦向点、点全在机翼上、坐标线对齐 LE | 后掠 LE/激波相对网格 ~45° 斜，需两/三方向同时加密 → 点数爆炸 |
| 锥削 | 短翼尖弦照样给足弦向点 | 同一网格线 Δx 相同 → 翼尖欠分辨或全局浪费 |
| 求解器 | 三对角线扫 → SLOR/AF2/多重网格直接可用 | 丢三对角结构 → 只能上通用稀疏解（NKS），每步更重 |

### 3.3 网格分布建议

- `ξ` 方向：cosine 聚点于 LE/TE（捕捉吸力峰与激波）。
- LE 落在两条网格线**之间**（避免钝前缘无穷斜率破坏 BC 施加）。
- `η`（展向）：聚点于翼尖（梯度陡、近奇异）。
- `ζ`（竖向）：向远场拉伸。
- 展向计算域延伸到 `z_max ≫ b/2`（见 §4），**翼尖不是域边界**。

---

## 4. 边界条件

| 位置 | 条件（稳态） | 说明 |
|---|---|---|
| 翼面（`ζ=0`，平面形内） | `φ_ζ = ∂f±/∂x - α(z)`（含扭转/弯度/厚度） | Neumann；Murman–Cole 在此施加 |
| 尾迹涡面（`z∈[0,b/2], x>x_TE`） | `Δφ = Γ(z)`，稳态 `∂(Δφ)/∂x = 0`，后缘施 Kutta | 三维是一张面，不是单条 cut |
| 对称面 `z=0` | `φ_z = 0`（Neumann，`w=0`） | 镜像；反对称载荷改 `φ=0` |
| 展向远场 `z_max` | `φ → 0`（Dirichlet） | 放几倍半展长外；或用涡远场渐近把边界拉近 |
| 翼尖 `z=b/2` | **无特殊 BC** | 域内部边线；`Δφ→0` 自然卸载（翼尖外 `φ` 单值连续） |
| 上/下/前/后远场 | 标准 TSD 远场（`φ=0` / `φ_x=0` 等） | 非定常需非反射 BC（见 Kwon–Vepa Table 1） |

实现提示：
- 对称面用反射 ghost：`φ[k=-1] = φ[k=+1]`。
- `z_max` 用 `φ=0` 简单但要放远；想省网格就上涡/偶极子远场渐近。
- 翼尖外 `z>b/2` 的 `y=0` 平面退化为普通流场面：`φ` 连续、`φ_y` 连续、无 BC，照常解齐次方程。

---

## 5. 求解流程（复用 2D 内核）

```
初始化 φ（自由流 / 低 AR 解，见 §7.3 continuation）
repeat（外迭代）:
    更新环量 Γ(z) / Kutta（环量内迭代）
    for k in 展向站:                      # 可并行
        计算当地 type-dependent 系数（含 ξ_z², 1/c²）
        交叉项 φ_ξη, φ_ξζ, φ_ηζ 用上一步值（滞后显式）
        SLOR 列扫（i 方向 Thomas）+ Murman–Cole 切换   # 复用 2D
    （可选）展向/竖向附加扫掠（AF2 的 sweep 2、3）
until 残差/RMS 误差收敛
重构 cp（isentropic 反演）
```

---

## 6. 数值要点

### 6.1 Murman–Cole type-dependent 差分
- 判别只看 `φ_ξξ` 系数符号：`>0` 当地亚声速→中心差分；`<0` 超声速→迎风（沿流向后差）。
- 四个算子：subsonic / supersonic / sonic-point / shock-point（与 2D 完全一致）。
- 系数里已含 `ξ_z²`（后掠 `tan²Λ` 效应）与 `1/c²`（锥削缩放）。

### 6.2 交叉项处理
- `φ_ξη, φ_ξζ, φ_ηζ` 一律**滞后显式**（previous iteration 的值）或并入 AF 扫描。
- 直梯形等截面机翼可先忽略 `ξ_zz, Y''` 一阶项验证主干。

### 6.3 守恒 vs 非守恒
- 非守恒：type-dependent 最自然（系数符号切换），且对弱激波常与实验更吻合（等效 mass source 误打误撞补了激波/边界层效应）。
- 守恒：激波位置/强度数学上正确（Lax）；强激波或要严格守恒时用。
- 编码差异不大，可做成 option（参考 Holst：两者切换主要在通量定义）。

### 6.4 迭代格式与速度
- **SLOR**：基线，简单，但三维收敛慢（迭代数 ~ O(N_max)）。
- **AF / AF2（Ballhaus–Goorjian）**：三次扫掠（弦向→展向→竖向），比 SLOR 快约一个量级；非定常/颤振几乎都用它（Batina AF，NASA TP-3129）。
- **多重网格**：迭代数近似网格无关，最快；以 AF 作 base smoother。
- 推荐路径：先 SLOR 跑通 → 换 AF2 提速 → 需要再上多重网格。

### 6.5 性能（Python）
- 纯 Python 三维 SLOR 太慢（分钟~十几分钟级）。
- 把列扫/AF 扫掠用 **Numba `@njit`** 编译 → 拉回交互级。
- 备选：scipy `solve_banded` / 稀疏直接解。

---

## 7. 计算成本预期

- 2D pyTSFoil：编译核 ~亚秒；含 Python 开销 ~10s 内。
- **3D 实算量 ≈ 2D 的 ~100×**（展向点数 ~30–50× × 迭代/交叉项惩罚 ~2–3×）。
  - 编译 SLOR：~10–60s 量级；
  - AF2 / 多重网格：~几秒；
  - 纯 Python 朴素 SLOR：分钟~十几分钟（务必编译内核）。
- 横向锚：TSD ≈ N–S 的 1/100（Kwon–Vepa）；全位势 ≈ Euler 的 1/10、3D 稳态典型 1 min–1 hr（Holst）。
- 放进优化循环跑几千次 → 小时级；此时 AF/多重网格 + 精简 `N_z` 是刚需，或按展向/工况并行。

### 7.3 高 AR continuation（Kwon–Vepa 的提速技巧）
- 只沿展向把低 AR 平面形拉伸到目标高 AR；展向算子光滑、单调收敛。
- 用低 AR 的收敛解当高 AR 的初值，逐步增大 AR → 每个 AR 都很快。
- 这套只有在结构化剪切网格上才干净可行。

---

## 8. 适用边界与坑位清单

按"哪个特征推到极限"排：

- **后掠 →** 高后掠（>30–40°）时非线性项 `(γ+1)M∞²` 系数未按法向流量缩放，`w~tanΛ·u` 偏离小扰动；前缘附近尤其失真。线性部分仍正确给出 `M∞cosΛ=1` 的临界推迟。高后掠可考虑基于法向马赫的修正 TSD。
- **锥削 →** 尖翼尖 `c(z)→0` 使 `1/c`、`ξ_z` 发散，变换奇异；翼尖需单独封口处理。
- **上反 →** `Y'` 大时平面尾迹假设受冲击；鸥翼折点 `Y''` 引入一阶奇异。
- **扭转 →** 最稳，唯一要求 `α(z)` 小。
- **翼尖 fairing / 钝头三维形状 →** 用斜率表达翼面，抓不到真实三维 fairing，翼尖激波位置略偏（Kwon–Vepa 明示）；但翼尖对总升力贡献小，对载荷/颤振无关紧要。
- **翼尖涡卷起 →** 始终抓不到（强非线性、非小扰动）；TSD 给的是展向平均载荷，不是涡核结构。
- **钝前缘 MAE 修正 →** 你 2D 的那套（Rusak 1993 / Rusak–Lee 2000）在 3D 里奇异性沿后掠线分布，内解匹配要在变换坐标里重推，不能直接套 2D 结果。
- **激波强度 →** 仅弱激波（法向 `M_n ≲ 1.3`）；isentropic、irrotational。需要更强激波时加 Batina 的 entropy/vorticity 修正（改流向通量 + cp 公式熵修正 + 尾迹熵对流 BC）。
- **cp 重构 →** 复用你已验证的 isentropic 反演（Mach 反算 cp / cp 正算）；3D 每个 `(i,k)` 表面点独立施加。

---

## 9. 建议模块结构（接入现有 pyTSFoil）

```
pytsfoil3d/
  geometry.py     # x_LE(z), c(z), Y(z), α(z), f±(x,z)；planform 定义
  metrics.py      # ξ_z, ξ_zz, 1/c, Y'；预存为 (i,k) 数组，乘进模板
  grid.py         # 结构化剪切网格：cosine-ξ, 聚 η 于 tip, 拉伸 ζ；z_max
  bc.py           # wall(含 twist)、wake sheet+Kutta、symmetry(ghost)、far-field
  solver.py       # 复用 2D：Murman–Cole + Thomas；加 k 维 + 交叉项滞后
                  #   baseline=SLOR；option=AF2 三扫掠
  cp.py           # isentropic cp 反演/正算（复用 2D）
  continuation.py # 低 AR → 高 AR 展向 morphing，热启动
  numba_kernels.py# @njit 列扫/AF 扫掠
```

落地顺序建议：
1. 先**矩形等截面无后掠机翼**（`x_LE'=c'=Y'=0`）跑通 `φ_zz` 耦合 + 对称面 + 翼尖外场 → 验证条带理论极限与三维释压。
2. 加**锥削 + 后掠**（开 `ξ_z`、`1/c²`、`φ_ξη` 滞后）→ 对 CRM / ONERA M6 验证。
3. 加**扭转**（BC 里 `α(z)`）、**上反**（`ζ=y-Y(z)`、`φ_ξζ/φ_ηζ`）。
4. SLOR → AF2 提速；Numba 编译内核；continuation 热启动。
5. 需要时加 entropy/vorticity 修正、MAE 前缘修正（3D 重推）。

---

## 10. 参考文献

- **Kwon & Vepa (2022)** — Transonic small disturbance unsteady potential flow over very high aspect ratio wings. （3D 非定常 TSD、shearing transformation、CRM/AGARD445.6/ONERA M6 验证、高 AR continuation、颤振）
- **Holst (2000)** — Transonic flow computations using nonlinear potential methods, *Prog. Aerospace Sci.* 36:1–61. （TSD/全位势综述、sheared-stretched 网格、SLOR/AF2/多重网格收敛对比、守恒 vs 非守恒、45° 梯度问题）
- Murman & Cole (1971) — type-dependent differencing。
- Bailey & Ballhaus (1975) — 3D TSD isolated-wing（守恒/非守恒、ONERA M6）。
- Batina — Efficient AF algorithm for unsteady TSD（J. Aircraft 1988/89；NASA TP-3129, 1992）。
- Ballhaus & Steger (1975) / Ballhaus–Goorjian — AF / AF2。
- 代码遗产：XTRAN3S、CAP-TSD（跨声速颤振标准工具）。
