# TSD 扰动速度限制器（投影 / 截断 SOR）

> 任务说明 + 原理描述，供 vibe coding 使用。
> 目标求解器：pyTSFoil（TSD 小扰动势流，SOR / SLOR 松弛）。

## 1. 背景与目的

跨声速小扰动（TSD）方程在求解时被组织为一个泊松型迭代

$$\Delta P = s,\qquad s=s(P_x,P_{xx},\dots)$$

其中 $P$ 是扰动势（perturbation potential），其梯度对应**扰动速度**：

$$u' = P_x,\qquad v' = P_y$$

（具体系数取决于求解器内部的无量纲化 / 相似律标度。）

小扰动理论的前提是 $|u'|/U_\infty \ll 1$。在钝前缘等位置存在真实物理奇异性，$P_x\to\infty$，使非线性源项 $s\sim(\gamma+1)P_xP_{xx}$ 发散，把 SOR 推向不稳定。

**限制器的定位是"护栏"，不是物理求解：** 在迭代过程中把扰动速度的模长（按分量）限制在 $|u'|\le D$，其中 $D$ 是一个**显著超过任何合理物理值**的阈值，对应"再大就会破坏 SDT 假设"。它应当**极少触发**；触发处的解本就因 SDT 失效而不可信，限制器的唯一作用是阻止该局部失效通过非线性反馈污染全局解、压住发散。

## 2. 任务内容

在现有 SOR / SLOR 松弛循环中加入一个**逐点投影（截断）步骤**：

1. 每次对内部节点（或一条线）完成常规松弛更新后，对该节点的 $P$ 值做一次截断，使其与所有网格相邻节点之间的差分速度满足 $|P_x|\le D$、$|P_y|\le D$。
2. 截断只作用在 $P$ **本身**上，不直接修改梯度场（见 §3.2）。
3. 提供诊断输出：每次扫掠被截断的节点数、截断前的最大 $|u'|$、是否出现不可行区间。
4. 提供开关与参数：阈值 $D$、截断欠松弛系数 $\theta$、是否仅在亚声速（椭圆）点启用、是否启用限制器。
5. 兼容非均匀（拉伸）网格——TSD 网格在前缘和激波附近高度聚集，差分约束必须用**局部网格间距**。
6. 边界节点（远场、翼面 BC）不参与截断，仅作为相邻已知值参与约束。

**接口建议（最终代码由我实现，这里给约定）：**

- 标量参数：`D`（与求解器内部 $P_x$ 同单位）、`theta`（默认 1.0）、`elliptic_only`（默认 False）、`enabled`（默认 True）。
- 逐点版（SOR）：在单点更新后调用 `clip_node(P, i, j, ...)`。
- 整线版（SLOR）：在 Thomas 解出整条线后，对该线做一次向量化截断。
- 返回 / 累计诊断：`n_clipped`, `max_u_before`, `n_infeasible`。

---

## 3. 原理描述

### 3.1 物理动机与约束的离散形式

要求扰动速度按分量不超过 $D$。在节点 $(i,j)$ 上，沿 $x$ 方向的边速度（前差分）为

$$u'_{i+\frac12,j}=\frac{P_{i+1,j}-P_{i,j}}{\Delta x_{i+\frac12}},\qquad \Delta x_{i+\frac12}=x_{i+1}-x_i$$

约束 $|u'|\le D$ 等价于对**每条网格边**限制差分幅值：

$$\bigl|P_a-P_b\bigr|\le D\cdot h_{ab}$$

其中 $h_{ab}$ 是这条边对应的**局部间距**（拉伸网格下东/西/南/北各不相同）。

> 这是逐方向（$L^\infty$）约束：分别保证 $|P_x|\le D$ 和 $|P_y|\le D$。对本用途这是**优点**——物理上限速本就按速度分量讲，且 TSD 真正敏感的是流向分量 $P_x$（它进非线性系数 $K-(\gamma+1)P_x$、决定声速线位置）。无需欧氏模长 $\sqrt{P_x^2+P_y^2}$ 的精确约束。

### 3.2 为什么作用在 $P$ 上而不是梯度场（关键）

不能"算出 $\nabla P$，把模长超限处缩回去"。被截断的向量场一般不再无旋（不是任何标量的梯度），积分回 $P$ 会不一致。正确做法是直接调整 $P$，使其**差分**满足上界。

另外：中心差分 $P_x\approx(P_{i+1}-P_{i-1})/(2h)$ 不含 $P_{i,j}$ 自身，逐点投影对当前点不起作用。因此限制器以**边（前差分）**定义速度并施加约束；只要相邻每条边的斜率都被压住，中心差分速度自然也被压住（不超过相邻两条边斜率的较大者）。

### 3.3 逐点投影公式

固定节点 $(i,j)$ 的（已按 Gauss–Seidel 次序更新的）邻居值，$P_{i,j}$ 的可行区间为

$$L_{i,j}=\max_{n\in\{N,S,E,W\}}\bigl(P_n - D\,h_n\bigr),\qquad
U_{i,j}=\min_{n\in\{N,S,E,W\}}\bigl(P_n + D\,h_n\bigr)$$

其中 $h_n$ 是到邻居 $n$ 的局部间距。更新为常规松弛值后截断：

$$P_{i,j}\leftarrow \mathrm{clip}\bigl(P_{i,j}^{\text{relax}},\,L_{i,j},\,U_{i,j}\bigr)$$

由于在 Gauss–Seidel 扫掠中，稍后更新某邻居时会再次对该邻居关于本点截断，每条成对约束从两端各检查一次，迭代收敛到（近似）可行点。

### 3.4 在 SOR / SLOR 中的位置

**SOR（逐点）：** 常规更新后立即截断。

```
P_star = poisson_update(P, i, j, s, h)          # 五点/类型相关差分
P_relax = P[i,j] + omega*(P_star - P[i,j])       # SOR
P[i,j]  = clip(P_relax, L(i,j), U(i,j))          # 投影
```

**SLOR（整线，你当前的方案）：** 先用 Thomas 解出整条线 $\{P_{i,j}\}_j$，再对整条线逐点做一次截断（用该线两侧已更新的相邻线作为约束）。截断在 Thomas 解之后、进入下一条线之前进行。线内沿松弛方向的边约束在截断后可能略有残差，由后续扫掠收敛吸收——对极少触发的护栏可忽略。

### 3.5 欠松弛与收敛

硬截断不光滑，若 $D$ 卡太紧、激活区在迭代中漂移，可能出现极限环。对策：

$$P_{i,j}\leftarrow P_{i,j}^{\text{relax}}+\theta\bigl(\mathrm{clip}(\cdot)-P_{i,j}^{\text{relax}}\bigr),\quad 0<\theta\le 1$$

默认 $\theta=1$（直接截断）；若收敛抖动则降到 $0.5\!\sim\!0.8$。配合**宽松的 $D$**（护栏应几乎不触发），对收敛的影响可忽略。

### 3.6 不可行区间处理

若 $L_{i,j}>U_{i,j}$，说明相邻节点本身已相差超过 $2Dh$（上游约束失败或网格过粗）。处理：

$$P_{i,j}\leftarrow \tfrac12\bigl(L_{i,j}+U_{i,j}\bigr)$$

并对 `n_infeasible` 计数 + 告警。频繁出现不可行 ⇒ $D$ 太小或网格在该处分辨不足，应作为信号反馈给用户。

### 3.7 仅亚声速区（可选）

TSD 是混合型方程：超声速区双曲、用 Murman–Cole 类型相关迎风差分。若担心截断与迎风性相互作用，可设 `elliptic_only=True`，仅在椭圆（亚声速）点启用限制器。一般情形下超声速区速度也有界，可保持全场启用；该开关用于排障。

## 4. 伪代码

```python
def clip_node(P, i, j, x, y, D, theta=1.0):
    # 局部间距（非均匀网格）
    hE = x[i+1] - x[i]; hW = x[i] - x[i-1]
    hN = y[j+1] - y[j]; hS = y[j] - y[j-1]

    L = max(P[i+1,j] - D*hE, P[i-1,j] - D*hW,
            P[i,j+1] - D*hN, P[i,j-1] - D*hS)
    U = min(P[i+1,j] + D*hE, P[i-1,j] + D*hW,
            P[i,j+1] + D*hN, P[i,j-1] + D*hS)

    if L > U:                      # 局部不可行
        target = 0.5*(L + U)
        flag_infeasible()
    else:
        target = min(max(P[i,j], L), U)

    clipped = (target != P[i,j])
    P[i,j] += theta * (target - P[i,j])
    return clipped
```

SLOR 整线版：对解出的一整列做相同的逐点 `L/U` 计算与截断（向量化），约束取自两侧相邻列与列内上下邻居。

## 5. 参数与默认值

| 参数 | 含义 | 默认 |
|---|---|---|
| `D` | 速度上界，单位同求解器内部 $P_x$ | 由调用方设定 |
| `theta` | 截断欠松弛系数 | `1.0` |
| `elliptic_only` | 仅在亚声速点启用 | `False` |
| `enabled` | 总开关 | `True` |

**$D$ 的取法：** 若 $P$ 已按 $P_x=u'/U_\infty$ 无量纲化，则 $D=\alpha$，取 $\alpha\approx 0.3\!\sim\!0.5$ 作护栏；若 $P$ 有量纲或采用相似律标度，则 $D=\alpha\cdot U_\infty\cdot(\text{相应标度因子})$，由调用方按 pyTSFoil 的无量纲化提供。$D$ 必须显著高于任何预期的合理物理 $|u'|$，只截奇异性驱动的尖峰。

## 6. 诊断与验收标准

**每次扫掠输出：** `n_clipped`、`max_u_before`（截断前最大 $|u'|$）、`n_infeasible`。

**验收：**
1. 在良态、收敛的光滑算例（如薄翼小迎角、无强激波）中，限制器在收敛态应**不触发**（`n_clipped → 0`）。若触发，说明 $D$ 太小或物理本应失效。
2. 关闭限制器会发散 / 出现非物理速度尖峰的算例（如钝前缘），开启后应稳定收敛，且远离触发区的解与参考解一致。
3. 触发只应集中在物理奇异点附近少数节点，不应大面积铺开。
4. 拉伸网格下，约束用的是局部间距（同一条边在不同区域 $h$ 不同），不得用全局常数 $h$。

## 7. 注意事项

- 限制器是**瞬态护栏**，会在触发处系统性偏置解；这与匹配渐近 + 奇性扣除（Rusak）那种"去掉奇异主部、对光滑余项求解"不是一回事，**不能替代**奇性扣除。推荐二者叠加：先做奇性扣除让前缘 $P_x$ 有限，再加一个很宽松的限制器只防极端 spike，使其几乎永不触发。
- 若只在意迭代稳定性而不要求**输出**速度场被钳住，可改用更轻的变体：仅在组装非线性源项 $s$ 时对 $P_x$ 限幅，而 $P$ 的线性更新照常。该变体不碰积分性、不干扰收敛，但只保证"反馈进方程的速度"有界，不保证输出速度有界。本方案保证的是输出场。
- 边界条件（翼面流向 BC 设定 $P_y$、远场）不被截断；限制器只作用于内部未知量。

# pyTSFoil 相关修改计划

共涉及 3 个 Fortran 文件和 1 个 Python 文件，按依赖顺序实施。

## 步骤 1：`common_data.f90` — 新增限制器参数变量

在 `common_data` 模块的声明区（`Output control` 块之前）增加：

```fortran
! ------------------------------------------------
! Velocity limiter parameters
! ------------------------------------------------
logical :: VEL_LIM_ENABLED = .false.        ! 总开关（默认关闭，护栏按需开启）
real    :: VEL_LIM_D       = 5.0            ! 速度上界 D，与求解器内部 P_x 同单位
real    :: VEL_LIM_THETA   = 1.0            ! 截断欠松弛系数 θ（0 < θ ≤ 1）
logical :: VEL_LIM_ELLIPTIC_ONLY = .false.  ! 仅在亚声速（椭圆）点启用
```

同时在 `initialize_common()` 中将上述4个变量重置为默认值（与声明区一致）。

**D 的默认值说明：** pyTSFoil 采用 Krupp 相似律（SIMDEF=3），$P_x$ 在相似坐标下的典型合理量级为 $O(0.5)$，因此 $D=5.0$ 留足冗余、几乎不触发；对于强前缘奇点问题可降至 $D=2.0$。

## 步骤 2：`solver_data.f90` — 新增每次扫掠诊断计数器

在 `solver_data` 模块声明区末尾（`WSLP` 之后）增加：

```fortran
! Velocity limiter diagnostics (reset at start of each SYOR sweep)
integer :: VEL_LIM_N_CLIPPED   = 0    ! 本次扫掠中被截断的节点数
real    :: VEL_LIM_MAX_U_BEFORE = 0.0  ! 截断前最大 |P_x|（流向分量）
integer :: VEL_LIM_N_INFEASIBLE = 0    ! 本次扫掠中不可行区间节点数
```

同时在 `initialize_solver_data()` 中将上述3个变量重置为零。

## 步骤 3：`main_iteration.f90` — 核心修改

分三处修改，均在同一个文件内。

### 3a. 新增私有子程序 `CLIPLN`

在 `SYOR` 子程序之后（`SOLVE` 之前）的 `contains` 区增加：

```fortran
! Clip one x-column of P to enforce |P_x|, |P_y| <= VEL_LIM_D (velocity limiter).
subroutine CLIPLN(I_col, JBOT_loc, JTOP_loc, VC_col)
    use common_data, only: X, Y, N_MESH_POINTS, &
                           VEL_LIM_ENABLED, VEL_LIM_D, VEL_LIM_THETA, &
                           VEL_LIM_ELLIPTIC_ONLY
    use solver_data, only: P, VEL_LIM_N_CLIPPED, VEL_LIM_MAX_U_BEFORE, &
                           VEL_LIM_N_INFEASIBLE
    implicit none
    integer, intent(in) :: I_col, JBOT_loc, JTOP_loc
    real,    intent(in) :: VC_col(N_MESH_POINTS)  ! 1-M² 符号数组，供 elliptic_only 判断

    integer :: J
    real    :: hE, hW, hN, hS, D, Lbound, Ubound, P_cur, target

    if (.not. VEL_LIM_ENABLED) return

    D = VEL_LIM_D

    do J = JBOT_loc, JTOP_loc

        ! 仅亚声速区启用时，跳过超声速（双曲型）节点
        if (VEL_LIM_ELLIPTIC_ONLY .and. VC_col(J) < 0.0) cycle

        ! 局部网格间距（非均匀网格）
        hE = X(I_col+1) - X(I_col)
        hW = X(I_col)   - X(I_col-1)
        hN = Y(J+1) - Y(J)
        hS = Y(J)   - Y(J-1)

        ! 截断前最大流向速度（诊断用）
        VEL_LIM_MAX_U_BEFORE = max(VEL_LIM_MAX_U_BEFORE, &
                                   abs(P(J, I_col+1) - P(J, I_col))   / hE, &
                                   abs(P(J, I_col)   - P(J, I_col-1)) / hW)

        ! 可行区间 [Lbound, Ubound]
        Lbound = max(P(J, I_col+1) - D*hE,  P(J, I_col-1) - D*hW, &
                     P(J+1, I_col) - D*hN,  P(J-1, I_col) - D*hS)
        Ubound = min(P(J, I_col+1) + D*hE,  P(J, I_col-1) + D*hW, &
                     P(J+1, I_col) + D*hN,  P(J-1, I_col) + D*hS)

        P_cur = P(J, I_col)

        if (Lbound > Ubound) then
            ! 不可行区间：取中点，计数告警
            target = 0.5*(Lbound + Ubound)
            VEL_LIM_N_INFEASIBLE = VEL_LIM_N_INFEASIBLE + 1
        else
            target = min(max(P_cur, Lbound), Ubound)
        end if

        if (target /= P_cur) then
            P(J, I_col) = P_cur + VEL_LIM_THETA * (target - P_cur)
            VEL_LIM_N_CLIPPED = VEL_LIM_N_CLIPPED + 1
        end if

    end do

end subroutine CLIPLN
```

`CLIPLN` 不公开（不加入 `public` 列表），仅在 `SYOR` 内部调用。

### 3b. `SYOR` 中重置诊断 + 调用 `CLIPLN`

**修改 `SYOR` 的 use 列表**，增加：

```fortran
use common_data, only: ..., VEL_LIM_ENABLED          ! 在原 use common_data 行追加
use solver_data, only: ..., VEL_LIM_N_CLIPPED, VEL_LIM_MAX_U_BEFORE, VEL_LIM_N_INFEASIBLE
```

**在 `SYOR` 第 35 行（`BIGRL = 0.0` 之后）** 增加每次扫掠诊断重置：

```fortran
! Reset limiter diagnostics for this sweep
VEL_LIM_N_CLIPPED    = 0
VEL_LIM_MAX_U_BEFORE = 0.0
VEL_LIM_N_INFEASIBLE = 0
```

**在第 166 行（error 计算循环之后，supersonic BC `if (AK<=0.0...)` 之前）** 增加 CLIPLN 调用：

```fortran
! Apply velocity limiter (projected clip) to this column
call CLIPLN(I, JBOT, JTOP, VC)
```

此处 `VC` 是 SYOR 内已计算的本地数组（当前列 $1-M^2$ 值），直接传入。

### 3c. `SOLVE` 中打印诊断

**修改 `SOLVE` 的 use 列表**，增加：

```fortran
use common_data, only: ..., VEL_LIM_ENABLED           ! 追加到原 use common_data 行
use solver_data, only: ..., VEL_LIM_N_CLIPPED, VEL_LIM_MAX_U_BEFORE, VEL_LIM_N_INFEASIBLE
```

**在 `SOLVE` 的 OUTERR 打印块（第 296 行附近）** 增加限制器诊断输出：

```fortran
if (VEL_LIM_ENABLED .and. FLAG_OUTPUT == 1) then
    write(*, '(3X,"LIMITER: n_clipped=",I6,2X,"max_u_before=",E11.4,2X,"n_infeasible=",I5)') &
        VEL_LIM_N_CLIPPED, VEL_LIM_MAX_U_BEFORE, VEL_LIM_N_INFEASIBLE
end if
```

放在 NWDGE 相关输出之前，与 CL/CM 行处于同一 `if (OUTERR .and. FLAG_OUTPUT == 1)` 块内。

## 步骤 4：`pytsfoil/pytsfoil.py` — Python 接口

### 4a. `_default_config()` 中增加限制器配置项

在 `'apply_le_correction'` 行之后追加：

```python
# Velocity limiter (projected SOR clip)
'vel_lim_enabled': False,       # 总开关，默认关闭
'vel_lim_d': 5.0,               # 速度上界 D（相似坐标系 P_x 单位）
'vel_lim_theta': 1.0,           # 截断欠松弛系数
'vel_lim_elliptic_only': False, # 仅亚声速点启用
```

`initialize_data()` 中已有通用循环 `setattr(tsf.common_data, key.lower(), value)`，无需额外修改即可将上述4个参数传入 Fortran。

### 4b. `run_fortran_solver()` 求解后读取诊断

在 `tsf.main_iteration.solve()` 调用之后增加诊断读取，并记录到 `self.data_summary`：

```python
# Read velocity limiter diagnostics
if self.config['vel_lim_enabled']:
    self.data_summary['vel_lim_n_clipped']    = int(tsf.solver_data.vel_lim_n_clipped)
    self.data_summary['vel_lim_max_u_before'] = float(tsf.solver_data.vel_lim_max_u_before)
    self.data_summary['vel_lim_n_infeasible'] = int(tsf.solver_data.vel_lim_n_infeasible)
    if self.config['flag_print_info']:
        print(f"[Limiter] last-sweep: n_clipped={self.data_summary['vel_lim_n_clipped']}, "
              f"max_u_before={self.data_summary['vel_lim_max_u_before']:.4f}, "
              f"n_infeasible={self.data_summary['vel_lim_n_infeasible']}")
```

## 边界条件安全性核查

- `SYOR` 外层循环范围为 `I = IUP, IDOWN`，`CLIPLN` 只在该范围内被调用，不触及远场边界（`IMIN`、`IMAX`）。
- `CLIPLN` 内层循环范围为 `JBOT_loc` 到 `JTOP_loc`，不触及 `JMIN`/`JMAX`。
- 翼面 BC（`FXUBC`/`FXLBC`）和尾迹跳跃（`PJUMP`）均在单独步骤中施加，不受 `CLIPLN` 影响。
- `CLIPLN` 使用的相邻节点（包括边界点 `P(JMIN,I)`、`P(JMAX,I)` 等）只作为已知约束参与 `Lbound/Ubound` 计算，不被修改。

## 验收标准（对应分析文档 §6）

1. 光滑收敛算例中，`n_clipped` 在迭代末期应趋向 0。
2. 仅开启限制器（不改其他参数）的算例，全局 $C_L$、$C_P$ 分布与关闭时一致（若 $D$ 足够宽松）。
3. 限制器只在少数节点触发，不大面积铺开。
4. 拉伸网格验证：`CLIPLN` 中 `hE, hW, hN, hS` 取局部差分（非全局常数），覆盖前缘聚集区。
5. `n_infeasible > 0` 出现时，应视为警告信号，提示 $D$ 可能设置过小或该处网格分辨率不足。
