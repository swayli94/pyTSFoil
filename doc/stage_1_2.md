# 阶段 1-2：矩形等截面无后掠机翼（双端对称面）

基于 `plan.md` 和 Stage 0 的工作，搭建并测试 3D TSD SLOR 求解器。

考虑到 Stage 1-1（`stage_1_1_fail.md`）的 3D 求解器因显式 φ_zz 收敛下限无法满足
CVERGE 标准，本阶段对问题进行进一步简化：**两端均取对称面**，消除翼尖，
以干净验证 3D 框架的基本功能。

---

## 目标

在已验证的 `thomas_column_sweep` 基础上，套上展向 k 循环，
跑通 φ_zz 展向耦合 + 对称面，不含任何平面形度规（`ξ_z=0`, `1/c²=1`, `Y'=0`）。

**简化问题**：矩形等截面无后掠无上反无扭转机翼，展长为 1，展向网格均匀，
**机翼两端的边界条件均为对称面**（`φ_z=0`）——两端都用 ghost cell 对称，
不存在翼尖。精确 3D 解 = 2D 解（无限展长翼，各站 φ_zz=0）。

---

## 文件结构

```
stage_1/
  __init__.py
  numba_kernels.py   # thomas_column_sweep_3d（@njit），compute_phi_zz_rhs_station
  solver.py          # 3D SLOR 主控：k 循环 + 双端对称 φ_zz + Kutta + 远场 BC
  postprocess.py     # surface_cp_3d_station，spanwise_cl（全展向积分）
  run_stage1.py      # 验证脚本，输出 result_*.png
```

---

## 实现要点

### 1. 展向网格与边界条件

展向坐标 η 从 0 到 `full_span=1.0`，nk 个均匀站点，dz = 1/(nk-1)。

| 边界 | 条件 | Ghost cell |
|------|------|-----------|
| k=0 | φ_z=0（对称面） | P[-1] = P[1] |
| k=nk-1 | φ_z=0（对称面） | P[nk] = P[nk-2] |
| 所有 k | 机翼壁面 BC + Kutta | apply_wall_bc=True |

无翼尖，无展向远场 Dirichlet。精确解 P[k] 对 k 均匀（与 2D 相同）。

### 2. φ_zz 处理：显式全残差

φ_zz 贡献以全残差写入 RHS（显式 Jacobi 滞后），**不修改 DIAG**：

```
# 均匀网格（dz），二阶中心差分
phi_zz[k] = (P[k-1] - 2P[k] + P[k+1]) / dz²

# 边界（ghost cell 对称）
k=0    : phi_zz[0]     = 2*(P[1]     - P[0])     / dz²
k=nk-1 : phi_zz[nk-1]  = 2*(P[nk-2]  - P[nk-1])  / dz²

# 更新
RHS[j] -= phi_zz_full[j, i]   # 全残差（含 -cc·P[k] 项）
DIAG[j]  不变                  # phi_zz_diag = 0
```

**为什么 phi_zz_diag = 0**：
- 若将 cc = 2/dz² ≈ 1682（nk=30，span=1）加入 DIAG，会使近跨声速工况
  DIAG_2D + cc ≈ 0（DIAG_2D 量级约 -1765），Thomas 算法近奇异 → 数值爆炸。
- 对于本阶段的精确解（φ_zz=0），phi_zz_diag 对固定点无影响，不需要。

**为什么 phi_zz_rhs 必须包含 -cc·P[k] 项**：
- 若仅传入邻站贡献 cd·P[k-1] + cu·P[k+1]（遗漏 -cc·P[k]），
  则 RHS = -(2D残差) - (cd·P[k-1] + cu·P[k+1])，
  在 z-uniform 暖启动时 RHS = -2/dz²·P ≠ 0，第一步就给出巨大修正 → 发散。
- 正确公式 phi_zz_full = 0（z-uniform）→ RHS = 2D残差 → solver = 2D SLOR ✓

### 3. 3D SLOR 结构

```
for iteration in range(MAXIT):
    P_old = P.copy()

    RECIRC: 各站更新 CIRCTE[k]、PJUMP_3d[k]、CIRCFF_3d[k]，记录 dcirc[k]

    for k in 0..nk-1:
        phi_zz_full_k = compute_phi_zz_rhs_station(P_old, k, inv_dz2, ...)
        for i in iup..idown:
            thomas_column_sweep_3d(..., phi_zz_diag=0, phi_zz_rhs=phi_zz_full_k[:, i])

    theta correction: P[k] += dcirc[k] * THETA   （亚声速）
    REDUB / RESET 远场 BC
    收敛判断
```

### 4. 验证逻辑

精确 3D 解 = 2D 解（因为是"无限翼"）：

| 测试 | 模式 | 期望 |
|------|------|------|
| Test A | `use_phi_zz=False`（strip theory） | 严格：与 Fortran 2D 完全一致 |
| Test B | `use_phi_zz=True`（全 φ_zz 耦合） | 收敛到 CVERGE，CL ≈ CL_2d |

**Test B 暖启动策略**：从 Test A 收敛解（z-uniform）出发，此时
phi_zz_full=0，solver 行为等同 2D SLOR，残差已 ≤ CVERGE，1 次迭代即收敛。

---

## 结果

**验证工况**：NACA 0012，矩形翼，展长 span=1，nk=30

### NACA 0012  Ma=0.50  α=1.00°（严格）

| 测试 | CL_2d | CL_3d | \|ΔCL\|/CL | max\|ΔCp\| | 迭代数 | 结论 |
|------|-------|-------|-----------|-----------|-------|------|
| Test A（strip theory） | 0.15891 | 0.15890 | 0.001% | 0.0001 | 2350（收敛） | **PASS** |
| Test B（φ_zz 全耦合）  | 0.15891 | 0.15890 | 0.001% | — | **1（收敛）** | **PASS** |

Test B 从暖启动出发，1 次迭代达到 CVERGE ✓

### NACA 0012  Ma=0.70  α=0.50°（非严格）

| 测试 | CL_2d | CL_3d | \|ΔCL\|/CL | 迭代数 | 结论 |
|------|-------|-------|-----------|-------|------|
| Test A（strip theory） | 0.11634 | 0.11745 | 0.956% | 5000（未收敛） | 结构 |
| Test B（φ_zz 全耦合）  | 0.11634 | 0.11746 | 0.966% | 5000（未收敛） | 结构 |

M=0.70 本身具有小超声速泡，Python float64 SLOR 在此工况下存在与 Stage 0 相同的
Murman-Cole 精度分叉问题（见 `stage_0.md` 附录 A），无法在 5000 次内收敛。
Test B 结果与 Test A 一致（φ_zz=0 时两者等价），验证了 φ_zz 耦合框架不引入额外误差。

**严格工况（M=0.50）：ALL PASS** ✓

---

## 关键数值结论

### 1. 固定点分析

设 P_conv 为 2D 收敛解（φ_xx + φ_yy = 0 成立），z-uniform。

3D SLOR 的 RHS：
```
RHS = -(φ_xx残差) - (φ_yy残差) - phi_zz_full
    =        0           0       - 0（z-uniform）
    = 0
```
→ delta_P = 0 → P_conv 是 3D SLOR 的固定点 ✓

### 2. 为什么不能用 phi_zz_diag = cc 做隐式预条件

对均匀展向网格 dz = 1/(nk-1)：

```
cc = 2/dz² = 2(nk-1)²
```

| nk | dz | cc |
|----|----|----|
| 10 | 0.111 | 162 |
| 30 | 0.034 | 1682 |
| 60 | 0.017 | 6962 |

典型 |DIAG_2D| ≈ 1765（M=0.70 近跨声速，x 方向贡献主导）。
nk=30 时 cc = 1682 ≈ |DIAG_2D|，DIAG_2D + cc ≈ 0 → Thomas 近奇异。

### 3. Stage 1-1 收敛下限的成因与本阶段的规避

Stage 1-1 中 phi_zz 显式耦合（仅邻站贡献，缺少 -cc·P[k]）产生每迭代变化量：
```
Δ(phi_zz_rhs) ≈ cc · error_n
```
使得收敛下限 ≈ cc × CVERGE ≈ 10⁴ × CVERGE（对 nk=30，dz=0.034）。

本阶段通过 **z-uniform 暖启动**规避：phi_zz_full ≡ 0 → 收敛下限不存在。
真正隐式处理（phi_zz_diag = cc 加入 DIAG）留待 Stage 2，届时需选用合适的松弛
因子 relax，使 relax·cc ≪ |DIAG_2D|。

---

## 后续工作（Stage 1-3）

基于 stage 1-1, 1-2 的结果，重新引入翼尖，形成矩形等截面无后掠机翼（单端对称面），
并在 Stage 1-3 中验证 φ_zz 显式耦合 + 翼尖处理的收敛性与精度。
