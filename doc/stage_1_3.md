# 阶段 1-3：矩形等截面无后掠机翼（单端对称面 + 翼尖）

基于 stage 1-1、1-2 的结果，重新引入翼尖，形成矩形等截面无后掠机翼（根部对称面），
并在 Stage 1-3 中验证 φ_zz 显式耦合 + 翼尖边界条件的收敛性与精度。

---

## 目标

- 机翼区域 k=0（根部对称面）到 k=k_tip（翼尖）施加壁面 BC + Kutta 条件
- 翼外区域（k > k_tip）无翼面 BC，满足无来流 Laplace 方程
- 展向远场（k=nk-1）：Dirichlet φ=0
- 使用 **全 φ_zz 显式残差**（同 Stage 1-2），消除 Stage 1-1 收敛下限问题
- 验证有限展长效应：CL_tip < CL_2d

**测试工况**：AR=6（弦长 1，半展 3），nk=30，z_max=9（= 3 × half_span）

---

## 文件结构

```
stage_1_3/
  __init__.py
  numba_kernels.py   # thomas_column_sweep_3d（apply_wall_bc 参数）
                     # compute_phi_zz_rhs_station（Dirichlet 远场，vs Stage 1-2 Neumann）
  solver.py          # build_spanwise_grid, solve_3d（k_tip，off-wing BCs，RESET）
  postprocess.py     # surface_cp_3d_station，spanwise_cl（仅积分 k=0..k_tip）
  run_stage1.py      # 验证脚本
```

---

## 实现要点

### 1. 展向网格与边界条件

| 区域 | k 范围 | 边界条件 |
|------|--------|---------|
| 根部对称面 | k=0 | φ_z=0，ghost P[-1]=P[1] |
| 机翼区域 | k=1..k_tip | 壁面 BC + Kutta（apply_wall_bc=True） |
| 翼外区域 | k_tip+1..nk-2 | 无翼面 BC（apply_wall_bc=False），纯 Laplace |
| 展向远场 | k=nk-1 | Dirichlet φ=0，phantom P[nk]=0 |

```
z_max = z_max_factor × half_span = 3×3 = 9
dz = z_max / (nk-1) = 9/29 ≈ 0.310
k_tip = argmin|eta - half_span|  （AR=6, nk=30 → k_tip=10，z_tip≈3.10）
```

**与 Stage 1-2 的关键差别**：
- Stage 1-2：k=nk-1 用 Neumann（对称面），精确解 φ_zz=0（无限翼）
- Stage 1-3：k=nk-1 用 Dirichlet（远场 φ=0）；k>k_tip 无壁面 BC

### 2. φ_zz 处理（同 Stage 1-2：全残差，phi_zz_diag=0）

```
phi_zz_full = inv_dz2 × (P[k-1] - 2P[k] + P[k+1])

k=0       : phi_zz = 2 × inv_dz2 × (P[1] - P[0])         （Neumann ghost）
0<k<nk-1  : phi_zz = inv_dz2 × (P[k-1] - 2P[k] + P[k+1])
k=nk-1    : phi_zz = inv_dz2 × (P[nk-2] - 2P[nk-1])       （Dirichlet P[nk]=0）

RHS[j] -= phi_zz_full[j, i]
DIAG[j] 不变（phi_zz_diag = 0）
```

**cc = 2/dz² 对本阶段的数值**：dz=0.310 → cc ≈ 20.8（远小于 Stage 1-2 的 cc=1682）

### 3. 关键参数变化 vs Stage 1-2

| 参数 | Stage 1-2 | Stage 1-3 |
|------|-----------|-----------|
| span | 1.0（两端对称） | 3.0（半展，根对称+翼尖） |
| z_max | 1.0 | 9.0 |
| nk | 30 | 30 |
| dz | 0.034 | 0.310 |
| cc = 2/dz² | 1682 | 20.8 |
| z-Jacobi 谱半径 ρ | cos(π/58)≈0.9985 | cos(π/58)≈0.9985 |
| 精确解 | φ_zz=0（无限翼） | 有限展长，有翼尖卸载 |

### 4. 翼外区域处理

- RECIRC：k>k_tip 的站点 CIRCTE=0，CIRCFF=0，PJUMP=0（零环量）
- SYOR：apply_wall_bc=False → 跳过壁面/Kutta BC 注入
- RESET：k>k_tip 的 x/y 远场 BC = 0（CIRCFF=0，DUB=0）
- 每步强制 P[nk-1]=0（展向 Dirichlet）

---

## 测试结构

| 测试 | 模式 | 期望 |
|------|------|------|
| Test A | use_phi_zz=False（strip theory） | 各站 = 2D，CL_root ≈ CL_2d |
| Test B | use_phi_zz=True（全 φ_zz 耦合） | 收敛；CL_tip < CL_2d（翼尖卸载） |

---

## Test A 结果（PASS）

strip theory（各站独立，φ_zz=0），结果与 Fortran 2D 一致：

### NACA 0012  Ma=0.50  α=1.00°（严格）

| CL_fort | CL_root | \|ΔCL\|/CL | max\|ΔCp\| | 迭代数 | 结论 |
|---------|---------|-----------|-----------|-------|------|
| 0.15891 | 0.15890 | 0.001% | 0.0001 | 2350（收敛） | **PASS** |

### NACA 0012  Ma=0.70  α=0.50°（非严格）

| CL_fort | CL_root | \|ΔCL\|/CL | max\|ΔCp\| | 迭代数 | 结论 |
|---------|---------|-----------|-----------|-------|------|
| 0.11634 | 0.11745 | 0.956% | 0.4436 | 5000（未收敛） | 结构（同 Stage 0） |

---

## Test B 调试历程（进行中）

### 问题根源分析

Test B 暖启动：机翼站点 P[k≤k_tip]=P_2D，翼外 P[k>k_tip]=0。

此时翼尖站点的 φ_zz：

```
phi_zz[k_tip] = inv_dz2 × (P[k_tip-1] - 2P[k_tip] + P[k_tip+1])
              = inv_dz2 × (P_2D - 2P_2D + 0)
              = -P_2D / dz²  ≈  -3.1    （|P_2D|~0.1, dz=0.31, inv_dz2≈10.4）
```

该 φ_zz 通过 SYOR 在翼尖站点产生大修正 ΔP，特别是在后缘 (i=ite) 处改变 CIRCTE，
随后 **theta 修正步** `P += dcirc × THETA` 将 CIRCTE 的变化放大到整个流场 → 发散。

**核心不稳定机制**：
```
φ_zz 扰动 → ΔP[k_tip, j, ite] → ΔCIRCTE → theta 放大 → 更大扰动 → 正反馈
```

Stage 1-1 中也遇到了相同的 theta 放大问题（记录在 `stage_1_1_fail.md`）。

### 尝试修复记录

#### 尝试 1：机翼内余弦过渡（alpha=cos(π/2·k/k_tip) for k≤k_tip）

- alpha 在 k=0 为 1.0，k_tip 为 0.0，翼外为 0
- phi_zz[k_tip] = inv_dz2 × P[k_tip-1]（k_tip-1 站仍为 ~0.174·P_2D）
- 结果：iter=1 error=4.51e-02，iter=14 发散（error=50.16）
- 原因：alpha=0 at k_tip 时，phi_zz[k_tip] 仍从 k_tip-1 得到贡献，并不为零

#### 尝试 2：机翼均匀 + 翼外余弦过渡（alpha=1 for k≤k_tip，余弦衰减 for k>k_tip）

- phi_zz[k_tip] = inv_dz2×(alpha[k_tip+1]-1)×P_2D ≈ -0.034·P_2D（减小约30倍）
- iter=1 error=1.06e-01，iter=9 发散
- 原因：翼外站点初始化为 alpha·P_2D，SYOR 驱动其向无翼面 Laplace 方程收敛时，
  对翼尖站产生快速变化的 φ_zz，仍触发 theta 正反馈

#### 尝试 3：φ_zz 缓入（0→1 线性 ramp，RAMP=500 迭代）

- 暖启动：机翼 P=P_2D，翼外 P=0
- 缩放：phi_zz_rhs × min(iter/500, 1.0)
- iter=1 error=5.81e-04（显著降低），iter=210 发散（error=23.70）
- 原因分析：
  - 2D 收敛在 ~2350 迭代内完成，z-Jacobi 收敛需 ~3500+ 迭代（ρ=0.9985）
  - 2D 收敛"超前"：机翼各站接近 P_2D，但翼外 P[k_tip+1] 仍近零
  - 随 ramp 增大，有效 phi_zz[k_tip] 持续增大，dcirc 增大，theta 放大 → 发散

#### 尝试 4：禁用 3D 模式下的 theta 修正

- 修改 solver.py：`if AK >= 0.0 and not (use_phi_zz and nk > 1):` 跳过 theta 步
- 结果：iter=1 error=2.91e-01，iter=6 仍发散（error=75.43）
- 原因：此次无 φ_zz ramp，iter=1 的大误差直接来自 SYOR 给出的大 φ_zz 修正
  （theta 被禁用但 SYOR 本身的修正量已很大）

### 问题总结

存在两个独立的不稳定因素：

| 问题 | 来源 | 修复思路 |
|------|------|---------|
| 第一步 SYOR 大修正 | P[k_tip+1]=0，phi_zz[k_tip]=-P_2D/dz² | φ_zz 缓入（ramp） |
| theta 正反馈放大 | phi_zz 驱动 CIRCTE 变化，theta 持续放大 | 禁用/削弱 theta |

两者需**同时**处理：φ_zz ramp + 禁用 theta。

**下一步**：重新启用 φ_zz ramp（RAMP=500），同时保持禁用 theta（已在 solver.py），
观察是否收敛。

---

## 数值参数

```python
AR        = 6.0
HALF_SPAN = 3.0
N_K       = 30
Z_MAX_FAC = 3.0     # z_max = 9
MAXIT_3D  = 15000   # z-Jacobi 谱半径 ≈0.9985，需 ~3500+ 迭代

# 展向 z-Jacobi 分析（Neumann-Dirichlet BCs）
ρ ≈ cos(π / (2×(nk-1))) = cos(π/58) ≈ 0.9985
# 收敛 3 个数量级所需迭代数：
N_conv ≈ ln(1000) / ln(1/0.9985) ≈ 4609
```

---

## 后续工作

1. φ_zz ramp（0→1，500 迭代）+ 禁用 theta → 验证是否稳定收敛
2. 若稳定：验证 CL_tip < CL_2d（有限展长效应）
3. 检查 M=0.70 工况（非严格，结构验证）
4. 更新结果图表与本文档
