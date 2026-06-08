# Fortran 函数分类：Python 化决策表

*最后分析日期：2026-06-09*  
*参见 [project_overview.md](project_overview.md) 查看文件结构和求解流程*

---

## 类别 A：适合 Python 化（I/O 和预/后处理）

这些函数不在热循环中，逻辑简单，或已有 NumPy/SciPy 等价实现。

### 已完成（已经 Python 化）

| 函数 | 原 Fortran 位置 | Python 等价 |
|---|---|---|
| `BODY` | solver_functions（已删除） | `compute_geometry_derivatives()` |
| `CDCOLE` | solver_base.f90:325 | `cdcole_python()` pytsfoil.py:844 |
| 所有文件输出 | io_module.f90 | `output_field()`, `output_shock()` |
| 网格生成 | 原 MESH 子程序 | `set_mesh()`, `clustcos()` |
| 翼型读取 | 原 BODY/READ 部分 | `set_airfoil()` |

### 建议 Python 化（下一步目标）

#### 1. `io_module.f90` — 全部 3 个子程序
- `open_output_file()`, `open_summary_file()`, `close_output_files()`
- **理由**：纯文件 I/O，Python 的 `open()`/`with` 更安全，且 Python 侧已管理输出目录
- **做法**：在 Python 侧统一管理文件句柄；Fortran 侧的 WRITE 语句改为接受从 Python 传入的 unit，或完全移除 Fortran 文件写出

#### 2. `INPERR()` / `report_convergence_error()` — common_data.f90
- **理由**：错误处理应用 Python 异常（`raise ValueError`），不应由 Fortran `STOP` 强制终止进程
- **做法**：Fortran 侧改为返回错误码（整数）给 Python，Python 侧检查并抛出异常

#### 3. `SCALE()` — solver_functions.f90:24
- **理由**：纯数学变换（~100 行公式），只调用一次，无性能问题
- **注意**：计算 `CPFACT/CLFACT/CDFACT/YFACT/VFACT` 等系数，结果写入 Fortran common；若 Python 化需同步写回 Fortran 模块变量

#### 4. `EMACH1()` — solver_functions.f90:527
- **理由**：仅在后处理中调用（`output_field` 循环），已从 Python 调用，公式简单
- **做法**：直接在 Python 中实现公式，去掉 f2py 调用开销

#### 5. `LIFT()`, `DRAG()`, `PITCH()` — solver_base.f90:248-320
- **理由**：只在后处理调用一次，逻辑简单（积分+乘系数）
- **注意**：需访问 P 数组和 FD 系数，可在 Python 中直接读取 `tsf.solver_data.p`

#### 6. `SIMP()`, `TRAP()` — math_module.f90
- **理由**：可用 `scipy.integrate.simpson` / `numpy.trapz` 完全替代
- **注意**：CDCOLE 已 Python 化，这两个函数目前在 Fortran 侧仅被 CDCOLE 和后处理使用

#### 7. `ANGLE()` — solver_base.f90:220
- **理由**：预处理，只调用一次，简单角度公式
- **做法**：Python 中用 `np.arctan2` 实现，结果通过 f2py 写回 `tsf.solver_data.theta`

---

## 类别 B：保留 Fortran（数值求解核心，Python 化后效率极低）

这些函数在主迭代循环内部，网格规模通常 100×100 = 10,000 点，迭代次数 100~1000 次，总操作量 $10^6 \sim 10^7$。用纯 Python 循环慢 100× 以上。

### 绝对不能 Python 化（热循环核心）

| 函数 | 位置 | 调用频率 | 原因 |
|---|---|---|---|
| `SYOR()` | main_iteration.f90:15 | 每次迭代 1 次 | 内层 SOR 扫描 + 三对角求解，O(N×M) |
| `PX(I,J)` | solver_base.f90:17 | SYOR 内 O(N×M) 次 | 有限差分，最热路径 |
| `PY(I,J)` | solver_base.f90:43 | SYOR 内 O(N×M) 次 | 有限差分 + Kutta 判断，最热路径 |
| `DIFCOE()` | solver_base.f90:111 | 每次 solve 前 1 次 | 预计算所有 FD 系数，内部 O(N×M) |
| `BCEND()` | solver_functions.f90:186 | SYOR 每列调用 | 应用远场 BC，在 SYOR 内部调用 |

### 强烈建议保留 Fortran（迭代控制逻辑）

| 函数 | 位置 | 调用频率 | 原因 |
|---|---|---|---|
| `SOLVE()` | main_iteration.f90:190 | 调用一次但驱动整个循环 | 迭代框架 + 收敛判断，含对 SYOR 的调用 |
| `RECIRC()` | main_iteration.f90:399 | 每次迭代 1 次 | 环量边界更新，紧耦合 SYOR |
| `REDUB()` | main_iteration.f90:440 | 每 25 次迭代 1 次 | 偶极子强度积分，在循环内 |
| `RESET()` | main_iteration.f90:496 | 每次迭代 1 次 | 更新远场边界值，在循环内 |

---

## 类别 C：修正函数分析（在 Fortran 还是 Python 中修正）

### 已有修正（Fortran 侧）

#### 1. 粘性楔修正 `VWEDGE()` + `WANGLE()` — solver_functions.f90:568
- **作用**：激波/边界层干扰修正，修改翼面斜率边界条件（`WSLP` 数组）
- **调用时机**：在 SOLVE 主循环内，每 `NDWDGE` 次迭代调用一次
- **结论**：**保留 Fortran**。`WSLP` 直接影响下一次 SYOR 的边界条件，在 Python 中调用后还要把结果写回 Fortran，往返开销和同步复杂度高
- **若需改进**：可在 Python 层添加前/后处理包装，但 `VWEDGE` 本身留在 Fortran

#### 2. 远场边界修正 `FARFLD()` + `DROOTS()` + `VROOTS()` — solver_functions.f90:316+
- **作用**：自由流 / 洞壁 / 射流等不同边界条件下的偶极子 + 涡势
- **调用时机**：求解前调用一次（`FARFLD`），然后每次迭代通过 `BCEND` 应用
- **结论**：`FARFLD` 可 Python 化（只调一次，无性能要求）；`BCEND` 保留 Fortran（在 SYOR 每列调用）

### 建议新增修正（应在 Python 层实现）

以下修正适合在 Python 层（后处理或预处理）实现，不涉及迭代内部：

| 修正类型 | 建议层次 | 原因 |
|---|---|---|
| 可压缩性修正（Karman-Tsien, Prandtl-Glauert） | Python 后处理 | 对 Cp 做解析修正，不影响迭代 |
| 黏性摩擦阻力（蒙皮摩擦积分） | Python 后处理 | 独立计算，加在 CDCOLE 结果上 |
| 洞壁干扰修正（Garner 方法等） | Python 预处理 | 修正输入参数 EMACH/ALPHA 后再求解 |
| 激波损失修正（熵增） | Python 后处理 | 已在 `cdcole_python` 中部分实现 |
| 翼型几何扰动（鼓包/舵面偏转） | Python 预处理 | 在 `compute_geometry_derivatives` 中添加 |

---

## 总结决策表

```
函数                   Python化  性能敏感  在迭代内  建议
──────────────────────────────────────────────────────────
SOLVE / SYOR           否        ★★★★★    是        保留Fortran（核心）
PX / PY                否        ★★★★★    是        保留Fortran（最热路径）
DIFCOE                 否        ★★★      否        保留Fortran（可接受）
BCEND                  否        ★★★      是        保留Fortran（在SYOR内）
RECIRC/REDUB/RESET     否        ★★       是        保留Fortran（迭代内）
VWEDGE/WANGLE          否        ★★       是        保留Fortran（修正+迭代耦合）
──────────────────────────────────────────────────────────
SCALE                  是        ★        否        可Python化（调一次）
FARFLD                 是        ★        否        可Python化（调一次）
ANGLE                  是        ★        否        可Python化（调一次）
EMACH1                 已Python  ★        否        已从Python调
LIFT/DRAG/PITCH        是        ★        否        建议Python化
SIMP/TRAP              是        ★        否        用scipy替代
──────────────────────────────────────────────────────────
io_module（全部）       已计划    无        否        应移至Python
INPERR/convergence     已计划    无        否        改为Python异常
CDCOLE                 已Python  ★★       否        已完成
──────────────────────────────────────────────────────────
```
