# pyTSFoil 项目概览（Fortran 分支）

pyTSFoil 是 TSFOIL2 的 Python 封装，TSFOIL2 是用于二维翼型跨声速小扰动（TSD）流动分析的经典 CFD 求解器。Fortran 代码通过 f2py 编译为 `.so` 扩展（`tsfoil_fortran`），再由 `pytsfoil/pytsfoil.py` 调用。

**分支目标：**
1. 整理 Fortran 代码，提升可读性
2. 用 Python 替换 Fortran 的 I/O 函数
3. 添加修正函数以提高结果精度

---

## Fortran 源文件结构（pytsfoil/src/）

| 文件 | 作用 |
|---|---|
| `common_data.f90` | 全局参数、网格、翼型数组、错误处理 |
| `solver_data.f90` | 求解器数组：势场 P、有限差分系数、边界值 |
| `math_module.f90` | SIMP（Simpson 积分）、TRAP（梯形积分） |
| `solver_base.f90` | PX/PY 有限差分、DIFCOE、LIFT/DRAG/PITCH/CDCOLE |
| `solver_functions.f90` | SCALE、SETBC、BCEND、FARFLD、EMACH1、VWEDGE、DROOTS/VROOTS |
| `main_iteration.f90` | SOLVE（外层循环）、SYOR（SOR 扫描）、RECIRC、REDUB、RESET |
| `io_module.f90` | 打开/关闭 tsfoil2.out 和 smry.out |

---

## 已完成的 Python 化（在 pytsfoil.py 中）

| 原 Fortran 函数 | Python 等价 | 位置 |
|---|---|---|
| `BODY` | `compute_geometry_derivatives()` | pytsfoil.py:471 |
| `CDCOLE` | `cdcole_python()` | pytsfoil.py:844 |
| 所有文件输出 | `output_field()`, `output_shock()` | pytsfoil.py:595, 716 |
| 网格生成 | `set_mesh()`, `clustcos()` | pytsfoil.py:302 |
| 翼型读取/分离 | `set_airfoil()` | pytsfoil.py:250 |

---

## 求解流程

```
1. 初始化
   └─ initialize_common() / initialize_solver_data()  [Fortran]

2. 几何设置
   └─ set_airfoil()                 [Python]
   └─ set_mesh()                    [Python]
   └─ compute_mesh_indices()        [Python]

3. 求解
   └─ SCALE()                       [Fortran] — 相似变量变换
   └─ FARFLD()                      [Fortran] — 远场边界条件
   └─ compute_geometry_derivatives() [Python]  — 翼面斜率（替代 BODY）
   └─ DIFCOE()                      [Fortran] — 有限差分系数预计算
   └─ SETBC()                       [Fortran] — 设置翼面边界条件
   └─ SOLVE()                       [Fortran] — 主迭代（SOR）
        ├─ RECIRC() — 环量更新
        ├─ SYOR()   — 一次 SOR 扫描（内层，最热路径）
        ├─ REDUB()  — 偶极子强度更新（每25次）
        └─ RESET()  — 远场边界刷新

4. 后处理
   └─ LIFT(), PITCH()               [Fortran] — 升力/俯仰力矩系数
   └─ output_shock()                [Python]  — 表面 Cp/Ma 输出
   └─ output_field()                [Python]  — 全场数据输出
   └─ cdcole_python()               [Python]  — 动量积分阻力
```

---

*详细函数分类见 [function_classification.md](function_classification.md)*
