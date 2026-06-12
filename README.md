# Branch task

Continue from branch 'fortran', use the refactored Fortran source code and the new Python code.

Task 1: 't-c-aoa' decomposition of TSD (good)

Task 2: velocity limiter in Fortran SOR iterations (bad)

Task 3: integration boundary layer method coupling (good)

Task 4: IBL + V-limiter (bad)

Task 5: IBL + MAE LE correction (bad)

Task 6: IBL + TE correction (good)

**Conclusion**: MAE, singularity subtraction, velocity limiter are no good.
IBL + TE correction is good. The 't-c-aoa' decomposition of TSD is insightful.
Move to new branch 'correction'.
