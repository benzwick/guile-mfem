#  MFEM + guile-mfem (FEM library)

This repository provides GNU Guile binding for MFEM. MFEM is a high performance parallel finite element method (FEM) library (http://mfem.org/).

Forked from [PyMFEM](https://github.com/mfem/PyMFEM) and adapted to target Guile 3.0 via SWIG.

## Install
### Build from source (Serial MFEM)
```shell
$ git clone https://github.com/benzwick/guile-mfem.git
$ cd guile-mfem
$ cmake -B build -DMFEM_DIR=/path/to/mfem
$ cmake --build build -j$(nproc)
```

### Build with additional features (MPI, GPU, GSLIB, libCEED)

```shell
$ cmake -B build -DMFEM_DIR=/path/to/mfem -DGUILE_MFEM_USE_MPI=ON
$ cmake --build build -j$(nproc)
```

#### Cleaning
```shell
$ rm -rf build
```
#### Run test
```shell
cd build && ctest --output-on-failure
```

## Usage
TODO: Guile usage example. Below is the PyMFEM (Python) version for reference.

This example (modified from `ex1.cpp`) solves the Poisson equation,
$$\nabla \cdot (\alpha \nabla u) = f$$
in a square and plots the result using matplotlib.
Use the badge above to open this in Colab.

```python
import mfem.ser as mfem

# Create a square mesh
mesh = mfem.Mesh(10, 10, "TRIANGLE")

# Define the finite element function space
fec = mfem.H1_FECollection(1, mesh.Dimension())   # H1 order=1
fespace = mfem.FiniteElementSpace(mesh, fec)

# Define the essential dofs
ess_tdof_list = mfem.intArray()
ess_bdr = mfem.intArray([1]*mesh.bdr_attributes.Size())
fespace.GetEssentialTrueDofs(ess_bdr, ess_tdof_list)

# Define constants for alpha (diffusion coefficient) and f (RHS)
alpha = mfem.ConstantCoefficient(1.0)
rhs = mfem.ConstantCoefficient(1.0)

"""
Note
-----
In order to represent a variable diffusion coefficient, you
must use a numba-JIT compiled function. For example:

>>> @mfem.jit.scalar
>>> def alpha(x):
>>>     return x+1.0
"""

# Define the bilinear and linear operators
a = mfem.BilinearForm(fespace)
a.AddDomainIntegrator(mfem.DiffusionIntegrator(alpha))
a.Assemble()
b = mfem.LinearForm(fespace)
b.AddDomainIntegrator(mfem.DomainLFIntegrator(rhs))
b.Assemble()

# Initialize a gridfunction to store the solution vector
x = mfem.GridFunction(fespace)
x.Assign(0.0)

# Form the linear system of equations (AX=B)
A = mfem.OperatorPtr()
B = mfem.Vector()
X = mfem.Vector()
a.FormLinearSystem(ess_tdof_list, x, b, A, X, B)
print("Size of linear system: " + str(A.Height()))

# Solve the linear system using PCG and store the solution in x
AA = mfem.OperatorHandle2SparseMatrix(A)
M = mfem.GSSmoother(AA)
mfem.PCG(AA, M, B, X, 1, 200, 1e-12, 0.0)
a.RecoverFEMSolution(X, b, x)

# Extract vertices and solution as numpy arrays
verts = mesh.GetVertexArray()
sol = x.GetDataArray()

# Plot the solution using matplotlib
import matplotlib.pyplot as plt
import matplotlib.tri as tri

triang = tri.Triangulation(verts[:,0], verts[:,1])

fig, ax = plt.subplots()
ax.set_aspect('equal')
tpc = ax.tripcolor(triang, sol, shading='gouraud')
fig.colorbar(tpc)
plt.show()
```
![](https://raw.githubusercontent.com/mfem/PyMFEM/master/docs/example_image.png)


## License
guile-mfem is licensed under BSD-3 license. All new contributions
must be made under this license. See [License](LICENSE) for details.

Please refer the developers' web sites for the external libraries
* MFEM: https://mfem.org/
* Hypre: https://computing.llnl.gov/projects/hypre-scalable-linear-solvers-multigrid-methods
* METIS: http://glaros.dtc.umn.edu/gkhome/metis/metis/overview
* libceed: https://github.com/CEED/libCEED
* gslib: https://github.com/Nek5000/gslib
