#  MFEM + guile-mfem (FEM library)

This repository provides GNU Guile binding for MFEM. MFEM is a high performance parallel finite element method (FEM) library (http://mfem.org/).

Forked from [PyMFEM](https://github.com/mfem/PyMFEM) and adapted to target Guile 3.0 via SWIG.

## Usage

Solve the Poisson equation $-\Delta u = 1$ with zero Dirichlet boundary
conditions on the star mesh (`examples/ex0-lisp.scm`):

```scheme
(use-modules (oop goops) (mfem))

(let ((mesh (make <Mesh> "star.mesh" 1 1)))
  (UniformRefinement mesh)
  (let ((fespace (make <FiniteElementSpace> mesh
                   (make <H1-FECollection> 1 (Dimension mesh)))))
    (let ((x (make <GridFunction> fespace))
          (b (make <LinearForm> fespace))
          (a (make <BilinearForm> fespace)))
      (Assign x 0.0)
      (AddDomainIntegrator b
        (make <DomainLFIntegrator> (make <ConstantCoefficient> 1.0)))
      (Assemble b)
      (AddDomainIntegrator a (make <DiffusionIntegrator>))
      (Assemble a)
      (let ((bdr (make <intArray>))
            (A (make <OperatorHandle>))
            (B (make <Vector>))
            (X (make <Vector>)))
        (GetBoundaryTrueDofs fespace bdr)
        (FormLinearSystem a bdr x b A X B)
        (PCG (Ptr A) (make <GSSmoother> (OperatorHandle2SparseMatrix A))
          B X 1 200 1e-12 0.0)
        (RecoverFEMSolution a X b x))
      (Save x "sol.gf")
      (Save mesh "mesh.mesh"))))
```

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

## License
guile-mfem is licensed under BSD-3 license. All new contributions
must be made under this license. See [License](LICENSE) for details.

Please refer the developers' web sites for the external libraries
* MFEM: https://mfem.org/
* Hypre: https://computing.llnl.gov/projects/hypre-scalable-linear-solvers-multigrid-methods
* METIS: http://glaros.dtc.umn.edu/gkhome/metis/metis/overview
* libceed: https://github.com/CEED/libCEED
* gslib: https://github.com/Nek5000/gslib
