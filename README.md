# Guile-MFEM: GNU Guile bindings for MFEM

[![Build and Test](https://github.com/benzwick/guile-mfem/actions/workflows/build-and-test-dispatch.yml/badge.svg)](https://github.com/benzwick/guile-mfem/actions/workflows/build-and-test-dispatch.yml)

Guile-MFEM brings [MFEM](https://mfem.org/),
a high-performance finite element method (FEM) library,
to GNU Guile 3.0 via SWIG.
Forked from [PyMFEM](https://github.com/mfem/PyMFEM).

## Usage

This example solves the Poisson problem $-\Delta u = 1$
with zero Dirichlet boundary conditions
(from `examples/ex0-lisp.scm`):

```scheme
(use-modules (oop goops) (mfem) (ice-9 getopt-long))

(define options (getopt-long (command-line)
                 '((mesh  (single-char #\m) (value #t))
                   (order (single-char #\o) (value #t)))))
(define mesh-file (option-ref options 'mesh "../data/star.mesh"))
(define order (string->number (option-ref options 'order "1")))

(let ((mesh (make <Mesh> mesh-file 1 1)))
  (UniformRefinement mesh)
  (let ((fespace (make <FiniteElementSpace> mesh
                   (make <H1-FECollection> order (Dimension mesh)))))
    (format #t "Number of unknowns: ~a~%" (GetTrueVSize fespace))
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

Run it:

```shell
$ guile -L build examples/ex0-lisp.scm
$ guile -L build examples/ex0-lisp.scm -- -m data/fichera.mesh -o 2
```

## Install

### Build from source
```shell
$ git clone https://github.com/benzwick/guile-mfem.git
$ cd guile-mfem
$ cmake -B build -DMFEM_DIR=/path/to/mfem
$ cmake --build build -j$(nproc)
```

Optional features: `-DGUILE_MFEM_USE_MPI=ON`, `-DGUILE_MFEM_USE_CUDA=ON`,
`-DGUILE_MFEM_USE_GSLIB=ON`, `-DGUILE_MFEM_USE_LIBCEED=ON`.

### Run tests
```shell
$ cd build
$ ctest --output-on-failure
```

## License

Guile-MFEM is licensed under BSD-3. See [LICENSE](LICENSE) for details.

## Related projects

* [MFEM](https://mfem.org/)
* [PyMFEM](https://github.com/mfem/PyMFEM)
* [Hypre](https://computing.llnl.gov/projects/hypre-scalable-linear-solvers-multigrid-methods)
* [METIS](http://glaros.dtc.umn.edu/gkhome/metis/metis/overview)
* [libCEED](https://github.com/CEED/libCEED)
* [gslib](https://github.com/Nek5000/gslib)
