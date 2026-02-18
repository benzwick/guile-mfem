# ADR-0009: Example and Application Coding Style

## Status

Accepted

## Context

Current `examples/ex0.scm` uses nested `let*` with `_` throwaway bindings to
sequence side-effecting MFEM calls. This is functional in the technical sense
but not idiomatic Scheme — the `_` bindings are noise, the nesting is deep,
and you can't easily evaluate individual steps at a REPL. The question: what
style should guile-mfem examples use, and how does that style scale to complex
applications?

### Approach A: Top-level defines

The simplest change: flatten `let*` to top-level `define` + bare expressions.

```scheme
;; MFEM Example 0 — Poisson: -Delta u = 1, u|dOmega = 0
(use-modules (oop goops) (mfem) (ice-9 getopt-long))

;; 1. Parse command line
(define option-spec
  '((mesh  (single-char #\m) (value #t))
    (order (single-char #\o) (value #t))))
(define options (getopt-long (command-line) option-spec))
(define mesh-file (option-ref options 'mesh "../data/star.mesh"))
(define order (string->number (option-ref options 'order "1")))

;; 2. Mesh
(define mesh (make <Mesh> mesh-file 1 1))
(UniformRefinement mesh)

;; 3. Finite element space
(define fec (make <H1-FECollection> order (Dimension mesh)))
(define fespace (make <FiniteElementSpace> mesh fec))
(format #t "Number of unknowns: ~a~%" (GetTrueVSize fespace))

;; ...
```

**Pros:**
- Most REPL-friendly — paste any block, inspect any variable (`mesh`,
  `fespace`, `x`)
- Closest to the C++ original (almost 1:1 correspondence)
- No Scheme-specific abstractions to learn — accessible to MFEM users new to
  Guile
- No `_` throwaway bindings; side effects are just bare expressions
- Zero infrastructure needed — works today

**Cons:**
- All names in top-level scope (fine for scripts/examples, not for libraries)
- Not reusable as-is — can't call with different parameters without `load`ing
  again
- Mutation-heavy (inherent to MFEM's C++ API, not really avoidable)

**Interactivity:** Excellent. At the REPL you'd build up the simulation step
by step:

```scheme
> (define mesh (make <Mesh> "../data/star.mesh" 1 1))
> (UniformRefinement mesh)
> (Dimension mesh)
$1 = 2
> (define fec (make <H1-FECollection> 1 (Dimension mesh)))
> ;; experiment with different orders, inspect objects, etc.
```

### Approach B: Wrap in a procedure, return values

```scheme
(define (solve-poisson mesh-file order)
  "Solve -Delta u = 1 with u|dOmega = 0.  Returns (values mesh x)."
  (define mesh (make <Mesh> mesh-file 1 1))
  (UniformRefinement mesh)
  (define fec (make <H1-FECollection> order (Dimension mesh)))
  (define fespace (make <FiniteElementSpace> mesh fec))
  ;; ... same body as Approach A ...
  (RecoverFEMSolution a X b x)
  (values mesh x))

;; Script use:
(let-values (((mesh x) (solve-poisson mesh-file order)))
  (Save x "sol.gf")
  (Save mesh "mesh.mesh"))

;; REPL use:
(define-values (mesh x) (solve-poisson "../data/star.mesh" 2))
```

Note: Guile allows `define` inside procedure bodies (internal definitions), so
this looks identical to Approach A but wrapped in a
`(define (solve-poisson ...) ...)`.

**Pros:**
- Reusable: call multiple times with different meshes/orders
- Encapsulated: internal names don't leak to top level
- Still close to C++ (body is the same)

**Cons:**
- Slightly less interactive — you run the whole thing or nothing (can't easily
  stop mid-way)
- Internal `define`s are Guile-specific (R5RS only allows them at the start of
  a body)

**Scaling:** For larger examples, procedures compose well:

```scheme
(define (setup-mesh file)    ...)
(define (setup-fespace mesh) ...)
(define (assemble-system fespace) ...)
(define (solve-and-save mesh fespace system) ...)
```

### Approach C: Convenience layer (future)

Higher-level wrappers that encode common MFEM patterns:

```scheme
(define mesh (make <Mesh> "../data/star.mesh" 1 1))
(UniformRefinement mesh)
(define fespace (h1-space mesh #:order 1))

(define b (linear-form fespace
            (domain-integrator (make <ConstantCoefficient> 1.0))))
(define a (bilinear-form fespace
            (domain-integrator (make <DiffusionIntegrator>))))

(define x (solve-system a b
            #:ess-bdr (boundary-dofs fespace)
            #:solver 'pcg #:precond 'gauss-seidel))

(Save x "sol.gf")
(Save mesh "mesh.mesh")
```

Where `h1-space`, `linear-form`, `bilinear-form`, `solve-system` are thin
Scheme wrappers defined in a `(mfem convenience)` module.

**Pros:**
- Concise — ex0 shrinks from ~40 lines to ~15
- Reads like a problem description rather than API calls
- Keyword arguments make intent clear (`#:order 1`, `#:solver 'pcg`)

**Cons:**
- Requires building and maintaining a convenience layer on top of MFEM bindings
- Premature until more examples are ported and patterns stabilize
- Users still need raw MFEM API for anything the wrappers don't cover
- Two APIs to document and maintain

**Scaling to multiphysics:** This is where a DSL-like approach shines:

```scheme
(define thermal
  (physics #:mesh mesh
           #:fespace (h1-space mesh #:order 2)
           #:bilinear (domain-integrator (make <DiffusionIntegrator> kappa))
           #:rhs (domain-integrator (make <DomainLFIntegrator> heat-source))))

(define structural
  (physics #:mesh mesh
           #:fespace (h1-space mesh #:order 1 #:vdim 3)
           #:bilinear (domain-integrator (make <ElasticityIntegrator> lambda mu))))

(couple! thermal structural #:transfer 'l2-projection)
(time-step! (list thermal structural) #:dt 0.01 #:t-final 1.0)
```

But this is a significant framework effort — closer to building a
domain-specific simulation language than wrapping MFEM.

## Decision

**Start with Approach A** for all examples now. It's honest, requires nothing
extra, and mirrors MFEM's C++ examples closely (which is the primary
documentation). Approach B can be used where it makes sense (solver utilities,
reusable setup functions).

**Grow toward Approach C** organically: as more examples are ported (ex1
through ex5+), common patterns will emerge. Extract helpers only when the same
boilerplate appears in 3+ examples.

The roadmap for production-scale applications:

1. Raw bindings (what we have) — for power users and custom physics
2. Convenience wrappers (Approach C) — for common workflows
3. Physics framework (Approach C, extended) — for multiphysics coupling

Each layer builds on the previous one, and users can drop down to any level.

## Consequences

- Examples read like transliterated C++, lowering the barrier for MFEM users
  coming from C++.
- Every variable is available at the REPL after pasting a block, which helps
  new users explore.
- Examples are not reusable as library functions. This is acceptable because
  MFEM's own C++ examples are also standalone `main()` programs.
- A future convenience layer (Approach C) can be introduced without changing
  existing examples — it would be a new `(mfem convenience)` module.
