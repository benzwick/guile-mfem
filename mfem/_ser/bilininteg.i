//
// Copyright (c) 2020-2025, Princeton Plasma Physics Laboratory, All rights reserved.
//
%module bilininteg
%insert("goops") %{(use-modules (mfem array) (mfem densemat) (mfem eltrans) (mfem fe) (mfem fespace) (mfem integrator) (mfem intrules) (mfem nonlininteg) (mfem operators) (mfem sparsemat) (mfem vector))%}
%{
#include "mfem.hpp"
%}

%include "exception.i"

%import "globals.i"
%import "array.i"
%import "coefficient.i"
%import "matrix.i"
%import "vector.i"
%import "gridfunc.i"
%import "fespace.i"
%import "fe_coll.i"
%import "intrules.i"
%import "densemat.i"
%import "sparsemat.i"
%import "lininteg.i"
%import "eltrans.i"
%import "linearform.i"
%import "fe.i"
%import "nonlininteg.i"

%ignore  mfem::MassIntegrator::SetupPA;

// Ignore overridden methods that lost default args in derived classes
%ignore mfem::TransposeIntegrator::AssembleEAInteriorFaces;
%ignore mfem::SumIntegrator::AssembleEAInteriorFaces;
%ignore mfem::DGTraceIntegrator::AssembleEAInteriorFaces;

%include "../common/kernel_dispatch.i"
%include "fem/bilininteg.hpp"

