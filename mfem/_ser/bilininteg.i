//
// Copyright (c) 2020-2025, Princeton Plasma Physics Laboratory, All rights reserved.
//
%module bilininteg
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

%include "../common/kernel_dispatch.i"
%include "fem/bilininteg.hpp"

