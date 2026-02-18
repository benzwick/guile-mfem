//
// Copyright (c) 2020-2025, Princeton Plasma Physics Laboratory, All rights reserved.
//
%module nonlininteg
%insert("goops") %{(use-modules (mfem array) (mfem eltrans) (mfem fe) (mfem fespace) (mfem integrator) (mfem intrules) (mfem operators) (mfem vector))%}
%{
#include "mfem.hpp"
using namespace mfem;
%}

%include "exception.i"
%import "vector.i"
%import "operators.i"
%import "fespace.i"
%import "eltrans.i"
%import "integrator.i"

%include "fem/nonlininteg.hpp"

