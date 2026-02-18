//
// Copyright (c) 2020-2025, Princeton Plasma Physics Laboratory, All rights reserved.
//
%module nonlininteg
%insert("goops") %{(use-modules (array) (eltrans) (fe) (fespace) (integrator) (intrules) (operators) (vector))%}
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

