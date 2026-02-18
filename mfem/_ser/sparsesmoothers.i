//
// Copyright (c) 2020-2025, Princeton Plasma Physics Laboratory, All rights reserved.
//
%module sparsesmoothers
%insert("goops") %{(use-modules (mfem matrix) (mfem operators) (mfem vector))%}
%{
#include "linalg/sparsesmoothers.hpp"
%}

%include "exception.i"
%import "vector.i"
%import "operators.i"
%import "sparsemat.i"
%import "matrix.i"
%import "../common/exception.i"

%include "linalg/sparsesmoothers.hpp"
