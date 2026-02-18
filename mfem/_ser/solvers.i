//
// Copyright (c) 2020-2025, Princeton Plasma Physics Laboratory, All rights reserved.
//
%module solvers
%insert("goops") %{(use-modules (mfem operators) (mfem vector))%}

%{
#include "linalg/handle.hpp"
#include "linalg/matrix.hpp"
#include "linalg/sparsemat.hpp"
#include "linalg/solvers.hpp"
%}

%include "../common/mfem_config.i"

%include "exception.i"
%import "globals.i"
%import "vector.i"
%import "operators.i"
%import "matrix.i"
%import "sparsemat.i"

// Forward declaration
%inline %{
  namespace mfem{
     class IterativeSolver;
  }
%}
%include "linalg/solvers.hpp"

#ifdef MFEM_USE_SUITESPARSE
%rename($ignore, %$isfunction) "";
%rename($ignore, %$isclass) "";
%include <umfpack.h>
%include <klu.h>
#endif
