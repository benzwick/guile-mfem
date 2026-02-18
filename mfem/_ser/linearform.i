//
// Copyright (c) 2020-2025, Princeton Plasma Physics Laboratory, All rights reserved.
//
%module linearform
%insert("goops") %{(use-modules (mfem fespace) (mfem lininteg) (mfem vector))%}
%{
#include <iostream>
#include <sstream>
#include <fstream>
#include <limits>
#include <cmath>
#include <cstring>
#include <ctime>
#include "mfem.hpp"
%}

%include "exception.i"
%import "coefficient.i"
%import "array.i"
%import "mesh.i"
%import "intrules.i"
%import "fe.i"
%import "fe_coll.i"
%import "densemat.i"
%import "sparsemat.i"
%import "vector.i"
%import "eltrans.i"
%import "lininteg.i"

// Transfer ownership of integrators to LinearForm.
// C++ deletes them in the destructor; prevent Guile GC from doing the same.
%apply SWIGTYPE *DISOWN { mfem::LinearFormIntegrator *lfi };

%include "../common/deprecation.i"
DEPRECATED_METHOD(mfem::LinearForm::GetFES())

%include "fem/linearform.hpp"

