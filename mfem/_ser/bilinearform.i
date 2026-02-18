//
// Copyright (c) 2020-2025, Princeton Plasma Physics Laboratory, All rights reserved.
//
%module bilinearform
%insert("goops") %{(use-modules (array) (bilininteg) (densemat) (fespace) (handle) (lininteg) (matrix) (operators) (sparsemat) (vector))%}
%{
#include "fem/bilinearform.hpp"
using namespace mfem;
%}

%include "exception.i"

%import "globals.i"
%import "mem_manager.i"
%import "array.i"
%import "fespace.i"
%import "fe_coll.i"
%import "intrules.i"
%import "matrix.i"
%import "vector.i"
%import "densemat.i"
%import "sparsemat.i"
%import "lininteg.i"
%import "eltrans.i"
%import "bilininteg.i"
%import "linearform.i"
%import "gridfunc.i"
%import "hybridization.i"
%import "staticcond.i"

// Transfer ownership of integrators to BilinearForm/MixedBilinearForm.
// C++ deletes them in the destructor; prevent Guile GC from doing the same.
%apply SWIGTYPE *DISOWN { mfem::BilinearFormIntegrator *bfi };

%include "../common/deprecation.i"
DEPRECATED_METHOD(mfem::BilinearForm::GetFES())

%import "../common/array_instantiation_macro.i"
IGNORE_ARRAY_METHODS(mfem::BilinearForm *)
INSTANTIATE_ARRAY0(BilinearForm *, BilinearForm, 1)

%include "fem/bilinearform.hpp"

// instatitate template methods
%define FORM_SYSTEM_MATRIX_WRAP(OsType)
%template(FormLinearSystem) mfem::BilinearForm::FormLinearSystem<mfem:: ## OsType>;
%template(FormSystemMatrix) mfem::BilinearForm::FormSystemMatrix<mfem:: ## OsType>;
%template(FormRectangularLinearSystem) mfem::MixedBilinearForm::FormRectangularLinearSystem<mfem:: ## OsType>;
%template(FormRectangularSystemMatrix) mfem::MixedBilinearForm::FormRectangularSystemMatrix<mfem:: ## OsType>;
%enddef

FORM_SYSTEM_MATRIX_WRAP(SparseMatrix)

 /*
#ifdef MFEM_USE_MPI
  FORM_SYSTEM_MATRIX_WRAP(mfem::HypreParMatrix)
#endif
 */

#ifdef MFEM_USE_PETSC
  FORM_SYSTEM_MATRIX_WRAP(mfem::PetscParMatrix)
#endif
