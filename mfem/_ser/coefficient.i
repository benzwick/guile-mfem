//
// Copyright (c) 2020-2025, Princeton Plasma Physics Laboratory, All rights reserved.
//
/*
   coefficient.i
   SWIG interface file for coefficient.hpp
*/
%module coefficient
%insert("goops") %{(use-modules (mfem array) (mfem densemat) (mfem eltrans) (mfem intrules) (mfem symmat) (mfem vector))%}
%feature("autodoc", "1");
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

%import "globals.i"
%import "array.i"
%import "matrix.i"
%import "symmat.i"
%import "intrules.i"
%import "sparsemat.i"
%import "densemat.i"
%import "vector.i"
%import "eltrans.i"

%ignore Function;

%feature("notabstract") mfem::VectorFunctionCoefficient;
%feature("notabstract") mfem::VectorConstantCoefficient;
%feature("notabstract") mfem::VectorDeltaCoefficient;
%feature("notabstract") mfem::MatrixArrayCoefficient;
%feature("notabstract") mfem::MatrixFunctionCoefficient;
%feature("notabstract") mfem::MatrixConstantCoefficient;
%feature("notabstract") mfem::CurlGridFunctionCoefficient;
%feature("notabstract") mfem::SymmetricMatrixConstantCoefficient;
%feature("notabstract") mfem::SymmetricMatrixFunctionCoefficient;
%feature("notabstract") mfem::VectorQuadratureFunctionCoefficient;

%include "../common/typemap_macros.i"
LIST_TO_MFEMOBJ_POINTERARRAY_IN(mfem::IntegrationRule const *irs[],  mfem::IntegrationRule *, 0)
LIST_TO_MFEMOBJ_ARRAY_IN(const mfem::Array<mfem::Coefficient*> & coefs,  mfem::Coefficient *)
LIST_TO_MFEMOBJ_ARRAY_IN(const mfem::Array<mfem::VectorCoefficient*> & coefs,  mfem::VectorCoefficient *)
LIST_TO_MFEMOBJ_ARRAY_IN(const mfem::Array<mfem::MatrixCoefficient*> & coefs,  mfem::MatrixCoefficient *)

/* define CoefficientPtrArray, VectorCoefficientPtrArray, MatrixCoefficientPtrArray */
%import "../common/array_listtuple_typemap.i"
ARRAY_LISTTUPLE_INPUT_SWIGOBJ(mfem::Coefficient *, 1)
ARRAY_LISTTUPLE_INPUT_SWIGOBJ(mfem::VectorCoefficient *, 1)
ARRAY_LISTTUPLE_INPUT_SWIGOBJ(mfem::MatrixCoefficient *, 1)

%import "../common/array_instantiation_macro.i"
IGNORE_ARRAY_METHODS(mfem::Coefficient *)
INSTANTIATE_ARRAY0(Coefficient *, Coefficient, 1)
IGNORE_ARRAY_METHODS(mfem::VectorCoefficient *)
INSTANTIATE_ARRAY0(VectorCoefficient *, VectorCoefficient, 1)
IGNORE_ARRAY_METHODS(mfem::MatrixCoefficient *)
INSTANTIATE_ARRAY0(MatrixCoefficient *, MatrixCoefficient, 1)

%include "fem/coefficient.hpp"

