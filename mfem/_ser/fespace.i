// SPDX-FileCopyrightText: 2020-2025 Princeton Plasma Physics Laboratory
// SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
// SPDX-License-Identifier: BSD-3-Clause

%module fespace
%insert("goops") %{(use-modules (mfem array) (mfem eltrans) (mfem intrules) (mfem mesh) (mfem operators) (mfem sparsemat) (mfem vector))%}
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
%import "array.i"
%import "vector.i"
%import "coefficient.i"
%import "matrix.i"
%import "mesh.i"
%import "intrules.i"
%import "fe.i"
%import "fe_coll.i"
%import "doftrans.i"
%import "densemat.i"
%import "sparsemat.i"
%import "eltrans.i"
%import "lininteg.i"
%import "handle.i"
%import "ordering.i"
%import "restriction.i"
%import "../common/exception.i"

%import "../common/io_stream_typemap.i"
OSTREAM_TYPEMAP(std::ostream&)
ISTREAM_TYPEMAP(std::istream&)

/* define FiniteElementSpacePtrArray */
%import "../common/array_listtuple_typemap.i"
ARRAY_LISTTUPLE_INPUT_SWIGOBJ(mfem::FiniteElementSpace *, 1)

%import "../common/data_size_typemap.i"
XXXPTR_SIZE_IN(mfem::FiniteElementSpace **data_, int asize, mfem::FiniteElementSpace *)

%import "../common/array_instantiation_macro.i"
IGNORE_ARRAY_METHODS(mfem::FiniteElementSpace *)
INSTANTIATE_ARRAY0(FiniteElementSpace *, FiniteElementSpace, 1)

%include "fem/fespace.hpp"

/*
fem/fespace.hpp:   void Save(std::ostream &out) const;
fem/fespace.hpp:   void Save(std::ostream &out) const;
*/
OSTREAM_ADD_DEFAULT_STDOUT_FILE(FiniteElementSpace, Save)
OSTREAM_ADD_DEFAULT_STDOUT_FILE(QuadratureSpace, Save)


%extend mfem::FiniteElementSpace{
  virtual DofTransformation *GetElementDofTransformation(int elem) const{
    mfem::Array<int> dofs;
    return self->GetElementDofs(elem, dofs);
  }
  virtual DofTransformation *GetBdrElementDofTransformation(int bel) const {
    mfem::Array<int> dofs;
    return self->GetBdrElementDofs(bel, dofs);
  }
  virtual DofTransformation *GetElementVDofTransformation(int elem) const {
    mfem::Array<int> dofs;
    return self->GetElementVDofs(elem, dofs);
  }
  virtual DofTransformation *GetBdrElementVDofTransformation(int bel) const {
    mfem::Array<int> dofs;
    return self->GetBdrElementVDofs(bel, dofs);
  }
};