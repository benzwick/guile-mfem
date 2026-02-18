// SPDX-FileCopyrightText: 2020-2025 Princeton Plasma Physics Laboratory
// SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
// SPDX-License-Identifier: BSD-3-Clause

/*

   densemat.i

*/
%module densemat
%insert("goops") %{(use-modules (mfem array) (mfem vector) (mfem operators) (mfem matrix))%}

%feature("autodoc", "1");
%{
#include <fstream>
#include <iostream>
#include "mfem.hpp"
using namespace mfem;
%}

%include "exception.i"
%import "mem_manager.i"

%import "array.i"
%import "vector.i"
%import "operators.i"
%import "matrix.i"
%import "../common/ignore_common_functions.i"
%import "../common/exception.i"

%import "../common/io_stream_typemap.i"
OSTREAM_TYPEMAP(std::ostream&)

%ignore mfem::DenseMatrix::operator=;
%ignore mfem::DenseTensor::operator=;

%include "linalg/densemat.hpp"

%extend mfem::DenseMatrix {
  void Assign(const double v) {
    (* self) = v;
  }
  void Assign(const mfem::DenseMatrix &m) {
    (* self) = m;
  }

  double get(int i, int j) {
    return (* self)(i, j);
  }
  void set(int i, int j, const double v) {
    (* self)(i, j) = v;
  }
  SCM GetDataArray(void) const{
     double * A = self->Data();
     int L = self->Width() * self->Height();
     SCM vec = scm_make_f64vector(scm_from_int(L), scm_from_double(0.0));
     scm_t_array_handle handle;
     size_t len;
     ssize_t inc;
     double *elts = scm_f64vector_writable_elements(vec, &handle, &len, &inc);
     memcpy(elts, A, L * sizeof(double));
     scm_array_handle_release(&handle);
     return vec;
  }
};

%extend mfem::DenseTensor {
  void Assign(const double c) {
    (* self) = c;
  }
  void Assign(const mfem::DenseTensor &m) {
    (* self) = m;
  }

  const double get(int i, int j, int k) const{
    return (* self)(i, j, k);
  }
  void set(int i, int j, int k, const double v) {
    (* self)(i, j, k) = v;
  }
};

/*
  virtual void Print(std::ostream &out = mfem::out, int width_ = 4) const;
  virtual void PrintMatlab(std::ostream &out = mfem::out) const;
  virtual void PrintT(std::ostream &out = mfem::out, int width_ = 4) const;
*/
#ifndef SWIGIMPORTED
OSTREAM_ADD_DEFAULT_FILE(DenseMatrix, Print)
OSTREAM_ADD_DEFAULT_FILE(DenseMatrix, PrintT)
OSTREAM_ADD_DEFAULT_FILE(DenseMatrix, PrintMatlab)
#endif