// SPDX-FileCopyrightText: 2020-2025 Princeton Plasma Physics Laboratory
// SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
// SPDX-License-Identifier: BSD-3-Clause

%module sparsemat
%insert("goops") %{(use-modules (mfem array) (mfem vector) (mfem operators) (mfem matrix) (mfem densemat))%}
%{
#include <fstream>
#include <sstream>
#include <limits>
#include <cmath>
#include <cstring>
#include "mfem.hpp"
using namespace mfem;
%}

%include "exception.i"
%import "mem_manager.i"

%import "globals.i"
%import "array.i"
%import "vector.i"
%import "operators.i"
%import "matrix.i"
%import "densemat.i"
%import "../common/ignore_common_functions.i"
%import "../common/exception.i"
%import "globals.i"
%import "../common/io_stream_typemap.i"
OSTREAM_TYPEMAP(std::ostream&)

%ignore Walk;

// RAP_P, RAP_R replaces RAP, since RAP has two definition one accept
// pointer and the other accept reference. From Python, two
// can not be distingished..
%inline %{
  mfem::SparseMatrix *RAP_P (const mfem::SparseMatrix &A,
			     const mfem::SparseMatrix &R,
                             mfem::SparseMatrix *ORAP){
    return RAP(A, R, ORAP);
  }

  mfem::SparseMatrix *RAP_R(const mfem::SparseMatrix &Rt,
			    const mfem::SparseMatrix &A,
                            const mfem::SparseMatrix &P){
    return RAP(Rt, A, P);
  }

  mfem::SparseMatrix &OperatorPtr2SparseMatrix(mfem::OperatorPtr op) {
    return dynamic_cast<mfem::SparseMatrix &>(* op);
  }

  mfem::SparseMatrix &OperatorHandle2SparseMatrix(mfem::OperatorHandle op) {
    return dynamic_cast<mfem::SparseMatrix &>(* op);
  }
%}

%include "linalg/sparsemat.hpp"

%extend mfem::SparseMatrix {
  SCM GetIArray(void) const{
    const int * I = self->GetI();
    int L = self->Size();
    SCM vec = scm_make_s32vector(scm_from_int(L + 1), scm_from_int(0));
    scm_t_array_handle handle;
    size_t len;
    ssize_t inc;
    int32_t *elts = (int32_t *)scm_s32vector_writable_elements(vec, &handle, &len, &inc);
    memcpy(elts, I, (L + 1) * sizeof(int32_t));
    scm_array_handle_release(&handle);
    return vec;
  }
  SCM GetJArray(void) const{
    const int * I = self->GetI();
    const int * J = self->GetJ();
    int L = self->Size();
    int nnz = I[L];
    SCM vec = scm_make_s32vector(scm_from_int(nnz), scm_from_int(0));
    scm_t_array_handle handle;
    size_t len;
    ssize_t inc;
    int32_t *elts = (int32_t *)scm_s32vector_writable_elements(vec, &handle, &len, &inc);
    memcpy(elts, J, nnz * sizeof(int32_t));
    scm_array_handle_release(&handle);
    return vec;
  }
  SCM GetDataArray(void) const{
    const int * I = self->GetI();
    const double * A = self->GetData();
    int L = self->Size();
    int nnz = I[L];
    SCM vec = scm_make_f64vector(scm_from_int(nnz), scm_from_double(0.0));
    scm_t_array_handle handle;
    size_t len;
    ssize_t inc;
    double *elts = scm_f64vector_writable_elements(vec, &handle, &len, &inc);
    memcpy(elts, A, nnz * sizeof(double));
    scm_array_handle_release(&handle);
    return vec;
  }
};

/*
linalg/sparsemat.hpp:   void Print(std::ostream &out = mfem::out, int width_ = 4) const;
linalg/sparsemat.hpp:   void PrintMatlab(std::ostream &out = mfem::out) const;
linalg/sparsemat.hpp:   void PrintMM(std::ostream &out = mfem::out) const;
linalg/sparsemat.hpp:   void PrintCSR(std::ostream &out) const;
linalg/sparsemat.hpp:   void PrintCSR2(std::ostream &out) const;
linalg/sparsemat.hpp:   void PrintInfo(std::ostream &out) const;
*/

#ifndef SWIGIMPORTED
OSTREAM_ADD_DEFAULT_FILE(SparseMatrix, Print)
OSTREAM_ADD_DEFAULT_FILE(SparseMatrix, PrintMatlab)
OSTREAM_ADD_DEFAULT_FILE(SparseMatrix, PrintMM)
OSTREAM_ADD_DEFAULT_STDOUT_FILE(SparseMatrix, PrintCSR)
OSTREAM_ADD_DEFAULT_STDOUT_FILE(SparseMatrix, PrintCSR2)
OSTREAM_ADD_DEFAULT_STDOUT_FILE(SparseMatrix, PrintInfo)
#endif