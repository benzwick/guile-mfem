//
// Copyright (c) 2020-2025, Princeton Plasma Physics Laboratory, All rights reserved.
//
/*

   vector.i

*/
%module vector
%insert("goops") %{(use-modules (array))%}
%feature("autodoc", "1");
%{
#include "linalg/vector.hpp"
#include <sstream>
#include <fstream>
#include <iostream>
#include <limits>
#include <cmath>
#include <cstring>
#include <ctime>
#include "mfem.hpp"
%}

%include "exception.i"
%include "std_string.i"

%import "array.i"
%import "../common/ignore_common_functions.i"
%import "../common/numpy_int_typemap.i"
%import "../common/typemap_macros.i"
%import "../common/exception.i"

%import "mem_manager.i"

%import "../common/io_stream_typemap.i"
OSTREAM_TYPEMAP(std::ostream&)
ISTREAM_TYPEMAP(std::istream&)

ARRAY_TO_DOUBLEARRAY_IN(double *data_)

%ignore mfem::add;
%ignore mfem::subtract;
%ignore mfem::CheckFinite;    // conflicts with Vector::CheckFinite in GOOPS proxy
%ignore mfem::Distance;       // raw-pointer overload conflicts; use Vector::DistanceTo
%ignore mfem::DistanceSquared; // raw-pointer only; use Vector::DistanceSquaredTo
%ignore mfem::Vector::operator =;
%ignore mfem::Vector::operator double *;
%ignore mfem::Vector::operator const double *;
%ignore mfem::Vector::begin;  // 'begin' is a Scheme special form

// these inlines are to rename add/subtract...
%inline %{
void add_vector(const mfem::Vector &v1, const mfem::Vector &v2, mfem::Vector &v){
   add(v1, v2, v);
}
   /// Do v = v1 + alpha * v2.
void add_vector(const mfem::Vector &v1, double alpha, const mfem::Vector &v2, mfem::Vector &v){
   add(v1, alpha, v2, v);
}
   /// z = a * (x + y)
void add_vector(const double a, const mfem::Vector &x, const mfem::Vector &y, mfem::Vector &z){
   add(a, x, y, z);
}
  /// z = a * x + b * y
void add_vector (const double a, const mfem::Vector &x,
		   const double b, const mfem::Vector &y, mfem::Vector &z){
   add(a, x, b, y, z);
}
   /// Do v = v1 - v2.
void subtract_vector(const mfem::Vector &v1, const mfem::Vector &v2, mfem::Vector &v){
   subtract(v1, v2, v);
}
   /// z = a * (x - y)
void subtract_vector(const double a, const mfem::Vector &x,
		       const mfem::Vector &y, mfem::Vector &z){
   subtract(a, x, y, z);
}
%}

/* define VectorPtrArray */
%import "../common/array_listtuple_typemap.i"
ARRAY_LISTTUPLE_INPUT_SWIGOBJ(mfem::Vector *, 1)

%import "../common/data_size_typemap.i"
XXXPTR_SIZE_IN(mfem::Vector **data_, int asize, mfem::Vector *)

%import "../common/array_instantiation_macro.i"
IGNORE_ARRAY_METHODS(mfem::Vector *)
INSTANTIATE_ARRAY0(Vector *, Vector, 1)

%include "linalg/vector.hpp"

%extend mfem::Vector {
  /* define Assign as a replacement of = operator */
  Vector(const mfem::Vector &v, int offset, int size){
      mfem::Vector *vec;
      vec = new mfem::Vector(v.GetData() +  offset, size);
      return vec;
  }
  void Assign(const double v) {
    (* self) = v;
  }
  void Assign(const mfem::Vector &v) {
    (* self) = v;
  }
  void AssignData(SCM param) {
    /* Accept f64vector and assign its data to this Vector */
    if (!scm_is_true(scm_f64vector_p(param))){
       scm_misc_error("Vector-AssignData", "Input data must be f64vector", SCM_EOL);
    }
    scm_t_array_handle handle;
    size_t len;
    ssize_t inc;
    const double *elts = scm_f64vector_elements(param, &handle, &len, &inc);
    int vlen = self->Size();
    if ((int)len != vlen){
      scm_array_handle_release(&handle);
      scm_misc_error("Vector-Assign", "input data length does not match", SCM_EOL);
    }
    (* self) = const_cast<double*>(elts);
    scm_array_handle_release(&handle);
  }

  void set(int i, const double v) {
    int len = self->Size();
    if (i >= 0){
       (* self)(i) = v;
    } else {
      (* self)(len+i) = v;
    }
  }
  double get(int idx) {
    int len = self->Size();
    if (idx < 0) idx = len + idx;
    if (idx < 0 || idx >= len) {
      scm_misc_error("Vector-get", "Index out of range", SCM_EOL);
    }
    return (* self)(idx);
  }
  SCM GetDataArray(void) const{
     double * A = self->GetData();
     int L = self->Size();
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

/*
linalg/vector.hpp:   void Print(std::ostream &out = mfem::out, int width = 8) const;
linalg/vector.hpp:   void Print_HYPRE(std::ostream &out) const;
*/
#ifndef SWIGIMPORTED
OSTREAM_ADD_DEFAULT_FILE(Vector, Print)
OSTREAM_ADD_DEFAULT_STDOUT_FILE(Vector, Print_HYPRE)
#endif


