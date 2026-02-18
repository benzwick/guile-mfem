// SPDX-FileCopyrightText: 2020-2025 Princeton Plasma Physics Laboratory
// SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
// SPDX-License-Identifier: BSD-3-Clause

%module array
%feature("autodoc", "1");

//%rename(Equal) mfem::Array <class T>::operator=;
%{
#include <fstream>
#include <iostream>
#include <stdio.h>
#include "mfem.hpp"
%}

%include "exception.i"
%include "../common/exception.i"

%import "../common/io_stream_typemap.i"
OSTREAM_TYPEMAP(std::ostream&)
ISTREAM_TYPEMAP(std::istream&)

%import "mem_manager.i"
%import "../common/memorytype_typemap.i"
ENUM_TO_MEMORYTYPE(mfem::MemoryType mt)

/*  boolArray, intArray, doubleArray */
%import "../common/array_listtuple_typemap.i"
ARRAY_LISTTUPLE_INPUT(int, scm_to_long)
ARRAY_LISTTUPLE_INPUT(double, scm_to_double)
ARRAY_LISTTUPLE_INPUT(bool, scm_is_true)

%import "../common/data_size_typemap.i"
XXXPTR_SIZE_IN(int *data_, int asize, int)
XXXPTR_SIZE_IN(double *data_, int asize, double)
XXXPTR_SIZE_IN(bool *data_, int asize, bool)

%ignore mfem::Array::operator[];
%ignore mfem::Array2D::operator[];
%ignore mfem::BlockArray::operator[];
%ignore mfem::Array::operator=;
%ignore mfem::Array2D::operator=;
%ignore mfem::Array::operator T *;
%ignore mfem::Array::operator const T *;
%ignore mfem::Array::begin;  // 'begin' is a Scheme special form

%include "general/array.hpp"

%extend mfem::Array{
  mfem::Array (void *List_or_Tuple, T *_unused){
    /*
    This method is wrapped to receive list to create
    Array object
    */
    mfem::Array <T>  *arr;

    int size = *(int*)List_or_Tuple;
    arr = new mfem::Array<T>(size);
    return arr;
  }
  void set(int i, const T v) {
    if (i >= 0){
        (* self)[i] = v;
    } else {
       int len = self->Size();
       (* self)[len+i] = v;
    }
  }
  void Assign(const T &a){
     *self = a;
  }
};

/*
void Print(std::ostream &out = mfem::out, int width = 4) const;
void Save(std::ostream &out, int fmt = 0) const;
void Save(std::ostream &out, int fmt = 0) const
void Print(std::ostream &out = mfem::out, int width = 4);
*/
#ifndef SWIGIMPORTED
OSTREAM_ADD_DEFAULT_FILE(Array, Print)
OSTREAM_ADD_DEFAULT_FILE(Array2D, Print)
OSTREAM_ADD_DEFAULT_STDOUT_FILE(Array, Save)
OSTREAM_ADD_DEFAULT_STDOUT_FILE(Array2D, Save)
#endif

namespace mfem{
%template(doubleSwap) Swap<double>;
%template(intSwap) Swap<int>;
}

/*
  Instantiation of Array templates.

  We instantiate some common use cases. Array.cpp instantiate these specialization.

  template class Array<char>;
  template class Array<int>;
  template class Array<long long>;
  template class Array<double>;
  template class Array2D<int>;
  template class Array2D<double>;
*/

%import "../common/array_instantiation_macro.i"
INSTANTIATE_ARRAY_INT
INSTANTIATE_ARRAY_DOUBLE
INSTANTIATE_ARRAY_NUMPYARRAY(int8, char, NPY_BYTE)            // for 8bit data
INSTANTIATE_ARRAY_NUMPYARRAY(int64, long long, NPY_LONGLONG)  // 64bit

 /*
For other classes, we need to ignore some  methods
 To ignore methos defined in Array.cpp, we use
   IGNORE_ARRAY_METHODS_PREMITIVE
 In more genral object, we need to futher ignore methods which uses comparision (> or == operators).
   IGNORE_ARRAY_METHODS
 */

IGNORE_ARRAY_METHODS_PREMITIVE(bool)
INSTANTIATE_ARRAY_BOOL
IGNORE_ARRAY_METHODS_PREMITIVE(unsigned int)
INSTANTIATE_ARRAY_NUMPYARRAY(uint, unsigned int, NPY_UINT)       // 32bit

/* Array< Array<int> *> */
IGNORE_ARRAY_METHODS(mfem::Array<int> *)
INSTANTIATE_ARRAY2(Array<int> *, Array<int>, intArray, 1)

/*
   Array2D:: Assign and data access
*/

%extend mfem::Array2D{
  void Assign(const T &a){
     *self = a;
  }
  void Assign(const mfem::Array2D<T> &a){
     *self = a;
  }

  void set(int i, int j, const T v) {
    T *arr = self -> GetRow(i);
    arr[j] = v;
  }
  T get(int i, int j) {
    T *arr = self -> GetRow(i);
    return arr[j];
  }
}

%template(intArray2D) mfem::Array2D<int>;
%template(doubleArray2D) mfem::Array2D<double>;

/* Array2D<* DenseMatrix>, Array2D<* SparseMatrix>,  Array2D<* HypreParMatrix> */

IGNORE_ARRAY_METHODS(mfem::DenseMatrix *)
IGNORE_ARRAY_METHODS(mfem::SparseMatrix *)

%template(DenseMatrixArray2D) mfem::Array2D<mfem::DenseMatrix *>;
%template(SparseMatrixArray2D) mfem::Array2D<mfem::SparseMatrix *>;