//
// Copyright (c) 2020-2025, Princeton Plasma Physics Laboratory, All rights reserved.
//
%module operators
%insert("goops") %{(use-modules (array) (vector))%}

%feature("autodoc", "1");

%{
#include <fstream>
#include <iostream>

#include "mfem.hpp"
%}

%include "exception.i"
%import "mem_manager.i"
%import "vector.i"
%import "array.i"

%import "../common/io_stream_typemap.i"
OSTREAM_TYPEMAP(std::ostream&)

%ignore mfem::Operator::PrintMatlab;


/* define OperatorPtrArray and SolverPtrArray */
%import "../common/array_listtuple_typemap.i"
ARRAY_LISTTUPLE_INPUT_SWIGOBJ(mfem::Operator *, 1)
ARRAY_LISTTUPLE_INPUT_SWIGOBJ(mfem::Solver *, 1)


%import "../common/data_size_typemap.i"
XXXPTR_SIZE_IN(mfem::Operator **data_, int asize, mfem::Operator *)
XXXPTR_SIZE_IN(mfem::Solver **data_, int asize, mfem::Solver *)


%import "../common/array_instantiation_macro.i"
IGNORE_ARRAY_METHODS(mfem::Operator *)
INSTANTIATE_ARRAY0(Operator *, Operator, 1)
IGNORE_ARRAY_METHODS(mfem::Solver *)
INSTANTIATE_ARRAY0(Solver *, Solver, 1)


%include "linalg/operator.hpp"

/*
  void PrintMatlab(std::ostream & out, int n = 0, int m = 0) const;
*/
/*
#ifndef SWIGIMPORTED
OSTREAM_ADD_DEFAULT_FILE(Operator, PrintMatlab)
#endif
*/
