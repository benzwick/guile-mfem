//
// Copyright (c) 2020-2025, Princeton Plasma Physics Laboratory, All rights reserved.
//
%module std_vectors
//
//  std_vectors :
//     this modules gather all std::vector based object
%{
#include <vector>
#include "mfem.hpp"
%}


%include "exception.i"
%import "array.i"

%include "std_vector.i"
%template(vector_int) std::vector<int>;
%template(vector_Vector) std::vector<mfem::Vector>;
%template(vector_intArray) std::vector<mfem::Array<int>>;
%template(vector_FiniteElementSpace) std::vector<mfem::FiniteElementSpace *>;
%template(vector_Mesh) std::vector<mfem::Mesh *>;
%template(vector_SparseMatrix) std::vector<mfem::SparseMatrix *>;
