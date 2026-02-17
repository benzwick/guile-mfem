//
// Copyright (c) 2020-2025, Princeton Plasma Physics Laboratory, All rights reserved.
//
%module bounds

%{
#include "mfem.hpp"
%}

%include "../common/existing_mfem_headers.i"
#ifdef FILE_EXISTS_FEM_BOUNDS

%include "exception.i"
%import "vector.i"
%import "fespace.i"
%include "../common/exception.i"

%include "fem/bounds.hpp"

#endif
