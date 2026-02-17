//
// Copyright (c) 2020-2025, Princeton Plasma Physics Laboratory, All rights reserved.
//
%module attribute_sets
%feature("autodoc", "1");

%{
#include "mfem.hpp"
%}

%include "../common/existing_mfem_headers.i"
#ifdef FILE_EXISTS_MESH_ATTRIBUTE_SETS

%include "exception.i"
%include "std_string.i"
%include "../common/exception.i"

%import "array.i"
%import "arrays_by_name.i"
%import "../common/io_stream_typemap.i"
OSTREAM_TYPEMAP(std::ostream&)
ISTREAM_TYPEMAP(std::istream&)

%include "mesh/attribute_sets.hpp"

#endif
