//
// Copyright (c) 2020-2025, Princeton Plasma Physics Laboratory, All rights reserved.
//
%module arrays_by_name
%{
#include "mfem.hpp"
%}

%include "../common/existing_mfem_headers.i"

#ifdef FILE_EXISTS_GENERAL_ARRAYS_BY_NAME

%include "exception.i"
%include "../common/exception.i"

%import "array.i"
%import "../common/io_stream_typemap.i"
OSTREAM_TYPEMAP(std::ostream&)
ISTREAM_TYPEMAP(std::istream&)

%include "general/arrays_by_name.hpp"
%ignore mfem::ArraysByName::begin;
%ignore mfem::ArraysByName::end;

%template(intArraysByName) mfem::ArraysByName<int>;
#endif
