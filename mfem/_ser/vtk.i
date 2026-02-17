//
// Copyright (c) 2020-2025, Princeton Plasma Physics Laboratory, All rights reserved.
//
%module vtk
%{
#include  "mfem.hpp"
%}

%include "exception.i"
%import "element.i"
%include "../common/exception.i"

%import "../common/io_stream_typemap.i"
OSTREAM_TYPEMAP(std::ostream&)

%include "mesh/vtk.hpp"
