//
// Copyright (c) 2020-2025, Princeton Plasma Physics Laboratory, All rights reserved.
//
%module vertex

%{
#include "mfem.hpp"
%}

%include "exception.i"
%import "element.i"
%include "../common/exception.i"

%include "../common/deprecation.i"
DEPRECATED_OVERLOADED_METHOD(mfem::Vertex::SetCoords,
    	                     Vertex::SetCoords(const double *p) is deprecated,
			     len(args) == 2)

%include "mesh/vertex.hpp"

