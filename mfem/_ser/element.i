//
// Copyright (c) 2020-2025, Princeton Plasma Physics Laboratory, All rights reserved.
//
%module element

%{
#include <iostream>
#include "mfem.hpp"
%}

%include "exception.i"

%import "globals.i"
%import "array.i"
%import "densemat.i"
%import "geom.i"
%import "table.i"
%import "hash.i"
%import "../common/exception.i"

%include "../common/deprecation.i"
DEPRECATED_OVERLOADED_METHOD(mfem::Element::GetNFaces,
    	                     Element::GetNFaces(int & nFaceVertices) is deprecated,
			     len(args) == 1)

%include "mesh/element.hpp"

