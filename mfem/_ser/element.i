// SPDX-FileCopyrightText: 2020-2025 Princeton Plasma Physics Laboratory
// SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
// SPDX-License-Identifier: BSD-3-Clause

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