//
// Copyright (c) 2020-2025, Princeton Plasma Physics Laboratory, All rights reserved.
//
%module restriction
%{
#include  "mfem.hpp"
%}

%include "exception.i"
%import "element.i"
%include "../common/exception.i"

%import "operators.i"
%import "mesh.i"

%include "fem/restriction.hpp"

