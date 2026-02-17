//
// Copyright (c) 2020-2025, Princeton Plasma Physics Laboratory, All rights reserved.
//
%module ordering
%{
#include  "mfem.hpp"
%}

%include "exception.i"
%import "element.i"
%include "../common/typemap_macros.i"
%include "../common/exception.i"


%include "linalg/ordering.hpp"
