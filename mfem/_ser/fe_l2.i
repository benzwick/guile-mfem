//
// Copyright (c) 2020-2025, Princeton Plasma Physics Laboratory, All rights reserved.
//
%module fe_l2
%{
#include  "mfem.hpp"
%}

%include "exception.i"
%import "fe_h1.i"
%import "element.i"
%include "../common/typemap_macros.i"
%include "../common/exception.i"


%include "fem/fe/fe_l2.hpp"
