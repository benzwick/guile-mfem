//
// Copyright (c) 2020-2025, Princeton Plasma Physics Laboratory, All rights reserved.
//
%module fe_fixed_order
%{
#include  "mfem.hpp"
%}

%include "exception.i"
%import "fe.i"
%import "fe_base.i"
%import "element.i"
%include "../common/typemap_macros.i"
%include "../common/exception.i"


%include "fem/fe/fe_fixed_order.hpp"
