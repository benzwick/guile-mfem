//
// Copyright (c) 2020-2025, Princeton Plasma Physics Laboratory, All rights reserved.
//
%module globals
%{
#include  "mfem.hpp"
#include "general/globals.hpp"
%}

%include "exception.i"
%include "../common/typemap_macros.i"
%include "../common/exception.i"
%include "../common/mfem_config.i"

%include "std_string.i"

%immutable out;
%immutable err;

%include "general/globals.hpp"
