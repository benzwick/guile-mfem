//
// Copyright (c) 2020-2025, Princeton Plasma Physics Laboratory, All rights reserved.
//
%module staticcond
%{
#include "mfem.hpp"
%}

%include "exception.i"
%import "fespace.i"

%include "fem/staticcond.hpp"
