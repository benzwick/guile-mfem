//
// Copyright (c) 2020-2025, Princeton Plasma Physics Laboratory, All rights reserved.
//
%module hybridization
%{
#include "mfem.hpp"
%}

%include "exception.i"
%import "fespace.i"
%import "bilininteg.i"

%include "fem/hybridization.hpp"
