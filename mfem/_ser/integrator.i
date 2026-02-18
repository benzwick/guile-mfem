//
// Copyright (c) 2020-2025, Princeton Plasma Physics Laboratory, All rights reserved.
//
%module integrator
%insert("goops") %{(use-modules (intrules))%}

%{
#include "mfem.hpp"
%}

%include "../common/existing_mfem_headers.i"
#ifdef FILE_EXISTS_FEM_INTEGRATOR

%include "exception.i"
%include "../common/exception.i"

%import "intrules.i"


%include "fem/integrator.hpp"

#endif
