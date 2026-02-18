// SPDX-FileCopyrightText: 2020-2025 Princeton Plasma Physics Laboratory
// SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
// SPDX-License-Identifier: BSD-3-Clause

%module integrator
%insert("goops") %{(use-modules (mfem intrules))%}

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