// SPDX-FileCopyrightText: 2020-2025 Princeton Plasma Physics Laboratory
// SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
// SPDX-License-Identifier: BSD-3-Clause

%module bounds

%{
#include "mfem.hpp"
%}

%include "../common/existing_mfem_headers.i"
#ifdef FILE_EXISTS_FEM_BOUNDS

%include "exception.i"
%import "vector.i"
%import "fespace.i"
%include "../common/exception.i"

%include "fem/bounds.hpp"

#endif