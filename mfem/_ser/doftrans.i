// SPDX-FileCopyrightText: 2020-2025 Princeton Plasma Physics Laboratory
// SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
// SPDX-License-Identifier: BSD-3-Clause

%module doftrans
%{
#include  "mfem.hpp"
%}

%include "exception.i"
%import "vector.i"
%import "densemat.i"
%import "intrules.i"
%include "../common/typemap_macros.i"
%include "../common/exception.i"


%include "fem/doftrans.hpp"