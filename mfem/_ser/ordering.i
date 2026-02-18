// SPDX-FileCopyrightText: 2020-2025 Princeton Plasma Physics Laboratory
// SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
// SPDX-License-Identifier: BSD-3-Clause

%module ordering
%{
#include  "mfem.hpp"
%}

%include "exception.i"
%import "element.i"
%include "../common/typemap_macros.i"
%include "../common/exception.i"


%include "linalg/ordering.hpp"