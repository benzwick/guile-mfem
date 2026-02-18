// SPDX-FileCopyrightText: 2020-2025 Princeton Plasma Physics Laboratory
// SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
// SPDX-License-Identifier: BSD-3-Clause

%module restriction
%{
#include  "mfem.hpp"
%}

%include "exception.i"
%import "element.i"
%include "../common/exception.i"

%import "operators.i"
%import "mesh.i"

%include "fem/restriction.hpp"