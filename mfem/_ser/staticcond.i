// SPDX-FileCopyrightText: 2020-2025 Princeton Plasma Physics Laboratory
// SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
// SPDX-License-Identifier: BSD-3-Clause

%module staticcond
%{
#include "mfem.hpp"
%}

%include "exception.i"
%import "fespace.i"

%include "fem/staticcond.hpp"