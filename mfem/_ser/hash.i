// SPDX-FileCopyrightText: 2020-2025 Princeton Plasma Physics Laboratory
// SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
// SPDX-License-Identifier: BSD-3-Clause

%module hash

%{
#include "general/hash.hpp"
%}

%include "exception.i"
%import "array.i"
%import "vector.i"
%import "../common/exception.i"


%include "general/hash.hpp"