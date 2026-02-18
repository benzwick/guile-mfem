// SPDX-FileCopyrightText: 2020-2025 Princeton Plasma Physics Laboratory
// SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
// SPDX-License-Identifier: BSD-3-Clause

%module sort_pairs
%{
#include  "mfem.hpp"
#include "general/globals.hpp"
%}

%include "exception.i"
%include "../common/typemap_macros.i"
%include "../common/exception.i"

%include "general/sort_pairs.hpp"

namespace mfem{
%template(intintintTriple) Triple<int, int, int>;
 }