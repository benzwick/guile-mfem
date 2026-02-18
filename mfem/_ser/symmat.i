// SPDX-FileCopyrightText: 2020-2025 Princeton Plasma Physics Laboratory
// SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
// SPDX-License-Identifier: BSD-3-Clause

%module symmat
%insert("goops") %{(use-modules (mfem matrix))%}
%{
#include  "mfem.hpp"
#include "linalg/symmat.hpp"
%}

%include "exception.i"

%import "globals.i"
%import "matrix.i"
%include "../common/exception.i"


%import "../common/io_stream_typemap.i"
OSTREAM_TYPEMAP(std::ostream&)

%include "linalg/symmat.hpp"

OSTREAM_ADD_DEFAULT_FILE(DenseSymmetricMatrix, Print)