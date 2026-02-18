// SPDX-FileCopyrightText: 2020-2025 Princeton Plasma Physics Laboratory
// SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
// SPDX-License-Identifier: BSD-3-Clause

%module fe_coll
%insert("goops") %{(use-modules (mfem array))%}
%{
#include <iostream>
#include <sstream>
#include <fstream>
#include <limits>
#include <cmath>
#include <cstring>
#include <ctime>
#include "mfem.hpp"
%}

%include "exception.i"
%import "mesh.i"
%import "array.i"
%import "matrix.i"
%import "intrules.i"
%import "coefficient.i"
%import "fe.i"
%import "densemat.i"
%import "sparsemat.i"
%import "vector.i"
%import "eltrans.i"
%import "lininteg.i"
%import "../common/exception.i"

%include "fem/fe_coll.hpp"