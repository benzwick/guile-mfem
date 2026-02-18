// SPDX-FileCopyrightText: 2020-2025 Princeton Plasma Physics Laboratory
// SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
// SPDX-License-Identifier: BSD-3-Clause

%module enzyme
%{
#include "mfem.hpp"
%}

%include "../common/existing_mfem_headers.i"
#ifdef FILE_EXISTS_GENERAL_ENZYME

%import "exception.i"

%include "general/enzyme.hpp"

#endif