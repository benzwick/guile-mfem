// SPDX-FileCopyrightText: 2020-2025 Princeton Plasma Physics Laboratory
// SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
// SPDX-License-Identifier: BSD-3-Clause

%module(package="mfem._ser") version
%{
#include "general/version.hpp"
%}

%include "std_string.i"
%include "general/version.hpp"