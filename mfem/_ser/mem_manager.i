// SPDX-FileCopyrightText: 2020-2025 Princeton Plasma Physics Laboratory
// SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
// SPDX-License-Identifier: BSD-3-Clause

%module mem_manager
%{
#include  "mfem.hpp"
#include "general/mem_manager.hpp"
%}

%include "exception.i"
%import "globals.i"
%include "../common/typemap_macros.i"
%include "../common/exception.i"
%include "../common/enum_class.i"

//%include "general/mem_manager.hpp"
%include "../../headers/mem_manager.hpp"

ENUM_CLASS_WRAP(_mem_manager, MemoryType)
ENUM_CLASS_WRAP(_mem_manager, MemoryClass)