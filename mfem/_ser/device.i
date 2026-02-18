//
// Copyright (c) 2020-2025, Princeton Plasma Physics Laboratory, All rights reserved.
//
%module device
%{
#include  "mfem.hpp"
#include "general/device.hpp"
%}

%include "std_string.i"

%include "exception.i"
%include "../common/typemap_macros.i"
%include "../common/exception.i"

%import "enzyme.i"
%import "mem_manager.i"

%include "../common/deprecation.i"
DEPRECATED_METHOD(mfem::Device::GetMemoryType())
DEPRECATED_METHOD(mfem::Device::GetMemoryClass())

%include "general/device.hpp"
