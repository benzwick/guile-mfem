// SPDX-FileCopyrightText: 2020-2025 Princeton Plasma Physics Laboratory
// SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
// SPDX-License-Identifier: BSD-3-Clause

%module fe_base
%{
#include "mfem.hpp"
%}

%include "exception.i"
%import "intrules.i"
%import "geom.i"
%import "doftrans.i"
%include "../common/typemap_macros.i"
%include "../common/exception.i"

//forward declearation
%inline %{
namespace mfem{
  class FiniteElement;
}
%}

// this is to avoild extern breaks template
namespace mfem{
  %ignore poly1d;
}
%include "fem/fe/fe_base.hpp"