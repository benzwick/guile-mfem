// SPDX-FileCopyrightText: 2020-2025 Princeton Plasma Physics Laboratory
// SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
// SPDX-License-Identifier: BSD-3-Clause

%module eltrans
%insert("goops") %{(use-modules (mfem array) (mfem densemat) (mfem fe) (mfem intrules) (mfem vector))%}

%{
#include "mfem.hpp"
%}

%include "exception.i"

%import "globals.i"
%import "array.i"
%import "vector.i"
%import "densemat.i"
%import "fe.i"
%import "intrules.i"
%import "geom.i"
%import "../common/exception.i"
%import "../common/mfem_config.i"

%include "../common/deprecation.i"
DEPRECATED_METHOD(mfem::IsoparametricTransformation::FinalizeTransformation())


%ignore mfem::ElementTransformation::TransformBack;
%ignore mfem::IsoparametricTransformation::TransformBack;

%include "../common/kernel_dispatch.i"
%include "fem/eltrans.hpp"

//  special handling for TransformBack (this is because tol_0 is protected)
namespace mfem{
  #ifdef MFEM_USE_DOUBLE
  %extend IsoparametricTransformation{
     virtual int _TransformBack(const Vector &pt, IntegrationPoint &ip,
                                const real_t phys_tol = 1e-15){
       return self-> TransformBack(pt, ip, phys_tol);
     }
   };  //end of extend
  #elif defined(MFEM_USE_SINGLE)
  %extend IsoparametricTransformation{
     virtual int _TransformBack(const Vector &pt, IntegrationPoint &ip,
                                const real_t phys_tol = 1e-7){
       return self-> TransformBack(pt, ip, phys_tol);
     }
   };  //end of extend
  #endif
 } //end of namespace