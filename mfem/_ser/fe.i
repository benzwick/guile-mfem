//
// Copyright (c) 2020-2025, Princeton Plasma Physics Laboratory, All rights reserved.
//
%module fe
%insert("goops") %{(use-modules (array) (densemat) (intrules) (vector))%}

%{
#include <iostream>
#include "mfem.hpp"
%}

%immutable;
%ignore poly1d;
%mutable;

%include "exception.i"
%import "array.i"
%import "vector.i"
%import "geom.i"
%import "intrules.i"
%import "densemat.i"
%import "sparsemat.i"
%include "fe_base.i"
%import "fe_fixed_order.i"
%import "fe_h1.i"
%import "fe_nd.i"
%import "fe_rt.i"
%import "fe_l2.i"
%import "fe_nurbs.i"
%import "fe_pos.i"
%import "fe_ser.i"
%import "../common/exception.i"

%ignore mfem::DofToQuad::FE;
%include "fem/fe.hpp"

