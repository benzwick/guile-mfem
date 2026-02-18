//
// Copyright (c) 2020-2025, Princeton Plasma Physics Laboratory, All rights reserved.
//
%module lininteg
%insert("goops") %{(use-modules (array) (eltrans) (fe) (fespace) (integrator) (intrules) (operators) (vector))%}
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
%import "globals.i"


%import "fe.i"
%import "vector.i"
%import "eltrans.i"
%import "intrules.i"
%import "coefficient.i"
%import "fespace.i"

%include "fem/lininteg.hpp"

