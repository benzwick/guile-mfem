// SPDX-FileCopyrightText: 2020-2025 Princeton Plasma Physics Laboratory
// SPDX-FileCopyrightText: 2026 Benjamin F. Zwick
// SPDX-License-Identifier: BSD-3-Clause

%module gridfunc
%insert("goops") %{(use-modules (mfem array) (mfem coefficient) (mfem densemat) (mfem eltrans) (mfem fespace) (mfem intrules) (mfem mesh) (mfem vector))%}
%feature("autodoc", "1");

%{
  #include <fstream>
  #include <iostream>
  #include <sstream>
  #include <limits>
  #include <cmath>
  #include <cstring>
  #include <ctime>
  #include "mfem.hpp"

  using namespace mfem;
%}

%include "exception.i"
%include "std_string.i"

%import "array.i"
%import "vector.i"
%import "coefficient.i"
%import "mesh.i"
%import "fespace.i"
%import "bilininteg.i"
%import "linearform.i"
%import "fespace.i"
%import "fe_coll.i"
%import "intrules.i"
%import "densemat.i"
%import "sparsemat.i"
%import "lininteg.i"
%import "eltrans.i"
%import "bounds.i"

%import "../common/io_stream_typemap.i"
OSTREAM_TYPEMAP(std::ostream&)
ISTREAM_TYPEMAP(std::istream&)

%include "../common/exception.i"

%include "../common/typemap_macros.i"
LIST_TO_MFEMOBJ_POINTERARRAY_IN(mfem::IntegrationRule const *irs[],  mfem::IntegrationRule *, 0)

%include "fem/gridfunc.hpp"

namespace mfem{
%extend GridFunction{

GridFunction(mfem::FiniteElementSpace *fes, const mfem::Vector &v, int offset){
   mfem::GridFunction *gf;
   gf = new mfem::GridFunction(fes, v.GetData() + offset);
   return gf;
}
  void Assign(const double v) {
    (* self) = v;
  }
  void Assign(const mfem::Vector &v) {
    (* self) = v;
  }
  void Assign(const mfem::GridFunction &v) {
    (* self) = v;
  }

void SaveToFile(const char *gf_file, const int precision) const
   {
        std::cerr << "\nWarning Deprecated : Use Save(filename) insteead of SaveToFile \n";
	std::ofstream mesh_ofs(gf_file);
        mesh_ofs.precision(precision);
        self->Save(mesh_ofs);
   }

   }    // end of extend
 }   //end of namespace

/*
fem/gridfunc.hpp:   virtual void Save(std::ostream &out) const;
fem/gridfunc.hpp:   void Save(std::ostream &out) const;
fem/gridfunc.hpp:   void SaveVTK(std::ostream &out, const std::string &field_name, int ref);
fem/gridfunc.hpp:   void SaveSTL(std::ostream &out, int TimesToRefine = 1);
*/
#ifndef SWIGIMPORTED
OSTREAM_ADD_DEFAULT_FILE(GridFunction, Save)
OSTREAM_ADD_DEFAULT_FILE(QuadratureFunction, Save)
#endif