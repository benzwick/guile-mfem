//
// Copyright (c) 2020-2025, Princeton Plasma Physics Laboratory, All rights reserved.
//
%module mesh

%{
#include <iostream>
#include <sstream>
#include <fstream>
#include <limits>
#include <cmath>
#include <cstring>
#include <ctime>
#include <vector>
#include "mfem.hpp"
%}

%include "exception.i"

%include "std_string.i"

%import "matrix.i"
%import "mem_manager.i"
%import "array.i"
%import "attribute_sets.i"
%import "sort_pairs.i"
%import "ncmesh.i"
%import "vector.i"
%import "gridfunc.i"
%import "element.i"
%import "vertex.i"
%import "vtk.i"
 //%import "mesh/mesquite.hpp"
%import "densemat.i"
%import "sparsemat.i"
%import "eltrans.i"
%import "intrules.i"

%import "std_vectors.i"

%feature("notabstract") VectorFunctionCoefficient;
%feature("notabstract") VectorConstantCoefficient;
%import "coefficient.i"
%import "fe.i"

%import "../common/io_stream_typemap.i"
OSTREAM_TYPEMAP(std::ostream&)
ISTREAM_TYPEMAP(std::istream&)

// ignore these constructors, since element::type is given by
// string (see extend section below).
%typemap(typecheck) (int nx, int ny, int nz, mfem::Element::Type type) {
  $1 = 0; // ignore this pattern
}
%typemap(typecheck) (int nx, int ny, mfem::Element::Type type) {
  $1 = 0; // ignore this pattern
}

%immutable attributes;
%immutable bdr_attributes;
%ignore MesquiteSmooth;

%newobject mfem::Mesh::GetFaceToElementTable;
%newobject mfem::Mesh::GetVertexToElementTable;

%immutable mfem::MeshPart::mesh;
%immutable mfem::MeshPart::nodal_fes;
%immutable mfem::MeshPart::nodes;

%include "../common/exception.i"
%include "mesh/mesh.hpp"

%mutable;

namespace mfem{
%extend Mesh{
   Mesh(int nx, int ny, int nz, const char *type, bool generate_edges = 0,
        double sx = 1.0, double sy = 1.0, double sz = 1.0,
        bool sfc_ordering = true){
     mfem::Mesh *mesh;

     if (std::strcmp(type, "TETRAHEDRON") == 0) {
         mesh = new mfem::Mesh(nx, ny, nz, mfem::Element::TETRAHEDRON,
                               generate_edges, sx, sy, sz);

     }
     else if (std::strcmp(type, "HEXAHEDRON") == 0) {
         mesh = new mfem::Mesh(nx, ny, nz, mfem::Element::HEXAHEDRON,
                               generate_edges, sx, sy, sz);

     }
     else {
         return NULL;
     }
     return mesh;
   }
   Mesh(int nx, int ny,  const char *type, bool generate_edges = 0,
        double sx = 1.0, double sy = 1.0, bool sfc_ordering = true){
     mfem::Mesh *mesh;
     if (std::strcmp(type, "TRIANGLE") == 0) {
         mesh = new mfem::Mesh(nx, ny, mfem::Element::TRIANGLE,
                               generate_edges, sx, sy);

     }
     else if (std::strcmp(type, "QUADRILATERAL") == 0) {
         mesh = new mfem::Mesh(nx, ny, mfem::Element::QUADRILATERAL,
                               generate_edges, sx, sy);

     }
     else {
         return NULL;
     }
     return mesh;
   }

   void PrintToFile(const char *mesh_file, const int precision) const
   {
        std::cerr << "\nWarning Deprecated : Use Print(filename) insteead of SaveToFile \n";
        std::ofstream mesh_ofs(mesh_file);
        mesh_ofs.precision(precision);
        self->Print(mesh_ofs);
   }

  double GetScaledJacobian(int i, int sd=2)
  {
    // compute scaled Jacobian
    //   i : element index
    //   sd: subdivision
    //   https://github.com/mfem/mfem/pull/1835/files
    //
    double attr = mfem::infinity();
    mfem::DenseMatrix J(self->Dimension());

    mfem::Geometry::Type geom = self->GetElementBaseGeometry(i);
    mfem::ElementTransformation *T = self->GetElementTransformation(i);

    mfem::RefinedGeometry *RefG = mfem::GlobGeometryRefiner.Refine(geom, sd, 1);
    mfem::IntegrationRule &ir = RefG->RefPts;

    // For each element, find the minimal scaled Jacobian in a
    // lattice of points with the given subdivision factor.

    for (int j = 0; j < ir.GetNPoints(); j++)
      {
        T->SetIntPoint(&ir.IntPoint(j));
        mfem::Geometries.JacToPerfJac(geom, T->Jacobian(), J);

        // Jacobian determinant
        double sJ = J.Det();

        for (int k = 0; k < J.Width(); k++)
          {
            mfem::Vector col;
            J.GetColumnReference(k, col);
            // Scale by column norms
            sJ /= col.Norml2();
          }

        attr = fmin(sJ, attr);
      }
    return attr;
  }

  };  // end of extend
}     // end of namespace

/*
virtual void PrintXG(std::ostream &out = mfem::out) const;
virtual void Print(std::ostream &out = mfem::out) const { Printer(out); }
void PrintVTK(std::ostream &out);
virtual void PrintInfo(std::ostream &out = mfem::out)
*/

OSTREAM_ADD_DEFAULT_FILE(Mesh, PrintInfo)
OSTREAM_ADD_DEFAULT_FILE(Mesh, Print)
OSTREAM_ADD_DEFAULT_FILE(Mesh, PrintXG)
OSTREAM_ADD_DEFAULT_FILE(Mesh, PrintVTK)

