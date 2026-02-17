//
// Copyright (c) 2020-2025, Princeton Plasma Physics Laboratory, All rights reserved.
//
%module table

%feature("autodoc", "1");

%{
#include <fstream>
#include <iostream>
#include "mfem.hpp"
#include "general/table.hpp"
%}

%include "exception.i"
%import "array.i"
%import "../common/ignore_common_functions.i"
%import "../common/exception.i"

%import "../common/io_stream_typemap.i"
OSTREAM_TYPEMAP(std::ostream&)

%import "mem_manager.i"

%ignore mfem::Table::ReadWriteJ();

%include "general/table.hpp"

/*
  void Print(std::ostream & out = mfem::out, int width = 4) const;
  void PrintMatlab(std::ostream & out) const;
  void Save(std::ostream &out) const;
*/
#ifndef SWIGIMPORTED
OSTREAM_ADD_DEFAULT_FILE(Table, Print)
OSTREAM_ADD_DEFAULT_FILE(Table, PrintMatlab)
OSTREAM_ADD_DEFAULT_STDOUT_FILE(Table, Save)
#endif
