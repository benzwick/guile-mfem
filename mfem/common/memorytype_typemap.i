//
// Copyright (c) 2020-2025, Princeton Plasma Physics Laboratory, All rights reserved.
//
//
//   this typemap is used to replace MemoryType input
//   to integer in Guile (enum values)
//
// usage ENUM_TO_MEMORYTYPE(mfem::MemoryType mt)
%define ENUM_TO_MEMORYTYPE(P1)
  %typemap(in) ( ## P1 ##) {
  if (!scm_is_integer($input)) {
    SWIG_exception(SWIG_TypeError, "Expected integer for MemoryType");
  }
  int i = scm_to_int($input);
  $1 = static_cast< mfem::MemoryType >(i);
}

%typemap(typecheck) ( ## P1 ## ){
  $1 = scm_is_integer($input) ? 1 : 0;
}
%enddef
