//
// Copyright (c) 2020-2025, Princeton Plasma Physics Laboratory, All rights reserved.
//
//  conversion of Int (Guile integer)
%typemap(in) int {
  if (!scm_is_integer($input)) {
    SWIG_exception(SWIG_TypeError, "Input must be integer");
  };
  $1 = scm_to_int($input);
}
%typemap(typecheck,precedence=SWIG_TYPECHECK_INTEGER) int {
  $1 = scm_is_integer($input) ? 1 : 0;
}

