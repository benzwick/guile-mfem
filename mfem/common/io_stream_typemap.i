//
// Copyright (c) 2020-2025, Princeton Plasma Physics Laboratory, All rights reserved.
//
//
//
//  ostream
//
//

//usage OSTREAM_TYPEMAP(std::ostream&)
%define OSTREAM_TYPEMAP(T)
  %typemap(in) T (std::ofstream out_txt){
   //  string argument (filename)
   if (scm_is_string($input)) {
      char *filename = scm_to_utf8_string($input);
      out_txt.open(filename);
      free(filename);
      if (!out_txt.is_open()) {
         SWIG_exception(SWIG_RuntimeError, "Could not open file for writing");
      }
      $1 = &out_txt;
   } else {
      SWIG_exception(SWIG_TypeError, "First argument must be a string filename");
   }
}

%typemap(typecheck, precedence=SWIG_TYPECHECK_STRING) T {
  $1 = scm_is_string($input) ? 1 : 0;
}

%typemap(freearg) T {
  if (out_txt$argnum.is_open()){
    out_txt$argnum.close();
  }
}
%enddef

//This macro extend class to write file and stdout (no argument)
%define OSTREAM_ADD_DEFAULT_STDOUT_FILE(class, method)
%extend mfem::class {
void method(const char *file, int precision=16){
  std::ofstream ofile(file);
  if (!ofile)
     {
        std::cerr << "\nCan not produce output file: " << file << '\n' << std::endl;
        return;
      }
  ofile.precision(precision);
  self -> method(ofile);
  ofile.close();
  }
void method(void){
  self -> method(std::cout);
  }

};
%enddef

%define OSTREAM_ADD_DEFAULT_FILE(class, method)
%extend mfem::class {
void method(const char *file, int precision=16){
  std::ofstream ofile(file);
  if (!ofile)
     {
        std::cerr << "\nCan not produce output file: " << file << '\n' << std::endl;
        return;
      }
  ofile.precision(precision);
  self -> method(ofile);
  ofile.close();
  }
};
%enddef

//
//
//  istream
//
//
//usage ISTREAM_TYPEMAP(std::istream&)
%define ISTREAM_TYPEMAP(T)
  %typemap(in) T (std::ifstream in_txt){
   //  string argument (filename)
   if (scm_is_string($input)) {
      char *filename = scm_to_utf8_string($input);
      in_txt.open(filename, std::ifstream::in);
      free(filename);
      if (!in_txt.is_open()) {
         SWIG_exception(SWIG_RuntimeError, "Could not open file for reading");
      }
      $1 = &in_txt;
   } else {
      SWIG_exception(SWIG_TypeError, "First argument must be a string filename");
   }
}

%typemap(typecheck, precedence=SWIG_TYPECHECK_STRING_ARRAY) T {
  $1 = scm_is_string($input) ? 1 : 0;
}

%typemap(freearg) T {
  if (in_txt$argnum.is_open()){
    in_txt$argnum.close();
  }
}
%enddef

