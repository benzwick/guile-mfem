//
// Copyright (c) 2020-2025, Princeton Plasma Physics Laboratory, All rights reserved.
//
%define INSTANTIATE_ARRAY2(XXX, YYY, ZZZ, USEPTR)
#if USEPTR == 1
 //%template(##ZZZ##Ptr##Array) mfem::Array<mfem::XXX>;
%template(##ZZZ##Array) mfem::Array<mfem::XXX>;
#else
%template(##ZZZ##Array) mfem::Array<mfem::XXX>;
#endif
%extend mfem::Array<mfem::XXX> {

SCM get(int idx) {
    int len = self->Size();
    if (idx < 0) idx = len + idx;
    if (idx < 0 || idx >= len) {
      scm_misc_error("Array-get", "Index out of range", SCM_EOL);
    }
    int own =  0;
    swig_type_info *ty = $descriptor(const mfem::YYY *);
    #if USEPTR == 1
    return SWIG_NewPointerObj((void*)(self->operator[](idx)), ty, own);
    #else
    return SWIG_NewPointerObj((void*)&(self->operator[](idx)), ty, own);
    #endif
  }
 };
%enddef

%define INSTANTIATE_ARRAY(XXX)
INSTANTIATE_ARRAY0(XXX, XXX, 0)
%enddef

%define INSTANTIATE_ARRAY0(XXX, YYY, USEPTR)
INSTANTIATE_ARRAY2(XXX, YYY, YYY, USEPTR)
%enddef


%define INSTANTIATE_ARRAY_INT
%template(intArray) mfem::Array<int>;
%extend mfem::Array<int> {
  SCM get(int idx) {
    int len = self->Size();
    if (idx < 0) idx = len + idx;
    if (idx < 0 || idx >= len) {
      scm_misc_error("intArray-get", "Index out of range", SCM_EOL);
    }
    return scm_from_long(self->operator[](idx));
  }
  SCM GetDataArray(void) const{
     const int * A = self->GetData();
     int L = self->Size();
     SCM vec = scm_make_s32vector(scm_from_int(L), scm_from_int(0));
     scm_t_array_handle handle;
     size_t len;
     ssize_t inc;
     int32_t *elts = scm_s32vector_writable_elements(vec, &handle, &len, &inc);
     for (int i = 0; i < L; i++) {
       elts[i] = (int32_t)A[i];
     }
     scm_array_handle_release(&handle);
     return vec;
  }

 };
%enddef


%define INSTANTIATE_ARRAY_DOUBLE
%template(doubleArray) mfem::Array<double>;
%extend mfem::Array<double> {
  SCM get(int idx) {
    int len = self->Size();
    if (idx < 0) idx = len + idx;
    if (idx < 0 || idx >= len) {
      scm_misc_error("doubleArray-get", "Index out of range", SCM_EOL);
    }
    return scm_from_double(self->operator[](idx));
  }
  SCM GetDataArray(void) const{
     const double * A = self->GetData();
     int L = self->Size();
     SCM vec = scm_make_f64vector(scm_from_int(L), scm_from_double(0.0));
     scm_t_array_handle handle;
     size_t len;
     ssize_t inc;
     double *elts = scm_f64vector_writable_elements(vec, &handle, &len, &inc);
     memcpy(elts, A, L * sizeof(double));
     scm_array_handle_release(&handle);
     return vec;
  }
 };
%enddef

%define INSTANTIATE_ARRAY_BOOL
%template(boolArray) mfem::Array<bool>;
%extend mfem::Array<bool> {
  SCM get(int idx) {
    int len = self->Size();
    if (idx < 0) idx = len + idx;
    if (idx < 0 || idx >= len) {
      scm_misc_error("boolArray-get", "Index out of range", SCM_EOL);
    }
    return scm_from_bool(self->operator[](idx));
  }
 };
%enddef

// tool to define mfem::Array returning elements as integers
// mfem::Array(unsigned int) -> uintArray
%define INSTANTIATE_ARRAY_NUMPYARRAY(XXX, YYY, NP_TYPE)
%template(##XXX##Array) mfem::Array<YYY>;
%extend mfem::Array<YYY> {
  SCM get(int idx) {
    int len = self->Size();
    if (idx < 0) idx = len + idx;
    if (idx < 0 || idx >= len) {
      scm_misc_error("Array-get", "Index out of range", SCM_EOL);
    }
    return scm_from_long((long)self->operator[](idx));
  }

 };
%enddef

%define IGNORE_ARRAY_METHODS(XXX)
%ignore mfem::Array<XXX>::Union;
%ignore mfem::Array<XXX>::Find;
%ignore mfem::Array<XXX>::FindSorted;
%ignore mfem::Array<XXX>::Sort;
%ignore mfem::Array<XXX>::DeleteFirst;
%ignore mfem::Array<XXX>::Unique;
%ignore mfem::Array<XXX>::PartialSum;
%ignore mfem::Array<XXX>::Abs;
%ignore mfem::Array<XXX>::Sum;
%ignore mfem::Array<XXX>::IsSorted;
%ignore mfem::Array<XXX>::Save;
%ignore mfem::Array<XXX>::Max;
%ignore mfem::Array<XXX>::Min;
%ignore mfem::Array<XXX>::IsConstant;
%ignore mfem::Array<XXX>::Print;
%ignore mfem::Array<XXX>::PrintGZ;
%ignore mfem::Array<XXX>::SaveGZ;
%ignore mfem::Array<XXX>::Load;
%ignore mfem::Array2D<XXX>::Min;
%ignore mfem::Array2D<XXX>::Max;
%ignore mfem::Array2D<XXX>::Print;
%ignore mfem::Array2D<XXX>::PrintGZ;
%ignore mfem::Array2D<XXX>::SaveGZ;
%ignore mfem::Array2D<XXX>::Load;
%ignore mfem::Array2D<XXX>::Save;
%enddef


%define IGNORE_ARRAY_METHODS_PREMITIVE(XXX)
%ignore mfem::Array<XXX>::PartialSum;
%ignore mfem::Array<XXX>::Abs;
%ignore mfem::Array<XXX>::Sum;
%ignore mfem::Array<XXX>::IsSorted;
%ignore mfem::Array<XXX>::IsConstant;
%ignore mfem::Array<XXX>::Save;
%ignore mfem::Array<XXX>::Max;
%ignore mfem::Array<XXX>::Min;
%ignore mfem::Array<XXX>::Print;
%ignore mfem::Array<XXX>::PrintGZ;
%ignore mfem::Array<XXX>::SaveGZ;
%ignore mfem::Array<XXX>::Load;
%ignore mfem::Array2D<XXX>::Print;
%ignore mfem::Array2D<XXX>::PrintGZ;
%ignore mfem::Array2D<XXX>::SaveGZ;
%enddef

