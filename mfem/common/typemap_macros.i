//
// Copyright (c) 2020-2025, Princeton Plasma Physics Laboratory, All rights reserved.
//
// integer array output with known length (tuple)
// Guile: return as list (Guile has no tuples)
%define INTARRAY_OUT_TO_TUPLE(type_name, l)
%typemap(out) type_name{
  $result = SCM_EOL;
  for(int i = l - 1; i >= 0; i--) {
    $result = scm_cons(scm_from_long($1[i]), $result);
  }
}
%enddef

// integer array output with known length (list)
%define INTARRAY_OUT_TO_LIST(type_name, l)
%typemap(out) type_name{
  $result = SCM_EOL;
  for(int i = l - 1; i >= 0; i--) {
    $result = scm_cons(scm_from_long($1[i]), $result);
  }
}
%enddef

// integer output as int point
%define INTARRAY_OUT_TO_INT(type_name)
%typemap(out) type_name{
  $result = scm_from_long($1[0]);
}
%enddef

// wrap integer with  default -1
%define INT_DEFAULT_NEGATIVE_ONE(type_name)
%typemap(in) (type_name) {
  if (scm_is_integer($input)) {
     $1 = scm_to_long($input);
  } else {
    SWIG_exception(SWIG_ValueError, "Expecting a integer");
  }
}
%typemap(typecheck) (type_name) {
  $1 = scm_is_integer($input) ? 1 : 0;
}
%enddef

// known length of list/tuple to int pointer
// Guile: accept list only (no tuples)
%define LIST_TO_INTARRAY_IN(type_name, l)
%typemap(in) type_name (int temp[l]){
  if (scm_is_true(scm_list_p($input))) {
     int ll = scm_to_int(scm_length($input));
     for (int i = 0; i < ll; i++) {
        SCM s = scm_list_ref($input, scm_from_int(i));
        temp[i] = (int)scm_to_long(s);
     }
  } else {
    SWIG_exception(SWIG_ValueError, "Expecting a list");
  }

  $1 = temp;
}
%typemap(typecheck, precedence=SWIG_TYPECHECK_POINTER) (type_name) {
  $1 = 0;
  if (scm_is_true(scm_list_p($input))){
    if  (scm_to_int(scm_length($input)) == 2){
        $1 = 1;
    }
  }

}
%enddef

// int pointer input for single int
%define INT_TO_INTARRAY_IN(type_name)
%typemap(in) type_name (int temp){
  if (scm_is_integer($input)) {
     temp = scm_to_long($input);
  } else {
    SWIG_exception(SWIG_ValueError, "Expecting a integer");
  }
  $1 = &temp;
}
%typemap(typecheck, precedence=SWIG_TYPECHECK_POINTER) (type_name) {
  $1 = scm_is_integer($input) ? 1 : 0;
}
%enddef


// double pointer with undefined length
//  this macro accepts either
//     SWIG pointer: passed through
//     f64vector: data is accessed directly (replaces numpy)
//     or
//     list: list is copied, value will be managed by MFEM
%define ARRAY_TO_DOUBLEARRAY_IN(type_name)
%typemap(in) (type_name){
  int i, si;
  if (SWIG_ConvertPtr($input, (void **) &$1, $1_descriptor, 0) != -1){

  }
  else if (scm_is_true(scm_f64vector_p($input))){
    scm_t_array_handle handle;
    size_t len;
    ssize_t inc;
    const double *elts = scm_f64vector_elements($input, &handle, &len, &inc);
    $1 = const_cast<double*>(elts);
    scm_array_handle_release(&handle);
  }
  else {
     if (!scm_is_true(scm_list_p($input))) {
        SWIG_exception(SWIG_ValueError, "Expecting a list");
     }
     si = scm_to_int(scm_length($input));
     $1 = new double[si];
     for (i = 0; i < si; i++) {
        SCM s = scm_list_ref($input, scm_from_int(i));
        if (scm_is_number(s)) {
            $1[i] = scm_to_double(s);
        } else {
	  delete[] $1;
	  SWIG_exception(SWIG_ValueError, "List items must be integer/float");
        }
     }
  }

}

%typemap(typecheck ) (type_name){
   if (SWIG_ConvertPtr($input, (void **) &$1, $1_descriptor, 1) != -1){
      $1 = 1;
   }
   else if (scm_is_true(scm_list_p($input))){
      $1 = 1;
   }
   else if (scm_is_true(scm_f64vector_p($input))){
      $1 = 1;
   }
   else {
      $1 = 0;
   }
}
%enddef

//
//  List (=[OBJTYPE,OBJTYPE ...]) -> OBJTYPE *[]
//
//  This macro generates temporary OBJTYPE *[]
//  This pointer array is NOT deleted if KEEP is true
//  Otherwise deleted.
//
%define LIST_TO_MFEMOBJ_POINTERARRAY_IN(type_name, OBJTYPE, KEEPLINK)
%typemap(in) type_name (OBJTYPE  *tmp_ptrarray, bool allocated=false){
  //  List -> OBJTYPE

  int res = 0;
  if (scm_is_true(scm_list_p($input))) {
     int ll = scm_to_int(scm_length($input));
     $1 = new OBJTYPE [ll];
     allocated = true;
     for (int i = 0; i < ll; i++) {
       OBJTYPE ttt;
       SCM s = scm_list_ref($input, scm_from_int(i));
       res = SWIG_ConvertPtr(s, (void **) &ttt,
			     $descriptor(OBJTYPE),
			     0);
       if (!SWIG_IsOK(res)) {
         SWIG_exception(SWIG_ValueError, "can not convert a list item to OBJTYPE");
       }
       $1[i] = ttt;
     }
  } else {
      int res = SWIG_ConvertPtr($input, (void **) &tmp_ptrarray, $1_descriptor, SWIG_POINTER_NO_NULL);
      if (SWIG_CheckState(res)){
 	 $1 = tmp_ptrarray;
      } else {
         SWIG_exception(SWIG_ValueError, "Expecting a list or OBJTYPE *[]");
      }
  }

  tmp_ptrarray = $1;
}

%typemap(freearg) type_name{
  if ($1 != 0){
      if (allocated$argnum){
         delete $1;
      }
  }
}
%typemap(typecheck, precedence=SWIG_TYPECHECK_POINTER) (type_name) {
  $1 = 0;
  if (scm_is_true(scm_list_p($input))){
      $1 = 1;
  }
  OBJTYPE *ttt;
  int res = SWIG_ConvertPtr($input, (void **) &ttt, $descriptor($input), SWIG_POINTER_NO_NULL);
  if (SWIG_CheckState(res)){
     $1 = 1;
  }
}

%enddef


//
//  List (=[OBJTYPE,OBJTYPE ...]) -> mfem:Array<OBJTYPE)
//
//  This macro generates temporary mfem::Array<OBJTYPE>
//  The Array is deleted using freearg.
//
%define LIST_TO_MFEMOBJ_ARRAY_IN(type_name, OBJTYPE)
%typemap(in) type_name (mfem::Array<OBJTYPE>  *tmp_ptrarray, bool allocated=false){
  //  List (=[OBJTYPE,OBJTYPE ...]) -> mfem:Array<OBJTYPE)
  int res = 0;
  if (scm_is_true(scm_list_p($input))) {
     int ll = scm_to_int(scm_length($input));
     $1 = new mfem::Array<OBJTYPE>(ll);
     allocated = true;
     for (int i = 0; i < ll; i++) {
       OBJTYPE ttt;
       SCM s = scm_list_ref($input, scm_from_int(i));
       res = SWIG_ConvertPtr(s, (void **) &ttt,
			     $descriptor(OBJTYPE),
			     0);
       if (!SWIG_IsOK(res)) {
         SWIG_exception(SWIG_ValueError, "Expecting a list element to be <OBJTYPE *>");
       }
       $1[0][i] = ttt;
     }
  } else {
      int res = SWIG_ConvertPtr($input, (void **) &tmp_ptrarray, $1_descriptor, SWIG_POINTER_NO_NULL);
      if (SWIG_CheckState(res)){
 	 $1 = tmp_ptrarray;
      } else {
         SWIG_exception(SWIG_ValueError, "Expecting a list or Array<OBJTYPE>");
      }
  }
  tmp_ptrarray = $1;
}

%typemap(freearg) type_name{
  if ($1 != 0){
      if (allocated$argnum){
         delete $1;
      }
  }
}
%typemap(typecheck, precedence=SWIG_TYPECHECK_POINTER) (type_name) {
  $1 = 0;
  if (scm_is_true(scm_list_p($input))){
     $1 = 1;
  }
  OBJTYPE *ttt;
  int res = SWIG_ConvertPtr($input, (void **) &ttt, $1_descriptor, SWIG_POINTER_NO_NULL);
  if (SWIG_CheckState(res)){
     $1 = 1;
  }

}
%enddef
//
//  [#t, ....] -> mfem::Array<bool>
//
//  At moment, we assume Array<bool> will be copied. And thus it is safe to delete.
//
%define LIST_TO_MFEMOBJ_BOOLARRAY_IN(type_name)
%typemap(in) type_name (mfem::Array<bool>  *tmp_ptrarray, bool allocated=false){
  //  List (=[#t, #t....]) -> mfem:Array<bool>
  int res = 0;
  if (scm_is_true(scm_list_p($input))) {
     int ll = scm_to_int(scm_length($input));
     $1 = new mfem::Array<bool>(ll);
     allocated = true;
     for (int i = 0; i < ll; i++) {
       SCM s = scm_list_ref($input, scm_from_int(i));
       if (!scm_is_bool(s)){
         SWIG_exception(SWIG_ValueError, "Expecting a list element to be bool");
       }
       $1[0][i] = scm_is_true(s);
     }
  } else {
      int res = SWIG_ConvertPtr($input, (void **) &tmp_ptrarray, $1_descriptor, SWIG_POINTER_NO_NULL);
      if (SWIG_CheckState(res)){
         $1 = tmp_ptrarray;
      } else {
         SWIG_exception(SWIG_ValueError, "Expecting a list or Array<bool>");
      }
  }
  tmp_ptrarray = $1;
}

%typemap(freearg) type_name{
  if ($1 != 0){
      if (allocated$argnum){
         delete $1;
      }
  }
}
%typemap(typecheck, precedence=SWIG_TYPECHECK_POINTER) (type_name) {
  $1 = 0;
  if (scm_is_true(scm_list_p($input))){
     $1 = 1;
  }
  mfem::Array<bool> *ttt;
  int res = SWIG_ConvertPtr($input, (void **) &ttt, $1_descriptor, SWIG_POINTER_NO_NULL);
  if (SWIG_CheckState(res)){
     $1 = 1;
  }

}
%enddef
