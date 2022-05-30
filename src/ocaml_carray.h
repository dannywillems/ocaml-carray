#ifndef OCAML_CARRAY_h
#define OCAML_CARRAY_h

#define Carray_val(v)                                                          \
  ((*(void **)Data_custom_val(Field(v, 0))) +                                  \
   Int_val(Field(v, 2)) * Int_val(Field(v, 3)))

#endif
