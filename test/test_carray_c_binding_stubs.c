#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "blst.h"
#include "caml_bls12_381_stubs.h"
#include "ocaml_carray.h"

CAMLprim value caml_carray_sum_blst_fr_stubs(value vres, value varray,
                                             value vlength) {
  CAMLparam3(vres, varray, vlength);
  blst_fr *array = (blst_fr *)(Carray_val(varray));
  int length = Int_val(vlength);
  memset(Blst_fr_val(vres), 0, sizeof(blst_fr));
  for (int i = 0; i < length; i++) {
    blst_fr_add(Blst_fr_val(vres), Blst_fr_val(vres), array + i);
  }
  CAMLreturn(Val_unit);
}
