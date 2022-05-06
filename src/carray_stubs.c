#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define Internal_Carray_val(v) (*(void **)Data_custom_val(v))

#define Internal_Carray_with_ofs_val(v, s)                                     \
  ((*(void **)Data_custom_val(Field(v, 0))) + Int_val(Field(v, 1)) * s)

static void finalize_carray(value v) { free(Internal_Carray_val(v)); }

// Debugging routines
void print_memory(void *ptr, int size) {
  unsigned char *c = (unsigned char *)ptr;
  int i = 0;

  while (i != size)
    printf("%02x", c[i++]);
  printf("\n");
}

static struct custom_operations carray_elmt_ops = {"carray_elmt",
                                                   custom_finalize_default,
                                                   custom_compare_default,
                                                   custom_hash_default,
                                                   custom_serialize_default,
                                                   custom_deserialize_default,
                                                   custom_compare_ext_default,
                                                   custom_fixed_length_default};

static struct custom_operations carray_ops = {"carray",
                                              finalize_carray,
                                              custom_compare_default,
                                              custom_hash_default,
                                              custom_serialize_default,
                                              custom_deserialize_default,
                                              custom_compare_ext_default,
                                              custom_fixed_length_default};

CAMLprim value caml_allocate_carray_stubs(value n, value size) {
  CAMLparam2(n, size);
  CAMLlocal1(block);
  int n_c = Int_val(n);
  int size_c = Int_val(size);
  size_t out_of_heap_memory_size = n_c * size_c;
  void *p = malloc(out_of_heap_memory_size);
  if (p == NULL) {
    caml_raise_out_of_memory();
  }
  block = caml_alloc_custom_mem(&carray_ops, sizeof(void **),
                                out_of_heap_memory_size);
  void **d = (void **)Data_custom_val(block);
  *d = p;
  CAMLreturn(block);
}

CAMLprim value caml_carray_of_array_stubs(value buffer, value array, value n,
                                          value size) {
  CAMLparam4(buffer, array, n, size);
  int n_c = Int_val(n);
  int size_c = Int_val(size);

  void *buffer_c = Internal_Carray_val(buffer);
  for (int i = 0; i < n_c; i++) {
    memcpy(buffer_c + i * size_c, Data_custom_val(Field(array, i)), size_c);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value caml_array_of_carray_stubs(value buffer, value array, value n,
                                          value size) {
  CAMLparam4(buffer, array, n, size);
  int n_c = Int_val(n);
  int size_c = Int_val(size);

  void *array_c = Internal_Carray_with_ofs_val(array, size_c);
  for (int i = 0; i < n_c; i++) {
    memcpy(Data_custom_val(Field(buffer, i)), array_c + i * size_c, size_c);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value caml_make_carray_stubs(value n, value size, value v) {
  CAMLparam3(n, size, v);
  CAMLlocal1(block);
  int n_c = Int_val(n);
  int size_c = Int_val(size);
  size_t out_of_heap_memory_size = n_c * size_c;
  void *p = malloc(out_of_heap_memory_size);
  if (p == NULL) {
    caml_raise_out_of_memory();
  }
  block = caml_alloc_custom_mem(&carray_ops, sizeof(void **),
                                out_of_heap_memory_size);
  void **d = (void **)Data_custom_val(block);
  *d = p;
  for (int i = 0; i < n_c; i++) {
    memcpy(p + i * size_c, Data_custom_val(v), size_c);
  }
  CAMLreturn(block);
}

CAMLprim value caml_set_carray_stubs(value buffer, value v, value i,
                                     value size) {
  CAMLparam4(buffer, v, i, size);
  int i_c = Int_val(i);
  int size_c = Int_val(size);
  void *buffer_c = Internal_Carray_with_ofs_val(buffer, size_c);

  memcpy(buffer_c + i_c * size_c, Data_custom_val(v), size_c);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_get_carray_stubs(value buffer, value carray, value i,
                                     value size) {
  CAMLparam4(buffer, carray, i, size);
  int i_c = Int_val(i);
  int size_c = Int_val(size);
  void *carray_c = Internal_Carray_with_ofs_val(carray, size_c);

  memcpy(Data_custom_val(buffer), carray_c + i_c * size_c, size_c);

  CAMLreturn(Val_unit);
}

CAMLprim value caml_sub_carray_stubs(value buffer, value input, value offset,
                                     value length, value size) {
  CAMLparam5(buffer, input, offset, length, size);
  int size_c = Int_val(size);
  void *buffer_c = Internal_Carray_with_ofs_val(buffer, size_c);
  void *input_c = Internal_Carray_with_ofs_val(input, size_c);
  int offset_c = Int_val(offset);
  int length_c = Int_val(length);

  for (int i = 0; i < length_c; i++) {
    memcpy(buffer_c + i * size_c, input_c + (offset_c + i) * size_c, size_c);
  }

  CAMLreturn(Val_unit);
}

CAMLprim value caml_copy_carray_stubs(value output, value input, value length,
                                      value size) {
  CAMLparam4(output, input, length, size);
  int length_c = Int_val(length);
  int size_c = Int_val(size);
  void *output_c = Internal_Carray_with_ofs_val(output, size_c);
  void *input_c = Internal_Carray_with_ofs_val(input, size_c);

  for (int i = 0; i < length_c; i++) {
    memcpy(output_c + i * size_c, input_c + i * size_c, size_c);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value caml_iter_carray_stubs(value f, value array, value length,
                                      value size) {
  CAMLparam4(f, array, length, size);
  CAMLlocal1(block);
  int size_c = Int_val(size);
  int length_c = Int_val(length);
  void *array_c = Internal_Carray_with_ofs_val(array, size_c);
  // FIXME: what about GC collection with parameter 0 and 1? What is size_c is
  // big?
  block = caml_alloc_custom(&carray_elmt_ops, size_c, 0, 1);

  for (int i = 0; i < length_c; i++) {
    memcpy(Data_custom_val(block), array_c + i * size_c, size_c);
    caml_callback(f, block);
  }

  CAMLreturn(Val_unit);
}

CAMLprim value caml_map_carray_stubs(value voutput, value vf, value vinput,
                                     value vlength, value vsize_a,
                                     value vsize_b) {
  CAMLparam5(voutput, vf, vinput, vlength, vsize_a);
  CAMLxparam1(vsize_b);
  CAMLlocal2(block, vres);
  int size_a = Int_val(vsize_a);
  int size_b = Int_val(vsize_b);
  int length = Int_val(vlength);
  void *input = Internal_Carray_with_ofs_val(vinput, size_a);
  void *output = Internal_Carray_with_ofs_val(voutput, size_b);
  // FIXME: what about GC collection with parameter 0 and 1? What is size_c is
  // big?
  block = caml_alloc_custom(&carray_elmt_ops, size_a, 0, 1);

  /* clock_t start_time = clock(); */
  for (int i = 0; i < length; i++) {
    memcpy(Data_custom_val(block), input + i * size_a, size_a);
    vres = caml_callback(vf, block);
    memcpy(output + i * size_b, Data_custom_val(vres), size_b);
  }
  /* double elapsed_time = (double)(clock() - start_time) / CLOCKS_PER_SEC; */
  /* printf("Done in %f seconds\n", elapsed_time); */

  CAMLreturn(Val_unit);
}

CAMLprim value caml_map_carray_stubs_bytecode(value args[], int argc) {
  return caml_map_carray_stubs(args[0], args[1], args[2], args[3], args[4],
                               args[5]);
}
