# ocaml-carray: contiguous C array in OCaml

**WARNING**: do not use in production. The library is not safe yet and only
works with values allocated in a custom block.

This library provides an interface to work with contiguous C arrays of C values
(can be expanded to more types, but it is not done yet) which can be used to
write for more efficient C bindings.

When writing bindings to C libraries, developers might need to deal with
internal C types used by the library. C values are often stored in what is
called [a custom block](https://v2.ocaml.org/manual/intfc.html#s%3Ac-custom). To allocate a custom block, the following code is often used:
```C
#define T_val(v) ((t *)Data_custom_val(v))

static struct custom_operations t_ops = {"t",
                                         custom_finalize_default,
                                         custom_compare_default,
                                         custom_hash_default,
                                         custom_serialize_default,
                                         custom_deserialize_default,
                                         custom_compare_ext_default,
                                         custom_fixed_length_default};

CAMLprim value caml_allocate_t_stubs(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(block);
  block = caml_alloc_custom(&t_ops, sizeof(t), 0, 1);
  CAMLreturn(block);
}
```

and on the OCaml side, we write something like:
```ocaml
type t

external allocate : unit -> t = "caml_allocate_t_stubs"
```

From there, we can define OCaml arrays of type `t` with the OCaml type `t array`
and write stubs using `T_val(Field(varray, i))` to access the ith element of the
OCaml array. However, it is not the most efficient as we might want to take
advantage of the CPU cache and some C compiler optimisations by using a
contiguous piece of memory.

This library is an attempt to provide a `Carray` module to mimic the `Array`
module interface where values of type `t Carray.t` are contigous C arrays
storing values of the underlying C type represented by `t`.

The library exposes a C macro `Carray_val(v)` to access the underlying C array
stored in `v`.
