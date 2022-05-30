# ocaml-carray: contiguous C arrays in OCaml

## Motivation

OCaml arrays are not always contiguous piece of memory, requiring accessing
different chunks of memory when accessing individual elements. When requiring a
value in memory, the CPU will fetch the RAM and load not only the particular
value but a memory page (a contiguous piece of memory) and add it to its cache.
The CPU will use its cache to load the values in its registers. It is not
efficient with large OCaml arrays as the CPU will constantly fetch the RAM to
load different memory pages in its cache.

Also, when using the C FFI, the user must know the memory representation of an
array and use the non user-friendly low-level interface macro `Field`.

## This work

This library provides a polymorphic interface mocking a subset of the `Array`
interface to work with contiguous piece of memory. Using the library should be
as easy as adding `module Array = Carray`.

A C macro `Carray_val` is also provided for developers writing bindings and
requires to simply cast in the underlying C type.

It has also been observed sub arrays are sometimes used for read-only
operations. However, `Array.sub` allocates a fresh copy of the requested sub
part. `Carray` leverages this memory cost by providing noalloc variants, like
`sub_noalloc`.

## Performances

The concept has been tested and used in real world applications like the
polynomial library used by Nomadic Labs to implement zk-rollups. A speed up of
around 50% has been observed when using contiguous arrays compared to OCaml
arrays to compute NTT/FFT.

## Usage

This library is **experimental**. Use this library with caution. The interface
might change in the future.

```shell
opam install carray.0.0.1
```

## Links

- **Repository**: https://gitlab.com/dannywillems/ocaml-carray
- **License**: [MIT](https://gitlab.com/dannywillems/ocaml-carray/-/blob/0.0.1/LICENSE)
- **Release**: [0.0.1](https://gitlab.com/dannywillems/ocaml-carray/-/tags/0.0.1)
- **Documentation**: https://dannywillems.gitlab.io/ocaml-carray/carray/index.html
- **Nomadic Labs website**: https://nomadic-labs.com
- **Tezos ZK-rollups repository**: https://gitlab.com/nomadic-labs/privacy-team
