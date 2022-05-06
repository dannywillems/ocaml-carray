module Stubs = struct
  type 'a carray

  type 'a t = 'a carray * int

  (* always n followed by size_in_bytes *)
  external allocate_carray : int -> int -> 'a carray
    = "caml_allocate_carray_stubs"

  external of_array : 'a carray -> 'a array -> int -> int -> unit
    = "caml_carray_of_array_stubs"
    [@@noalloc]

  external to_array : 'a array -> 'a t -> int -> int -> unit
    = "caml_array_of_carray_stubs"
    [@@noalloc]

  external make : int -> int -> 'a -> 'a carray = "caml_make_carray_stubs"

  external set : 'a t -> 'a -> int -> int -> unit = "caml_set_carray_stubs"
    [@@noalloc]

  external get : 'a -> 'a t -> int -> int -> unit = "caml_get_carray_stubs"
    [@@noalloc]

  (** [sub output (input, ofs) offset len size_in_bytes] *)
  external sub : 'a t -> 'a t -> int -> int -> int -> unit
    = "caml_sub_carray_stubs"
    [@@noalloc]

  (** [copy (output, ofs) (input, ofs) len size_in_bytes] *)
  external copy : 'a t -> 'a t -> int -> int -> unit = "caml_copy_carray_stubs"
    [@@noalloc]

  external iter : ('a -> unit) -> 'a t -> int -> int -> unit
    = "caml_iter_carray_stubs"

  (* external map : ('a -> 'b) -> 'a t -> 'b t -> int -> int -> int -> unit *)
  (*   = "caml_map_carray_stubs_bytecode" "caml_map_carray_stubs" *)
end

module Make (P : sig
  type t

  val size_in_bytes : int

  (* IMPROVEME/FIXME/COMMENTME: This is only used for get. I'd like to have a better interface.
     It is not common to have a fresh fn in the interface *)
  val fresh : unit -> t
end) =
struct
  type 'a t = 'a Stubs.carray * int * int

  let allocate n = (Stubs.allocate_carray n P.size_in_bytes, n, 0)

  (* FIXME: what about empty list? *)
  let make size v =
    if size < 1 then
      raise @@ Invalid_argument "Carray.make: size be greater than 1" ;
    (Stubs.make size P.size_in_bytes v, size, 0)

  let set (carray, n, ofs) v i =
    if i < ofs || i >= n then
      raise @@ Invalid_argument "Carray.set: index out of bounds" ;
    Stubs.set (carray, ofs) v i P.size_in_bytes

  let get (carray, n, ofs) i =
    if i < ofs || i >= n then
      raise @@ Invalid_argument "Carray.get: index out of bounds" ;
    let v = P.fresh () in
    Stubs.get (Obj.magic v) (carray, ofs) i P.size_in_bytes ;
    Obj.magic v

  let init n f =
    if n < 1 then
      raise @@ Invalid_argument "Carray.init: size be greater than 1" ;
    let ofs = 0 in
    let carray = Stubs.allocate_carray n P.size_in_bytes in
    for i = 0 to n - 1 do
      Stubs.set (carray, ofs) (f i) i P.size_in_bytes
    done ;
    (carray, n, ofs)

  let of_array array =
    let ofs = 0 in
    let length = Array.length array in
    if length < 1 then
      raise @@ Invalid_argument "Carray.of_array: size be greater than 1" ;
    let carray = Stubs.allocate_carray length P.size_in_bytes in
    Stubs.of_array carray array length P.size_in_bytes ;
    (carray, length, ofs)

  let to_array (carray, length, ofs) =
    let array = Array.init length (fun _ -> Obj.magic P.(fresh ())) in
    Stubs.to_array array (carray, ofs) length P.size_in_bytes ;
    array

  let length (_carray, length, _ofs) = length

  (* FIXME: check out of bounds *)
  let sub (carray, _n, ofs) offset length =
    let ofs' = 0 in
    let new_carray = Stubs.allocate_carray length P.size_in_bytes in
    Stubs.sub (new_carray, ofs') (carray, ofs) offset length P.size_in_bytes ;
    (new_carray, length, ofs')

  (* FIXME: check out of bounds *)
  let sub_noalloc (carray, _n, ofs) offset length =
    (carray, length, ofs + offset)

  let copy (carray, n, ofs) =
    let new_carray = Stubs.allocate_carray n P.size_in_bytes in
    Stubs.copy (carray, ofs) (new_carray, 0) n P.size_in_bytes ;
    (new_carray, n, 0)

  (* FIXME: It looks like it is slower than Array.iter when addition
     Bls12_381.Fr points with add_inplace *)
  let iter f (carray, n, ofs) = Stubs.iter f (carray, ofs) n P.size_in_bytes

  let to_list a = Array.to_list (to_array a)

  let of_list l = of_array (Array.of_list l)

  let for_all f a =
    let res = ref true in
    let f' x = res := !res && f x in
    iter f' a ;
    !res
end
