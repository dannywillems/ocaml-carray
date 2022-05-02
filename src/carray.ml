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

  external map : ('a -> 'b) -> 'a t -> 'b t -> int -> int -> int -> unit
    = "caml_map_carray_stubs_bytecode" "caml_map_carray_stubs"
end

module type S = sig
  (** The type of C arrays *)
  type 'a t

  (** [make n x] returns [a] fresh array of length [n], initialized with [x].
      Each element of the array is a fresh copy of [a]. This is different from
      [Array.make] *)
  val make : int -> 'a -> 'a t

  (** [init n f] returns a fresh array of length [n], with element number [i]
      initialized to the result of [f i]. In other terms, [init n f] tabulates the
      results of [f] applied to the integers [0] to [n-1]. *)
  val init : int -> (int -> 'a) -> 'a t

  (** [of_array a] builds a C array with the same content than the Caml array [a] *)
  val of_array : 'a array -> 'a t

  (** [to_array a] returns an Caml array with the same content than the C array [a] *)
  val to_array : 'a t -> 'a array

  (** [set a n x] modifies array [a] in place, replacing element number [n] with
      [x] *)
  val set : 'a t -> 'a -> int -> unit

  (** [get a n] gets an returns the element number [n] of the array [a]. The
      first element has number [0]. The last element has number length [length -
      1]. *)
  val get : 'a t -> int -> 'a

  (** Return the length (number of elements) of the given array *)
  val length : 'a t -> int

  (** [sub a pos len] returns a fresh array of length [len], containing the
       elements number [pos] to [pos + len - 1] of array [a].
  *)
  val sub : 'a t -> int -> int -> 'a t

  (** [sub_noalloc a pos len] is the same than {!sub} but does not perform
      allocation. The elements of the output are physically the same than the
      input. Consequently, modifying the elements of the input will modify the
      output, and inversely.
  *)
  val sub_noalloc : 'a t -> int -> int -> 'a t

  (** [copy a] returns a fresh copy of [a] *)
  val copy : 'a t -> 'a t

  (** [iter f a] iterates [f] over [a] *)
  val iter : ('a -> unit) -> 'a t -> unit

  (* FIXME: mmh, how can you allocate the type 'b t??? *)
  (* val map : ('a -> 'b) -> 'a t -> 'b t *)
end

module Make (P : sig
  type t

  val size_in_bytes : int

  (* IMPROVEME/FIXME/COMMENTME: This is only used for get. I'd like to have a better interface.
     It is not common to have a fresh fn in the interface *)
  val fresh : unit -> t
end) : S = struct
  type 'a t = 'a Stubs.carray * int * int

  let make size v = (Stubs.make size P.size_in_bytes v, size, 0)

  (* FIXME: check i < length *)
  let set (carray, _len, ofs) v i = Stubs.set (carray, ofs) v i P.size_in_bytes

  (* FIXME: check i < length *)
  let get (carray, _, ofs) i =
    let v = P.fresh () in
    Stubs.get (Obj.magic v) (carray, ofs) i P.size_in_bytes ;
    Obj.magic v

  let init n f =
    let ofs = 0 in
    let carray = Stubs.allocate_carray n P.size_in_bytes in
    for i = 0 to n - 1 do
      Stubs.set (carray, ofs) (f i) i P.size_in_bytes
    done ;
    (carray, n, ofs)

  let of_array array =
    let ofs = 0 in
    let length = Array.length array in
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
end
