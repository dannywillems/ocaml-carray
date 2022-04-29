module Stubs = struct
  type 'a carray

  (* always n followed by size_in_bytes *)
  external allocate_carray : int -> int -> 'a carray
    = "caml_allocate_carray_stubs"

  external of_array : 'a carray -> 'a array -> int -> int -> unit
    = "caml_carray_of_array_stubs"

  external to_array : 'a array -> 'a carray -> int -> int -> unit
    = "caml_array_of_carray_stubs"

  external make : int -> int -> 'a -> 'a carray = "caml_make_carray_stubs"

  external set : 'a carray -> 'a -> int -> int -> unit = "caml_set_carray_stubs"

  external get : 'a -> 'a carray -> int -> int -> unit = "caml_get_carray_stubs"

  external sub : 'a carray -> 'a carray -> int -> int -> int -> unit
    = "caml_sub_carray_stubs"
end

module type S = sig
  type 'a t

  val make : int -> 'a -> 'a t

  val init : int -> (int -> 'a) -> 'a t

  val of_array : 'a array -> 'a t

  val to_array : 'a t -> 'a array

  val set : 'a t -> 'a -> int -> unit

  val get : 'a t -> int -> 'a

  val length : 'a t -> int

  val sub : 'a t -> int -> int -> 'a t
end

module Make (P : sig
  type t

  val size_in_bytes : int

  (* IMPROVEME/FIXME/COMMENTME: This is only used for get. I'd like to have a better interface.
     It is not common to have a fresh fn in the interface *)
  val fresh : unit -> t
end) : S = struct
  type 'a t = 'a Stubs.carray * int

  let make size v = (Stubs.make size P.size_in_bytes v, size)

  (* FIXME: check i < length *)
  let set (carray, _) v i = Stubs.set carray v i P.size_in_bytes

  (* FIXME: check i < length *)
  let get (carray, _) i =
    let v = P.fresh () in
    Stubs.get (Obj.magic v) carray i P.size_in_bytes ;
    Obj.magic v

  let init n f =
    let carray = Stubs.allocate_carray n P.size_in_bytes in
    for i = 0 to n - 1 do
      Stubs.set carray (f i) i P.size_in_bytes
    done ;
    (carray, n)

  let of_array array =
    let length = Array.length array in
    let carray = Stubs.allocate_carray length P.size_in_bytes in
    Stubs.of_array carray array length P.size_in_bytes ;
    (carray, length)

  let to_array (carray, length) =
    let array = Array.init length (fun _ -> Obj.magic P.(fresh ())) in
    Stubs.to_array array carray length P.size_in_bytes ;
    array

  let length (_carray, length) = length

  (* FIXME: check out of bounds *)
  let sub (carray, _n) offset length =
    let new_carray = Stubs.allocate_carray length P.size_in_bytes in
    Stubs.sub new_carray carray offset length P.size_in_bytes ;
    (new_carray, length)
end
