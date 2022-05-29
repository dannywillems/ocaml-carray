module Stubs = struct
  type 'a carray

  type 'a t = 'a carray * int

  external allocate_carray_elem : int -> 'a = "caml_allocate_carray_elem_stubs"

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

  external iteri : (int -> 'a -> unit) -> 'a t -> int -> int -> unit
    = "caml_iteri_carray_stubs"

  external map : 'b t -> ('a -> 'b) -> 'a t -> int -> int -> int -> unit
    = "caml_map_carray_stubs_bytecode" "caml_map_carray_stubs"

  external mem : 'a t -> 'a -> int -> int -> bool = "caml_mem_carray_stubs"

  external exists : 'a t -> ('a -> bool) -> int -> int -> bool
    = "caml_exists_carray_stubs"

  external append : 'a t -> 'a t -> 'a t -> int -> int -> int -> unit
    = "caml_append_carray_stubs_bytecode" "caml_append_carray_stubs"
end

module Generic = struct
  (* we encode the size in bytes of each element in the type *)
  type 'a t = 'a Stubs.carray * int * int * int

  let compute_size_in_bytes v =
    let v = Obj.magic v in
    if Obj.tag v <> Obj.custom_tag then
      raise
      @@ Invalid_argument
           "Carray: only custom block without out-of-heap values are supported \
            at the moment" ;
    (Obj.reachable_words v - 2) * 8

  (* FIXME: what about empty list? *)
  let make size v =
    let size_in_bytes = compute_size_in_bytes v in
    if size < 1 then
      raise @@ Invalid_argument "Carray.make: size be greater than 1" ;
    (Stubs.make size size_in_bytes v, size, 0, size_in_bytes)

  let set (carray, n, ofs, size_in_bytes) v i =
    if i < ofs || i >= n then
      raise @@ Invalid_argument "Carray.set: index out of bounds" ;
    Stubs.set (carray, ofs) v i size_in_bytes

  let get (carray, n, ofs, size_in_bytes) i =
    if i < ofs || i >= n then
      raise @@ Invalid_argument "Carray.get: index out of bounds" ;
    let v = Stubs.allocate_carray_elem size_in_bytes in
    Stubs.get (Obj.magic v) (carray, ofs) i size_in_bytes ;
    Obj.magic v

  let init n f =
    if n < 1 then
      raise @@ Invalid_argument "Carray.init: size be greater than 1" ;
    let ofs = 0 in
    (* we suppose the output of f on 0 exists... It should be as it is used to
       initialize the array later *)
    let size_in_bytes = compute_size_in_bytes (f 0) in
    let carray = Stubs.allocate_carray n size_in_bytes in
    for i = 0 to n - 1 do
      Stubs.set (carray, ofs) (f i) i size_in_bytes
    done ;
    (carray, n, ofs, size_in_bytes)

  let of_array array =
    let ofs = 0 in
    let length = Array.length array in
    if length < 1 then
      raise @@ Invalid_argument "Carray.of_array: size be greater than 1" ;
    let size_in_bytes = compute_size_in_bytes array.(0) in
    let carray = Stubs.allocate_carray length size_in_bytes in
    Stubs.of_array carray array length size_in_bytes ;
    (carray, length, ofs, size_in_bytes)

  let to_array (carray, length, ofs, size_in_bytes) =
    let array =
      Array.init length (fun _ ->
          Obj.magic (Stubs.allocate_carray_elem size_in_bytes))
    in
    Stubs.to_array array (carray, ofs) length size_in_bytes ;
    array

  let length (_carray, length, _ofs, _size_in_bytes) = length

  (* FIXME: check out of bounds *)
  let sub (carray, _n, ofs, size_in_bytes) offset length =
    let ofs' = 0 in
    let new_carray = Stubs.allocate_carray length size_in_bytes in
    Stubs.sub (new_carray, ofs') (carray, ofs) offset length size_in_bytes ;
    (new_carray, length, ofs', size_in_bytes)

  (* FIXME: check out of bounds *)
  let sub_noalloc (carray, _n, ofs, size_in_bytes) offset length =
    (carray, length, ofs + offset, size_in_bytes)

  let copy (carray, n, ofs, size_in_bytes) =
    let new_carray = Stubs.allocate_carray n size_in_bytes in
    Stubs.copy (carray, ofs) (new_carray, 0) n size_in_bytes ;
    (new_carray, n, 0, size_in_bytes)

  (* FIXME: It looks like it is slower than Array.iter when addition
     Bls12_381.Fr points with add_inplace *)
  let iter f (carray, n, ofs, size_in_bytes) =
    Stubs.iter f (carray, ofs) n size_in_bytes

  let iteri f (carray, n, ofs, size_in_bytes) =
    Stubs.iteri f (carray, ofs) n size_in_bytes

  let to_list a = Array.to_list (to_array a)

  let of_list l = of_array (Array.of_list l)

  let for_all f a =
    let res = ref true in
    let f' x = res := !res && f x in
    iter f' a ;
    !res

  let map f (input_carray, input_n, input_ofs, input_size_in_bytes) =
    let init_v =
      get (input_carray, input_n, input_ofs, input_size_in_bytes) 0
    in
    let output_carray, _output_n, output_ofs, output_size_in_bytes =
      make input_n (f init_v)
    in
    Stubs.map
      (output_carray, output_ofs)
      f
      (input_carray, input_ofs)
      input_n
      input_size_in_bytes
      output_size_in_bytes ;
    (output_carray, input_n, output_ofs, output_size_in_bytes)

  let mem x (carray, n, ofs, size_in_bytes) =
    Stubs.mem (carray, ofs) x n size_in_bytes

  let exists f (carray, n, ofs, size_in_bytes) =
    Stubs.exists (carray, ofs) f n size_in_bytes

  let append (carray, n, ofs, size_in_bytes) (carray', n', ofs', _size_in_bytes)
      =
    if n + n' > Sys.max_array_length then
      raise (Invalid_argument "Carray.append: output array too large") ;
    let v = Stubs.allocate_carray_elem size_in_bytes in
    let output, output_length, output_ofs, _size_in_bytes =
      make (n + n') (Obj.magic v)
    in
    Stubs.append
      (output, output_ofs)
      (carray, ofs)
      (carray', ofs')
      n
      n'
      size_in_bytes ;
    (output, output_length, output_ofs, size_in_bytes)

  let concat array_list =
    if List.length array_list = 0 then
      raise (Invalid_argument "Carray.concat: empty list given") ;
    let size = List.fold_left (fun res (_, n, _, _) -> res + n) 0 array_list in
    let _, _, _, size_in_bytes = List.hd array_list in
    if size > Sys.max_array_length then
      raise (Invalid_argument "Carray.append: output array too large") ;
    let v = Stubs.allocate_carray_elem size_in_bytes in
    let output, output_length, output_ofs, _ = make size (Obj.magic v) in
    let current_ofs = ref 0 in
    List.iter
      (fun (carray, n, ofs, _) ->
        Stubs.copy (output, !current_ofs) (carray, ofs) n size_in_bytes ;
        current_ofs := n + !current_ofs)
      array_list ;
    (output, output_length, output_ofs, size_in_bytes)

  let fill (carray, n, ofs, size_in_bytes) pos len v =
    if pos + len > n then
      raise (Invalid_argument "Carray.fill: invalid pos or len") ;
    for i = 0 to len - 1 do
      Stubs.set (carray, ofs) v (i + pos) size_in_bytes
    done

  let blit (carray, n, ofs, size_in_bytes) src_pos (carray', n', ofs', _)
      dst_pos len =
    if src_pos + len > n || dst_pos + len > n' then
      raise (Invalid_argument "Carray.blit: invalid src_pos, dst_pos or len") ;
    for i = 0 to len - 1 do
      let v = get (carray, n, ofs, size_in_bytes) (src_pos + i) in
      set (carray', n', ofs', size_in_bytes) v (dst_pos + i)
    done
end

include Generic
