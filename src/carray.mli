module Make (P : sig
  type t

  val size_in_bytes : int

  (* IMPROVEME/FIXME/COMMENTME: This is only used for get. I'd like to have a better interface.
     It is not common to have a fresh fn in the interface *)
  val fresh : unit -> t
end) : sig
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

  (** [to_array a] returns a Caml array with the same content than the C array [a] *)
  val to_array : 'a t -> 'a array

  (** [of_list l] returns a C array with the same content than the list [l] *)
  val of_list : 'a list -> 'a t

  (** [to_list a] returns a Caml list with the same content than the array [a] *)
  val to_list : 'a t -> 'a list

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

  (** Same as {!Array.iter}, but the function is applied to the index of the
      element as first argument, and the element itself as second argument

      TODO: easy
  *)
  (* val iteri : (int -> 'a -> unit) -> 'a array -> unit *)

  (** [for_all f a] checks if all elements of the array satisfy the predicate [f] *)
  val for_all : ('a -> bool) -> 'a t -> bool

  (* FIXME: mmh, how can you allocate the type 'b t??? *)
  (* val map : ('a -> 'b) -> 'a t -> 'b t *)

  (** [append v1 v2] returns a fresh array containing the concatenation of the
      arrays [v1] and [v2].

      Raises [Invalid_argument] if [length v1 + length v2 >
      Sys.max_array_length]

      TODO: easy
  *)
  (* val append : 'a array -> 'a array -> 'a array *)

  (** Same as Array.append, but concatenates a list of arrays

      TODO: easy
  *)
  (* val concat : 'a array list -> 'a array *)

  (** [fill a pos len x] modifies the array [a] in place, storing [x] in
      elements number [pos] to [pos + len - 1].

      Raises [Invalid_argument] if [pos] and [len] do not designate a valid
      subarray of [a].

      TODO: easy
  *)
  (* val fill : 'a array -> int -> int -> 'a -> unit *)

  (** [blit src src_pos dst dst_pos len] copies [len] elements from array [src],
      starting at element number [src_pos], to array [dst], starting at element
      number [dst_pos]. It works correctly even if [src] and [dst] are the same array,
      and the source and destination chunks overlap.

      Raises [Invalid_argument] if [src_pos] and [len] do not designate a valid
      subarray of [src], or if [dst_pos] and [len] do not designate a valid subarray of
      [dst].

      TODO: easy
  *)
  (* val blit : 'a array -> int -> 'a array -> int -> int -> unit *)

  (** [exists f [|a1; ...; an|]] checks if at least one element of the array
      satisfies the predicate [f]. That is, it returns [(f a1) || (f a2) || ... ||
      (f an)].

      TODO: easy
  *)
  (* val exists : ('a -> bool) -> 'a array -> bool *)

  (** [mem a set] is [true] if and only if a is structurally equal to an element
      of [l] (i.e. there is an [x] in [l] such that [compare a x = 0]).

      TODO: not sure it is easy. Might need a compare function in the functor.
  *)
  (* val mem : 'a -> 'a array -> bool *)
end
