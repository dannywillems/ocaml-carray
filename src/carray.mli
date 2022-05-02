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

  (** [for_all f a] checks if all elements of the array satisfy the predicate [f] *)
  val for_all : ('a -> bool) -> 'a t -> bool

  (* FIXME: mmh, how can you allocate the type 'b t??? *)
  (* val map : ('a -> 'b) -> 'a t -> 'b t *)
end
