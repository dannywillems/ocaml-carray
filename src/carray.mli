(** The type of C arrays *)
type 'a t

(** Return the length (number of elements) of the given array *)
val length : 'a t -> int

(** [get a n] gets an returns the element number [n] of the array [a]. The
    first element has number [0]. The last element has number length
    [length - 1]. *)
val get : 'a t -> int -> 'a

(** [set a n x] modifies array [a] in place, replacing element number [n] with
    [x] *)
val set : 'a t -> 'a -> int -> unit

(** [make n x] returns a fresh array of length [n], initialized with [x].
    Each element of the array is a fresh copy of [a]. This is different from
    [Array.make] *)
val make : int -> 'a -> 'a t

(* (\** [create_float n] returns a fresh float array of length [n], *)
(*     with uninitialized data. *)
(* *\) *)
(* val create_float : int -> float t *)

(** [init n f] returns a fresh array of length [n], with element number [i]
    initialized to the result of [f i]. In other terms, [init n f] tabulates the
    results of [f] applied to the integers [0] to [n-1]. *)
val init : int -> (int -> 'a) -> 'a t

(* (\** [make_matrix dimx dimy e] returns a two-dimensional array *)
(*     (an array of arrays) with first dimension [dimx] and *)
(*     second dimension [dimy]. All the elements of this new matrix *)
(*     are initially physically equal to [e]. *)
(*     The element ([x,y]) of a matrix [m] is accessed *)
(*     with the notation [m.(x).(y)]. *)
(*     @raise Invalid_argument if [dimx] or [dimy] is negative or *)
(*     greater than {!Sys.max_array_length}. *)
(*     If the value of [e] is a floating-point number, then the maximum *)
(*     size is only [Sys.max_array_length / 2]. *\) *)
(* val make_matrix : int -> int -> 'a -> 'a t t *)

(** [append v1 v2] returns a fresh array containing the concatenation of the
    arrays [v1] and [v2].

    Raises [Invalid_argument] if [length v1 + length v2 > Sys.max_array_length]
*)
val append : 'a t -> 'a t -> 'a t

(** Same as {!append}, but concatenates a list of arrays *)
val concat : 'a t list -> 'a t

(** [sub a pos len] returns a fresh array of length [len], containing the
     elements number [pos] to [pos + len - 1] of array [a].
*)
val sub : 'a t -> int -> int -> 'a t

(** [copy a] returns a fresh copy of [a] *)
val copy : 'a t -> 'a t

(** [fill a pos len x] modifies the array [a] in place, storing [x] in
    elements number [pos] to [pos + len - 1].

    Raises [Invalid_argument] if [pos] and [len] do not designate a valid
    subarray of [a].
*)
val fill : 'a t -> int -> int -> 'a -> unit

(** [blit src src_pos dst dst_pos len] copies [len] elements from array [src],
    starting at element number [src_pos], to array [dst], starting at element
    number [dst_pos]. It works correctly even if [src] and [dst] are the same array,
    and the source and destination chunks overlap.

    Raises [Invalid_argument] if [src_pos] and [len] do not designate a valid
    subarray of [src], or if [dst_pos] and [len] do not designate a valid subarray of
    [dst].
*)
val blit : 'a t -> int -> 'a t -> int -> int -> unit

(** [of_list l] returns a C array with the same content than the list [l] *)
val of_list : 'a list -> 'a t

(** [to_list a] returns a Caml list with the same content than the array [a] *)
val to_list : 'a t -> 'a list

(** {1 Iterators} *)

(** [iter f a] iterates [f] over [a] *)
val iter : ('a -> unit) -> 'a t -> unit

(** Same as {!iter}, but the function is applied to the index of the
    element as first argument, and the element itself as second argument
*)
val iteri : (int -> 'a -> unit) -> 'a t -> unit

(** [map f a] applies function [f] to all the elements of [a], and builds an
    array with the results returned by
    [f]: [[| f a.(0); f a.(1); ...; f a.(length a - 1) |]].
*)
val map : ('a -> 'b) -> 'a t -> 'b t

(** Same as {!map}, but the function is applied to the index of the element as
    first argument, and the element itself as second argument.
*)
val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t

(* (\** [fold_left f init a] computes *)
(*    [f (... (f (f init a.(0)) a.(1)) ...) a.(n-1)], *)
(*    where [n] is the length of the array [a]. *\) *)
(* val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a *)

(* (\** [fold_left_map] is a combination of {!fold_left} and {!map} that threads an *)
(*     accumulator through calls to [f]. *)
(* *\) *)
(* val fold_left_map : ('a -> 'b -> 'a * 'c) -> 'a -> 'b array -> 'a * 'c array *)

(* (\** [fold_right f a init] computes *)
(*    [f a.(0) (f a.(1) ( ... (f a.(n-1) init) ...))], *)
(*    where [n] is the length of the array [a]. *\) *)
(* val fold_right : ('b -> 'a -> 'a) -> 'b array -> 'a -> 'a *)

(** {1 Iterators on two arrays} *)

(* (\** [iter2 f a b] applies function [f] to all the elements of [a] *)
(*    and [b]. *)
(*    @raise Invalid_argument if the arrays are not the same size. *)
(* *\) *)
(* val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit *)

(* (\** [map2 f a b] applies function [f] to all the elements of [a] *)
(*    and [b], and builds an array with the results returned by [f]: *)
(*    [[| f a.(0) b.(0); ...; f a.(length a - 1) b.(length b - 1)|]]. *)
(*    @raise Invalid_argument if the arrays are not the same size. *)
(* *\) *)
(* val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t *)

(** {1 Array scanning} *)

(** [for_all f [|a1; ...; an|]] checks if all elements
   of the array satisfy the predicate [f]. That is, it returns
   [(f a1) && (f a2) && ... && (f an)].
*)
val for_all : ('a -> bool) -> 'a t -> bool

(** [exists f [|a1; ...; an|]] checks if at least one element of
    the array satisfies the predicate [f]. That is, it returns
    [(f a1) || (f a2) || ... || (f an)].
*)
val exists : ('a -> bool) -> 'a t -> bool

(* (\** Same as {!for_all}, but for a two-argument predicate. *)
(*    @raise Invalid_argument if the two arrays have different lengths. *)
(* *\) *)
(* val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool *)

(* (\** Same as {!exists}, but for a two-argument predicate. *)
(*    @raise Invalid_argument if the two arrays have different lengths. *)
(* *\) *)
(* val exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool *)

(** [mem a set] is true if and only if [a] is structurally equal
    to an element of [l] (i.e. there is an [x] in [l] such that
    [compare a x = 0]).
*)
val mem : 'a -> 'a t -> bool

(* (\** Same as {!mem}, but uses physical equality *)
(*    instead of structural equality to compare list elements. *)
(* *\) *)
(* val memq : 'a -> 'a t -> bool *)

(* (\** [find_opt f a] returns the first element of the array [a] that satisfies *)
(*     the predicate [f], or [None] if there is no value that satisfies [f] in the *)
(*     array [a]. *)
(* *\) *)
(* val find_opt : ('a -> bool) -> 'a t -> 'a option *)

(* (\** [find_map f a] applies [f] to the elements of [a] in order, and returns the *)
(*     first result of the form [Some v], or [None] if none exist. *)
(* *\) *)
(* val find_map : ('a -> 'b option) -> 'a t -> 'b option *)

(** {1 Arrays of pairs} *)

(* (\** [split [|(a1,b1); ...; (an,bn)|]] is [([|a1; ...; an|], [|b1; ...; *)
(*     bn|])]. *\) *)
(* val split : ('a * 'b) array -> 'a t * 'b t *)

(* (\** [combine [|a1; ...; an|] [|b1; ...; bn|]] is [[|(a1,b1); ...; (an,bn)|]]. *)
(*     Raise [Invalid_argument] if the two arrays have different lengths. *\) *)
(* val combine : 'a t -> 'b t -> ('a * 'b) t *)

(** {1 Sorting} *)

(* (\** Sort an array in increasing order according to a comparison *)
(*    function.  The comparison function must return 0 if its arguments *)
(*    compare as equal, a positive integer if the first is greater, *)
(*    and a negative integer if the first is smaller (see below for a *)
(*    complete specification).  For example, {!Stdlib.compare} is *)
(*    a suitable comparison function. After calling [sort], the *)
(*    array is sorted in place in increasing order. *)
(*    [sort] is guaranteed to run in constant heap space *)
(*    and (at most) logarithmic stack space. *)
(*    The current implementation uses Heap Sort.  It runs in constant *)
(*    stack space. *)
(*    Specification of the comparison function: *)
(*    Let [a] be the array and [cmp] the comparison function.  The following *)
(*    must be true for all [x], [y], [z] in [a] : *)
(* -   [cmp x y] > 0 if and only if [cmp y x] < 0 *)
(* -   if [cmp x y] >= 0 and [cmp y z] >= 0 then [cmp x z] >= 0 *)
(*    When [sort] returns, [a] contains the same elements as before, *)
(*    reordered in such a way that for all i and j valid indices of [a] : *)
(* -   [cmp a.(i) a.(j)] >= 0 if and only if i >= j *)
(* *\) *)
(* val sort : ('a -> 'a -> int) -> 'a t -> unit *)

(* (\** Same as {!sort}, but the sorting algorithm is stable (i.e. *)
(*    elements that compare equal are kept in their original order) and *)
(*    not guaranteed to run in constant heap space. *)
(*    The current implementation uses Merge Sort. It uses a temporary array of *)
(*    length [n/2], where [n] is the length of the array.  It is usually faster *)
(*    than the current implementation of {!sort}. *)
(* *\) *)
(* val stable_sort : ('a -> 'a -> int) -> 'a t -> unit *)

(* (\** Same as {!sort} or {!stable_sort}, whichever is *)
(*     faster on typical input. *\) *)
(* val fast_sort : ('a -> 'a -> int) -> 'a t -> unit *)

(** {1 Arrays and Sequences} *)

(* (\** Iterate on the array, in increasing order. Modifications of the *)
(*     array during iteration will be reflected in the sequence. *)
(* *\) *)
(* val to_seq : 'a t -> 'a Seq.t *)

(* (\** Iterate on the array, in increasing order, yielding indices along elements. *)
(*     Modifications of the array during iteration will be reflected in the *)
(*     sequence. *)
(* *\) *)
(* val to_seqi : 'a t -> (int * 'a) Seq.t *)

(* (\** Create an array from the generator *\) *)
(* val of_seq : 'a Seq.t -> 'a t *)

(** {1 Additional functions compared to the module Array} *)

(** [sub_noalloc a pos len] is the same than {!sub} but does not perform
      allocation. The elements of the output are physically the same than the
      input. Consequently, modifying the elements of the input will modify the
      output, and inversely.
*)
val sub_noalloc : 'a t -> int -> int -> 'a t

(** [of_array a] builds a C array with the same content than the Caml array [a] *)
val of_array : 'a array -> 'a t

(** [to_array a] returns a Caml array with the same content than the C array [a] *)
val to_array : 'a t -> 'a array
