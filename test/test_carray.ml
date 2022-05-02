(* TODO:
   - add bound checks tests
*)
let array_for_all2 p l1 l2 =
  let n1 = Array.length l1 and n2 = Array.length l2 in
  if n1 <> n2 then invalid_arg "Array.for_all2"
  else
    let rec loop i =
      if i = n1 then true
      else if p (Array.unsafe_get l1 i) (Array.unsafe_get l2 i) then
        loop (succ i)
      else false
    in
    loop 0

module Make (M : sig
  type t

  val size_in_bytes : int

  val fresh : unit -> t

  val add_inplace : t -> t -> unit

  val eq : t -> t -> bool

  val copy : t -> t

  val zero : t

  val is_zero : t -> bool

  (* val to_bytes : t -> Bytes.t *)
end) =
struct
  module CArray = Carray.Make (M)

  let r = M.fresh ()

  let test_length_when_make_used () =
    let n = 1 + Random.int 1_000 in
    let carray = CArray.make n r in
    assert (CArray.length carray = n)

  let test_length_when_init_used () =
    let n = 1 + Random.int 1_000 in
    let carray = CArray.init n (fun _ -> M.fresh ()) in
    assert (CArray.length carray = n)

  let test_make () =
    let n = 1 + Random.int 1_000 in
    let carray = CArray.make n r in
    let output = CArray.to_array carray in
    assert (Array.for_all (fun x -> M.eq x r) output)

  let test_init () =
    let n = 2 in
    let exp_output = Array.init n (fun _ -> M.fresh ()) in
    let carray = CArray.init n (fun i -> exp_output.(i)) in
    let output = CArray.to_array carray in
    assert (array_for_all2 M.eq exp_output output)

  let test_sub () =
    let n = 1 + Random.int 1_000 in
    let offset = Random.int (n - 1) in
    let len = 1 + Random.int (n - offset) in
    let init_values = Array.init n (fun _ -> M.fresh ()) in
    let exp_output = Array.sub init_values offset len in
    let carray = CArray.init n (fun i -> init_values.(i)) in
    let sub_carray = CArray.sub carray offset len in
    let output = CArray.to_array sub_carray in
    assert (CArray.length sub_carray = len) ;
    assert (array_for_all2 M.eq exp_output output)

  let test_set () =
    let n = 1_000 in
    let input' = Array.init n (fun _ -> M.fresh ()) in
    let input = CArray.init n (fun i -> input'.(i)) in
    let input_caml = CArray.to_array input in
    assert (array_for_all2 M.eq input_caml input') ;
    CArray.set input (M.fresh ()) (Random.int n) ;
    let input_caml' = CArray.to_array input in
    assert (not (array_for_all2 M.eq input_caml input_caml'))

  let test_copy_returns_a_correct_fresh_copy () =
    let n = 1 + Random.int 1_000 in
    let input = CArray.init n (fun _ -> M.fresh ()) in
    let input' = CArray.copy input in
    let output = CArray.to_array input' in
    let exp_output = CArray.to_array input in
    assert (CArray.length input' = CArray.length input) ;
    assert (array_for_all2 M.eq exp_output output) ;
    CArray.set input (M.fresh ()) (Random.int n) ;
    let output = CArray.to_array input' in
    let exp_output = CArray.to_array input in
    assert (not (array_for_all2 M.eq exp_output output))

  let test_iter_add_inplace () =
    let n = 1 + Random.int 1_000 in
    let array = Array.init n (fun _ -> M.fresh ()) in
    let carray = CArray.init n (fun i -> array.(i)) in
    let res = M.(copy zero) in
    CArray.iter (fun x -> M.add_inplace res x) carray ;
    let res' = M.(copy zero) in
    Array.iter (fun x -> M.add_inplace res' x) array ;
    assert (M.eq res res')

  let test_of_list () =
    let n = 1 + Random.int 1_000 in
    let array = Array.init n (fun _ -> M.fresh ()) in
    let carray = CArray.of_list (Array.to_list array) in
    assert (array_for_all2 M.eq array (CArray.to_array carray))

  let test_to_list () =
    let n = 1 + Random.int 1_000 in
    let array = Array.init n (fun _ -> M.fresh ()) in
    let clist = CArray.to_list (CArray.of_array array) in
    assert (List.for_all2 M.eq (Array.to_list array) clist)

  let test_for_all () =
    let n = 1 + Random.int 1_000 in
    let array = Array.init n (fun _ -> M.(copy zero)) in
    let carray = CArray.of_array array in
    assert (CArray.for_all M.is_zero carray)

  let get_tests name =
    let open Alcotest in
    [ ( Printf.sprintf "Instantiate with %s" name,
        [ test_case "init" `Quick test_init;
          test_case "make" `Quick test_make;
          test_case "sub" `Quick test_sub;
          test_case "set" `Quick test_set;
          test_case "copy" `Quick test_copy_returns_a_correct_fresh_copy;
          test_case "iter" `Quick test_iter_add_inplace;
          test_case "of_list" `Quick test_of_list;
          test_case "to_list" `Quick test_to_list;
          test_case "for_all" `Quick test_for_all;
          test_case "length when make used" `Quick test_length_when_make_used;
          test_case "length when init used" `Quick test_length_when_init_used ]
      ) ]
end

module BLS_Fr_tests = Make (struct
  include Bls12_381.Fr

  let fresh () = random ()
end)

module BLS_G1_tests = Make (struct
  include Bls12_381.G1

  (* This is the size in byte for one element of type t, not the size of a
     serialized value! *)
  let size_in_bytes = 48 * 3

  let fresh () = random ()
end)

module BLS_G2_tests = Make (struct
  include Bls12_381.G2

  (* This is the size in byte for one element of type t, not the size of a
     serialized value! *)
  let size_in_bytes = 96 * 3

  let fresh () = random ()
end)

let () =
  let open Alcotest in
  run
    "Carray"
    (BLS_Fr_tests.get_tests "Bls12_381.Fr"
    @ BLS_G1_tests.get_tests "Bls12_381.G1"
    @ BLS_G2_tests.get_tests "Bls12_381.G2")
