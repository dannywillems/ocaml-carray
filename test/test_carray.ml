module Make (M : sig
  type t

  val size_in_bytes : int

  val fresh : unit -> t

  val eq : t -> t -> bool
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
    assert (Array.for_all2 M.eq exp_output output)

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
    assert (Array.for_all2 M.eq exp_output output)

  let test_set () =
    let n = 1_000 in
    let input' = Array.init n (fun _ -> M.fresh ()) in
    let input = CArray.init n (fun i -> input'.(i)) in
    let input_caml = CArray.to_array input in
    assert (Array.for_all2 M.eq input_caml input') ;
    CArray.set input (M.fresh ()) (Random.int n) ;
    let input_caml' = CArray.to_array input in
    assert (not (Array.for_all2 M.eq input_caml input_caml'))

  let test_copy_returns_a_correct_fresh_copy () =
    let n = 1 + Random.int 1_000 in
    let input = CArray.init n (fun _ -> M.fresh ()) in
    let input' = CArray.copy input in
    let output = CArray.to_array input' in
    let exp_output = CArray.to_array input in
    assert (CArray.length input' = CArray.length input) ;
    assert (Array.for_all2 M.eq exp_output output) ;
    CArray.set input (M.fresh ()) (Random.int n) ;
    let output = CArray.to_array input' in
    let exp_output = CArray.to_array input in
    assert (not (Array.for_all2 M.eq exp_output output))

  let get_tests name =
    let open Alcotest in
    [ ( Printf.sprintf "Instantiate with %s" name,
        [ test_case "init" `Quick test_init;
          test_case "make" `Quick test_make;
          test_case "sub" `Quick test_sub;
          test_case "set" `Quick test_set;
          test_case "copy" `Quick test_copy_returns_a_correct_fresh_copy;
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
