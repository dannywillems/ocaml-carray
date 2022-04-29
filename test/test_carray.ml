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

  let test_make_with_to_array () =
    let n = 1 + Random.int 1_000 in
    let carray = CArray.make n r in
    let output = CArray.to_array carray in
    assert (Array.for_all (fun x -> M.eq x r) output)

  let test_init_with_to_array () =
    let n = 1 + Random.int 1_000 in
    let exp_output = Array.init n (fun _ -> M.fresh ()) in
    let carray = CArray.init n (fun i -> exp_output.(i)) in
    let output = CArray.to_array carray in
    assert (Array.for_all2 M.eq exp_output output)

  let test_sub_with_to_array () =
    let n = 1 + Random.int 1_000 in
    let offset = Random.int (n - 1) in
    let len = 1 + Random.int (n - offset) in
    let init_values = Array.init n (fun _ -> M.fresh ()) in
    let exp_output = Array.sub init_values offset len in
    let carray = CArray.init n (fun i -> init_values.(i)) in
    let output = CArray.(to_array (sub carray offset len)) in
    assert (Array.for_all2 M.eq exp_output output)

  let get_tests name =
    let open Alcotest in
    [ ( Printf.sprintf "Instantiate with %s" name,
        [ test_case "init with to_array" `Quick test_init_with_to_array;
          test_case "make with to_array" `Quick test_make_with_to_array;
          test_case "sub with to_array" `Quick test_sub_with_to_array;
          test_case "length when make used" `Quick test_length_when_make_used;
          test_case "length when init used" `Quick test_length_when_init_used ]
      ) ]
end

module BLS_Fr_tests = Make (struct
  include Bls12_381.Fr

  let fresh () = copy zero
end)

module BLS_G1_tests = Make (struct
  include Bls12_381.G1

  let fresh () = copy zero
end)

module BLS_G2_tests = Make (struct
  include Bls12_381.G2

  let fresh () = copy zero
end)

let () =
  let open Alcotest in
  run
    "Carray"
    (BLS_Fr_tests.get_tests "Bls12_381.Fr"
    @ BLS_G1_tests.get_tests "Bls12_381.G1"
    @ BLS_G2_tests.get_tests "Bls12_381.G2")
