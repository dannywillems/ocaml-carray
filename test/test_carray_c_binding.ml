module CarrayFr = Carray.Make (struct
  include Bls12_381.Fr

  let fresh () = random ()
end)

module Stubs = struct
  external add : Bls12_381.Fr.t -> Bls12_381.Fr.t CarrayFr.t -> int -> unit
    = "caml_carray_sum_blst_fr_stubs"
end

let test_add () =
  let n = 1 + Random.int 1_000_000 in
  let array = Array.init n (fun _ -> Bls12_381.Fr.random ()) in
  let carray = CarrayFr.init n (fun i -> array.(i)) in
  let output = Bls12_381.Fr.(copy zero) in
  let exp_output = Bls12_381.Fr.add_bulk (Array.to_list array) in
  Stubs.add output carray (CarrayFr.length carray) ;
  assert (Bls12_381.Fr.eq output exp_output)

let () =
  let open Alcotest in
  run "Carray" [("Bls12_381.Fr bindings C", [test_case "add" `Quick test_add])]
