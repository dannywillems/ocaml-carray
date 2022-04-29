let n = 10000000

module CArray = Carray.Make (struct
  include Bls12_381.Fr

  let fresh () = random ()
end)

let array = Array.init n (fun _ -> Bls12_381.Fr.random ())

let carray = CArray.init n (fun i -> array.(i))

let with_time f =
  let start_time = Sys.time () in
  let res = f () in
  let end_time = Sys.time () in
  (end_time -. start_time, res)

let () =
  let time, res =
    with_time (fun () ->
        let res = Bls12_381.Fr.(copy zero) in
        CArray.iter (fun x -> Bls12_381.Fr.add_inplace res x) carray ;
        res)
  in
  let time', res' =
    with_time (fun () ->
        let res = Bls12_381.Fr.(copy zero) in
        Array.iter (fun x -> Bls12_381.Fr.add_inplace res x) array ;
        res)
  in
  assert (Bls12_381.Fr.eq res res') ;
  Printf.printf "C Array = %f, Caml array: %f\n" time time'
