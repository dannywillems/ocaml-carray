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

  val fresh : unit -> t

  val eq : t -> t -> bool

  val add : t -> t -> t

  val copy : t -> t

  val zero : t

  val is_zero : t -> bool

  (* val to_bytes : t -> Bytes.t *)
end) =
struct
  let r = M.fresh ()

  let test_length_when_make_used () =
    let n = 1 + Random.int 30 in
    let carray = Carray.make n r in
    assert (Carray.length carray = n)

  let test_length_when_init_used () =
    let n = 1 + Random.int 30 in
    let carray = Carray.init n (fun _ -> M.fresh ()) in
    assert (Carray.length carray = n)

  let test_make () =
    let n = 1 + Random.int 30 in
    let carray = Carray.make n r in
    let output = Carray.to_array carray in
    assert (Array.for_all (fun x -> M.eq x r) output)

  let test_init () =
    let n = 2 in
    let exp_output = Array.init n (fun _ -> M.fresh ()) in
    let carray = Carray.init n (fun i -> exp_output.(i)) in
    let output = Carray.to_array carray in
    assert (array_for_all2 M.eq exp_output output)

  let test_sub () =
    let n = 2 + Random.int 30 in
    let offset = Random.int (n - 1) in
    let len = 1 + Random.int (n - offset) in
    let init_values = Array.init n (fun _ -> M.fresh ()) in
    let exp_output = Array.sub init_values offset len in
    let carray = Carray.init n (fun i -> init_values.(i)) in
    let sub_carray = Carray.sub carray offset len in
    let output = Carray.to_array sub_carray in
    assert (Carray.length sub_carray = len) ;
    assert (array_for_all2 M.eq exp_output output)

  let test_set () =
    let n = 1_000 in
    let input' = Array.init n (fun _ -> M.fresh ()) in
    let input = Carray.init n (fun i -> input'.(i)) in
    let input_caml = Carray.to_array input in
    assert (array_for_all2 M.eq input_caml input') ;
    Carray.set input (M.fresh ()) (Random.int n) ;
    let input_caml' = Carray.to_array input in
    assert (not (array_for_all2 M.eq input_caml input_caml'))

  let test_copy_returns_a_correct_fresh_copy () =
    let n = 1 + Random.int 30 in
    let input = Carray.init n (fun _ -> M.fresh ()) in
    let input' = Carray.copy input in
    let output = Carray.to_array input' in
    let exp_output = Carray.to_array input in
    assert (Carray.length input' = Carray.length input) ;
    assert (array_for_all2 M.eq exp_output output) ;
    Carray.set input (M.fresh ()) (Random.int n) ;
    let output = Carray.to_array input' in
    let exp_output = Carray.to_array input in
    assert (not (array_for_all2 M.eq exp_output output))

  (* let test_iter_add_inplace () = *)
  (*   let n = 1 + Random.int 30 in *)
  (*   let array = Array.init n (fun _ -> M.fresh ()) in *)
  (*   let carray = Carray.init n (fun i -> array.(i)) in *)
  (*   let res = M.(copy zero) in *)
  (*   Carray.iter (fun x -> M.add_inplace res x) carray ; *)
  (*   let res' = M.(copy zero) in *)
  (*   Array.iter (fun x -> M.add_inplace res' x) array ; *)
  (*   assert (M.eq res res') *)

  (* let test_iteri_add_inplace () = *)
  (*   let n = 1 + Random.int 30 in *)
  (*   let array = Array.init n (fun _ -> M.fresh ()) in *)
  (*   let array2 = Array.init n (fun _ -> M.fresh ()) in *)
  (*   let carray = Carray.init n (fun i -> array.(i)) in *)
  (*   let res = M.(copy zero) in *)
  (*   Carray.iteri (fun i x -> M.add_inplace res (M.add x array2.(i))) carray ; *)
  (*   let res' = M.(copy zero) in *)
  (*   Array.iteri (fun i x -> M.add_inplace res' (M.add x array2.(i))) array ; *)
  (*   assert (M.eq res res') *)

  let test_append () =
    let n = 1 + Random.int 30 in
    let n' = 1 + Random.int 30 in
    let carray = Carray.init n (fun _ -> M.fresh ()) in
    let carray' = Carray.init n' (fun _ -> M.fresh ()) in
    let exp_res =
      Array.append (Carray.to_array carray) (Carray.to_array carray')
    in
    let res = Carray.append carray carray' in
    assert (array_for_all2 M.eq (Carray.to_array res) exp_res)

  let test_fill () =
    let n = 1 + Random.int 30 in
    let r = M.fresh () in
    let carray = Carray.init n (fun _ -> M.fresh ()) in
    let array = Carray.to_array carray in
    let pos = Random.int n in
    let len = Random.int (n - pos) in
    Array.fill array pos len r ;
    Carray.fill carray pos len r ;
    assert (array_for_all2 M.eq array (Carray.to_array carray))

  let test_blit () =
    let n = 1 + Random.int 30 in
    let n' = 1 + Random.int 30 in
    let carray = Carray.init n (fun _ -> M.fresh ()) in
    let carray' = Carray.init n' (fun _ -> M.fresh ()) in
    let array = Carray.to_array carray in
    let array' = Carray.to_array carray in
    let pos = Random.int (min n n') in
    let pos' = Random.int (min n' n) in
    let len = Random.int (min n n' - max pos pos') in
    Array.blit array pos array' pos' len ;
    Carray.blit carray pos carray' pos' len ;
    assert (array_for_all2 M.eq array (Carray.to_array carray))

  let test_concat () =
    let k = 1 + Random.int 10 in
    let l =
      List.init k (fun _ ->
          Carray.init (1 + Random.int 30) (fun _ -> M.fresh ()))
    in
    let exp_res = Array.concat (List.map Carray.to_array l) in
    let res = Carray.concat l in
    assert (array_for_all2 M.eq (Carray.to_array res) exp_res)

  let test_mem () =
    let n = 1 + Random.int 30 in
    let carray = Carray.init n (fun _ -> M.fresh ()) in
    assert (Carray.mem (Carray.get carray 0) carray) ;
    assert (not (Carray.mem r carray))

  let test_exists () =
    let n = 1 + Random.int 30 in
    let carray = Carray.init n (fun _ -> M.fresh ()) in
    assert (Carray.exists (fun x -> M.eq x (Carray.get carray 0)) carray) ;
    assert (not (Carray.exists (fun x -> M.eq r x) carray))

  let test_map () =
    let n = 1 + Random.int 30 in
    let array = Array.init n (fun _ -> M.fresh ()) in
    let f x = M.add x r in
    let carray = Carray.init n (fun i -> array.(i)) in
    let res = Carray.map f carray in
    let exp_res = Array.map f array in
    assert (array_for_all2 M.eq exp_res (Carray.to_array res))

  let test_mapi () =
    let n = 1 + Random.int 30 in
    let array = Array.init n (fun _ -> M.fresh ()) in
    let array' = Array.init n (fun _ -> M.fresh ()) in
    let f i x = M.add (M.add x r) (Array.get array' i) in
    let carray = Carray.init n (fun i -> array.(i)) in
    let res = Carray.mapi f carray in
    let exp_res = Array.mapi f array in
    assert (array_for_all2 M.eq exp_res (Carray.to_array res))

  let test_of_list () =
    let n = 1 + Random.int 30 in
    let array = Array.init n (fun _ -> M.fresh ()) in
    let carray = Carray.of_list (Array.to_list array) in
    assert (array_for_all2 M.eq array (Carray.to_array carray))

  let test_to_list () =
    let n = 1 + Random.int 30 in
    let array = Array.init n (fun _ -> M.fresh ()) in
    let clist = Carray.to_list (Carray.of_array array) in
    assert (List.for_all2 M.eq (Array.to_list array) clist)

  let test_for_all () =
    let n = 1 + Random.int 30 in
    let array = Array.init n (fun _ -> M.(copy zero)) in
    let carray = Carray.of_array array in
    assert (Carray.for_all M.is_zero carray)

  let get_tests name =
    let open Alcotest in
    let test_case a b c =
      Gc.full_major () ;
      test_case a b c
    in
    [ ( Printf.sprintf "Instantiate with %s" name,
        [ test_case "init" `Quick test_init;
          test_case "make" `Quick test_make;
          test_case "sub" `Quick test_sub;
          test_case "set" `Quick test_set;
          test_case "copy" `Quick test_copy_returns_a_correct_fresh_copy;
          (* FIXME: find another way to test than using inplace operations *)
          (* test_case "iter" `Quick test_iter_add_inplace; *)
          (* test_case "iteri" `Quick test_iteri_add_inplace; *)
          test_case "map" `Quick test_map;
          test_case "mapi" `Quick test_mapi;
          test_case "append" `Quick test_append;
          test_case "concat" `Quick test_concat;
          test_case "mem" `Quick test_mem;
          test_case "exists" `Quick test_exists;
          test_case "fill" `Quick test_fill;
          test_case "blit" `Quick test_blit;
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

  let fresh () = random ()
end)

module BLS_G2_tests = Make (struct
  include Bls12_381.G2

  let fresh () = random ()
end)

module Int64_tests = Make (struct
  type t = Int64.t

  let add = Int64.add

  let zero = Int64.zero

  let is_zero x = Int64.equal x Int64.zero

  let eq = Int64.equal

  let copy x = x

  let fresh () = Int64.of_int (Random.int 1_000_000_000)
end)

module Int32_tests = Make (struct
  type t = Int32.t

  let add = Int32.add

  let zero = Int32.zero

  let is_zero x = Int32.equal x Int32.zero

  let eq = Int32.equal

  let copy x = x

  let fresh () = Int32.of_int (Random.int 1_000_000_000)
end)

module Nativeint_tests = Make (struct
  type t = nativeint

  let add = Nativeint.add

  let zero = Nativeint.zero

  let is_zero x = Nativeint.equal x Nativeint.zero

  let eq = Nativeint.equal

  let copy x = x

  let fresh () = Nativeint.of_int (Random.int 1_000_000_000)
end)

let () =
  let open Alcotest in
  run
    "Carray"
    (BLS_Fr_tests.get_tests "Bls12_381.Fr"
    @ BLS_G1_tests.get_tests "Bls12_381.G1"
    @ Nativeint_tests.get_tests "Nativeint"
    @ Int32_tests.get_tests "Int32"
    @ Int64_tests.get_tests "Int64"
    @ BLS_G2_tests.get_tests "Bls12_381.G2")
