open Fermat_prime

let rec ext_euclid n1 n2 =
  match Z.equal n2 Z.zero with
  | true -> (Z.one, Z.zero, n1)
  | false ->
    let (q, r) = Z.ediv_rem n1 n2 in
    let (a, b, d) = ext_euclid n2 r in
    (b, (Z.sub a (Z.mul b q)), d)
;;

let public_key p q =
  let n = Z.mul p q in
  let k = Z.mul (Z.pred p) (Z.pred q) in
  let rec loop k e =
    match Z.equal (Z.gcd e k) Z.one with
    | true -> (n, e)
    | false -> loop k (Z.of_int (Random.int 100000))
  in
  loop k (Z.of_int (Random.int 100000))
;;

let get_decryption_exp p q =
  let e = snd (public_key p q) in
  let k = Z.mul (Z.pred p) (Z.pred q) in
  let (d, _, _) = ext_euclid e k in
  d
;;

let encrypt m n e =
  let l = String.length m in
  let text =
    if String.length (Z.to_string text) < String.length (Z.to_string n) then
