(* Liam Murphy *)
let gen_larg_odd_int power =
  let rec loop n power =
    let dig = Z.of_int (Random.int 10) in
    match power = 0 with
    | true -> (
        match Z.rem dig (Z.of_int 2) = Z.zero with
        | true -> Z.add n (Z.succ dig)
        | false -> Z.add n dig
      )
    | false ->
      let num = Z.mul dig (Z.pow (Z.of_int 10) power)
      in
      loop (Z.add num n) (power - 1)
  in
  loop Z.zero power
;;

let rec fermat_test n n_trials =
  match n_trials = 0 with
  | true -> true
  | false ->
    let a = Z.of_int (Random.int (999999999)) in
    let modnum = Z.powm a (Z.pred n) n in
    match Z.equal modnum Z.one with
    | true -> fermat_test n (n_trials - 1)
    | false -> false
;;

let rec fermat_prime num_digs n_trials =
  let n = gen_larg_odd_int num_digs in
  match fermat_test n n_trials with
  | true -> n
  | false -> fermat_prime num_digs n_trials
;;

let _ = print_string (Z.to_string (fermat_prime 2000 10)); print_newline (); print_float (Sys.time ())
