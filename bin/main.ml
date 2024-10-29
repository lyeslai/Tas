open Ast
open Terms
open Type


(*
let term = App (Abs ("x", Var "x"), Var "y") in
let result = ltr_cbv_norm_with_timeout term 100 in
print_endline ("Résultat : " ^ print_term result)
*)

(*
let term = App (Abs ("x", App (Var "x", Var "x")), Abs ("x", App (Var "x", Var "x"))) in
try
  let result = ltr_cbv_norm_timeout term 10 in
  print_endline ("Résultat : " ^ print_term result)
with
| Timeout -> print_endline "Divergence : le terme dépasse la limite des étapes"
*)

(* bin/main.ml *)

let test_normalisation term max_steps =
  try
    let result = ltr_cbv_norm_timeout term max_steps in
    print_endline ("Résultat : " ^ print_term result)
  with
  | Timeout -> print_endline "Divergence : limite d'étapes atteinte"

  let () =
  (* Tester différents termes *)
  print_endline "Test de l'identité I :";
  test_normalisation i 100;

  print_endline "Test de delta (duplication) :";
  test_normalisation delta 100;

  print_endline "Test de Omega (diverge) :";
  test_normalisation omega 100;

  print_endline "Test de S combinator :";
  test_normalisation s 100;

  print_endline "Test de S K K :";
  test_normalisation skk 100;

  print_endline "Test de S I I :";
  test_normalisation sii 100;

  print_endline "Test de Church 0 :";
  test_normalisation church_0 100;

  print_endline "Test de Church 1 :";
  test_normalisation church_1 100;

  print_endline "Test de Church 2 :";
  test_normalisation church_2 100;

  print_endline "Test de Church 3 :";
  test_normalisation church_3 100;

  print_endline "Test du successeur de 0 :";
  test_normalisation (App (succ, church_0)) 100;

  print_endline "Test de l'addition de 1 et 2 :";
  test_normalisation (App (App (add, church_1), church_2)) 100;

  print_endline "Test de la multiplication de 2 et 3 :";
  test_normalisation (App (App (mult, church_2), church_3)) 100;

  print_endline "Test de la puissance 2^3 :";
  test_normalisation (App (App (pow, church_2), church_3)) 100
