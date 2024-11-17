open Ast
open Type

let print_result term =
  match ltr_cbv_norm term with
  | Nat n -> Printf.printf "Résultat : %d\n" n
  | List l -> Printf.printf "Résultat : %s\n" (print_term (List l))
  | t -> Printf.printf "Résultat : %s\n" (print_term t)

(* Tests pour les opérations arithmétiques, listes, conditions, références, assignations, et fonctions *)

(* Tests d'addition, soustraction, multiplication *)
let test_addition =
  let term = Add (Nat 3, Nat 5) in
  print_endline "Test Addition : 3 + 5";
  print_result term
let test_soustraction = 
  let term = Sub (Nat 10, Nat 4) in
  print_endline "Test Soustraction : 10 - 4";
  print_result term
let test_multiplication =
  let term = Mult (Nat 6, Nat 7) in
  print_endline "Test Multiplication : 6 * 7";
  print_result term

(* Tests sur les listes *)
let test_list = 
  let term = List [Nat 1; Nat 2; Nat 3] in
  print_endline "Test Liste : [1; 2; 3]";
  print_result term

let test_head =
  let term = Head (List [Nat 5; Nat 10; Nat 15]) in
  print_endline "Test Tête de liste : head([5; 10; 15])";
  print_result term
  
(* Test de queue de liste *)
let test_tail =
  let term = Tail (List [Nat 5; Nat 10; Nat 15]) in
  print_endline "Test Queue de liste : tail([5; 10; 15])";
  print_result term

(* Tests pour IfZero et IfEmpty *)
let test_if_zero =
  let term = IfZero (Nat 0, Nat 1, Nat 2) in
  print_endline "Test IfZero : if 0 then 1 else 2";
  print_result term

let test_if_zero_non_zero =
  let term = IfZero (Nat 5, Nat 1, Nat 2) in
  print_endline "Test IfZero avec un non-zéro : if 5 then 1 else 2";
  print_result term

(* Test IfEmpty *)
let test_if_empty =
  let term = IfEmpty (List [], Nat 1, Nat 2) in
  print_endline "Test IfEmpty : if empty([]) then 1 else 2";
  print_result term

(* Tests pour Ref, Deref, Assign *)
let test_ref =
  let term = Ref (Nat 42) in
  print_endline "Test Référence : Ref 42";
  print_result term

let test_deref =
  let term = Deref (Ref (Nat 42)) in
  print_endline "Test Déréférencement : !(Ref 42)";
  print_result term

let test_assign =
  let term = Assign (Ref (Nat 42), Nat 21) in
  print_endline "Test Assignation : (Ref 42) := 21";
  print_result term

(* Tests pour Unit *)
let test_unit =
  let term = Unit in
  print_endline "Test Unit : ()";
  print_result term

(* Tests de fonctions polymorphes *)
let test_identite =
  let term = App (Abs ("x", Var "x"), Nat 42) in
  print_endline "Test Identité (polymorphe) : λx.x avec 42";
  print_result term

let test_identite_liste =
  let term = App (Abs ("x", Var "x"), List [Nat 1; Nat 2; Nat 3]) in
  print_endline "Test Identité (polymorphe) : λx.x avec [1; 2; 3]";
  print_result term

(* Test de la fonction de composition *)
let test_composition = 
  let compose = Abs ("f", Abs ("g", Abs ("x", App (Var "f", App (Var "g", Var "x"))))) in
  let term = App (App (compose, Abs ("y", Add (Var "y", Nat 1))), Abs ("z", Mult (Var "z", Nat 2))) in
  let full_term = App (term, Nat 3) in
  print_endline "Test Composition (polymorphe) : (λf.λg.λx.f(g(x))) (λy.y+1) (λz.z*2) avec 3";
  print_result full_term

(* Test de la fonction factorielle *)
let test_factorielle =
  let factorial = Fix (Abs ("fact",
                      Abs ("n",
                          IfZero (Var "n",
                                  Nat 1,
                                  Mult (Var "n", App (Var "fact", Sub (Var "n", Nat 1))))))) in
  let term = App (factorial, Nat 5) in
  print_endline "Test Factorielle : factorial(5)";
  print_result term

(* Fonction main pour exécuter tous les tests *)
let main () =
  test_addition;
  test_soustraction;
  test_multiplication;
  test_list;
  test_head;
  test_tail;
  test_if_zero;
  test_if_zero_non_zero;
  test_if_empty;
  test_ref;
  test_deref;
  test_assign;
  test_unit;
  test_identite;
  test_identite_liste;
  test_composition;
  test_factorielle

let () = main ()
