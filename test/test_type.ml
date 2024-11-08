open Ast
open Type


let env = []  (* Environnement vide *)

(* Fonction identité *)
let term_id = Abs ("x", Var "x")
let type_id = inferer_type term_id env 100
let () = match type_id with
  | Some ty -> print_endline ("Type de l'identité : " ^ print_type ty)
  | None -> print_endline "Échec de typage pour l'identité"

(* Application de l'identité à une variable y *)
let term_app_id = App (term_id, Var "y")
let type_app_id = inferer_type term_app_id [("y", Nat)] 100
let () = match type_app_id with
  | Some ty -> print_endline ("Type de (λx.x) y : " ^ print_type ty)
  | None -> print_endline "Échec de typage pour (λx.x) y"

(* Fonction constante λx.λy.x *)
let term_const = Abs ("x", Abs ("y", Var "x"))
let type_const = inferer_type term_const env 100
let () = match type_const with
  | Some ty -> print_endline ("Type de la constante : " ^ print_type ty)
  | None -> print_endline "Échec de typage pour la constante"

(* Fonction de composition λf.λg.λx.f (g x) *)
let term_comp = Abs ("f", Abs ("g", Abs ("x", App (Var "f", App (Var "g", Var "x")))))
let type_comp = inferer_type term_comp env 100
let () = match type_comp with
  | Some ty -> print_endline ("Type de la composition : " ^ print_type ty)
  | None -> print_endline "Échec de typage pour la composition"
