open Ast


type ptype = 
  | Var of string
  | Arr of ptype * ptype
  | Nat 
  | List of ptype
  | Forall of string * ptype
  | Ref of ptype
  | Unit
  | Prod of ptype * ptype   (* Nouveau : type produit *)
  | Sum of ptype * ptype    (* Nouveau : type somme *)


let rec print_type (t : ptype) : string =
  match t with
  | Var x -> x
  | Nat -> "Nat"
  | Arr (t1 , t2) -> "(" ^ ( print_type t1) ^" -> "^ ( print_type t2) ^")"
  | List t -> "list : [" ^ print_type t ^ "]"
  | Forall (x, t) -> "forall : ( " ^ x ^ "."  ^ print_type t ^ ")"  
  | Unit -> "Unit"
  | Ref t -> "Ref : (" ^ print_type t ^ ")"
  | Prod (t1, t2) -> "(" ^ print_type t1 ^ " * " ^ print_type t2 ^ ")"
  | Sum (t1, t2) -> "(" ^ print_type t1 ^ " + " ^ print_type t2 ^ ")"

let compteur_var_t : int ref = ref 0
let nouvelle_var_t () : string =
  compteur_var_t := !compteur_var_t + 1;
  "T" ^ string_of_int !compteur_var_t


type equa = (ptype * ptype) list 
type env = (string * ptype) list


let rec cherche_type (v : string) (e : env) : ptype =
  match e with 
  | [] -> failwith ("variable " ^ v ^ " pas présente dans l'environnement")
  | (x, t) :: reste -> if x = v then t else cherche_type v reste




let generaliser (env : env) (t : ptype) : ptype =
  let rec vars_libres (t : ptype) (acc : string list) =
    match t with
    | Var x -> if List.mem x acc then acc else x :: acc
    | Arr (t1, t2) -> vars_libres t2 (vars_libres t1 acc)
    | List t1 -> vars_libres t1 acc
    | Forall (x, t1) -> List.filter (fun v -> v <> x) (vars_libres t1 acc)
    | Nat -> acc
    | Ref t1 -> vars_libres t1 acc 
    | Unit -> acc
  in
  let fv = vars_libres t [] in
  let env_vars = List.fold_left (fun acc (_, t) -> vars_libres t acc) [] env in
  let libres = List.filter (fun x -> not (List.mem x env_vars)) fv in
  List.fold_right (fun x acc -> Forall (x, acc)) libres t



  
let rec genere_equa (te : pterm) (ty : ptype) (e : env) : equa =
  match te with
  | Var v ->
      let tv = cherche_type v e in [(tv, ty)]
  | App (t1, t2) ->
      let ta = Var (nouvelle_var_t()) in
      let eq1 = genere_equa t1 (Arr (ta, ty)) e in
      let eq2 = genere_equa t2 ta e in 
      eq1 @ eq2
  | Abs (x, body) -> 
      let ta = Var (nouvelle_var_t()) in 
      let tr = Var (nouvelle_var_t()) in  
      let eq = (ty, Arr (ta, tr)) in 
      let env_ajout = (x, ta) :: e in 
      let eq_body = genere_equa body tr env_ajout in  
      eq :: eq_body



  | Nat _ -> [(ty, Nat)]
  | Add (t1, t2) ->
      let eq1 = genere_equa t1 Nat e in 
      let eq2 = genere_equa t2 Nat e in 
      (ty, Nat) :: (eq1 @ eq2)
  | Sub (t1, t2) ->
      let eq1 = genere_equa t1 Nat e in 
      let eq2 = genere_equa t2 Nat e in 
      (ty, Nat) :: (eq1 @ eq2)
  | Mult (t1, t2) ->
      let eq1 = genere_equa t1 Nat e in 
      let eq2 = genere_equa t2 Nat e in 
      (ty, Nat) :: (eq1 @ eq2)

      (*elle est bien ???*)
  | List t -> 
      let element_type = Var (nouvelle_var_t()) in
      let eq_list = List.fold_left (fun acc t -> acc @ genere_equa t element_type e) [] t in
      (ty, List element_type) :: eq_list
  | Cons (head, tail) -> 
      let te = Var (nouvelle_var_t()) in
      let eq1 = genere_equa head te e in
      let eq2 = genere_equa tail (List te) e in
      (ty, List te) :: (eq1 @ eq2)
  | Head t -> 
      let te = Var (nouvelle_var_t()) in 
      let eq = genere_equa t (List te) e in 
      (ty, te) :: eq
  | Tail t -> 
      let te = Var (nouvelle_var_t()) in 
      let eq = genere_equa t (List te) e in 
      (ty, List te) :: eq
  | IfZero (cond, t1, t2) ->  
      let eq1 = genere_equa cond Nat e in 
      let eq2 = genere_equa t1 ty e in
      let eq3 = genere_equa t2 ty e in 
      eq1 @ eq2 @ eq3
  | IfEmpty (cond, t1, t2) -> 
      let te = Var (nouvelle_var_t()) in 
      let eq1 = genere_equa cond (List te) e in 
      let eq2 = genere_equa t1 ty e in
      let eq3 = genere_equa t2 ty e in 
      eq1 @ eq2 @ eq3
  | Let (x, t1, t2) ->
      let t0 = Var (nouvelle_var_t()) in
      let eq1 = genere_equa t1 t0 e in
      let t0_gen = generaliser e t0 in
      let env_ajout = (x, t0_gen) :: e in
      let eq2 = genere_equa t2 ty env_ajout in
      eq1 @ eq2
  | Fix t ->
      let ta = Var (nouvelle_var_t()) in
      let eq = genere_equa t (Arr (ta, ty)) e in
      eq

  | Unit -> [(ty, Unit)]
  | Ref t ->
      let te = Var (nouvelle_var_t()) in
      let eq = genere_equa t te e in
      (ty, Ref te) :: eq
  | Deref t ->  
      let te = Var (nouvelle_var_t()) in
      let eq = genere_equa t (Ref te) e in
      (ty, te) :: eq
  | Assign (t1, t2) ->  
      let te = Var (nouvelle_var_t()) in
      let eq1 = genere_equa t1 (Ref te) e in
      let eq2 = genere_equa t2 te e in
      (ty, Unit) :: (eq1 @ eq2)  

  | Pair (t1, t2) ->
      let t1_type = Var (nouvelle_var_t()) in
      let t2_type = Var (nouvelle_var_t()) in
      let eq1 = genere_equa t1 t1_type e in
      let eq2 = genere_equa t2 t2_type e in
      (ty, Prod (t1_type, t2_type)) :: (eq1 @ eq2)
  | Fst t ->
      let t1_type = Var (nouvelle_var_t()) in
      let t2_type = Var (nouvelle_var_t()) in
      let eq = genere_equa t (Prod (t1_type, t2_type)) e in
      (ty, t1_type) :: eq
  | Snd t ->
      let t1_type = Var (nouvelle_var_t()) in
      let t2_type = Var (nouvelle_var_t()) in
      let eq = genere_equa t (Prod (t1_type, t2_type)) e in
      (ty, t2_type) :: eq
  | Inl t ->
      let t1_type = Var (nouvelle_var_t()) in
      let t2_type = Var (nouvelle_var_t()) in
      let eq = genere_equa t t1_type e in
      (ty, Sum (t1_type, t2_type)) :: eq
  | Inr t ->
      let t1_type = Var (nouvelle_var_t()) in
      let t2_type = Var (nouvelle_var_t()) in
      let eq = genere_equa t t2_type e in
      (ty, Sum (t1_type, t2_type)) :: eq
  | Case (t, (x1, t1), (x2, t2)) ->
      let t1_type = Var (nouvelle_var_t()) in
      let t2_type = Var (nouvelle_var_t()) in
      let result_type = Var (nouvelle_var_t()) in
      let eq1 = genere_equa t (Sum (t1_type, t2_type)) e in
      let eq2 = genere_equa t1 result_type ((x1, t1_type) :: e) in
      let eq3 = genere_equa t2 result_type ((x2, t2_type) :: e) in
      (ty, result_type) :: (eq1 @ eq2 @ eq3)
  
let rec occur_check (v : string) (t : ptype) : bool = 
  match t with 
  | Var x -> x = v 
  | Arr (t1, t2) -> occur_check v t1 || occur_check v t2 
  | Nat -> false
  | List t -> occur_check v t
  | Forall (_, t) -> occur_check v t
  | Unit -> false
  | Ref t -> occur_check v t


let rec substitue (v : string) (substitution : ptype) (t : ptype) : ptype = 
  match t with 
  | Var x -> if x = v then substitution else t 
  | Arr (t1, t2) -> Arr (substitue v substitution t1, substitue v substitution t2) 
  | Nat -> t
  | List t -> List (substitue v substitution t)
  | Forall (x, t) -> 
      if x = v then Forall (x, t)
      else Forall (x, substitue v substitution t)
  | Unit -> t
  | Ref t -> Ref (substitue v substitution t)
  | Prod (t1, t2) -> Prod (substitue v substitution t1, substitue v substitution t2)
  | Sum (t1, t2) -> Sum (substitue v substitution t1, substitue v substitution t2)


let substitue_equations (v : string) (substitution : ptype) (eq : equa) : equa = 
  List.map (fun (t1, t2) -> (substitue v substitution t1, substitue v substitution t2)) eq
  

let rec unification_step (eq : equa) : equa option = 
  match eq with 
  | [] -> Some [] 
  | (t1, t2) :: rest ->
     if t1 = t2 then unification_step rest 
    else 
      match (t1, t2) with
      | (Var x, t) | (t, Var x) -> 
          if occur_check x t then None 
          else
            let sub_rest = substitue_equations x t rest in 
            (match unification_step sub_rest with 
            | Some r -> Some ((Var x, t) :: r)
            | None -> None)
      | Arr (tga, tgr), Arr (tda, tdr) ->
          unification_step ((tga, tda) :: (tgr, tdr) :: rest)

      | List t1, List t2 -> unification_step ((t1,t2):: rest)
      | Forall (x,t1) , t2 | t2, Forall (x, t1) -> 
        let t1_ouvert = substitue x (Var (nouvelle_var_t())) t1 in 
        unification_step ((t1_ouvert , t2) :: rest) 
      | Ref t1, Ref t2 -> unification_step ((t1, t2) :: rest)
      | (Prod (t1a, t1b), Prod (t2a, t2b)) :: rest ->
        unification_step ((t1a, t2a) :: (t1b, t2b) :: rest)
      | (Sum (t1a, t1b), Sum (t2a, t2b)) :: rest ->
        unification_step ((t1a, t2a) :: (t1b, t2b) :: rest)
      | _ -> None 

let rec unification (eq : equa) : equa option = 
  match unification_step eq with 
  | Some [] -> Some []
  | Some new_eq when new_eq = eq -> Some new_eq
  | Some new_eq -> unification new_eq
  | None -> None


exception Timeout

let rec resoudre_equations (equations : equa) (etapes_restantes : int) : equa option = 
  if etapes_restantes <= 0 then 
    raise Timeout
  else  
    match unification_step equations with 
    | Some [] -> Some []
    | Some new_eq when new_eq = equations -> Some new_eq
    | Some new_eq -> resoudre_equations new_eq (etapes_restantes - 1)
    | None -> None


let rec appliquer_substitution (substitutions : equa) (t : ptype) : ptype =
  match substitutions with
  | [] -> t
  | (Var v, t') :: rest -> appliquer_substitution rest (substitue v t' t)
  | _ -> t


let inferer_type (term : pterm) (env : env) (limit : int) : ptype option =
  let t = Var (nouvelle_var_t()) in
  let equations = genere_equa term t env in
  try
    match resoudre_equations equations limit with
    | None -> print_endline "Échec de l'unification des équations"; None
    | Some eqs -> 
        let final_type = appliquer_substitution eqs t in
        Some final_type
  with
  | Timeout -> print_endline "Timeout atteint lors de l'unification"; None
