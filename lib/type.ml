type ptype = 
| Var of string
| Arr of ptype * ptype
| Nat 


let rec print_type (t : ptype) : string =
  match t with
  | Var x -> x
  | Nat -> "Nat"
  | Arr (t1 , t2) -> "(" ˆ ( print_type t1) ˆ" -> "ˆ ( print_type t2) ˆ")"
  


let compteur_var_t : int ref = ref 0

let nouvelle_var_t () : string = compteur_var := ! compteur_var + 1;
  "T"ˆ( string_of_int ! compteur_var )


  type equa = (ptype * ptype) list 

  type env = (string * ptype) list

let rec cherche_type (v : string) (e : env) : ptype =
  match e with 
  | [] -> failwith("variable pas presente dans l'env")
  | (x,t) :: reste -> if x = v then t else cherche_type v reste
  


let rec genere_equa (te : pterm) (ty : ptype) (e : env) : equa =
  match te with
  Var v ->
    let tv = cherche_type v env in [(tv,ty)]
  | App (t1 , t2) -> 
    let ta = Var (nouvelle_var_t()) in
    let eq1 = genere_equa t1 (Arr(ta, ty )) env in
    let eq2 = genere_equa t2 ta env in 
    eq1 @ eq2
  | Abs (x, body) -> 
    let ta = Var(nouvelle_var_t()) in 
    let tr = Var (nouvelle_var_t()) in  
    let eq = (ty , Arr (ta , tr)) in 
    let env_ajout = (x, ta) :: e in 
    let eq_body = genere_equa t tr env_ajout in 
    eq :: eq_body


let rec occur_check (v: string) (t : ptype) : bool = 
  match t with 
  |Var x ->  x = v 
  |Arr (t1,t2) ->  occur_check v t1 || occur_check v t2 
  |Nat -> false


let rec substitue (v : string) (substitution : ptype) (t:ptype) : ptype = 
  match t with 
  |Var x -> if x = v then substitution else t 
  |Arr (t1,t2) -> Arr (substitue v substitution t1, substitue v substitution t2) 
  |Nat -> Nat

let rec substitue_equations (v: string) (substitution : ptype) (eq : equa) : ptype = 
  List.map(fun (t1,t2) -> (substitue v substitution t1 , substitue v substitution t2)) eq

  

let unification_step (eq : equa) : equa option = 
  match eq with 
  |[] -> Some [] 
  |(t1,t2) :: rest -> if t1 = t2 then unification_step rest 
  else 
    match (t1,t2) with
    | (Var x, t) | (t , Var x) -> 
    if occur_check v t then 
      None 
    else
      let sub_rest = substitue_equations v t rest in 
      (match unification_step sub_rest with 
      |Some r -> Some ((Var x,t) :: result)
      |None -> None) 
    | Arr(tga , tgr), Arr(tda, tdr) ->
      unification_step ((tga,tda) :: (tgr,tdr) :: rest)
    | _ -> None 

let rec unification (eq : equa) : equa option = 
  match unification_step eq with 
  |Some [] -> Some []
  |Some new_eq when new_eq = eq -> Some new_eq
  |Some new_eq -> unification new_eq
  |None -> None


exception Timeout

let rec resoudre_equations (equations : equa) (etapes_restantes : int) : equa option = 
  if etapes_restantes <= 0 then 
    raise Timeout
else  
  match unification_step etapes_restantes with 
  |Some [] -> Some []
  |Some new_eq when new_eq = etapes_restantes -> Some new_eq
  |Some new_eq -> resoudre_equations new_eq (etapes_restantes - 1)
  |None -> None

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
  