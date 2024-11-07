
type pterm = 
| Var of string
| App of pterm * pterm
| Abs of string * pterm


let rec print_term (t : pterm) : string =
    match t with
    Var x -> x
    | App (t1 , t2) -> "(" ^ ( print_term t1) ^" "^ ( print_term t2) ^ ")"
    | Abs (x, t) -> "(fun "^ x ^" -> " ^ ( print_term t) ^")"


let compteur_var : int ref = ref 0
let nouvelle_var () : string = compteur_var := ! compteur_var + 1;
"X"^( string_of_int ! compteur_var )


(*utilisation de deux fonction var_mapping = liste de correspondance
rechercheVAr = c'est juste fonction qui une variable et elle cherche dans var_mapping*)
let  alphaconv (t : pterm) : pterm =
    let rec rechercheVar x mapvar = 
        match mapvar with
        | [] -> x 
        | (y, new_var) :: reste -> if x = y then new_var else rechercheVar x reste
    in
    
    let rec alphaconv_aux (p_aux : pterm) (var_mapping : (string * string) list) : pterm =
        match p_aux with 
        | Var x -> 
            Var (rechercheVar x var_mapping)
        | App (t1,t2) -> 
            App (alphaconv_aux t1 var_mapping, alphaconv_aux t2 var_mapping)
        | Abs (x, body) ->
            let new_var = nouvelle_var() in 
            let new_var_mapping = (x, new_var) :: var_mapping in 
            Abs (new_var, alphaconv_aux body new_var_mapping)


        | Nat -> _ p_aux
        | Add (t1, t2) -> Add (alphaconv_aux t1 var_mapping, alphaconv_aux t2 var_mapping)
        | Sub (t1, t2) -> Sub (alphaconv_aux t1 var_mapping, alphaconv_aux t2 var_mapping)
        | List ts -> List (List.map(fun t -> alphaconv_aux t var_mapping) ts)
        | Cons (h ,t) -> Cons (alphaconv_aux h var_mapping, alphaconv_aux t var_mapping)
        | Head t -> Head (alphaconv_aux t var_mapping)
        | Tail t -> Tail (alphaconv_aux t var_mapping)
        | IsEmpty t -> IsEmpty (alphaconv_aux t var_mapping)
        | IfZero (cond, t1, t2) -> IfZero (alphaconv_aux cond var_mapping, alphaconv_aux t1 var_mapping, alphaconv_aux t2 var_mapping)
        | IfEmpty (cond, t1, t2) -> IfEmpty (alphaconv_aux cond var_mapping, alphaconv_aux t1 var_mapping, alphaconv_aux t2 var_mapping)
        | Let (v, e1, e2) ->
            let new_var = nouvelle_var () in
            let new_var_mapping = (v, new_var) :: var_mapping in
            Let (new_var, alphaconv_aux e1 var_mapping, alphaconv_aux e2 new_var_mapping)
        | Fix (v, body) ->
            let new_var = nouvelle_var () in
            let new_var_mapping = (v, new_var) :: var_mapping in
            Fix (new_var, alphaconv_aux body new_var_mapping)

        in alphaconv_aux t []
            

let rec substitution (x : string) (n : pterm) (t: pterm) : pterm =
    match t with 
    | Var v -> 
        if v = x then n else Var v    
    | App(t1 ,t2 ) ->
        App (substitution x n t1, substitution x n t2)
    | Abs(v, body) ->
        if v = x then Abs(v , body)
        else 

             (* Si v est une variable libre dans n, on doit éviter la capture. *)
        if substitution v (Var v) n <> n then
            (* On renomme la variable liée pour éviter la capture. *)
            let new_var = nouvelle_var () in
            let renamed_body = substitution v (Var new_var) body in
            Abs (new_var, substitution x n renamed_body)
          else
            (* Sinon, on applique la substitution directement dans le corps. *)
            Abs (v, substitution x n body)




    | Nat _ -> t
    | Add (t1, t2) -> Add (substitution x n t1, substitution x n t2)
    | Sub (t1, t2) -> Sub (substitution x n t1, substitution x n t2)
    | List ts -> List (List.map (substitution x n) ts)
    | Cons (h, t2) -> Cons (substitution x n h, substitution x n t2)
    | Head t1 -> Head (substitution x n t1)
    | Tail t1 -> Tail (substitution x n t1)
    | IsEmpty t1 -> IsEmpty (substitution x n t1)
    | IfZero (cond, t1, t2) -> IfZero (substitution x n cond, substitution x n t1, substitution x n t2)
    | IfEmpty (cond, t1, t2) -> IfEmpty (substitution x n cond, substitution x n t1, substitution x n t2)
    | Let (v, e1, e2) ->
        if v = x then Let (v, substitution x n e1, e2)
        else Let (v, substitution x n e1, substitution x n e2)
    | Fix (v, body) ->
        if v = x then t else Fix (v, substitution x n body)            





    


(*Ltr-CbV Syntaxe des valeur : qu'on peut pas reduire 
-Var , abs , app
Beta-reduction uniquement si l'argument est une valeur
On reduit d'abord la fonction et apres l'argument
On reduit pas sous le lambda*)        

(*on doit check si c'est une valeur ou pas*)
let valeur (t : pterm) : bool = 
    match t with 
    | Var _ -> true
    | Abs (_,_) -> true
    | _ -> false

    let rec ltr_cbv_step (t : pterm) : pterm option =
        match t with
        | Var _ | Abs _ | Nat _ | List _ -> None

        | App (Abs (x, body), arg) when valeur arg -> Some (substitution x arg body)
        | App (t1, t2) when not (valeur t1) -> (
            match ltr_cbv_step t1 with
            | Some t1' -> Some (App (t1', t2))
            | None -> None)
        | App (t1, t2) -> (
            match ltr_cbv_step t2 with
            | Some t2' -> Some (App (t1, t2'))
            | None -> None)
      
        (* Cas de l'opérateur let *)
        | Let (x, e1, e2) -> 
            (match ltr_cbv_step e1 with
            | Some e1' -> Some (Let (x, e1', e2))  (* Continue d'évaluer e1 si nécessaire *)
            | None -> Some (substitution x e1 e2))  (* Remplace x par e1 dans e2 *)
      
        (* Cas de l'opérateur fix pour récursivité *)
        | Fix (phi, M) -> Some (substitution phi t M)  (* Remplace phi par fix (phi -> M) dans M *)
      
        (* Cas de IfZero *)
        | IfZero (Nat 0, t1, _) -> Some t1  (* Si la condition est zéro, évalue le cas "then" *)
        | IfZero (Nat _, _, t2) -> Some t2  (* Si la condition est non zéro, évalue le cas "else" *)
        | IfZero (c, t1, t2) -> 
            (match ltr_cbv_step c with
            | Some c' -> Some (IfZero (c', t1, t2))
            | None -> None)  (* Continue à évaluer la condition *)
      
        (* Cas de IfEmpty *)
        | IfEmpty (List [], t1, _) -> Some t1  (* Si la liste est vide, évalue le cas "then" *)
        | IfEmpty (List _, _, t2) -> Some t2   (* Si la liste n'est pas vide, évalue le cas "else" *)
        | IfEmpty (c, t1, t2) ->
            (match ltr_cbv_step c with
            | Some c' -> Some (IfEmpty (c', t1, t2))
            | None -> None)  (* Continue à évaluer la condition *)
      
        (* Cas de l'addition *)
        | Add (Nat n1, Nat n2) -> Some (Nat (n1 + n2))
        | Add (t1, t2) when not (valeur t1) -> (
            match ltr_cbv_step t1 with
            | Some t1' -> Some (Add (t1', t2))
            | None -> None)
        | Add (t1, t2) -> (
            match ltr_cbv_step t2 with
            | Some t2' -> Some (Add (t1, t2'))
            | None -> None)
      
        (* Cas de la soustraction *)
        | Sub (Nat n1, Nat n2) -> Some (Nat (n1 - n2))
        | Sub (t1, t2) when not (valeur t1) -> (
            match ltr_cbv_step t1 with
            | Some t1' -> Some (Sub (t1', t2))
            | None -> None)
        | Sub (t1, t2) -> (
            match ltr_cbv_step t2 with
            | Some t2' -> Some (Sub (t1, t2'))
            | None -> None)
      
        (* Cas pour les listes et leurs opérations *)
        | Cons (h, List tl) -> Some (List (h :: tl))
        | Cons (h, t) -> (
            match ltr_cbv_step h with
            | Some h' -> Some (Cons (h', t))
            | None -> (
                match ltr_cbv_step t with
                | Some t' -> Some (Cons (h, t'))
                | None -> None))
      
        | Head (List (h :: _)) -> Some h
        | Head _ -> None
      
        | Tail (List (_ :: tl)) -> Some (List tl)
        | Tail _ -> None
      
        | IsEmpty (List []) -> Some (Nat 1)
        | IsEmpty (List _) -> Some (Nat 0)
        | IsEmpty _ -> None



(*Continue la reduction jusqu'a atteindre la forme normale et la renvoie*)            
let rec ltr_cbv_norm (t : pterm) : pterm =  
    match ltr_cbv_step t with 
    |Some t' -> ltr_cbv_norm t' 
    |None -> t


exception Timeout

let rec ltr_cbv_norm_timeout (t:pterm) (etapes_restante: int) : pterm =
    if etapes_restante <= 0 then
        raise Timeout
    else
        match ltr_cbv_step t with 
        |Some t' -> 
            print_endline ("Étape : " ^ print_term t');
            ltr_cbv_norm_timeout t' (etapes_restante - 1)
        |None -> t 
