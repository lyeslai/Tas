
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

            (*la condition de variable libre généré par AI *)

             (* Si v est une variable libre dans n, on doit éviter la capture. *)
        if substitution v (Var v) n <> n then
            (* On renomme la variable liée pour éviter la capture. *)
            let new_var = nouvelle_var () in
            let renamed_body = substitution v (Var new_var) body in
            Abs (new_var, substitution x n renamed_body)
          else
            (* Sinon, on applique la substitution directement dans le corps. *)
            Abs (v, substitution x n body)


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
    | Var _ -> None
    | Abs (_, _) -> None
    | App (t1,t2) ->
        if not (valeur t1) then
            match ltr_cbv_step t1 with
            | Some t1' -> Some (App(t1', t2))
            | None -> None
            else if not (valeur t2 ) then 
                match ltr_cbv_step t2 with
                |Some t2' -> Some (App(t1, t2'))
                |None -> None
        else
            match t1 with 
            |Abs (x, body) -> Some (substitution x t2 body)
            |_ -> None 



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
