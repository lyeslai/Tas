
open Ast



let i = Abs ("x" , Var "x")

let delta = Abs("x", App(Var "x" , Var "x"))

let omega = App (delta , delta)

let k = Abs ("x", Abs ("y", Var "x"))

let s = Abs ("x" , Abs ("y" , Abs ("z", App (App (Var "x" , Var "z"), App (Var "y" , Var "z")))))

let skk = App (App (s,k) , k)

let sii = App (App (s,i) , i)



(*entiers de church*)
let church_0 = Abs ("f", Abs ("x" , Var "x"))

let church_1 = Abs ("f", Abs ("x" , App (Var "f", Var "x")))
  
let church_2 = Abs ("f", Abs ("x" , App (Var "f" , App (Var "f", Var "x"))) )

let church_3 = Abs ("f", Abs ("x",  App (Var "f", App ((Var "f" , App (Var "f", Var "x")))) ))


(*Arithmetique*)
let succ = Abs ("n", Abs ("f", Abs ("x", App (Var "f", App (App (Var "n", Var "f"), Var "x")))))

let add = Abs ("m", Abs ("n", Abs ("f", Abs ("x", App (App (Var "m", Var "f"), App (App (Var "n", Var "f"), Var "x"))))))

let mult = Abs ("m", Abs ("n", Abs ("f", App (Var "m", App (Var "n", Var "f")))))

let pow = Abs ("m", Abs ("n", App (Var "n", Var "m")))

