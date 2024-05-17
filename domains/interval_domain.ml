(*
  Cours "Sémantique et Application à la Vérification de programmes"

  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

(*
  Signature of abstract domains representing sets of integers
  (for instance: constants or intervals).
 *)

 open Frontend
 open Abstract_syntax_tree
 
 
type number = MinusInfty | PlusInfty | N of Z.t

type interval = number*number


let to_string_numb a = match a with
| MinusInfty -> "-oo"
| PlusInfty -> "+oo"
| N n -> Z.to_string n


let inf_eq a b = match a,b with
| MinusInfty,_ -> true
| _,MinusInfty -> false
| _,PlusInfty -> true
| PlusInfty,_ -> false
| N a,N b -> a<=b


let sup_eq a b = match a,b with
| PlusInfty,_ -> true
| _,PlusInfty -> false
| _,MinusInfty -> true
| MinusInfty,_ -> false
| N a,N b -> a>=b

let inf a b = not (sup_eq a b)


let sup a b = not (inf_eq a b)


let zero = N Z.zero


let contains_zero i =
  fst i <= zero && snd i >= zero

let numbMin a b = if inf a b then a else b

let numbMax a b = if sup a b then a else b


let num_un_minus a = match a with
| MinusInfty -> PlusInfty
| PlusInfty -> MinusInfty
| N n -> N (Z.(~-) n)

let num_plus a b round_up = match a,b with
| N n1,N n2 -> N (Z.(+) n1 n2)
| MinusInfty,PlusInfty | PlusInfty,MinusInfty -> if round_up then PlusInfty else MinusInfty
| _,PlusInfty | PlusInfty,_ -> PlusInfty
| _,MinusInfty | MinusInfty,_ -> MinusInfty

let num_minus a b round_up = num_plus a (num_un_minus b) round_up


let num_times a b = match a,b with
| N n1,N n2 -> N (Z.( * ) n1 n2)
| a,b when a=zero || b=zero -> zero
| a,b when sup a zero && sup b zero -> PlusInfty
| a,b when inf a zero && inf a zero -> PlusInfty
| _ -> MinusInfty


let num_divide a b = match a,b with
| N n1,N n2 -> N (Z.(/) n1 n2)
| _,PlusInfty | _,MinusInfty -> zero
| PlusInfty,a when sup a zero -> PlusInfty
| PlusInfty,_ -> MinusInfty
| MinusInfty,a when inf a zero -> PlusInfty
| MinusInfty,_ -> MinusInfty


let rec minList l = match l with
  | [] -> failwith "non"
  | x::[] -> x
  | x::q -> numbMin x (minList q)

let rec maxList l = match l with
  | [] -> failwith "non"
  | x::[] -> x
  | x::q -> numbMax x (maxList q)


let print_num fmt a = match a with
| MinusInfty -> Format.fprintf fmt "-oo"
| PlusInfty -> Format.fprintf fmt "+oo"
| N n -> Z.pp_print fmt n
  

 
 module INTERVALDOMAIN : Value_domain.VALUE_DOMAIN =
   struct
     
 
     (* type of abstract elements *)
     (* an element of type t abstracts a set of integers *)
 
     type t = interval

     let to_string a = 
     ("["^(to_string_numb (fst a))^","^(to_string_numb (snd a))^"]")
 
     (* unrestricted value: [-oo,+oo] *)
 
     (* bottom value: empty set *)
     let bottom = PlusInfty,MinusInfty
 
     (* constant: {c} *)
     let const n = N n,N n
 
     (* interval: [a,b] *)
     let rand a b = N a,N b
 
     let top = MinusInfty,PlusInfty
 
 
     (* unary operation *)
     let unary a op = match op with
      | AST_UNARY_MINUS -> num_un_minus (fst a),num_un_minus (snd a)
      | _ -> a
 
     (* binary operation *)
     let binary a b op = let x,y = a in let z,t = b in match op with
     | AST_DIVIDE | AST_MODULO when contains_zero b -> raise DivisionByZero
     | AST_PLUS -> num_plus x z false,num_plus y t true
     | AST_MINUS -> num_minus x t false,num_minus y z true
     | AST_MULTIPLY -> let l = [num_times x z; num_times x t; num_times y z; num_times y t] in
      minList l, maxList l 
     | AST_DIVIDE -> let l = [num_divide x z; num_divide x t; num_divide y z; num_divide y t] in
      minList l, maxList l 
     | AST_MODULO -> top
 
 
 
 
 
 
     (* set-theoretic operations *)
     let join x y = let a,b = x in let c,d = y in numbMin a c, numbMax b d


     let meet x y = let a,b = x in let c,d = y in (numbMax a c,numbMin b d)
 
     (* widening *)
     let widen a b = top
 
     (* narrowing *)
     let narrow x y = let a,b = x in let c,d = y in let x = if inf a c then a else d in
      let y = if sup b d then b else c in x,y


     (* subset inclusion of concretizations *)
     let subset x y = let a,b = x in let c,d = y in a<=c && d<=b
 
     (* check the emptiness of the concretization *)
     let is_bottom a = sup (fst a) (snd a)



     (* comparison *)
     (* [compare x y op] returns (x',y') where
        - x' abstracts the set of v  in x such that v op v' is true for some v' in y
        - y' abstracts the set of v' in y such that v op v' is true for some v  in x
        i.e., we filter the abstract values x and y knowing that the test is true
 
        a safe, but not precise implementation, would be:
        compare x y op = (x,y)
      *)
      let rec compare x y op = let a,b = x in let c,d = y in match op with
      | AST_EQUAL -> (meet x y,meet x y)
      | AST_NOT_EQUAL -> if a=b || c=d then (narrow x y, narrow y x) else x,y
      | AST_LESS_EQUAL -> (a,numbMin b d),(numbMax a c,d)
      | AST_LESS -> (a,numbMin b (num_minus d (N Z.one) true)),(numbMax (num_plus a (N Z.one) true) c,d)
      | AST_GREATER_EQUAL -> let r1,r2 = compare y x AST_LESS_EQUAL in r2,r1
      | AST_GREATER -> let r1,r2 = compare y x AST_LESS in r2,r1
      
 
     (* backards unary operation *)
     (* [bwd_unary x op r] return x':
        - x' abstracts the set of v in x such as op v is in r
        i.e., we fiter the abstract values x knowing the result r of applying
        the operation on x
      *)
     let bwd_unary x op r = match op with
     | AST_UNARY_PLUS -> meet x r
     | AST_UNARY_MINUS -> meet (num_un_minus(fst x),num_un_minus(fst x)) r
 


     (* backward binary operation *)
     (* [bwd_binary x y op r] returns (x',y') where
       - x' abstracts the set of v  in x such that v op v' is in r for some v' in y
       - y' abstracts the set of v' in y such that v op v' is in r for some v  in x
       i.e., we filter the abstract values x and y knowing that, after
       applying the operation op, the result is in r
       *)
     let bwd_binary x y op r = let a,b = x in let c,d = y in let e,f = r in if op=AST_DIVIDE && contains_zero y then raise DivisionByZero else match op with
     | AST_PLUS -> (numbMax a (num_minus e c true),numbMin b (num_minus f d true)),(numbMax c (num_minus e a true),numbMin d (num_minus f b true))
     | AST_MINUS -> (numbMax a (num_plus e c true),numbMin b (num_plus f d true)),(numbMax c (num_minus a e true),numbMin d (num_minus b f true))
     | AST_MODULO -> x,y                      (*les intervalles et modulo marchent vraiment pas ensemble*)
     | AST_DIVIDE -> meet x (binary r y AST_MULTIPLY),meet y (binary x r AST_DIVIDE)
     | AST_MULTIPLY -> meet x (binary r y AST_DIVIDE),meet y (binary r x AST_DIVIDE)



 
     (* print abstract element *)
     let print fmt a = Format.fprintf fmt "[%a,%a]" print_num (fst a) print_num (snd a)
     

 
 end
 
 
 