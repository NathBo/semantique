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
 
 
type cong = C of Z.t * Z.t | CBot


let constains_zero a = match a with
  | CBot -> false
  | C (a,b) -> b = Z.zero || a = Z.one || (a=Z.zero && b=Z.zero)


let divides a b = match a,b with
| _,b when b=Z.zero -> true
| a,b -> Z.(mod) a b = Z.zero



 
 module CONGRUENCEDOMAIN : Value_domain.VALUE_DOMAIN =
   struct
     
 
     (* type of abstract elements *)
     (* an element of type t abstracts a set of integers *)
 
     type t = cong

     let to_string a = match a with
     | CBot -> "CBot"
     | C (a,b) -> (Z.to_string a)^"Z+"^(Z.to_string b)
 
     (* unrestricted value: [-oo,+oo] *)
 
     (* bottom value: empty set *)
     let bottom = CBot
 
     (* constant: {c} *)
     let const n = C (Z.zero,n)

     let top = C (Z.zero,Z.one)
 
     (* interval: [a,b] *)
     let rand a b = if a=b then const a else top
 
  
 
 
     (* unary operation *)
     let unary a op = match op,a with
      | AST_UNARY_PLUS,_ -> a
      | AST_UNARY_MINUS,CBot -> CBot
      | AST_UNARY_MINUS,C(a,b) -> C(a,Z.(-) a b)
 
     (* binary operation *)
     let binary a b op = match op,a,b with
      | AST_MODULO,_,_ | AST_DIVIDE,_,_ when constains_zero b -> raise DivisionByZero
      | _,_,CBot -> CBot
      | _,CBot,_ -> CBot
      | AST_PLUS,C(a,b),C(c,d) when a=c -> C(a,Z.(+) b d)
      | AST_PLUS,_,_ -> top
      | AST_MINUS,C(a,b),C(c,d) when a=c -> C(a,Z.(-) b d)
      | AST_MINUS,_,_ -> top
      | AST_MULTIPLY,C(a,b),C(c,d) when a=c-> C(a, Z.( * ) b d)
      | AST_MULTIPLY,_,_ -> top
      | AST_DIVIDE,_,_ -> top
      | AST_MODULO,C(a,b),C(c,d) when c=Z.zero && divides d a -> C(Z.zero,Z.(mod) b d)
      | AST_MODULO,_,_ -> top

      
 
 
     (* comparison *)
     (* [compare x y op] returns (x',y') where
        - x' abstracts the set of v  in x such that v op v' is true for some v' in y
        - y' abstracts the set of v' in y such that v op v' is true for some v  in x
        i.e., we filter the abstract values x and y knowing that the test is true
 
        a safe, but not precise implementation, would be:
        compare x y op = (x,y)
      *)
     let rec compare x y op = match (x,y,op) with
      | CBot,_,_ | _,CBot,_ -> (CBot,CBot)
      | C(a,b),C(c,d),AST_EQUAL when a=c -> if b=d then (x,y) else (CBot,CBot)
      | C(a,b),C(c,d),AST_NOT_EQUAL when a=c -> if b<>d then (x,y) else (CBot,CBot)
      | C(a,b),C(c,d),AST_EQUAL when divides c a -> if divides c (Z.(-) b d) then (x,y) else (CBot,CBot)
      | C(a,b),C(c,d),AST_NOT_EQUAL when divides c a -> if divides c (Z.(-) b d) then (x,y) else (CBot,CBot)
      | C(a,b),C(c,d),_ when divides a c -> let rep1,rep2 = compare y x op in rep2,rep1
      | _ -> (x,y)
 
 
 
 
     (* set-theoretic operations *)
     let join x y = match x,y with
     | CBot,a | a,CBot -> a
     | C(a,b),C(c,d) when divides c a -> if divides c (Z.(-) b d) then C(c,d) else top
     | C(a,b),C(c,d) when divides a c -> if divides a (Z.(-) d b) then C(a,b) else top
     | _ -> top


     let meet a b = match a,b with
     | CBot,_ | _,CBot -> CBot
     | C(a,b),C(c,d) when divides c a -> if divides c (Z.(-) b d) then C(a,b) else bottom
     | C(a,b),C(c,d) when divides a c -> if divides a (Z.(-) d b) then C(c,d) else bottom
     | _ -> bottom
 
     (* widening *)
     let widen a b = top
 
     (* narrowing *)
     let narrow a b = match a,b with
     | CBot,_  -> CBot
     | _,CBot -> a
     | C(a,b),C(c,d) when divides a c -> if divides c (Z.(-) b d) then bottom else C(a,b)
     | C(a,b),C(c,d) when c = Z.(+) a a -> if divides a (Z.(-) d b) then C(c,a) else C(a,b)
     | _ -> a



     (* subset inclusion of concretizations *)
     let subset a b = match a,b with
     | CBot,_ -> true
     | _,CBot -> false
     | C(a,b),C(c,d) when divides c a -> divides c (Z.(-) b d)
     | _ -> true
 
     (* check the emptiness of the concretization *)
     let is_bottom a = a = CBot
 
     (* backards unary operation *)
     (* [bwd_unary x op r] return x':
        - x' abstracts the set of v in x such as op v is in r
        i.e., we fiter the abstract values x knowing the result r of applying
        the operation on x
      *)
     let bwd_unary x op r = match op with
     | AST_UNARY_PLUS -> meet x r
     | AST_UNARY_MINUS -> meet (unary x AST_UNARY_MINUS) r
 
     (* backward binary operation *)
     (* [bwd_binary x y op r] returns (x',y') where
       - x' abstracts the set of v  in x such that v op v' is in r for some v' in y
       - y' abstracts the set of v' in y such that v op v' is in r for some v  in x
       i.e., we filter the abstract values x and y knowing that, after
       applying the operation op, the result is in r
       *)
     let bwd_binary x y op r = match op,x,y with
     | AST_DIVIDE,_,b | AST_MODULO,_,b when constains_zero b -> raise DivisionByZero
     | _ ->
      let aux n nr b = match op with
      | AST_PLUS -> Const (Z.(-) nr n)
      | AST_MINUS when b -> Const(Z.(-) n nr )
      | AST_MINUS -> Const(Z.(+) n nr )
      | AST_MULTIPLY when n<>Z.zero && Z.(mod) nr n = Z.zero -> Const(Z.(/) nr n)
      | AST_MULTIPLY when n=Z.zero && nr=Z.zero -> Top
      | AST_MULTIPLY -> Bottom
      | AST_MODULO when not b && Z.abs nr>= Z.abs n -> Bottom     (*a%b<b*)
      | AST_MODULO when b && Z.abs nr > Z.abs n -> Bottom          (*a/b<=a*)
      | _ -> Top in
      match x,y,r with
      | Const(n1),Const(n2),_ when subset (Const (apply_int_bin_op op n1 n2)) r -> (x,y)
      | Const(_),Const(_),_ -> (Bottom,Bottom)
      | Const(n),Top,Const(nr) -> let a = aux n nr true in if a=Bottom then (Bottom,Bottom) else Const(n),a
      | Top,Const(n),Const(nr) -> let a = aux n nr false in if a=Bottom then (Bottom,Bottom) else a,Const(n)
      | Const(n),Top,Top -> Const(n),Top
      | Top,Const(n),Top -> Top,Const(n)
      | _,_,Bottom | _,Bottom,_ | Bottom,_,_ -> Bottom,Bottom
      | Top,Top,_ -> Top,Top
 
     (* print abstract element *)
     let print fmt a = match a with
     | CBot -> Format.fprintf fmt "CBot"
     | C(a,b) -> Format.fprintf fmt "%aZ+%a" Z.pp_print a Z.pp_print b

 
 end
 
 
 