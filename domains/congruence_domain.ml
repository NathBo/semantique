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
| a,_ when a=Z.zero -> false
| a,b -> Z.(mod) b a = Z.zero



 
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

     let top = C (Z.one,Z.zero)
 
     (* interval: [a,b] *)
     let rand a b = if a=b then const a else top
 
  
 
 
     (* unary operation *)
     let unary a op = match op,a with
      | AST_UNARY_PLUS,_ -> a
      | AST_UNARY_MINUS,CBot -> CBot
      | AST_UNARY_MINUS,C(a,b) -> C(a,Z.(-) a b)
 
     (* binary operation *)
     let rec binary a b op = match op,a,b with
      | AST_MODULO,_,_ | AST_DIVIDE,_,_ when constains_zero b -> raise DivisionByZero
      | _,_,CBot -> CBot
      | _,CBot,_ -> CBot
      | AST_PLUS,C(a,b),C(c,d) when a=c -> C(a,Z.(+) b d)
      | AST_PLUS,C(a,b),C(c,d) when divides a c -> C(a,Z.(+) b d)
      | AST_PLUS,C(a,b),C(c,d) when divides c a -> C(c,Z.(+) b d)
      | AST_PLUS,_,_ -> top
      | AST_MINUS,ca,cb -> binary a (unary b AST_UNARY_MINUS) AST_PLUS
      | AST_MULTIPLY,C(a,b),C(c,d) when a=c-> C(a, Z.( * ) b d)
      | AST_MULTIPLY,C(a,b),C(c,d) when (c=Z.zero && d=Z.zero) || (a=Z.zero && b=Z.zero) -> C(Z.zero,Z.zero)
      | AST_MULTIPLY,C(a,b),C(c,d) when c=Z.zero -> C(Z.( * ) a d,Z.( * ) b d)
      | AST_MULTIPLY,C(c,d),C(a,b) when c=Z.zero -> C(Z.( * ) a d,Z.( * ) b d)
      | AST_MULTIPLY,C(a,b),C(c,d) when d=Z.zero -> C(Z.( * ) a c,Z.( * ) b c)
      | AST_MULTIPLY,C(c,d),C(a,b) when d=Z.zero -> C(Z.( * ) a c,Z.( * ) b c)
      | AST_MULTIPLY,_,_ -> top
      | AST_DIVIDE,C(a,b),C(c,d) when a=Z.zero && c=Z.zero -> C(Z.zero,Z.(/) b d)
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
     
 
 
 
 
     (* set-theoretic operations *)
     let join x y = match x,y with
     | CBot,a | a,CBot -> a
     | C(a,b),C(c,d) when divides c a -> if divides c (Z.(-) b d) then C(c,d) else top
     | C(a,b),C(c,d) when divides a c -> if divides a (Z.(-) d b) then C(a,b) else top
     | _ -> top


     let meet a b = match a,b with
     | CBot,_ | _,CBot -> CBot
     | C(a,b),C(c,d) when c=Z.one -> C(a,b)
     | C(a,b),C(c,d) when a=Z.one -> C(c,d)
     | C(a,b),C(c,d) when divides c a -> if divides c (Z.(-) b d) then C(c,d) else bottom
     | C(a,b),C(c,d) when divides a c -> if divides a (Z.(-) d b) then C(a,b) else bottom
     | _ -> top (*possibilite de faire mieux avec les restes chinois*)


     let rec compare x y op = match (x,y,op) with
      | CBot,_,_ | _,CBot,_ -> (CBot,CBot)
      | C(a,b),C(c,d),AST_EQUAL -> (meet x y), (meet x y)
      | C(a,b),C(c,d),AST_NOT_EQUAL when a=c -> if b<>d then (x,y) else (CBot,CBot)
      | C(a,b),C(c,d),AST_NOT_EQUAL when divides c a -> if divides c (Z.(-) b d) then (x,y) else (CBot,CBot)
      | C(a,b),C(c,d),AST_NOT_EQUAL when divides a c -> let rep1,rep2 = compare y x op in rep2,rep1
      | _ -> (x,y)
 
     (* widening *)
     let widen a b = join a b
 
     (* narrowing *)
     let narrow a b = match a,b with
     | CBot,_  -> CBot
     | _,CBot -> a
     | C(a,b),C(c,d) when divides a c -> if divides c (Z.(-) b d) then bottom else C(a,b)
     | C(a,b),C(c,d) when c = Z.(+) a a -> if divides a (Z.(-) d b) then C(c,a) else C(a,b)
     | _ -> a



     (* subset inclusion of concretizations *)
     let subset x y = match x,y with
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
     | AST_UNARY_MINUS -> meet (unary r AST_UNARY_MINUS) x
 
     (* backward binary operation *)
     (* [bwd_binary x y op r] returns (x',y') where
       - x' abstracts the set of v  in x such that v op v' is in r for some v' in y
       - y' abstracts the set of v' in y such that v op v' is in r for some v  in x
       i.e., we filter the abstract values x and y knowing that, after
       applying the operation op, the result is in r
       *)
     let bwd_binary x y op r = match op,x,y,r with
     | _,CBot,_,_ | _,_,CBot,_ | _,_,_,CBot -> CBot,CBot
     | AST_DIVIDE,_,b,_ | AST_MODULO,_,b,_ when constains_zero b -> raise DivisionByZero
     | AST_PLUS,_,_,_ -> meet x (binary r y AST_MINUS),meet y (binary r x AST_MINUS)
     | AST_MINUS,_,_,_ -> meet x (binary r y AST_PLUS),meet y (binary r x AST_PLUS)
     | AST_MULTIPLY,_,_,_-> let rep1 = if constains_zero y && constains_zero r then x else meet x (binary r y AST_DIVIDE) in
      let rep2 = if constains_zero x && constains_zero r then y else meet y (binary r x AST_DIVIDE) in
      rep1,rep2
     | AST_DIVIDE,_,_,_ -> x,y
     | AST_MODULO,C(a,b),C(c,d),C(e,f) when c=Z.zero && e = Z.zero && a=Z.one -> C(d,f),y
     | AST_MODULO,C(a,b),C(c,d),C(e,f) when c=Z.zero && e = Z.zero && a=d -> C(a,f),y
     | AST_MODULO,C(a,b),C(c,d),C(e,f) when c=Z.zero && e = Z.zero -> if Z.(mod) f d = Z.(mod) b d then C(d,Z.(mod) a d),y else if divides d a then (CBot,CBot) else x,y
     | AST_MODULO,_,_,_ -> x,y

     
 
     (* print abstract element *)
     let print fmt a = match a with
     | CBot -> Format.fprintf fmt "CBot"
     | C(a,b) -> Format.fprintf fmt "%aZ+%a" Z.pp_print a Z.pp_print b

 
 end
 
 
 
