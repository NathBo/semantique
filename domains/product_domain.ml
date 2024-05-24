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
 open Interval_domain
 open Congruence_domain
 
 
module ID = INTERVALDOMAIN

module CD = CONGRUENCEDOMAIN
  

 
 module PRODUCTDOMAIN : Value_domain.VALUE_DOMAIN =
   struct
     
 
     (* type of abstract elements *)
     (* an element of type t abstracts a set of integers *)
 
     type t = ID.t * CD.t

     let to_string (i,c) = 
     "("^(ID.to_string i)^","^(CD.to_string c)^")"
 
     (* unrestricted value: [-oo,+oo] *)
 
     (* bottom value: empty set *)
     let bottom = ID.bottom,CD.bottom
 
     (* constant: {c} *)
     let const n = ID.const n,CD.const n
 
     (* interval: [a,b] *)
     let rand a b = ID.rand a b,CD.rand a b
 
     let top = ID.top,CD.top
 
 
     (* unary operation *)
     let unary (i,c) op = 
      ID.unary i op, CD.unary c op
 
     (* binary operation *)
     let binary (i1,c1) (i2,c2) op = 
      ID.binary i1 i2 op, CD.binary c1 c2 op


 
 
 
 
 
 
     (* set-theoretic operations *)
     let join (i1,c1) (i2,c2) =
      ID.join i1 i2, CD.join c1 c2


     let meet (i1,c1) (i2,c2) =
      ID.meet i1 i2, CD.meet c1 c2
 
     (* widening *)
     let widen (i1,c1) (i2,c2) =
      ID.widen i1 i2, CD.widen c1 c2
 
     (* narrowing *)
     let narrow (i1,c1) (i2,c2) =
      ID.narrow i1 i2, CD.narrow c1 c2


     (* subset inclusion of concretizations *)
     let subset (i1,c1) (i2,c2) =
      ID.subset i1 i2, CD.subset c1 c2
 
     (* check the emptiness of the concretization *)
     let is_bottom (i1,c1) (i2,c2) =
      ID.is_bottom i1 i2 && CD.is_bottom c1 c2



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
      | AST_NOT_EQUAL -> if a=b && c=d then (narrow x y, narrow y x) else x,y
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
     | AST_UNARY_MINUS -> meet (num_un_minus(snd r),num_un_minus(fst r)) x
 


     (* backward binary operation *)
     (* [bwd_binary x y op r] returns (x',y') where
       - x' abstracts the set of v  in x such that v op v' is in r for some v' in y
       - y' abstracts the set of v' in y such that v op v' is in r for some v  in x
       i.e., we filter the abstract values x and y knowing that, after
       applying the operation op, the result is in r
       *)
     let bwd_binary x y op r = let a,b = x in let c,d = y in let e,f = r in if op=AST_DIVIDE && contains_zero y then raise DivisionByZero else match op with
     | AST_PLUS -> meet x (binary r y AST_MINUS),meet y (binary r x AST_MINUS)
     | AST_MINUS -> (numbMax a (num_plus e c true),numbMin b (num_plus f d true)),(numbMax c (num_minus a e true),numbMin d (num_minus b f true))
     | AST_MODULO -> x,y                      (*les intervalles et modulo marchent vraiment pas ensemble*)
     | AST_DIVIDE -> meet x (binary r y AST_MULTIPLY),meet y (lenientbinary x r AST_DIVIDE)
     | AST_MULTIPLY -> let rep1 = if contains_zero y && contains_zero r then x else meet x (lenientbinary r y AST_DIVIDE) in
      let rep2 = if contains_zero x && contains_zero r then y else meet y (lenientbinary r x AST_DIVIDE) in
      rep1,rep2



 
     (* print abstract element *)
     let print fmt a = Format.fprintf fmt "[%a,%a]" print_num (fst a) print_num (snd a)
     

 
 end
 
 
 
