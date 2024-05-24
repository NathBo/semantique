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
      ID.subset i1 i2 && CD.subset c1 c2
 
     (* check the emptiness of the concretization *)
     let is_bottom (i1,c1) =
      ID.is_bottom i1 || CD.is_bottom c1



     (* comparison *)
     (* [compare x y op] returns (x',y') where
        - x' abstracts the set of v  in x such that v op v' is true for some v' in y
        - y' abstracts the set of v' in y such that v op v' is true for some v  in x
        i.e., we filter the abstract values x and y knowing that the test is true
 
        a safe, but not precise implementation, would be:
        compare x y op = (x,y)
      *)
      let rec compare (i1,c1) (i2,c2) op =
        let a1,a2 =  ID.compare i1 i2 op in
        let b1,b2 = CD.compare c1 c2 op in
        (a1,b1),(a2,b2)



     (* backards unary operation *)
     (* [bwd_unary x op r] return x':
        - x' abstracts the set of v in x such as op v is in r
        i.e., we fiter the abstract values x knowing the result r of applying
        the operation on x
      *)
     let bwd_unary (i1,c1) op (i2,c2) =
      ID.bwd_unary i1 op i2, CD.bwd_unary c1 op c2
 


     (* backward binary operation *)
     (* [bwd_binary x y op r] returns (x',y') where
       - x' abstracts the set of v  in x such that v op v' is in r for some v' in y
       - y' abstracts the set of v' in y such that v op v' is in r for some v  in x
       i.e., we filter the abstract values x and y knowing that, after
       applying the operation op, the result is in r
       *)
     let bwd_binary (i1,c1) (i2,c2) op (i3,c3) =
      let a1,a2 = ID.bwd_binary i1 i2 op i3 in
      let b1,b2 = CD.bwd_binary c1 c2 op c3 in
      (a1,b1),(a2,b2)



 
     (* print abstract element *)
     let print fmt a = Format.fprintf fmt "(%a,%a)" ID.print (fst a) CD.print (snd a)
     

 
 end
 
 
 
