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
 
 
type cst = Bottom | Top | Const of Z.t
 
 
 module CONCRETE_DOMAIN : Value_domain.VALUE_DOMAIN =
   struct
     
 
     (* type of abstract elements *)
     (* an element of type t abstracts a set of integers *)
 
     type t = cst
 
     (* unrestricted value: [-oo,+oo] *)
 
     (* bottom value: empty set *)
     let bottom = Bottom
 
     (* constant: {c} *)
     let const n = Const n
 
     (* interval: [a,b] *)
     let rand a b = Top
 
     let top = Top (*pas de top pour concrete domain*)
 
 
     (* unary operation *)
     let unary a op = match a with
      | Const n -> Const (apply_int_un_op op n)
      | a -> a
 
     (* binary operation *)
     let binary a b op = match a,b with
      | Const a, Const b -> Const (apply_int_bin_op op a b)
      | Bottom,_ | _,Bottom -> Bottom
      | _ -> Top
 
 
     (* comparison *)
     (* [compare x y op] returns (x',y') where
        - x' abstracts the set of v  in x such that v op v' is true for some v' in y
        - y' abstracts the set of v' in y such that v op v' is true for some v  in x
        i.e., we filter the abstract values x and y knowing that the test is true
 
        a safe, but not precise implementation, would be:
        compare x y op = (x,y)
      *)
     let compare x y op =
       let rec aux other_set elt acc =
         if IntSet.exists (apply_compare_op op elt) other_set
         then IntSet.add elt acc
         else acc in
       (IntSet.fold (aux y) x IntSet.empty, IntSet.fold (aux x) y IntSet.empty)
 
 
 
 
     (* set-theoretic operations *)
     let join = IntSet.union
     let meet = IntSet.inter
 
     (* widening *)
     let widen a b = failwith "pas implemente widen"
 
     (* narrowing *)
     let narrow = IntSet.diff
 
     (* subset inclusion of concretizations *)
     let subset = IntSet.subset
 
     (* check the emptiness of the concretization *)
     let is_bottom = IntSet.is_empty
 
     (* backards unary operation *)
     (* [bwd_unary x op r] return x':
        - x' abstracts the set of v in x such as op v is in r
        i.e., we fiter the abstract values x knowing the result r of applying
        the operation on x
      *)
     let bwd_unary x op r =
       meet (IntSet.map (apply_int_un_op op) x) r
 
     (* backward binary operation *)
     (* [bwd_binary x y op r] returns (x',y') where
       - x' abstracts the set of v  in x such that v op v' is in r for some v' in y
       - y' abstracts the set of v' in y such that v op v' is in r for some v  in x
       i.e., we filter the abstract values x and y knowing that, after
       applying the operation op, the result is in r
       *)
     let bwd_binary x y op r =   (*tres potentiellement des erreurs ds cette fonction*)
       let works op elt z =
         IntSet.mem (apply_int_bin_op op elt z) r in
       let rec aux other_set elt acc =
         if IntSet.exists (works op elt) other_set
         then IntSet.add elt acc
         else acc in
       (IntSet.fold (aux y) x IntSet.empty, IntSet.fold (aux x) y IntSet.empty)
 
     (* print abstract element *)
     let print fmt a =
       let rec aux fmt x =
         Z.pp_print fmt x in
       Format.fprintf fmt "IntSet : ";
       IntSet.iter (aux fmt) a;
       Format.fprintf fmt "\n"
 
 end
 
 
 