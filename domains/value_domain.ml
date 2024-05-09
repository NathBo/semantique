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


module IntSet = Set.Make( 
  struct
    let compare = compare
    type t = int
  end )

type value_Set = All | S of IntSet.t

module VALUE_DOMAIN =
  struct
    

    (* type of abstract elements *)
    (* an element of type t abstracts a set of integers *)

    type t = value_Set

    (* unrestricted value: [-oo,+oo] *)
    let top = All

    (* bottom value: empty set *)
    let bottom = S IntSet.empty

    (* constant: {c} *)
    let const n = S (IntSet.singleton n)

    (* interval: [a,b] *)
    let rand a b = 
      let rec aux a b s =
        if a>b
        then s
        else IntSet.add a (aux (a+1) b s) in
      S (aux a b IntSet.empty)


    (* unary operation *)
    let unary a op = IntSet.map (apply_int_un_op op) a

    (* binary operation *)
    let binary a b op = 
      let aux b_elt acc =
        IntSet.union acc (IntSet.map (apply_int_bin_op op b_elt) a) in
      IntSet.fold aux b IntSet.empty


    (* comparison *)
    (* [compare x y op] returns (x',y') where
       - x' abstracts the set of v  in x such that v op v' is true for some v' in y
       - y' abstracts the set of v' in y such that v op v' is true for some v  in x
       i.e., we filter the abstract values x and y knowing that the test is true

       a safe, but not precise implementation, would be:
       compare x y op = (x,y)
     *)
    let compare = failwith "pas implemente"


    (* backards unary operation *)
    (* [bwd_unary x op r] return x':
       - x' abstracts the set of v in x such as op v is in r
       i.e., we fiter the abstract values x knowing the result r of applying
       the operation on x
     *)
    let bwd_unary = failwith "pas implemente"

     (* backward binary operation *)
     (* [bwd_binary x y op r] returns (x',y') where
       - x' abstracts the set of v  in x such that v op v' is in r for some v' in y
       - y' abstracts the set of v' in y such that v op v' is in r for some v  in x
       i.e., we filter the abstract values x and y knowing that, after
       applying the operation op, the result is in r
      *)
    let bwd_binary = failwith "pas implemente"


    (* set-theoretic operations *)
    let join = failwith "pas implemente"
    let meet = failwith "pas implemente"

    (* widening *)
    let widen = failwith "pas implemente"

    (* narrowing *)
    let narrow = failwith "pas implemente"

    (* subset inclusion of concretizations *)
    let subset = failwith "pas implemente"

    (* check the emptiness of the concretization *)
    let is_bottom = failwith "pas implemente"

    (* print abstract element *)
    let print = failwith "pas implemente"

end


