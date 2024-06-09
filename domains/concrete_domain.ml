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
    type t = Z.t
  end )

  type concrete = CrTOP | IS of IntSet.t


module CONCRETE_DOMAIN : Value_domain.VALUE_DOMAIN =
  struct
    

    (* type of abstract elements *)
    (* an element of type t abstracts a set of integers *)

    type t = concrete

    (* unrestricted value: [-oo,+oo] *)

    (* bottom value: empty set *)
    let bottom =  IS IntSet.empty

    (* constant: {c} *)
    let const n = IS (IntSet.singleton n)

    (* interval: [a,b] *)
    let rand a b = 
      let rec aux a b s =
        if a>b
        then s
        else IntSet.add a (aux (Z.(+) a Z.one) b s) in
      IS (aux a b IntSet.empty)

    let top = CrTOP


    (* unary operation *)
    let unary a op = match a with
    | CrTOP -> CrTOP
    | IS a -> IS (IntSet.map (apply_int_un_op op) a)

    (* binary operation *)
    let binary a b op = match op,a,b with
      | AST_DIVIDE,_,CrTOP | AST_MODULO,_,CrTOP -> raise DivisionByZero
      | _,CrTOP,_ | _,_,CrTOP -> CrTOP
      | AST_DIVIDE,_,IS b | AST_MODULO,_,IS b when IntSet.mem Z.zero b -> raise DivisionByZero
      | _,IS a,IS b ->
        let aux b_elt acc =
          IntSet.union acc (IntSet.map (apply_int_bin_op op b_elt) a) in
        IS (IntSet.fold aux b IntSet.empty)


    (* comparison *)
    (* [compare x y op] returns (x',y') where
       - x' abstracts the set of v  in x such that v op v' is true for some v' in y
       - y' abstracts the set of v' in y such that v op v' is true for some v  in x
       i.e., we filter the abstract values x and y knowing that the test is true

       a safe, but not precise implementation, would be:
       compare x y op = (x,y)
     *)
    let compare x y op = match x,y with
    | CrTOP,_ | _,CrTOP -> CrTOP,CrTOP
    | IS x, IS y ->
      let rec aux other_set elt acc =
        if IntSet.exists (apply_compare_op op elt) other_set
        then IntSet.add elt acc
        else acc in
      (IS (IntSet.fold (aux y) x IntSet.empty), IS (IntSet.fold (aux x) y IntSet.empty))


(* subset inclusion of concretizations *)

let subset a b = match a,b with
| _,CrTOP -> true
| CrTOP,_ -> false
| IS a, IS b -> (IntSet.subset a b)

  let narrow a b = match a,b with
  | _,CrTOP -> bottom
  | CrTOP,_ -> CrTOP
  | IS a, IS b -> IS (IntSet.diff a b)

    (* set-theoretic operations *)
    let join a b = match a,b with
    | CrTOP,_ | _,CrTOP -> CrTOP
    | IS a, IS b -> IS (IntSet.union a b)
    let meet a b = match a,b with
    | CrTOP,x -> x
    | x,CrTOP -> x
    | IS a, IS b -> IS (IntSet.inter a b)

    (* widening *)
    let widen x y = if subset x y then CrTOP
    else if subset y x then x
    else CrTOP



    (* check the emptiness of the concretization *)
    let is_bottom a = match a with
    | CrTOP -> false
    | IS a -> IntSet.is_empty a

    (* backards unary operation *)
    (* [bwd_unary x op r] return x':
       - x' abstracts the set of v in x such as op v is in r
       i.e., we fiter the abstract values x knowing the result r of applying
       the operation on x
     *)
    let bwd_unary x op r = match x,r with
    | _,CrTOP -> x
    | x, IS r -> meet ( IS (IntSet.map (apply_int_un_op op) r)) x

    (* backward binary operation *)
    (* [bwd_binary x y op r] returns (x',y') where
      - x' abstracts the set of v  in x such that v op v' is in r for some v' in y
      - y' abstracts the set of v' in y such that v op v' is in r for some v  in x
      i.e., we filter the abstract values x and y knowing that, after
      applying the operation op, the result is in r
      *)
    let bwd_binary x y op r = match op,x,y,r with
    | AST_DIVIDE,_,CrTOP,_ | AST_MODULO,_,CrTOP,_ -> raise DivisionByZero
    | _,_,_,CrTOP | _,CrTOP,_,_ | _,_,CrTOP,_ -> x,y
    | AST_DIVIDE,_,IS b,_ | AST_MODULO,_,IS b,_ when IntSet.mem Z.zero b -> raise DivisionByZero
    | _, IS x, IS y, IS r ->
      let works op elt z =
        IntSet.mem (apply_int_bin_op op elt z) r in
      let rec aux other_set elt acc =
        if IntSet.exists (works op elt) other_set
        then IntSet.add elt acc
        else acc in
      (IS(IntSet.fold (aux y) x IntSet.empty), IS(IntSet.fold (aux x) y IntSet.empty))

    (* print abstract element *)
    let print fmt a = match a with
    | CrTOP -> Format.fprintf fmt "Top";
    | IS a ->
      let rec aux fmt x =
        Z.pp_print fmt x in
      Format.fprintf fmt "IntSet : ";
      IntSet.iter (aux fmt) a;
      Format.fprintf fmt "\n"

    let to_string env = match env with
      | CrTOP -> "top"
      | IS env ->
      let rep = ref "{" in
      let aux a = rep:= !rep^" "^(Z.to_string a) in
      IntSet.iter aux env;
      !rep^"}"

end


