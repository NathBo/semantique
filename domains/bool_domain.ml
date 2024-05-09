open Frontend
open Abstract_syntax_tree

type value_Set = True | False | TrueOrFalse | Neither


module BOOL_DOMAIN =
  struct

    (* type of abstract elements *)
    (* an element of type t abstracts a set of integers *)

    (* unrestricted value: [-oo,+oo] *)
    let top  = TrueOrFalse

    (* bottom value: empty set *)
    let bottom = Neither

    (* constant: {c} *)
    let const b = if b then True else False

    (* interlet: [a,b] *)
    let rand = TrueOrFalse


    (* unary operation *)
    let unary a op = match op with
    | AST_NOT -> const (not a)

    (* binary operation *)
    let binary a b op = match (op,a,b) with
      | (AST_AND,False,_) | (AST_AND,_,False) -> False
      | (AST_AND,TrueOrFalse,_) | (AST_AND,_,TrueOrFalse) -> TrueOrFalse
      | (AST_AND,True,True) -> True
      | (AST_OR,True,_) | (AST_OR,_,True) -> True
      | (AST_OR,TrueOrFalse,_) | (AST_OR,_,TrueOrFalse) -> TrueOrFalse
      | (AST_OR,False,False) -> False
      | _ -> Neither


    (* comparison *)
    (* [compare x y op] returns (x',y') where
       - x' abstracts the set of v  in x such that v op v' is true for some v' in y
       - y' abstracts the set of v' in y such that v op v' is true for some v  in x
       i.e., we filter the abstract values x and y knowing that the test is true

       a safe, but not precise implementation, would be:
       compare x y op = (x,y)
     *)
    (*let compare: t -> t -> compare_op -> (t * t)*)

    (*imo ya pas besoin vu qu'on a pas le droit de comparer des bool*)


    (* backards unary operation *)
    (* [bwd_unary x op r] return x':
       - x' abstracts the set of v in x such as op v is in r
       i.e., we fiter the abstract values x knowing the result r of applying
       the operation on x
     *)
    let bwd_unary x op r = match (x,op,r) with
      | (False,AST_NOT,False) | (True, AST_NOT, True) -> Neither
      | (_,AST_NOT,False) -> True
      | (_,AST_NOT,True) -> False
      | (_,AST_NOT,TrueOrFalse) -> TrueOrFalse
      | _ -> Neither

     (* backward binary operation *)
     (* [bwd_binary x y op r] returns (x',y') where
       - x' abstracts the set of v  in x such that v op v' is in r for some v' in y
       - y' abstracts the set of v' in y such that v op v' is in r for some v  in x
       i.e., we filter the abstract values x and y knowing that, after
       applying the operation op, the result is in r
      *)
    let bwd_binary: t -> t -> int_binary_op -> t -> (t * t)


    (* set-theoretic operations *)
    let join: t -> t -> t
    let meet: t -> t -> t

    (* widening *)
    let widen: t -> t -> t

    (* narrowing *)
    let narrow: t -> t -> t

    (* subset inclusion of concretizations *)
    let subset: t -> t -> bool

    (* check the emptiness of the concretization *)
    let is_bottom: t -> bool

    (* print abstract element *)
    let print: Format.formatter -> t -> unit

end;;


