open Frontend
open Abstract_syntax_tree

type bool_Set = True | False | TrueOrFalse | Neither


module BOOL_DOMAIN =
  struct

    (* type of abstract elements *)
    (* an element of type t abstracts a set of integers *)

    (* unrestricted value: [-oo,+oo] *)

    type t = bool_Set


    let top  = TrueOrFalse

    (* bottom value: empty set *)
    let bottom = Neither

    (* constant: {c} *)
    let const b = if b then True else False

    (* interlet: [a,b] *)
    let rand = TrueOrFalse


    (* unary operation *)
    let unary a op = match a,op with
    | (True,AST_NOT) -> False
    | (False,AST_NOT) -> True
    | (TrueOrFalse,AST_NOT) -> TrueOrFalse
    | _ -> Neither

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
    let bwd_binary x y op r = failwith "beaucoup trop cancer"


    (* set-theoretic operations *)
    let join a b = match a,b with
      | (_,TrueOrFalse) | (TrueOrFalse,_) | (True,False) | (False,True) -> TrueOrFalse
      | (True,_) | (_,True) -> True
      | (False,_) | (_,False) -> False
      | (Neither,Neither) -> Neither


    let meet a b = match a,b with
      | (Neither,_) | (_,Neither) | (True,False) | (False,True) -> Neither
      | (True,_) | (_,True) -> True
      | (False,_) | (_,False) -> False
      | (TrueOrFalse,TrueOrFalse) -> TrueOrFalse

    (* widening *)
    let widen = failwith "ca veut dire koi mdr ?"

    (* narrowing *)
    let narrow a b = match (a,b) with
      | (a,Neither) -> a
      | (_,TrueOrFalse) | (Neither,_) -> Neither
      | (True,False) | (TrueOrFalse,False) -> True
      | (False,True) | (TrueOrFalse,True) -> False
      | (True,True) | (False,False) -> Neither

    (* subset inclusion of concretizations *)
    let subset a b = (narrow a b) = Neither

    (* check the emptiness of the concretization *)
    let is_bottom a = a = Neither 

    (* print abstract element *)
    let print fmt a = match a with
    | TrueOrFalse -> Format.printf fmt "TrueOrFalse"
    | True -> Format.printf fmt "True"
    | False -> Format.printf fmt "False"
    | Neither -> Format.printf fmt "Neither"

end;;


