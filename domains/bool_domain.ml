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
    let binary: t -> t -> int_binary_op -> t


    (* comparison *)
    (* [compare x y op] returns (x',y') where
       - x' abstracts the set of v  in x such that v op v' is true for some v' in y
       - y' abstracts the set of v' in y such that v op v' is true for some v  in x
       i.e., we filter the abstract values x and y knowing that the test is true

       a safe, but not precise implementation, would be:
       compare x y op = (x,y)
     *)
    let compare: t -> t -> compare_op -> (t * t)


    (* backards unary operation *)
    (* [bwd_unary x op r] return x':
       - x' abstracts the set of v in x such as op v is in r
       i.e., we fiter the abstract values x knowing the result r of applying
       the operation on x
     *)
    let bwd_unary: t -> int_unary_op -> t -> t

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


