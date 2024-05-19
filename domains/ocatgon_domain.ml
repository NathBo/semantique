open Frontend
open Abstract_syntax_tree

module Octagon : Value_domain.VALUE_DOMAIN = struct
  type constraint_type =
    | Plus of int * int * Z.t  (* xi + xj ≤ c *)
    | Minus of int * int * Z.t (* xi - xj ≤ c *)

  type t = {
    constraints: constraint_type list;
    vars: int;
  }

  let top = { constraints = []; vars = 0 }
  let bottom = { constraints = [Plus (0, 0, Z.of_int (-1))]; vars = 0 }

  let const c = { constraints = [Plus (0, 0, c)]; vars = 1 }
  
  let rand a b = 
    if Z.leq a b then 
      { constraints = [Plus (0, 0, b); Plus (1, 1, Z.neg a)]; vars = 2 }
    else 
      bottom

  let unary x op =
    match op with
    | AST_UNARY_PLUS -> x
    | AST_UNARY_MINUS -> 
        { x with constraints = List.map (function
          | Plus (i, j, c) -> Minus (i, j, Z.neg c)
          | Minus (i, j, c) -> Plus (i, j, Z.neg c)) x.constraints }

  let binary x y op =
    let apply_binary_op op c1 c2 = match op with
      | AST_PLUS -> Z.add c1 c2
      | AST_MINUS -> Z.sub c1 c2
      | AST_MULTIPLY -> Z.mul c1 c2
      | AST_DIVIDE -> if Z.equal c2 Z.zero then raise DivisionByZero else Z.div c1 c2
      | AST_MODULO -> if Z.gt c2 Z.zero then Z.rem c1 c2 else raise DivisionByZero
    in
    match op with
    | AST_PLUS | AST_MINUS -> 
        { constraints = List.map (fun (Plus (i, j, c1)) ->
            Plus (i, j, apply_binary_op op c1 (Z.zero))) (x.constraints @ y.constraints);
          vars = max x.vars y.vars }
    | _ -> 
        (* For non-linear operations, we assume over-approximation *)
        top

  let compare x y op =
    let apply_compare_op op c = match op with
      | AST_EQUAL -> c = Z.zero
      | AST_NOT_EQUAL -> c <> Z.zero
      | AST_LESS -> c < Z.zero
      | AST_LESS_EQUAL -> c <= Z.zero
      | AST_GREATER -> c > Z.zero
      | AST_GREATER_EQUAL -> c >= Z.zero
    in
    let filter_constraints constraints =
      List.filter (function
        | Plus (_, _, c) -> apply_compare_op op c
        | Minus (_, _, c) -> apply_compare_op op c) constraints
    in
    ({ x with constraints = filter_constraints x.constraints },
     { y with constraints = filter_constraints y.constraints })

  let bwd_unary x op r =
    match op with
    | AST_UNARY_PLUS -> x
    | AST_UNARY_MINUS ->
        { x with constraints = List.filter (function
          | Plus (_, _, c) -> List.exists (function
              | Minus (_, _, rc) -> Z.equal c (Z.neg rc)
              | _ -> false) r.constraints
          | Minus (_, _, c) -> List.exists (function
              | Plus (_, _, rc) -> Z.equal c (Z.neg rc)
              | _ -> false) r.constraints) x.constraints }

  let bwd_binary x y op r =
    let apply_binary_op op c1 c2 = match op with
      | AST_PLUS -> Z.sub c1 c2
      | AST_MINUS -> Z.add c1 c2
      | AST_MULTIPLY -> Z.div c1 c2
      | AST_DIVIDE -> Z.mul c1 c2
      | AST_MODULO -> Z.rem c1 c2
    in
    let filter_constraints constraints =
      List.filter (function
        | Plus (_, _, c) -> List.exists (fun (Plus (_, _, rc)) -> Z.leq c (apply_binary_op op rc Z.zero)) r.constraints
        | Minus (_, _, c) -> List.exists (fun (Minus (_, _, rc)) -> Z.leq c (apply_binary_op op rc Z.zero)) r.constraints) constraints
    in
    ({ x with constraints = filter_constraints x.constraints },
     { y with constraints = filter_constraints y.constraints })

  let join x y = 
    { constraints = List.append x.constraints y.constraints; vars = max x.vars y.vars }

  let meet x y = 
    let common_constraints = List.filter (fun c -> List.mem c y.constraints) x.constraints in
    { constraints = common_constraints; vars = max x.vars y.vars }

  let widen x y = 
    join x y

  let narrow x y = 
    meet x y

  let subset x y = 
    List.for_all (fun c -> List.mem c y.constraints) x.constraints

  let is_bottom x = 
    List.exists (function
      | Plus (i, j, c) -> i = 0 && j = 0 && Z.lt c Z.zero
      | Minus (i, j, c) -> i = 0 && j = 0 && Z.lt c Z.zero) x.constraints

  let print fmt x = 
    let constraint_to_string = function
      | Plus (i, j, c) -> Printf.sprintf "x%d + x%d ≤ %s" i j (Z.to_string c)
      | Minus (i, j, c) -> Printf.sprintf "x%d - x%d ≤ %s" i j (Z.to_string c)
    in
    let constraints_str = String.concat ", " (List.map constraint_to_string x.constraints) in
    Format.fprintf fmt "{ %s }" constraints_str

  let to_string x =
    let constraint_to_string = function
      | Plus (i, j, c) -> Printf.sprintf "x%d + x%d ≤ %s" i j (Z.to_string c)
      | Minus (i, j, c) -> Printf.sprintf "x%d - x%d ≤ %s" i j (Z.to_string c)
    in
    let constraints_str = String.concat ", " (List.map constraint_to_string x.constraints) in
    "{ " ^ constraints_str ^ " }"

end
