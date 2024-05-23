open Frontend
open Abstract_syntax_tree

module type VARS = sig
  val support: var list
end



module OctagonDomain   = struct
  type t = int array array

  let init =
    let n = List.length Vars.support in
    let octagon = Array.make_matrix n n 0 in
    for i = 0 to n - 1 do
      octagon.(i).(i) <- 1
    done;
    octagon

  let bottom =
    let n = List.length Vars.support in
    Array.make_matrix n n min_int

  let assign octagon var expr = octagon (* Implementation needed *)

  let guard octagon expr = octagon (* Implementation needed *)

  let join octagon1 octagon2 = octagon1 (* Implementation needed *)

  let meet octagon1 octagon2 = octagon1 (* Implementation needed *)

  let widen octagon1 octagon2 = octagon1 (* Implementation needed *)

  let narrow octagon1 octagon2 = octagon1 (* Implementation needed *)

  let subset octagon1 octagon2 = true (* Implementation needed *)

  let is_bottom octagon = false (* Implementation needed *)

  let rec eval_int_expr env = function
    | CFG_int_unary (op, expr) ->
        let value = eval_int_expr env expr in
        apply_int_un_op op value
    | CFG_int_binary (op, expr1, expr2) ->
        let value1 = eval_int_expr env expr1 in
        let value2 = eval_int_expr env expr2 in
        apply_int_bin_op op value1 value2
    | CFG_int_var var -> (* Look up var in environment and return its value *)
        Z.zero (* Placeholder implementation *)
    | CFG_int_const z -> z
    | CFG_int_rand (lower, upper) ->
        (* Non-deterministic choice, return a random value between lower and upper bounds *)
        Z.zero (* Placeholder implementation *)

  let rec eval_bool_expr env = function
    | CFG_bool_unary (op, expr) ->
        let value = eval_bool_expr env expr in
        match op with
        | AST_NOT -> not value
    | CFG_bool_binary (op, expr1, expr2) ->
        let value1 = eval_bool_expr env expr1 in
        let value2 = eval_bool_expr env expr2 in
        (match op with
        | AST_AND -> value1 && value2
        | AST_OR -> value1 || value2)
    | CFG_compare (op, expr1, expr2) ->
        let value1 = eval_int_expr env expr1 in
        let value2 = eval_int_expr env expr2 in
        apply_compare_op op value1 value2
    | CFG_bool_const b -> b
    | CFG_bool_rand -> (* Non-deterministic choice, return true or false randomly *)
        true (* Placeholder implementation *)

  let print fmt octagon = (* Implementation needed *)
    Format.fprintf fmt "Octagon abstract element"
end
