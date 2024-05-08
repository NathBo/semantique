(*
  Cours "Sémantique et Application à la Vérification de programmes"

  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

open Frontend
open Cfg

let eval_bool_expr bexpr = match bexpr with
  | CFG_bool_const b -> b
  | _ -> failwith "TODO bool"



let iterate filename cfg =
  let _ = Random.self_init () in

  let iter_arc arc: unit =
    match arc.arc_inst with
    | CFG_skip _ -> ()
    | CFG_assert (bexpr,ext) ->  if not (eval_bool_expr bexpr)
      then begin
        print_endline ("File "^filename^", line "^(string_of_int (fst ext).pos_lnum)^": Assertion failure")
      end
    | _ -> failwith "TODO"
  in

  let iter_node node: unit =
    Format.printf "<%i>: ⊤@ " node.node_id
  in

  List.iter iter_arc cfg.cfg_arcs;
  Format.printf "Node Values:@   @[<v 0>";
  List.iter iter_node cfg.cfg_nodes;
  Format.printf "@]"
