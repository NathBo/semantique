(*
  Cours "Sémantique et Application à la Vérification de programmes"

  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

open Frontend
open Cfg

let eval_bool_expr bexpr = match bexpr with
  | CFG_bool_const b -> b
  | _ -> failwith "TODO bool"


let get_main_node cfg =
    List.fold_left (fun node f -> if f.func_name = "main" then f.func_entry else node) cfg.cfg_init_entry cfg.cfg_funcs

let iterate filename cfg =
  print_endline "WARNING, this iterator doesn't support loops and goto (back)";
  let _ = Random.self_init () in
  ignore filename; (* TODO *)

  (* let abstracts_values = failwith "un truc pas clair qui depend de domaine" in        TODO *)
  let worklist = ref [ cfg.cfg_init_entry ; get_main_node cfg ] in (* TODO add the main *)

  while !worklist <> [] do
    let node = List.hd !worklist in
    worklist := List.tl !worklist;

    Printf.printf "update node %d\n" node.node_id;
    
    if true then begin (* TODO : if the value as changed *)
      List.iter (fun arc -> worklist := arc.arc_dst :: !worklist) node.node_out
    end
  done


  (* old code

  let iter_arc arc: unit =
    match arc.arc_inst with
    | CFG_skip _ -> ()
    | CFG_assert (bexpr,ext) ->  if not (eval_bool_expr bexpr)
      then begin
        print_endline ("File "^filename^", line "^(string_of_int (fst ext).pos_lnum)^": Assertion failure")
      end
    | CFG_assign (_,_) ->
            print_endline "TODO cfg assign"
    | _ -> print_endline "TODO arc"
  in

  let iter_node node: unit =
    Format.printf "<%i>: ⊤@ " node.node_id
  in

  List.iter iter_arc cfg.cfg_arcs;
  Format.printf "Node Values:@   @[<v 0>";
  List.iter iter_node cfg.cfg_nodes;
  Format.printf "@]" *)
