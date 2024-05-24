(*
  Cours "Sémantique et Application à la Vérification de programmes"

  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

open Frontend
open Cfg
open! Domains
open! Domain

let print_node node =
    Printf.printf "node %d, %d, %d\n" node.node_id  (List.length node.node_in) (List.length node.node_out)


module NodeMap = Map.Make(
    struct
        let compare = compare
        type t = node
    end )
module NodeSet = Set.Make(
    struct
        let compare = compare
        type t = node
    end )


let get_main_node cfg =
    List.fold_left (fun node f -> if f.func_name = "main" then f.func_entry else node) cfg.cfg_init_entry cfg.cfg_funcs


let select_widening_node cfg =
    let result = ref NodeSet.empty in
    let in_progress = ref NodeSet.empty in
    let finished = ref NodeSet.empty in
    
    let rec dfs pos =
        if (not (NodeSet.mem pos !in_progress)) && (not (NodeSet.mem pos !finished))  then begin
            (* this node is seen for the first time *)
            in_progress := NodeSet.add pos !in_progress;
            List.iter (fun arc ->
                let dest = arc.arc_dst in
                if (NodeSet.mem dest !finished) then begin
                    (* ok : already finished *)
                    ()
                end else if (NodeSet.mem dest !in_progress) then begin
                    (* there is a loop *)
                    result := NodeSet.add dest !result
                end else begin
                    (* first time *)
                    dfs dest
                end
            ) pos.node_out;
            finished := NodeSet.add pos !finished;
        end
    in

    List.iter dfs cfg.cfg_nodes;
    !result

let replace_fct cfg_old = 
    let new_arc_id = ref ((List.fold_left (fun id arc -> max id arc.arc_id) 0 cfg_old.cfg_arcs)+1) in

    let rec remove_fct cfg1 =
        match cfg1.cfg_funcs with
        | [] -> cfg1 (* il n'y a plus de fonctions *)
        | [f] when f.func_name = "main" -> cfg1
        | f::l when f.func_name = "main" ->
                remove_fct {cfg_vars = cfg1.cfg_vars; cfg_funcs= l@[f]; cfg_nodes = cfg1.cfg_nodes; cfg_arcs = cfg1.cfg_arcs; cfg_init_entry = cfg1.cfg_init_entry; cfg_init_exit = cfg1.cfg_init_exit}
        | fct::remains_fct -> begin
            Printf.printf "remplace %s\n" fct.func_name;
            let new_arc_list = ref cfg_old.cfg_arcs in
            List.iter (fun arc -> 
                let arc1 = {
                    arc_id = !new_arc_id;
                    arc_src = arc.arc_src;
                    arc_dst = fct.func_entry;
                    arc_inst = CFG_skip "enter function" } in
                incr new_arc_id;
                new_arc_list := arc1 :: !new_arc_list;
                let arc2 = {
                    arc_id = !new_arc_id;
                    arc_src = fct.func_exit;
                    arc_dst = arc.arc_dst;
                    arc_inst = CFG_skip "leave function"} in
                incr new_arc_id;
                new_arc_list := arc2 :: !new_arc_list;

                print_node fct.func_entry;
                fct.func_entry.node_in <- arc1 :: fct.func_entry.node_in;
                arc.arc_src.node_out <- arc1 :: arc.arc_src.node_out;
                fct.func_exit.node_out <- arc2 :: fct.func_exit.node_out;
                arc.arc_dst.node_in <- arc2 :: arc.arc_dst.node_in;
                print_node fct.func_entry
            ) fct.func_calls;
            remove_fct {cfg_vars = cfg1.cfg_vars; cfg_funcs= remains_fct; cfg_nodes = cfg1.cfg_nodes; cfg_arcs = !new_arc_list; cfg_init_entry = cfg1.cfg_init_entry; cfg_init_exit = cfg1.cfg_init_exit}
        end
    in
    let cfg2 = remove_fct cfg_old in

    let is_not_fct_arc arc = match arc.arc_inst with
        | CFG_call _ -> false
        | _ -> true
    in
    
    let new_node_list = List.map (fun node ->
        node.node_in <- List.filter is_not_fct_arc node.node_in;
        node.node_out <- List.filter is_not_fct_arc node.node_out;
        node ) cfg2.cfg_nodes in

    let new_arc_list = List.filter is_not_fct_arc cfg2.cfg_arcs in
    
    {   cfg_vars = cfg2.cfg_vars;
        cfg_funcs = cfg2.cfg_funcs;
        cfg_nodes = new_node_list;
        cfg_arcs = new_arc_list;
        cfg_init_entry = cfg2.cfg_init_entry;
        cfg_init_exit = cfg2.cfg_init_exit;
    }




        


module ITERATOR_FONCTOR(VD:Value_domain.VALUE_DOMAIN) = 
    struct

    module DOMAIN = Domain.DOMAIN_FUNCTOR(VD) 

    let iterate filename cfg_with_fct =
        let cfg = replace_fct cfg_with_fct in
        Format.printf "%a" Cfg_printer.print_cfg cfg;
        let _ = Random.self_init () in

        let envs = ref (List.fold_left (fun map node -> NodeMap.add node DOMAIN.bottom map) NodeMap.empty cfg.cfg_nodes) in
        ignore envs;

        let worklist = ref [ cfg.cfg_init_entry ; get_main_node cfg ] in
        let already_seen = ref  NodeSet.empty in

        let widening_set = select_widening_node cfg in

        while !worklist <> [] do
            let node = List.hd !worklist in
            worklist := List.tl !worklist;

            
            Format.fprintf Format.std_formatter "update node %d\n" node.node_id;
            let old_value = NodeMap.find node !envs in

            let update = List.fold_left (fun value arc -> 
                let source = arc.arc_src in
                (*Format.fprintf Format.std_formatter " -> from %d\n" source.node_id;*)
                let curEnv = NodeMap.find source !envs in
                let newVal = match arc.arc_inst with
                    | CFG_skip _ -> curEnv 
                    | CFG_assign (var,iexpr) -> begin
                        try DOMAIN.assign curEnv var iexpr
                        with | Frontend.Abstract_syntax_tree.DivisionByZero -> 
                            print_endline ("Warning : File "^filename^": Division by zero");
                            DOMAIN.bottom (*TODO *)
                        end
                    | CFG_guard bexpr -> DOMAIN.guard curEnv bexpr
                    | CFG_assert (bexpr,ext) ->
                            let subEnv = DOMAIN.guard curEnv (negate bexpr) in
                            if not (DOMAIN.is_bottom subEnv) then
                                print_endline ("File "^filename^", line "^(string_of_int (fst ext).pos_lnum)^": Assertion failure")
                            ; DOMAIN.guard curEnv (bexpr)
                    | CFG_call fct -> ignore fct; failwith "this case is impossible"
                in DOMAIN.join value newVal
            ) DOMAIN.bottom node.node_in in

            let is_widen = NodeSet.mem node widening_set in
            let widen_value =
                if is_widen then DOMAIN.widen old_value update
                            else update in

            let no_change = ((DOMAIN.subset old_value widen_value) && (DOMAIN.subset widen_value old_value)) in
            envs := NodeMap.add node widen_value !envs ;

            Format.fprintf Format.std_formatter "old->new %d\n" node.node_id;
            DOMAIN.print Format.std_formatter old_value;
            DOMAIN.print Format.std_formatter widen_value;
            Format.print_newline ();

            let first_time = not (NodeSet.mem node !already_seen) in
            if (not no_change) || first_time then begin
                List.iter (fun arc -> worklist := arc.arc_dst :: !worklist) node.node_out
            end;

            if first_time then already_seen := NodeSet.add node !already_seen
        done

    end
