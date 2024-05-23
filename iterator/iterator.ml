(*
  Cours "Sémantique et Application à la Vérification de programmes"

  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

open Frontend
open Cfg
open! Domains
open! Domain



let eval_bool_expr bexpr = match bexpr with
  | CFG_bool_const b -> b
  | _ -> failwith "TODO bool"


let get_main_node cfg =
    List.fold_left (fun node f -> if f.func_name = "main" then f.func_entry else node) cfg.cfg_init_entry cfg.cfg_funcs


module ITERATOR_FONCTOR(VD:Value_domain.VALUE_DOMAIN) = 
    struct

    module DOMAIN = Domain.DOMAIN_FUNCTOR(VD) 
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


    let iterate filename cfg =
        let _ = Random.self_init () in

        let envs = ref (List.fold_left (fun map node -> NodeMap.add node DOMAIN.bottom map) NodeMap.empty cfg.cfg_nodes) in
        ignore envs;

        let worklist = ref [ cfg.cfg_init_entry ; get_main_node cfg ] in
        let already_seen = ref  NodeSet.empty in

        while !worklist <> [] do
            let node = List.hd !worklist in
            worklist := List.tl !worklist;

            
            (*Format.fprintf Format.std_formatter "update node %d\n" node.node_id;*)
            let old_value = NodeMap.find node !envs in

            let update = List.fold_left (fun value arc -> 
                let source = arc.arc_src in
                (*Format.fprintf Format.std_formatter " -> from %d\n" source.node_id;*)
                let curEnv = NodeMap.find source !envs in
                let newVal = match arc.arc_inst with
                    | CFG_skip _ -> curEnv 
                    | CFG_assign (var,iexpr) -> DOMAIN.assign curEnv var iexpr
                    | CFG_guard bexpr -> DOMAIN.guard curEnv bexpr
                    | CFG_assert (bexpr,ext) ->
                            let subEnv = DOMAIN.guard curEnv (negate bexpr) in
                            if not (DOMAIN.is_bottom subEnv) then
                                print_endline ("File "^filename^", line "^(string_of_int (fst ext).pos_lnum)^": Assertion failure")
                            ; curEnv
                    | CFG_call fct -> ignore fct; failwith "TODO call"
                in DOMAIN.join value newVal
            ) DOMAIN.bottom node.node_in in

            let no_change = ((DOMAIN.subset old_value update) && (DOMAIN.subset update old_value)) in
            envs := NodeMap.add node update !envs ;

            (*DOMAIN.print Format.std_formatter old_value;
            DOMAIN.print Format.std_formatter update;
            Format.print_newline ();*)

            let first_time = not (NodeSet.mem node !already_seen) in
            if (not no_change) || first_time then begin
                List.iter (fun arc -> worklist := arc.arc_dst :: !worklist) node.node_out
            end;

            if first_time then already_seen := NodeSet.add node !already_seen
        done

    end
