(*
  Cours "Sémantique et Application à la Vérification de programmes"

  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

open Frontend
open Cfg
open! Domains
open! Domain

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
        | fct::remains_fct -> begin
            let new_arc_list = ref cfg1.cfg_arcs in
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

                fct.func_entry.node_in <- arc1 :: fct.func_entry.node_in;
                arc.arc_src.node_out <- arc1 :: arc.arc_src.node_out;
                fct.func_exit.node_out <- arc2 :: fct.func_exit.node_out;
                arc.arc_dst.node_in <- arc2 :: arc.arc_dst.node_in;
            ) fct.func_calls;
            
            let init_exit = ref cfg1.cfg_init_exit in
            if fct.func_name = "main" then begin
                let arc_liaison = {
                    arc_id = !new_arc_id;
                    arc_src = cfg1.cfg_init_exit;
                    arc_dst = fct.func_entry;
                    arc_inst = CFG_skip "init to main"} in
                incr new_arc_id;
                new_arc_list := arc_liaison :: !new_arc_list;
                fct.func_entry.node_in <- arc_liaison :: fct.func_entry.node_in;
                cfg1.cfg_init_exit.node_out <- arc_liaison :: cfg1.cfg_init_exit.node_out;
                init_exit := fct.func_exit
            end;
                    

            remove_fct {cfg_vars = cfg1.cfg_vars; cfg_funcs= remains_fct; cfg_nodes = cfg1.cfg_nodes; cfg_arcs = !new_arc_list; cfg_init_entry = cfg1.cfg_init_entry; cfg_init_exit = !init_exit}
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




        


module ITERATOR_FONCTOR(VD:Value_domain.VALUE_DOMAIN) (DOMAIN:Domain_sig.DOMAIN) = 
    struct

    let init_envs_bottom cfg =
        List.fold_left (fun map node -> NodeMap.add node DOMAIN.bottom map) NodeMap.empty cfg.cfg_nodes

    let dfs cfg start update next envs_init=
        let envs = ref envs_init in
        let worklist = ref [ start ] in
        let already_seen = ref NodeSet.empty in
        let widening_set = select_widening_node cfg in
        while !worklist <> [] do
            let node = List.hd !worklist in
            worklist := List.tl !worklist;
            
            let old_value = NodeMap.find node !envs in
            
            let new_value = update node !envs in
            
            let is_widen = NodeSet.mem node widening_set in
            let widen_value =
                if is_widen then DOMAIN.widen old_value new_value
                            else new_value in

            let no_change = ((DOMAIN.subset old_value widen_value) && (DOMAIN.subset widen_value old_value)) in
            envs := NodeMap.add node widen_value !envs ;

            let first_time = not (NodeSet.mem node !already_seen) in
            if (not no_change) || first_time then
                worklist := (next node)@(!worklist);

            if first_time then already_seen := NodeSet.add node !already_seen
        done;
        !envs

    let update_forward first_pass with_assert filename node envs =
        let env_common = List.fold_left (fun value arc -> 
            let source = arc.arc_src in
            let curEnv = NodeMap.find source envs in
            let newVal = match arc.arc_inst with
                | CFG_skip _ -> curEnv 
                | CFG_assign (var,iexpr) -> begin
                    try 
                        DOMAIN.meet (DOMAIN.assign curEnv var iexpr) (DOMAIN.assign_top curEnv var)
                    with | Frontend.Abstract_syntax_tree.DivisionByZero -> 
                        print_endline ("Warning : File "^filename^": Division by zero");
                        DOMAIN.bottom (*TODO *)
                    end
                | CFG_guard bexpr -> DOMAIN.guard curEnv bexpr
                | CFG_assert (bexpr,ext) ->
                        if with_assert then begin
                            let subEnv = DOMAIN.guard curEnv (negate bexpr) in
                            if not (DOMAIN.is_bottom subEnv) then
                                print_endline ("File "^filename^", line "^(string_of_int (fst ext).pos_lnum)^": Assertion failure")
                        end;
                        DOMAIN.guard curEnv bexpr
                | CFG_call fct -> ignore fct; failwith "this case is impossible"
            in DOMAIN.join value newVal
        ) DOMAIN.bottom node.node_in in
        if first_pass then env_common else DOMAIN.meet env_common (NodeMap.find node envs)

    let forward filename cfg envs =
        let start = cfg.cfg_init_entry in
        let update = update_forward true true filename in
       
        let next node =
            List.rev (List.map (fun arc -> arc.arc_dst) node.node_out)
            (* en cas de boucle, il est préférable (en espérance) d'inverser l'ordre des noeuds vu la façon dont est construi le cfg *)
        in
        dfs cfg start update next envs (* return new envs *)

    let forward_without_assert first_pass filename cfg envs =
        let start = cfg.cfg_init_entry in
        let update = update_forward first_pass false filename in
       
        let next node =
            List.rev (List.map (fun arc -> arc.arc_dst) node.node_out)
            (* en cas de boucle, il est préférable (en espérance) d'inverser l'ordre des noeuds vu la façon dont est construi le cfg *)
        in
        dfs cfg start update next envs (* return new envs *)


    let backward iteration filename cfg envs =
        ignore filename; ignore cfg;
        let start = cfg.cfg_init_exit in

        let update node envs =
            List.fold_left (fun value arc -> 
                let dest = arc.arc_dst in
                let curEnv = NodeMap.find dest envs in
                let newVal = match arc.arc_inst with
                    | CFG_skip _ -> curEnv 
                    | CFG_assign (var,iexpr) ->
                        DOMAIN.bwd_assign (NodeMap.find node envs) var iexpr curEnv 
                    | CFG_guard bexpr -> DOMAIN.guard curEnv bexpr
                    | CFG_assert (bexpr,_) -> DOMAIN.guard curEnv (bexpr)
                    | CFG_call fct -> ignore fct; failwith "this case is impossible"
                in DOMAIN.meet value newVal
            ) (NodeMap.find node envs) node.node_out
        in

        let next node =
            List.rev (List.map (fun arc -> arc.arc_src) node.node_in)
        in
        
        let envs2 = dfs cfg start update next envs in
        Format.fprintf Format.std_formatter "\027[31m___________Result of the backward analysis_(%d)_______\027[0m\n" iteration;
        NodeMap.iter (fun node domain ->
            Format.fprintf Format.std_formatter "\n\027[33mnode %d :\027[0m\n" node.node_id;
            DOMAIN.print Format.std_formatter domain;
            Format.fprintf Format.std_formatter "\n"
        ) envs2;
        envs2

    let forward_backward filename cfg init_envs =
        let envs = ref init_envs in
        let continue = ref true in
        let iteration = ref 0 in
        while !continue do
            incr iteration;
            envs := forward_without_assert (!iteration <= 1) filename cfg !envs;
            envs := backward !iteration filename cfg !envs;

            let response = ref "?" in
            while not (List.mem !response ["";"Y";"y";"N";"n";"yes";"no";"Yes";"No"]) do
                Format.fprintf Format.std_formatter "\027[32mDo you want to iterate one more time ? [Y/n] : \027[0m\n";
                response := read_line();
            done;
            continue := List.mem !response ["";"Y";"y";"yes";"Yes"]
        done




    let iterate filename cfg_with_fct is_reverse =
        let cfg = replace_fct cfg_with_fct in
        let _ = Random.self_init () in
        let init_envs = init_envs_bottom cfg in
        if is_reverse then forward_backward filename cfg init_envs else ignore(forward filename cfg init_envs)

    end
