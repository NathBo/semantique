(*
  Cours "Sémantique et Application à la Vérification de programmes"

  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

open Frontend
open! Cfg


(* Signature for the variables *)

module type VARS = sig
  val support: var list
end

module Env = Map.Make( 
  struct
    let compare = compare
    type t = var
  end )



(*
  Signature of abstract domains representing sets of envrionments
  (for instance: a map from variable to their bounds).
 *)


module DOMAIN_FUNCTOR (VD:Value_domain.VALUE_DOMAIN) : Domain_sig.DOMAIN =
  struct

    (* type of abstract elements *)
    (* an element of type t abstracts a set of mappings from variables
       to integers
     *)

    type t = VD.t Env.t

    let to_string map =
      let rep = ref "{" in
      Env.iter (fun key value ->
        rep := !rep ^  key.var_name ^" -> " ^ VD.to_string value ^ ";"
      ) map;
      !rep^"}"

    let envfind v env = match Env.find_opt v env with
      | Some x -> x
      | None -> VD.bottom
    

    (* initial environment, with all variables initialized to 0 *)
    let init (var_list:var list) = List.fold_left (fun env key -> Env.add key (VD.const Z.zero) env) Env.empty var_list

    (* empty set of environments *)
    let bottom = Env.empty


    let rec evaluate env int_expr = match int_expr with
      | CFG_int_const n -> VD.const n
      | CFG_int_var v -> begin
              match Env.find_opt v env with
              | Some x -> x
              | None -> VD.bottom
          end
      | CFG_int_unary (op,i) -> VD.unary (evaluate env i) op
      | CFG_int_binary (op,i1,i2) -> VD.binary (evaluate env i1) (evaluate env i2) op
      | CFG_int_rand (n1,n2) -> VD.rand n1 n2


    (* assign an integer expression to a variable *)
    let assign env var int_expr = 
      Env.add var (evaluate env int_expr) env

    let assign_top env var = 
      Env.add var VD.top env
        

    (* abstract join *)
    let join a b =
      let rec aux _ vd1 vd2 =
        Some (VD.join vd1 vd2) in
      Env.union aux a b

    (* abstract meet *)
    let meet a b =
      let rec aux _ o_vd1 o_vd2 = match o_vd1,o_vd2 with
      | (_,None) | (None,_) -> None
      | (Some vd1,Some vd2) ->  Some (VD.meet vd1 vd2) in
      Env.merge aux a b


    (* prints *)
    let print fmt map =
      Format.fprintf fmt "{@[";
      Env.iter (fun key value ->
        Format.fprintf fmt "%s(%d) -> %a;@ " key.var_name key.var_id VD.print value
      ) map;
      Format.fprintf fmt "@]}"


    (* widening *)
    let widen domain_A domain_B = 
        Env.merge 
            (fun var valA valB -> match valA,valB with
                | (None, None) -> failwith "impossible"
                | (None, Some x) -> Some x
                | (Some x, None) -> Some x
                | (Some x,Some y) -> Some (VD.widen x y)
            ) domain_A domain_B

    (* narrowing *)
    let narrow a b =
      (*print_endline "On narrow :";
      print_endline (to_string a);
      print_endline (to_string b);*)
      let rec aux _ o_vd1 o_vd2 = match o_vd1,o_vd2 with
      | (_,None) | (None,_) -> None
      | (Some vd1,Some vd2) ->  Some (VD.narrow vd1 vd2) in
      let x = Env.merge aux a b in
      (*print_endline "On obtient :";
      prerr_endline (to_string x);*)
      x


    (*ne laisse pas passer les valeurs des variables qui feraient que int_expr ne serait pas à valeur dans vd*)
    let rec filter env int_expr vd = match int_expr with
    | CFG_int_const z -> if VD.subset (VD.const z) vd then env else Env.map (fun x -> VD.bottom) env
    | CFG_int_rand (a,b) -> if VD.subset (VD.rand a b) vd then env else Env.map (fun x -> VD.bottom) env
    | CFG_int_var v -> let x = VD.meet (envfind v env) vd in if VD.is_bottom x then Env.map (fun x -> VD.bottom) env else Env.add v x env
    | CFG_int_unary (op,e) -> filter env e (VD.bwd_unary (evaluate env e) op vd)   (*jpense c bon mais jsuis pas sur*)
    | CFG_int_binary (op,e1,e2) -> let vd1,vd2 = (VD.bwd_binary (evaluate env e1) (evaluate env e2) op vd) in
      filter (filter env e1 vd1) e2 vd2


    (* filter environments to keep only those satisfying the boolean expression *)
    let rec guard a bool_expr = match bool_expr with
      | CFG_bool_const b -> if b then a else Env.map (fun x -> VD.bottom) a
      | CFG_bool_rand -> a
      | CFG_bool_unary (AST_NOT,expr) -> guard a (negate expr)
      | CFG_bool_binary (op,e1,e2) -> begin match op with
        | AST_AND -> meet (guard a e1) (guard a e2)
        | AST_OR -> join (guard a e1) (guard a e2)
        end
      | CFG_compare (op,e1,e2) -> begin let vd1,vd2 = (VD.compare (evaluate a e1) (evaluate a e2) op) in
        print_endline "On a l'environnement :";
        print_endline (to_string a);
        print_endline "On a les valeurs de :";
        print_endline (VD.to_string vd1);
        print_endline (VD.to_string vd2);
        let x = filter (filter a e1 vd1) e2 vd2 in print_endline "L'environnement final est :"; print_endline (to_string x); x end

    let rec bwd_assign x var expr r = match expr with
        | CFG_int_var r_var -> assign r r_var (CFG_int_var var)
        | CFG_int_rand (_, _) -> x
        | CFG_int_const _ -> x
        | CFG_int_unary (u_op, expr2) ->
                let old_val_result = Env.find var r in
                let new_val_result = VD.bwd_unary VD.top u_op old_val_result in
                let new_env_result = Env.add var new_val_result r in
                bwd_assign x var expr2 new_env_result
        | CFG_int_binary (b_op, expr2, expr3) -> failwith "TODO"





    (* whether an abstract element is included in another one *)
    let subset a b =
      let rec aux v vd =
        Env.mem v b && VD.subset vd (envfind v b) in
      Env.for_all aux a

    (* whether the abstract element represents the empty set *)
    let is_bottom =
      let aux _ x =
        VD.is_bottom x in
      Env.for_all aux  


  end

