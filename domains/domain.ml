(*
  Cours "Sémantique et Application à la Vérification de programmes"

  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

open Frontend
open! Cfg

module VD = Value_domain.VALUE_DOMAIN

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


module DOMAIN =
  struct

    (* type of abstract elements *)
    (* an element of type t abstracts a set of mappings from variables
       to integers
     *)
    type t = VD.t Env.t

    (* initial environment, with all variables initialized to 0 *)
    let init (var_list:var list) = Env.of_list (List.map (fun x -> (x,VD.const Z.zero)) var_list)

    (* empty set of environments *)
    let bottom = Env.empty


    let rec evaluate env int_expr = match int_expr with
      | CFG_int_const n -> VD.const n
      | CFG_int_var v -> Env.find v env
      | CFG_int_unary (op,i) -> VD.unary (evaluate env i) op
      | CFG_int_binary (op,i1,i2) -> VD.binary (evaluate env i1) (evaluate env i2) op
      | CFG_int_rand (n1,n2) -> VD.rand n1 n2


    (* assign an integer expression to a variable *)
    let assign env var int_expr = 
      Env.add var (evaluate env int_expr) env


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

    (* widening *)
    let widen: t -> t -> t = failwith "pas implémenté"

    (* narrowing *)
    let narrow a b =
      let rec aux _ o_vd1 o_vd2 = match o_vd1,o_vd2 with
      | (_,None) | (None,_) -> None
      | (Some vd1,Some vd2) ->  Some (VD.narrow vd1 vd2) in
      Env.merge aux a b


    (*ne laisse pas passer les valeurs des variables qui feraient que int_expr ne serait pas à valeur dans vd*)
    let rec filter env int_expr vd = match int_expr with
    | CFG_int_const z -> if VD.subset (VD.const z) vd then env else bottom
    | CFG_int_rand (a,b) -> if VD.subset (VD.rand a b) vd then env else bottom
    | CFG_int_var v -> Env.add v (VD.meet (Env.find v env) vd) env
    | CFG_int_unary (op,e) -> filter env e (VD.bwd_unary (evaluate env e) op vd)   (*jpense c bon mais jsuis pas sur*)
    | CFG_int_binary (op,e1,e2) -> let vd1,vd2 = (VD.bwd_binary (evaluate env e1) (evaluate env e2) op vd) in
      filter (filter env e1 vd1) e2 vd2


    (* filter environments to keep only those satisfying the boolean expression *)
    let rec guard a bool_expr = match bool_expr with
      | CFG_bool_const b -> if b then a else Env.empty
      | CFG_bool_rand -> failwith "je sais pas, fais comme tu le sens sur les brand (et je parle pas du champion de League of Legends MDRRRRRR !!!!!!!)"
      | CFG_bool_unary (AST_NOT,expr) -> narrow a (guard a expr)
      | CFG_bool_binary (op,e1,e2) -> begin match op with
        | AST_AND -> meet (guard a e1) (guard a e2)
        | AST_OR -> join (guard a e1) (guard a e2)
        end
      | CFG_compare (op,e1,e2) -> let vd1,vd2 = (VD.compare (evaluate a e1) (evaluate a e2) op) in
        filter (filter a e1 vd1) e2 vd2


    (* whether an abstract element is included in another one *)
    let subset a b =
      let rec aux v vd =
        Env.mem v b && VD.subset vd (Env.find v b) in
      Env.for_all aux a

    (* whether the abstract element represents the empty set *)
    let is_bottom = Env.is_empty

    (* prints *)
    let print fmt map =
      Format.fprintf fmt "{@[";
      Env.iter (fun key value ->
        Format.fprintf fmt "%s -> %a;@ " key.var_name VD.print value
      ) map;
      Format.fprintf fmt "@]}"

  end

