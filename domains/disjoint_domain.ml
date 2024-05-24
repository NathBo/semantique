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








(*
  Signature of abstract domains representing sets of envrionments
  (for instance: a map from variable to their bounds).
 *)


module DOMAIN_DISJOINT (VD:Value_domain.VALUE_DOMAIN) : Domain_sig.DOMAIN =
  struct

    (* type of abstract elements *)
    (* an element of type t abstracts a set of mappings from variables
       to integers
     *)

    module D = Domain.DOMAIN_FUNCTOR(VD)

    module E = Domain.Env


    type t = (VD.t list) E.t

    let to_string map =
      let rep = ref "{" in
      E.iter (fun key value ->
        rep := !rep ^  key.var_name ^" -> [";
        List.iter (fun x -> rep:= !rep^(VD.to_string x)^",") value;
        rep := !rep^"];";
      ) map;
      rep:= !rep^"]";
      !rep^"}"

    let envfind v env = match E.find_opt v env with
      | Some x -> x
      | None -> []
    

    (* initial environment, with all variables initialized to 0 *)
    let init (var_list:var list) = List.fold_left (fun env key -> let x = E.add key [VD.const Z.zero] env in  x) E.empty var_list

    (* empty set of environments *)
    let bottom = E.empty


    let addposs nouv curr = nouv::curr

    let rec addposslist nouvl curr = match nouvl with
    | [] -> curr
    | x::q -> addposslist q (x::curr)


    let rec evaluate env int_expr = match int_expr with
    | CFG_int_const n -> [VD.const n]
    | CFG_int_var v -> E.find v env
    | CFG_int_unary (op,i) -> let l = evaluate env i in List.map (fun x -> VD.unary x op) l
    | CFG_int_binary (op,i1,i2) -> 
      let a = evaluate env i1 in
      let b = evaluate env i2 in
      let aux acc b_elt =
      addposslist (List.map (fun x -> VD.binary b_elt x op) a) acc in
      (List.fold_left aux b [])
    | CFG_int_rand(n1,n2) -> [VD.rand n1 n2]



    (* assign an integer expression to a variable *)
    let assign (env:VD.t list E.t) var int_expr = E.add var (evaluate env int_expr) env


    (* abstract join *)
    let join a b =
      let merge_fun _ a b = match a,b with
      | None,None -> None
      | Some x, None | None, Some x -> Some x
      | Some a, Some b -> Some (addposslist a b) in
      E.merge merge_fun a b

    (* abstract meet *)
    let meet a b = failwith "pas implémenté car pas utilisé dans l'itérateur"


    (* prints *)
    let print fmt map =
      Format.fprintf fmt "{@[";
      E.iter (fun key value ->
        Format.fprintf fmt "%s(%d) -> " key.var_name key.var_id;
        Format.pp_print_list VD.print fmt value
      ) map;
      Format.fprintf fmt "@]}"


    (* widening *)
    let widen domain_A domain_B = failwith "pas implémenté5"

    (* narrowing *)
    let narrow a b =failwith "pas implémenté6"


    (*ne laisse pas passer les valeurs des variables qui feraient que int_expr ne serait pas à valeur dans vd*)
    let rec filter env int_expr vd = failwith "pas implémenté7"


    (* filter environments to keep only those satisfying the boolean expression *)
    let rec guard a bool_expr = failwith "pas implémenté8"


    (* whether an abstract element is included in another one *)
    let subset a b =failwith "pas implémenté9"

    (* whether the abstract element represents the empty set *)
    let is_bottom a  = failwith "pas implémenté10"


  end

