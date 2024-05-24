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
      | None -> VD.bottom
    

    (* initial environment, with all variables initialized to 0 *)
    let init (var_list:var list) = List.fold_left (fun env key -> let x = E.add key [VD.const Z.zero] env in  x) E.empty var_list

    (* empty set of environments *)
    let bottom = E.empty




    (* assign an integer expression to a variable *)
    let assign (env:VD.t list E.t) var int_expr = failwith "pas implémenté1"


    (* abstract join *)
    let join a b =
      failwith "pas implémenté2"

    (* abstract meet *)
    let meet a b = failwith "pas implémenté3"


    (* prints *)
    let print fmt map =failwith "pas implémenté4"


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

