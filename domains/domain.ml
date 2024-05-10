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
    let init (var_list:var list) = Env.of_list (List.map (fun x -> (x,VD.const 0)) var_list)

    (* empty set of environments *)
    let bottom = Env.empty


    let rec evaluate env int_expr = match int_expr with
      | CFG_int_const n -> VD.const (Z.to_int n)
      | CFG_int_var v -> Env.find v env
      | CFG_int_unary (op,i) -> VD.unary (evaluate env i) op
      | CFG_int_binary (op,i1,i2) -> VD.binary (evaluate env i1) (evaluate env i2) op
      | CFG_int_rand (n1,n2) -> VD.rand (Z.to_int n1) (Z.to_int n2)


    (* assign an integer expression to a variable *)
    let assign env var int_expr = 
      Env.add var (evaluate env int_expr) env

    (* filter environments to keep only those satisfying the boolean expression *)
    let guard: t -> bool_expr -> t

    (* abstract join *)
    let join: t -> t -> t

    (* abstract meet *)
    let meet: t -> t -> t

    (* widening *)
    let widen: t -> t -> t

    (* narrowing *)
    let narrow: t -> t -> t

    (* whether an abstract element is included in another one *)
    let subset: t -> t -> bool

    (* whether the abstract element represents the empty set *)
    let is_bottom: t -> bool

    (* prints *)
    let print: Format.formatter -> t -> unit

  end

