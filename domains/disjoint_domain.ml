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


let list_iter2 f la lb =
  List.iter (fun a -> List.iter (fun b -> f a b) lb) la

let list_exists2 f la lb =
  List.exists (fun a -> List.exists (fun b -> f a b) lb) la



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


    let to_string_list vdl =
      let rep = ref "" in
      List.iter (fun x -> rep:= !rep^(VD.to_string x)^",") vdl;
      !rep

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
    | CFG_int_var v -> envfind v env
    | CFG_int_unary (op,i) -> let l = evaluate env i in List.map (fun x -> VD.unary x op) l
    | CFG_int_binary (op,i1,i2) -> 
      let a = evaluate env i1 in
      let b = evaluate env i2 in
      let rep = ref [] in
      let aux a_elt b_elt =
      rep := addposs ( VD.binary a_elt b_elt op) !rep in
      list_iter2 aux a b;
      !rep
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
    let meet a b = 
      let rec aux _ o_vd1 o_vd2 = match o_vd1,o_vd2 with
      | (_,None) | (None,_) -> None
      | (Some vd1,Some vd2) ->  Some (vd1) in     (*TODO ameliorer ça*)
      E.merge aux a b


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
    let narrow a b = a (*pas precis car de toute façon pas utilise par l'itérateur*)


    (*ne laisse pas passer les valeurs des variables qui feraient que int_expr ne serait pas à valeur dans vd*)
    let rec filter env int_expr vdl = match int_expr with
    | CFG_int_const z -> if List.exists (fun x -> VD.subset (VD.const z) x) vdl then env else E.map (fun x -> [VD.bottom]) env
    | CFG_int_rand (n1,n2) -> if List.exists (fun x -> VD.subset (VD.rand n1 n2) x) vdl then env else E.map (fun x -> [VD.bottom]) env
    | CFG_int_var v -> 
      let a = envfind v env in
      let b = vdl in
      let aux acc b_elt =
      addposslist (List.map (fun x -> VD.meet b_elt x) a) acc in
      let l = (List.fold_left aux b []) in
      if List.exists (fun x -> not (VD.is_bottom x)) l
      then E.add v l env
      else E.map (fun x -> [VD.bottom]) env
    | CFG_int_unary (op,e) -> let a = evaluate env e in
    let aux acc b_elt =
      addposslist (List.map (fun x -> VD.bwd_unary x op b_elt) a) acc in
      let b = (List.fold_left aux vdl []) in
      filter env e b
    | CFG_int_binary (op,e1,e2) ->
      let a = evaluate env e1 in
      let b = evaluate env e2 in
      let rep1 = ref [] in
      let rep2 = ref [] in
      let aux a_elt b_elt e_elt =
        let vd1,vd2 = VD.bwd_binary a_elt b_elt op e_elt in
        rep1 := addposs vd1 !rep1;
        rep2 := addposs vd2 !rep2 in
      List.iter (fun a -> list_iter2 (aux a) b vdl) a;
      filter (filter env e1 !rep1) e2 !rep2


    (* filter environments to keep only those satisfying the boolean expression *)
    let rec guard a bool_expr = match bool_expr with
    | CFG_bool_const b -> if b then a else E.map (fun x -> []) a
    | CFG_bool_rand -> a
    | CFG_bool_unary (AST_NOT,expr) -> guard a (negate expr)
    | CFG_bool_binary (op,e1,e2) -> begin match op with
        | AST_AND -> meet (guard a e1) (guard a e2)
        | AST_OR -> join (guard a e1) (guard a e2)
        end
    | CFG_compare (op,e1,e2) -> begin let vd1 = ref [] in
        let vd2 = ref [] in
        let rec aux a_elt b_elt =
          let v1,v2 = VD.compare a_elt b_elt op in
          print_endline (VD.to_string v1);
          print_endline (VD.to_string v2);
          vd1 := addposs v1 !vd1;
          vd2 := addposs v2 !vd2 in
        list_iter2 aux (evaluate a e1) (evaluate a e2);
        print_endline (to_string_list (evaluate a e1));
        print_endline "On a l'environnement :";
        print_endline (to_string a);
        print_endline "On a les valeurs de :";
        print_endline (to_string_list !vd1);
        print_endline (to_string_list !vd2);
        let x = filter (filter a e1 !vd1) e2 !vd2 in print_endline "L'environnement final est :"; print_endline (to_string x); x end



    (* whether an abstract element is included in another one *)
    let subset a b = E.for_all (fun v x -> E.mem v b && list_exists2 (fun y z -> VD.subset y z) x (E.find v b)) a

    (* whether the abstract element represents the empty set *)
    let is_bottom a  = E.for_all (fun _ x -> List.for_all VD.is_bottom x) a


  end

