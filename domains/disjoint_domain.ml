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


    let addposs nouv curr =
        if VD.is_bottom nouv || List.exists (VD.subset nouv) curr  then curr 
        else begin
            nouv::(List.filter (fun x -> not (VD.subset x nouv) ) curr )
        end

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



  
    let assign_top (env:VD.t list E.t) var = E.add var [VD.top] env
    let assign (env:VD.t list E.t) var int_expr = E.add var (evaluate env int_expr) env


    (* abstract join *)
    let join a b =
      let merge_fun _ a b = match a,b with
      | None,None -> None
      | Some x, None | None, Some x -> Some x
      | Some a, Some b -> Some (addposslist a b) in
      E.merge merge_fun a b


    (* whether the abstract element represents the empty set *)
    let is_bottom a  = E.for_all (fun _ x -> List.for_all VD.is_bottom x) a


    let list_meet vd1 vd2 =
      let rep = ref [] in
        let rec aux a_elt b_elt = rep := addposs (VD.meet a_elt b_elt) !rep in
        list_iter2 aux vd1 vd2; !rep

    (* abstract meet *)
    let meet a b = 
      let rec aux _ o_vd1 o_vd2 = match o_vd1,o_vd2 with
      | (_,None) | (None,_) -> None
      | (Some vd1,Some vd2) ->  let rep = ref [] in
        let rec aux a_elt b_elt = rep := addposs (VD.meet a_elt b_elt) !rep in
        list_iter2 aux vd1 vd2; Some(!rep) in     (*TODO ameliorer ça*)
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
    let widen domainA domainB =
        (* pour une variable donnée, on n'a pas moyen de savoir quels éléments de la listeA comparer à la listeB, le seul widen possible serait donc de se ramener à des listes à un seul élément (ce qui est dommage) *)
        E.merge (fun _ x y -> match x,y with
            | (None, None) -> failwith "impossible"
            | (Some x, None) | (None, Some x) -> Some x
            | (Some lstA, Some lstB) ->
                    let elemA = List.fold_left (fun union elem -> VD.join union elem) VD.bottom lstA in
                    let elemB = List.fold_left (fun union elem -> VD.join union elem) VD.bottom lstA in
                    Some [VD.widen elemA elemB]
        ) domainA domainB


    (* narrowing *)
    let narrow a b = a (*pas precis car de toute façon pas utilise par l'itérateur*)


    (*ne laisse pas passer les valeurs des variables qui feraient que int_expr ne serait pas à valeur dans vd*)
    let rec filter env int_expr vdl = match int_expr with
    | CFG_int_const z -> if List.exists (fun x -> VD.subset (VD.const z) x) vdl then env else E.map (fun x -> []) env
    | CFG_int_rand (n1,n2) -> if List.exists (fun x -> VD.subset (VD.rand n1 n2) x) vdl then env else E.map (fun x -> []) env
    | CFG_int_var v -> let x = list_meet (envfind v env) vdl in if List.for_all VD.is_bottom x then E.map (fun x -> []) env else E.add v x env
    | CFG_int_unary (op,e) -> let a = evaluate env e in
    let rep = ref [] in
    let aux a_elt b_elt =
      rep := addposs (VD.bwd_unary a_elt op b_elt) !rep in
      list_iter2 aux a vdl;
      filter env e !rep
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
          vd1 := addposs v1 !vd1;
          vd2 := addposs v2 !vd2 in
        list_iter2 aux (evaluate a e1) (evaluate a e2);
        let x = filter (filter a e1 !vd1) e2 !vd2 in x end



    (* whether an abstract element is included in another one *)
    let subset a b = E.for_all (fun v x -> E.mem v b && list_exists2 (fun y z -> VD.subset y z) x (E.find v b)) a



    let bwd_assign x var expr r = failwith "les extensions ne sont pas compatibles entre elles"

  end

