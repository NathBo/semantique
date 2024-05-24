(*
  Cours "Sémantique et Application à la Vérification de programmes"

  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

open Frontend
open Iterator
open Domains

module Iter_Constant = ITERATOR_FONCTOR(Constant_domain.CONSTANTDOMAIN)(Domain.DOMAIN_FUNCTOR(Constant_domain.CONSTANTDOMAIN))
module Iter_Concrete = ITERATOR_FONCTOR(Concrete_domain.CONCRETE_DOMAIN)(Domain.DOMAIN_FUNCTOR(Concrete_domain.CONCRETE_DOMAIN))
module Iter_Interval = ITERATOR_FONCTOR(Interval_domain.INTERVALDOMAIN)(Domain.DOMAIN_FUNCTOR(Interval_domain.INTERVALDOMAIN))
module Iter_Sign = ITERATOR_FONCTOR(Sign_domain.SIGN_DOMAIN)(Domain.DOMAIN_FUNCTOR(Sign_domain.SIGN_DOMAIN))
module Iter_Congruence = ITERATOR_FONCTOR(Congruence_domain.CONGRUENCEDOMAIN)(Domain.DOMAIN_FUNCTOR(Congruence_domain.CONGRUENCEDOMAIN))
module Iter_Product = ITERATOR_FONCTOR(Product_domain.PRODUCTDOMAIN)(Domain.DOMAIN_FUNCTOR(Product_domain.PRODUCTDOMAIN))
module Iter_Interval_Disjoint = ITERATOR_FONCTOR(Interval_domain.INTERVALDOMAIN)(Disjoint_domain.DOMAIN_DISJOINT(Interval_domain.INTERVALDOMAIN))

(* parse filename *)
let doit filename =
  let prog = File_parser.parse_file filename in
  let cfg = Tree_to_cfg.prog prog in
  if !Options.verbose then
    Format.printf "%a" Cfg_printer.print_cfg cfg;
  Cfg_printer.output_dot !Options.cfg_out cfg;

  match !Options.domain with
    | "constant" | "constants" -> Iter_Constant.iterate filename cfg
    | "concrete" -> Iter_Concrete.iterate filename cfg
    | "interval" -> Iter_Interval.iterate filename cfg
    | "sign" -> Iter_Sign.iterate filename cfg
    | "congruence" -> Iter_Congruence.iterate filename cfg
    | "product" -> Iter_Product.iterate filename cfg
    | "disjoint" -> Iter_Interval_Disjoint.iterate filename cfg
    | _ ->          Iter_Interval.iterate filename cfg


(* parses arguments to get filename *)
let main () =
  let _ = Options.init () in
  doit !Options.file

let _ = main ()
