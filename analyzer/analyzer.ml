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
module Iter_Congruence_Disjoint = ITERATOR_FONCTOR(Congruence_domain.CONGRUENCEDOMAIN)(Disjoint_domain.DOMAIN_DISJOINT(Congruence_domain.CONGRUENCEDOMAIN))
module Iter_Product_Disjoint = ITERATOR_FONCTOR(Product_domain.PRODUCTDOMAIN)(Disjoint_domain.DOMAIN_DISJOINT(Product_domain.PRODUCTDOMAIN))
module Iter_Constant_Disjoint = ITERATOR_FONCTOR(Constant_domain.CONSTANTDOMAIN)(Disjoint_domain.DOMAIN_DISJOINT(Constant_domain.CONSTANTDOMAIN))
module Iter_Concrete_Disjoint = ITERATOR_FONCTOR(Concrete_domain.CONCRETE_DOMAIN)(Disjoint_domain.DOMAIN_DISJOINT(Concrete_domain.CONCRETE_DOMAIN))
module Iter_Sign_Disjoint = ITERATOR_FONCTOR(Sign_domain.SIGN_DOMAIN)(Disjoint_domain.DOMAIN_DISJOINT(Sign_domain.SIGN_DOMAIN))


(* parse filename *)
let doit filename =
  let prog = File_parser.parse_file filename in
  let cfg = Tree_to_cfg.prog prog in
  if !Options.verbose then
    Format.printf "%a" Cfg_printer.print_cfg cfg;
  Cfg_printer.output_dot !Options.cfg_out cfg;

  match !Options.domain with
    | "constant" | "constants" -> Iter_Constant.iterate filename cfg !Options.backward
    | "concrete" -> Iter_Concrete.iterate filename cfg !Options.backward
    | "interval" -> Iter_Interval.iterate filename cfg !Options.backward
    | "sign" -> Iter_Sign.iterate filename cfg !Options.backward 
    | "congruence" -> Iter_Congruence.iterate filename cfg !Options.backward
    | "product" -> Iter_Product.iterate filename cfg !Options.backward
    | "disjoint" -> Iter_Interval_Disjoint.iterate filename cfg !Options.backward
    | "disjoint_congruence" -> Iter_Congruence_Disjoint.iterate filename cfg !Options.backward
    | "disjoint_concrete" -> Iter_Concrete_Disjoint.iterate filename cfg !Options.backward
    | "disjoint_sign" -> Iter_Sign_Disjoint.iterate filename cfg !Options.backward
    | "disjoint_constant" -> Iter_Constant_Disjoint.iterate filename cfg !Options.backward
    | "disjoint_product" -> Iter_Product_Disjoint.iterate filename cfg !Options.backward
    | "" ->          Iter_Interval.iterate filename cfg !Options.backward
    | other -> failwith ("unknow domain"^other)


(* parses arguments to get filename *)
let main () =
  let _ = Options.init () in
  doit !Options.file

let _ = main ()
