(*
  Cours "Sémantique et Application à la Vérification de programmes"

  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

open Frontend
open Iterator
open Domains

module Iter_Constant = ITERATOR_FONCTOR(Constant_domain.CONSTANTDOMAIN)
module Iter_Concrete = ITERATOR_FONCTOR(Concrete_domain.CONCRETE_DOMAIN)

(* parse filename *)
let doit filename =
  let prog = File_parser.parse_file filename in
  let cfg = Tree_to_cfg.prog prog in
  if !Options.verbose then
    Format.printf "%a" Cfg_printer.print_cfg cfg;
  Cfg_printer.output_dot !Options.cfg_out cfg;

  match !Options.domain with
    | "constant" -> Iter_Constant.iterate filename cfg
    | "concrete" -> Iter_Concrete.iterate filename cfg
    | _ ->          Iter_Constant.iterate filename cfg


(* parses arguments to get filename *)
let main () =
  let _ = Options.init () in
  doit !Options.file

let _ = main ()
