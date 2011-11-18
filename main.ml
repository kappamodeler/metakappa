(** 
 * main.ml 
 * Meta language for Kappa 
 * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
 * Russ Harmer PPS (CNRS)
 * 
 * Creation: Feb, the 17th of 2009
 * Last Moification: November, the 17th of 2012
 * 
 * To apply substitution to rules 
 * 
 * Copyright 2009,2010,2011 Institut National de Recherche en Informatique et   
 * en Automatique.  All rights reserved.  This file is distributed     
 * under the terms of the GNU Library General Public License *)


open Data_structures_metakappa
open Rename_agent 
open Lexing 
open Meta_lex

let log = empty_log 
let trace () = !Config_metakappa.trace 

let compile fic log =
    let d = open_in fic in
    let lexbuf = Lexing.from_channel d in
    let rep = Meta_parse.main token lexbuf in 
    rep,log  
    

let file = ref [] 
let _ = SuperargTk.parse Config_metakappa.options file  
let r,log  = compile (List.hd (!file)) log
let (r,def),log = Macro_processing.collect_def r log
let _ = if trace () then Macro_processing.print_def print_handler_error def 
let r,log  = Macro_processing.macro_expanse []  def fst ""  r [] log  
let decl,log  = Compile_directives.convert r log
let rules,log = List.map Compile_rule.convert r,log 
let decl,log = 
  List.fold_left 
    (fun (decl,log) x -> 
      Rename_rule.check_model 
	x 
	decl 
	log)
    (decl,log) 
    rules 
let decl,log = Agent_tree.complete decl log
let subs,log = Agent_tree.convert_declaration_into_solved_definition decl log
let _ = if trace () then Agent_tree.print_macro_tree print_handler_error subs 
let flags,log = 
  List.fold_left 
    (fun (flags,log) rule -> 
       Rename_rule.rename_obs 
	 rule 
	 flags  log)
    (Data_structures_metakappa.StringListMap.empty,log)
    rules  
let _ = Dump_on_line.rename_and_dump 
  stdout 
  rules 
  subs 
  flags 
  ()
let _ = dump_computation_steps stderr log 
let _ = dump_messages stderr log 



