(* 17/02/2009 *)
(* Meta language for Kappa systems *)
(* Jerome Feret*)

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
let (rules,flags),log  = 
  List.fold_left 
    (fun (model,log) rule -> 
      Rename_rule.transform_model 
	rule
	subs
	model 
        log )
    (([],StringMap.empty),log)
    rules 
let rules,log = 
  List.fold_left 
    (fun (model,log) rule -> Rename_rule.rename_obs rule flags model log)
    ([],log)
    rules
let log = Pretty_printing.print_model stdout rules log
let _ = dump_computation_steps stderr log 
let _ = dump_messages stderr log 



