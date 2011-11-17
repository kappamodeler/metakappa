(** 
 * pretty_printing.ml 
 * Meta language for Kappa 
 * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
 * Russ Harmer PPS (CNRS)
 * 
 * Creation: June, the 6th of 2009
 * Last Moification: June, the 6th of 2009
 * 
 * Backend: prtty printing functions
 * 
 * Copyright 2009,2010,2011 Institut National de Recherche en Informatique et   
 * en Automatique.  All rights reserved.  This file is distributed     
 * under the terms of the GNU Library General Public License *)

open Data_structures_metakappa

let print_agent log a = 
  let print_string s = Printf.fprintf log "%s" s in
  print_string a.agent_name;
  print_string "(";
  let _ = 
    List.fold_left  
      (fun bool s -> 
	let _ = if bool then print_string "," in 
	let _ = print_string (fst s);print_string (snd s) in 
	true)
      false a.interface in 
  print_string ")"

let print_agent_list log l bool = 
  List.fold_left 
    (fun bool agent -> 
      let _ = if bool then Printf.fprintf log " , " in
      let _ = print_agent log agent in
      true)
    bool l 



let print_rule log rule = 
  let mapsite f = 
    List.map 
      (fun agent -> {agent with interface = List.map (fun (a,b) -> (a,f b)) agent.interface}) in 
  let _ = if rule.flag <> [] then 
    let _ = Printf.fprintf log "\'" in
    let _ = List.iter (Printf.fprintf log "%s") (List.rev rule.flag) in
    let _ = Printf.fprintf log "\' " in ()
  in 
  let commonl = mapsite fst rule.hand_side_common in 
  let commonr = mapsite snd rule.hand_side_common in 
  let bool = print_agent_list log commonl false in 
  let bool = print_agent_list log rule.mod_left_hand_side bool in 
  let _  = print_agent_list log rule.fixed_left_hand_side bool in 
  let _ = Printf.fprintf log " %s " rule.sign in 
  let bool = print_agent_list log commonr false in 
  let bool = print_agent_list log rule.mod_right_hand_side bool in 
  let _ = print_agent_list log rule.fixed_right_hand_side bool in 
  let _ = Printf.fprintf log " %s" rule.rule_annotation in 
  () 

let print_model output model log = 
  let _ = Printf.fprintf output "%s" Config_metakappa.head in 
  let _ = 
    List.iter 
      (fun line -> 
	 match line with 
	     INIT_L (_,x,_) 
	   | DONT_CARE_L (x,_) -> Printf.fprintf output "%s" x 
	   | GEN_L ((_,_,_,_,s),_) -> Printf.fprintf output "%s" s
	   | CONC_L ((_,_,_,_,s),_) -> Printf.fprintf output "%s" s
	   | RULE_L _ -> ()
	   | COMMENTED_RULE_L (y,_) -> Printf.fprintf output "#";print_rule output y 
	   | PREPROCESSED_RULE (_,y,_) -> print_rule output  y
	   |	OBS_L (x,_,_) -> 
		  let _ = Printf.fprintf output "%sobs: '" "%"in 
                  let _ = List.iter (Printf.fprintf output "%s") (List.rev x) in
		  let _ = Printf.fprintf output "'\n" in ()
	   |	STORY_L (x,_,_) ->
		  let _ = Printf.fprintf output "%sstory: '" "%"in 
                  let _ = List.iter (Printf.fprintf output "%s") (List.rev x) in
		  let _ = Printf.fprintf output "'\n" in ()
      )
      model
  in log 
