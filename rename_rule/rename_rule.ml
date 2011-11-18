(** 
 * rename_rule.ml 
 * Meta language for Kappa 
 * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
 * Russ Harmer PPS (CNRS)
 * 
 * Creation: June, the 6th of 2009
 * Last Moification: November, the 18th of 2011
 * 
 * To apply substitution to rules 
 * 
 * Copyright 2009,2010,2011 Institut National de Recherche en Informatique et   
 * en Automatique.  All rights reserved.  This file is distributed     
 * under the terms of the GNU Library General Public License *)

open Data_structures_metakappa
open Rename_agent 

let add_decl a l = a::l 

let declare_full agent interface_database i =
  let interface = 
    List.fold_left 
      (fun set (a,_) -> SiteSet.add a set)
      SiteSet.empty
      agent.interface 
  in 
  try 
    let (int,line) = 
      AgentMap.find agent.agent_name interface_database.concrete_names
    in 
    match int with None -> raise Not_found 
    | Some int -> 
	if SiteSet.equal interface int 
	then 
	  Some  {interface_database 
		with 
		  concrete_names = 
		  AgentMap.add 
		    agent.agent_name 
		    (Some interface,add_decl i  line) 
		    interface_database.concrete_names}
	else 
	  failwith 
	    (List.fold_left 
	       (fun string i -> string^(string_of_int i)^";")
	       ("Incompatible interfaces for agent "^(agent.agent_name)^" between line "^(string_of_line i)^";")
	       line)
  with 
    Not_found -> 
      Some {interface_database 
	       with 
	     concrete_names = AgentMap.add agent.agent_name (Some interface,add_decl i []) interface_database.concrete_names}

let hd_of_flag rule = 
  if rule.flag<>[] then List.hd (List.rev rule.flag) else ""
let check_rule rule interface_database i log = 
  let f = 
    List.fold_left 
      (fun database ag -> 
	 match declare_full ag database i
	with None -> failwith ("Problem with rule "^(hd_of_flag rule)^" at line "^(string_of_int i))
	| Some a -> a) 
  in
  let g1 = 
    List.fold_left 
      (fun database ag -> 
	 let set = AgentSet.add ag.agent_name database.agents in 
	   if AgentSet.equal set database.agents 
	   then 
	     database
	   else
	     {database with agents = set}
      ) 
  in 
  let g2  = 
    List.fold_left 
      (fun database ag -> 
	 let set = AgentSet.add ag.agent_name database.agents in 
	   if AgentSet.equal set database.agents 
	   then 
	     database
	   else
	     {database with agents = set}
      ) 
  in 
  g2 
    (g2 
       (g1 
	  (f 
	     (f 
		interface_database 
		rule.fixed_left_hand_side)
	     rule.fixed_right_hand_side)
	  rule.hand_side_common)
       rule.mod_left_hand_side)
    rule.mod_right_hand_side,log  
    

let rename_rule rule interface_database flagmap (b1,b2) output log = 
  let rename l log = 
    let a,b = 
      List.fold_left 
	(fun (prefix_list,log) a -> 
	   let sol,log = rename_agent a interface_database log  in 
	     (List.fold_left 
		(fun a (kappa_prefix,tag_prefix) -> 
		 List.fold_left 
		   (fun a (tag_suffix,lineage,kappa_suffix)  -> 
		      let lineage,bool = string_of_lineage lineage in 
                        if bool 
                        then 
                          (kappa_suffix::kappa_prefix,["%",lineage]::(tag_suffix::tag_prefix))::a
                        else
                          (kappa_suffix::kappa_prefix,tag_prefix)::a
                   )
		   a 
		   sol)
		[] 
	      prefix_list,log))
	([[],[]],log) 
	l 
    in a,b
  in 
  let check l = List.for_all (Rename_agent.check_agent) l in 
  let hand_side_common',log = rename rule.hand_side_common log in 
  let mod_left_hand_side',log = rename rule.mod_left_hand_side log in 
  let mod_right_hand_side',log = rename rule.mod_right_hand_side log in
  let rule_list = 
    if check rule.fixed_left_hand_side && check rule.fixed_right_hand_side 
    then 
      List.fold_left 
	(fun liste (hs,hs') -> 
	   List.fold_left 
	     (fun liste (left,left') -> 
		List.fold_left 
		  (fun (liste,flags) (right,right') -> 
		     let add_flag = 
		       List.fold_left 
			 (List.fold_left (fun flag (a,b)-> 
					    if a = "%"
					    then 
					      if b="" then flag 
					      else 
						b::"."::flag
					    else
					      a::"/"::"b"::"."::flag))
		     in 
		     let flag' = add_flag (add_flag (add_flag rule.flag hs') left') right' in 
		     let rule' = 
		       {rule with 
			  flag = flag' ;
			  hand_side_common = List.rev hs ; 
			  mod_left_hand_side = List.rev left ;
			  mod_right_hand_side = List.rev right } 
		     in
		     let _ = Pretty_printing.print_rule output rule' in 
		     let _ = 
		    if b1
		    then 
		      let _ = Printf.fprintf output "%sobs: '" "%"in 
		      let _ = List.iter (Printf.fprintf output "%s") (List.rev flag') in
		      let _ = Printf.fprintf output "'\n" in ()
		  in 
		  let _ = 
		    if b2
		    then 
		      let _ = Printf.fprintf output "%sstory: '" "%"in 
                      let _ = List.iter (Printf.fprintf output "%s") (List.rev flag') in
		      let _ = Printf.fprintf output "'\n" in ()
		  in (liste,flags))
		  liste 
		  mod_right_hand_side')
	     liste 
	     mod_left_hand_side')
	([],flagmap) 
	hand_side_common',log
    else 
      ([],flagmap),log
  in 
  rule_list

let print_agent_list log l bool = 
  List.fold_left 
    (fun bool agent -> 
      let _ = if bool then Printf.fprintf log " , " in
      let _ = Pretty_printing.print_agent log agent in
      true)
    bool l 


let check_model line (interface_database:declaration) log = 
  match line with 
    INIT_L _   
  | OBS_L _
  | STORY_L _ 
  | DONT_CARE_L _ 
  | GEN_L _ 
  | COMMENTED_RULE_L _ 
  | CONC_L _ -> interface_database,log
  | RULE_L _ -> failwith "INTERNAL ERROR"
  | PREPROCESSED_RULE (_,y,i) -> check_rule y interface_database i log

let rename_obs rule flagset log = 
  let get_old s flagset = 
    try 
      StringListMap.find s flagset 
    with 
	Not_found -> []
  in 
  let add_old s line flagset = 
    let old = get_old s flagset in 
      StringListMap.add s (line::old) flagset
  in 
    match rule with 
	INIT_L _ | DONT_CARE_L _ | GEN_L _ | CONC_L _ | RULE_L _ | COMMENTED_RULE_L _ | PREPROCESSED_RULE _ -> flagset,log 
      | OBS_L (s,a,i) | STORY_L(s,a,i) ->
	  add_old s rule flagset,log
