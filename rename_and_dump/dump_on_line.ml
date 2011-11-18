(** 
 * dump_on_line.ml 
 * Meta language for Kappa 
 * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
 * Russ Harmer PPS (CNRS)
 * 
 * Creation: Nov, the 17th of 2011
 * Last Moification: Nov, the 18th of 2011 
 * 
 * Expand rules and dump the model in the same time
 * 
 * Copyright 2009,2010,2011 Institut National de Recherche en Informatique et   
 * en Automatique.  All rights reserved.  This file is distributed     
 * under the terms of the GNU Library General Public License *)

open Data_structures_metakappa
open Pretty_printing

let rename_and_dump output model subs flagset log = 
   let _ = Printf.fprintf output "%s" Config_metakappa.head in 
   let _ = 
     List.fold_left 
       (fun log line -> 
	  match line with 
	      INIT_L (_,x,_) 
	    | DONT_CARE_L (x,_) -> 
		let _ = Printf.fprintf output "%s" x in log
	    | GEN_L ((_,_,_,_,s),_) -> 
		let _ = Printf.fprintf output "%s" s in log
	    | CONC_L ((_,_,_,_,s),_) -> 
		let _ = Printf.fprintf output "%s" s in log
	    | RULE_L x -> log
	    | COMMENTED_RULE_L (y,_) -> 
		let _ = Printf.fprintf output "#";print_rule output y in log
	    | PREPROCESSED_RULE (x,rule,i) -> 
		begin
		  let _ = Printf.fprintf output "#";print_rule output rule in 
		  let list = 
		    try
		      StringListMap.find rule.flag flagset 
		    with 
			Not_found -> []
		  in  
		  let rec vide l (log,b1,b2) = 
		      match l 
		      with [] -> (log,b1,b2)
			| t::q -> 
			    match t 
			    with 
			      | OBS_L _ ->
				  if b1 then vide q (log,b1,b2)
				  else 
				    if b2 then (log,true,b2)
				    else vide q (log,true,b2)
			      | STORY_L _ ->
				  if b2 then vide q (log,b1,b2)
				  else
				    if b1 then (log,b1,true)
				    else vide q (log,b1,true)
			      | _ -> vide q (log,b1,b2)
		  in 
		  let log,b1,b2 = vide list (log,false,false) in
		  let _,log = Rename_rule.rename_rule rule subs flagset (b1,b2) output log in 
		    log
		end 
	    |	OBS_L (x,_,_) -> 
		  let _ = Printf.fprintf output "#%sobs: '" "%"in 
                  let _ = List.iter (Printf.fprintf output "%s") (List.rev x) in
		  let _ = Printf.fprintf output "'\n" in log
	    |	STORY_L (x,_,_) ->
		  let _ = Printf.fprintf output "#%sstory: '" "%"in 
                  let _ = List.iter (Printf.fprintf output "%s") (List.rev x) in
		  let _ = Printf.fprintf output "'\n" in log
      )
       log model
  in log 
