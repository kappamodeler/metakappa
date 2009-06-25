(* 2009/01/08*)
(* Meta language for Kappa *)
(* Jerome Feret*)
(* Agent interface*)
(* agent_interfaces.ml *)

open Error_handler 
open Data_structures_metakappa


let error i = 
  unsafe_frozen None (Some "agent_interfaces.ml") None (Some ("line "^(string_of_int i))) (fun () -> (failwith ("ERROR"^(string_of_int i)))) 



let convert_action action res = 
  match action with 
    Add_site _ | Rename _ -> action::res
  | Delete_site site -> (Rename (site,[]))::res
  | Mutate_site (site1,site2) -> (Rename (site1,[]))::(Add_site site2)::res

let compute_interface starting_interface directives log = 
  let map = 
    SiteSet.fold
      (fun (x:site) map  -> SiteMap.add x [x] map)
      starting_interface 
      SiteMap.empty 
  in
  let map,newsites,sources,targets,bool,log = 
    List.fold_left 
      (fun (map,newsites,sources,targets,bool,log) d -> 
	match d with 
	  Add_site (site) -> 
	    (map,SiteSet.add site newsites,sources,SiteSet.add site targets,bool,log)
	| Rename (site,l) -> 
	    if 
	      SiteSet.mem site sources 
	    then 
	      let mess = "Site "^site^" occurs several time as a modified site" in
		match !Config_metakappa.tolerancy with 
		    "0" -> failwith mess 
		  | "1" -> map,newsites,sources,targets,true,add_message mess (!Config_metakappa.log_warning) log
		  | "2" -> map,newsites,sources,targets,bool,add_message mess (!Config_metakappa.log_warning) log
	    else if 
	      try 
		let _ = 
		  SiteMap.find site map
		in false
	      with 
		Not_found -> true 
	    then 
	      let mess = "Site "^site^" is not defined" in
		match 
		  !Config_metakappa.tolerancy with 
		      "0" -> failwith mess
		    | "1" -> map,newsites,sources,targets,true,add_message mess (!Config_metakappa.log_warning) log
		    | "2" -> map,newsites,sources,targets,bool,add_message mess (!Config_metakappa.log_warning) log
	    else 
	      SiteMap.add site l map,
              newsites,
	      SiteSet.add site sources,
	      List.fold_left
		(fun targets site -> SiteSet.add site targets)
		targets l,bool,log
	| _ -> error 65)
      (map,SiteSet.empty,SiteSet.empty,SiteSet.empty,false,log) 
      directives
  in 
    if bool then 
      (None,log)
    else 
      Some (map,newsites),log

let compute_interface_portion  starting_interface directives log = 
  let map = 
    List.fold_left 
      (fun map x -> SiteMap.add x [x] map)
      SiteMap.empty 
      starting_interface 
  in
  let map,newsites,sources,targets,proper_targets,bool,log = 
    List.fold_left 
      (fun (map,newsites,sources,targets,proper_targets,bool,log) d -> 
	match d with 
	  Add_site (site) -> 
	      (map,
	       SiteSet.add site newsites,
               sources,
	       SiteSet.add site targets,
	       proper_targets,
	       bool,
	       log )
	| Rename (site,l) -> 
	    if 
	      SiteSet.mem site sources 
	    then 
	      failwith ("Site "^site^" occurs several time as a modified site")
	    else if 
	      try 
		let _ = 
		  SiteMap.find site map
		in false
	      with 
		Not_found -> true 
	    then 
	      let mess = "Site "^site^" is not defined" in
		match 
		  !Config_metakappa.tolerancy 
		with 
		      "0" -> failwith mess
		    | "1" -> map,newsites,sources,targets,proper_targets,true,log
		    | "2" -> map,newsites,sources,targets,proper_targets,bool,log

	    else 
	      let targets,proper_targets = 
		List.fold_left
		(fun (targets,proper_targets)  site -> SiteSet.add site targets,SiteSet.add site proper_targets)
		(targets,proper_targets) l
	      in
		(SiteMap.add site l map ,
		 newsites,
		 SiteSet.add site sources,
		 targets,
		 proper_targets,
		 bool,
		 log)
      
		  
	| _ -> error 34)
      (map,
       SiteSet.empty,
       SiteSet.empty,
       SiteSet.empty,
       SiteSet.empty,
       false,
       log) 
      directives
  in 
  if bool 
  then 
    None,log
  else 
    Some (map,SiteSet.diff newsites targets),log 


let abstract map = 
  (fun x -> 
    try 
      SiteMap.find x map
    with 
      Not_found -> error 76)
  
let compute_subs (starting_subs) directives log =
  let rep,bool,log = 
    SiteMap.fold
      (fun site l (map,bool,log) -> 
	 match compute_interface_portion l directives log 
	 with 
	     None,log -> (map,true,log)
	   | Some (a,b),log'-> 
	       SiteMap.add 
	       site 
		 (SiteMap.fold 
		    (fun l l2 sol -> 
		       List.fold_left 
			 (fun q a -> a::q)
			 (if SiteSet.mem l b then l::sol else sol) 
		    l2)
		    a
		    [])
		 map,
	       bool,
	       log)
      starting_subs
      (SiteMap.empty,false,log)
  in 
    if bool 
    then 
      None,log
    else
      Some rep,log 
