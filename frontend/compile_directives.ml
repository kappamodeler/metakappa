(* 2009/06/20*)
(* Meta language for Kappa *)
(* Jerome Feret LIENS (INRIA/ENS/CNRS) & Russ Harmer PPS (CNRS)*)
(* Academic uses only *)
(* Translation of agent variant instructions *)
(* compile_directives.ml *)

open Data_structures_metakappa

let trace = false
let trace_print = if trace then print_string else (fun _ ->())

let get_instr l = 
  List.fold_left 
    (fun liste a ->
      Agent_interfaces.convert_action a liste)
    [] 
    l

let get_interface parsed_agent = 
  match parsed_agent with 
    (a,b) -> (a,List.fold_left (fun set (a,_) -> SiteSet.add a set) SiteSet.empty b)
      
let add_concrete a b subs i = 
  try 
    let x,y=AgentMap.find a subs.concrete_names in 
    if SiteSet.equal 
	(match x
	with Some a -> a | _ -> raise Not_found)
	b 
    then {subs with concrete_names = AgentMap.add a (x,i::y) subs.concrete_names}
    else
      failwith "add_concrete"
  with 
    Not_found -> 
      {subs with concrete_names = AgentMap.add a (Some b,[i]) subs.concrete_names}

let add_gen (a,b,c,d,_) subs i = 
  let agent = 
    match a,b with _,Some e -> e
    | Some ((a,_),_),_-> a 
    | None,None -> raise Exit 
  in
  let old = 
    try 
      AgentMap.find agent subs.definitions 
    with 
	Not_found -> [] 
  in 
  let def = 
    match (a,b,c,d) with 
	Some ((a,b),_),_,_,_ -> 
	  let _ = trace_print "root\n" in
	    Root(List.fold_left (fun sol (a,_) -> SiteSet.add a sol) SiteSet.empty b),Some i
      | _,_,Some agent',list -> 
	  let _ = trace_print "variant\n" in 
	    Variant(agent',get_instr list),Some i 
      | _ -> raise Exit 
  in
    {subs with definitions = 
	AgentMap.add agent (def::old)  subs.definitions}
	    
let add_conc (a,b,c,d,e) subs i = 
  let subs = add_gen (a,b,c,d,e) subs i in 
  match a,b with 
    Some ((a,b),_),_ -> 
      begin
	let interface = List.fold_left (fun sol (a,_) -> SiteSet.add a sol) SiteSet.empty b in 
	let _ = trace_print "CONC\n" in 
	add_concrete a interface subs i 
      end
  | _, Some e -> 
      begin
	try 
	  let _ = AgentMap.find e subs.concrete_names in subs 
	with 
	  Not_found -> 
	    let _ = trace_print "NEWCONC\n" in 
	  {subs with concrete_names = AgentMap.add e (None,[]) subs.concrete_names}
      end
  | _ -> raise Exit 
 
let convert lines log = 
  List.fold_left 
    (fun 
       (sol,log)
       x 
       -> 
	match x 
	with 
	  INIT_L (a,_,i) -> 
	    List.fold_left 
	      (fun sol a -> 
		let ag,site = get_interface a in 
		add_concrete ag site sol i)
	      sol a , log 
	| DONT_CARE_L _ -> sol , log
	| GEN_L (p,i) -> add_gen p sol i ,log 
	| CONC_L (p,i) -> add_conc p sol i ,log
	| RULE_L _ -> sol ,log
	| PREPROCESSED_RULE _ -> sol,log
	| COMMENTED_RULE_L _ | OBS_L _ | STORY_L _ -> sol,log )
    ({ concrete_names=AgentMap.empty;
      definitions=AgentMap.empty;
      agents=AgentSet.empty},log)
    lines 

