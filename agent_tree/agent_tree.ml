(* 2009/06/20*)
(* Meta language for Kappa *)
(* Jerome Feret LIENS (INRIA/ENS/CNRS) & Russ Harmer PPS (CNRS)*)
(* Academic uses only *)
(* Agent definition dag definition *)
(* agent_tree.ml *)

open Error_handler 
open Data_structures_metakappa

let trace = false (* set to true to log all computation steps in this module *)

(* report an error at line i*)
let error i = 
  let _ = print_string (string_of_int i) in 
  let _ = print_newline () in 
  unsafe_frozen None (Some "agent_interfaces.ml") None (Some ("line "^(string_of_int i))) (fun () -> failwith ("error"^(string_of_int i)))

(* convert agent variant definition into *)
let convert_declaration_into_solved_definition  x  = 
  let add_count agent k map = 
    let old = 
      try 
	AgentMap.find agent map
      with 
	Not_found -> 
	  0 
    in 
    let k' = k + old in 
    if k'=0 
    then 
      (AgentMap.remove agent map,0)
    else
      (AgentMap.add agent k' map,k')
  in
  (* we compute the set of agent names *)
  let agentset = 
    AgentMap.fold
      (fun a _ -> AgentSet.add a)
      x.concrete_names
      (AgentMap.fold 
	 (fun a _ -> AgentSet.add a)
	 x.definitions
	 AgentSet.empty)
  in
  let fadd_list a b map = 
    let old =
      try 
	AgentMap.find a map 
      with 
	Not_found -> 
	  [] 
    in
    AgentMap.add a (b::old) map 
  in
  let succ,        (*map between each agent A and variants of A *)
    npred,         (*map between each agent and the number of its predecessors *)
    nsucc,         (*map between each agent and the number of successors *)
    interface_map (*map each agent to the list of interface when defined as a root *)
    = 
    AgentMap.fold 
      (fun agent_target list (succ,npred,nsucc,interface_map) -> 
	 List.fold_left 
	   (fun (succ,npred,nsucc,interface_map) (def,line) -> 
	      match def 
	      with Root l -> (succ,npred,nsucc,fadd_list agent_target l interface_map)
		| Unspecified -> (succ,npred,nsucc,fadd_list agent_target SiteSet.empty interface_map)
		| Variant (agent_source,d) -> 
	    	    if AgentSet.mem agent_source agentset 
		    then 
		      (fadd_list agent_source (agent_target,d) succ,
		       fst (add_count agent_target 1 npred),
		       fst (add_count agent_source 1 nsucc),
		       interface_map)
		    else
		      failwith 
			(
			  (match line with None -> "" | Some i -> (string_of_line i)^": ")
			  ^agent_target^" is introduced as a variant of "^agent_source^" that is not defined"
			)
	   )
	   (succ,npred,nsucc,interface_map) 
	   list)
      x.definitions 
      (AgentMap.empty,AgentMap.empty,AgentMap.empty,AgentMap.empty) in 

  let deal_with agent (working_list,set,npred) = 
    let set' = AgentSet.add agent (fst set)in 
    let succs,set'  = 
      if (fst set)==set' 
      then 
	[],set
      else 
	let set' = (set',agent::(snd set)) in 
	try
	  AgentMap.find agent succ,set'
	with 
	    Not_found -> [],set'
    in
      List.fold_left 
	(fun (working_list,set,npred) (agent,_) ->  
	   let npred,n = add_count agent (-1) npred in 
	     (if n=0 
	      then 
		agent::working_list
	      else
		working_list),
	   set,
	   npred)
	(working_list,set',npred) 
	succs 
  in
  let roots = 
    AgentMap.fold2 
      (fun x y l -> l)
      (fun x y l -> x::l)
      (fun x y z l -> if y=0 then x::l else l)
      npred 
      interface_map
      []
  in 
  let working_list,set,npred = 
    List.fold_left 
      (fun state agent -> deal_with agent state)
      ([],(AgentSet.empty,[]),npred)
      roots 
  in
  let failwith s = 
    let _ = print_string s in 
    let _ = print_newline () in 
    failwith s in 
  let def_map = 
    AgentMap.map 
      (fun list -> 
	 List.map (fun int -> (int,int,[])) list)
      interface_map 
  in 
  let def_map,sorted_agent,npred  = 
    let rec aux def_map (working_list,set,npred) = 
      match working_list with 
	[] -> def_map,List.rev (snd set),npred  
      |	t::q -> 
	  let list = 
	    try 
	      AgentMap.find t x.definitions
	    with 
	      Not_found -> error 102 
	  in 
	  let def_map = 
	    List.fold_left 
	      (fun def_map def -> 
		 let agent,decl,line = 
		   match def with 
		       Variant(a,decl),line -> a,decl,line
		     | _ -> error 107 
		 in
		 let line = 
		   match line with None -> "" 
		     | Some i -> (string_of_line i) 
		 in
		 let old_defs = 
		   try 
		     AgentMap.find agent def_map 
		   with 
		       Not_found -> error 113 
		 in 
		   List.fold_left 
		     (fun def_map  (old_interface,_,list) -> 
			let subs,new_sites  = Agent_interfaces.compute_interface old_interface decl in 
			let new_interface = 
			  SiteSet.fold 
			    (fun s interface  -> 
			       List.fold_left 
				 (fun interface s -> 
				    if SiteSet.mem s interface
				    then 
				      failwith 
					(line^": site "^s^" is defined several time in variant "^t)
				    else 
				      SiteSet.add s interface)
				 interface
				 ((Agent_interfaces.abstract subs) s))
			    old_interface 
			    SiteSet.empty
			in
			let new_interface = 
			  SiteSet.union new_interface new_sites 
			in 
			  fadd_list 
			    t
			    (new_interface,new_sites,decl::list) 
			    def_map)
		     def_map 
		     old_defs)
	      def_map 
	      list 
	  in 
	    aux 
	      def_map 
	      (deal_with t (q,set,npred))
    in
      aux 
	def_map 
	(working_list,set,npred)
  in
  
  let _ = if trace then print_string "C8\n" in 

  let _ = 
    if AgentMap.is_empty npred 
    then 
      () 
    else 
      let _ = print_string "Cicular dependences\n" in 
      let _ = 
	AgentMap.iter 
	  (fun a b -> print_string a;print_int b;print_string ",")
	   npred
      in 
      let _ = print_newline () in 
      failwith "Circular dependencies"
  in 
  
  

  let solve agent (interface,new_sites,dlist) map = 
    let rec aux working_list sol = 
      match working_list with 
	(t,d,subs)::q -> 
	  let subs' = Agent_interfaces.compute_subs subs d in 
	  let sol' = (t,subs')::sol in 
	  let q' = 
	    List.fold_left 
	      (fun q (t,d)  -> (t,d,subs')::q)
	      q 
	      (try AgentMap.find t succ with Not_found -> []) 
	       in 
	  aux q' sol' 
      |	[] -> sol 
    in 
    let sol = 
      aux [agent,[],
	    SiteSet.fold 
	      (fun a -> SiteMap.add a [a])
	      interface
	      SiteMap.empty 
	  ] [] 
    in
    fadd_list
      agent 
      (List.map 
	 (fun (t,sol) -> 
	    let sol = 
	      SiteSet.fold 
		(fun site map -> 
		   try 
		     let _ = SiteMap.find site map in 
		       map
		   with 
		       Not_found -> SiteMap.add site [site] map) 
	        new_sites  sol 
	    in 
	    let forbid = 
	     SiteMap.fold 
	       (fun a b c -> 
		 if b = [] then SiteSet.add a c 
		 else c)
	       sol 
	       SiteSet.empty
	   in 
	   {target_name=t;
             forbidden_sites=forbid;
	    substitutions=sol})
	 
	 (List.filter 
	    (fun (t,sol) -> 
	      ((try 
		  let _ = AgentMap.find t succ in false 
		with Not_found -> true)))
		sol)) map in
(*  let _ = 
    if trace 
    then 
      let _ = print_string "INTERFACE \n\n" in 
      let _ = 
	AgentMap.iter 
	  (fun a b -> 
	    print_string a;
	    SiteSet.iter print_string b;
	    print_newline ())
	  interface in 
      () in *)
  let sol = 
    List.fold_left 
      (fun map a -> 
	 let l = 
	 try 
	   AgentMap.find a def_map 
	 with 
	     Not_found -> [] 
	 in 
	   List.fold_left 
	     (fun map d -> solve a d map)
	   map 
	     l)
      AgentMap.empty 
      sorted_agent 
  in 
    AgentMap.map 
      (fun l -> List.flatten l) 
      sol 

let print_macro_tree handler y  = 
  let _ = handler.string "MACRO TREE \n\n" in 
  let _ = 
    AgentMap.iter 
      (fun a b -> 
	handler.string a;
	handler.line ();
	List.iter 
	  (fun a -> 
	    handler.string a.target_name;
	    handler.string ",";
	    SiteSet.iter handler.string a.forbidden_sites;
	    handler.string ",";
	    SiteMap.iter 
	      (fun a b -> 
		handler.string a;
		handler.string "\\{";
		List.iter handler.string b;
		handler.string "\\}")
	      a.substitutions;
	    handler.line ())
	  b;
	handler.line ();
	handler.line ())
      y
  in ()

let convert_declaration_into_solved_definition x = 
  if trace 
  then 
    let _ = print_declaration print_handler x in
    let y = convert_declaration_into_solved_definition x in
(*    let _ = print_macro_tree print_handler y in*)
    y
  else
    convert_declaration_into_solved_definition x


let complete subs = 
  let subs = 
    AgentMap.fold 
      (fun a (b,c) subs ->
	match b with None -> subs
	| Some interface -> 
	    try let _ = AgentMap.find a subs.definitions in subs 
	    with Not_found -> 
	      {subs with 
		definitions = 
		  AgentMap.add a [Root interface,None] subs.definitions})
      subs.concrete_names 
      subs 
  in 
  let subs = 
    AgentSet.fold 
      (fun a subs -> 
	  try let _ = AgentMap.find a subs.definitions in subs 
	    with Not_found -> 
	      {subs with 
		definitions = 
		  AgentMap.add a [Unspecified,None] subs.definitions})
      subs.agents 
      subs 
  in subs 
	
