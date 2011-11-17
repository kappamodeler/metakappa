(** 
 * config_metakappa.ml 
 * Meta language for Kappa 
 * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
 * Russ Harmer PPS (CNRS)
 * 
 * Creation: June, the 6th of 2009
 * Last Moification: June, the 6th of 2009
 * 
 * Config file
 * 
 * Copyright 2009,2010,2011 Institut National de Recherche en Informatique et   
 * en Automatique.  All rights reserved.  This file is distributed     
 * under the terms of the GNU Library General Public License *)



open Superarg
open SuperargTk

let ad2 d = 
  if d>= 0 && d<=9 then "0"^(string_of_int d)
  else string_of_int d
    
let time_stamp = 
  let tm = Unix.localtime (Unix.time ()) 
  in
  Printf.sprintf "%s/%s/%d (%s:%s:%s)" 
    (ad2 (tm.Unix.tm_mon+1)) 
    (ad2 tm.Unix.tm_mday) 
    (tm.Unix.tm_year + 1900)
    (ad2 tm.Unix.tm_hour) 
    (ad2 tm.Unix.tm_min) 
    (ad2 tm.Unix.tm_sec)

let version = "1.3.."^(string_of_int Svn_number.svn_number) 
let date = "2011.11.17"
let input_marshalling = ref "" 
let input_file = ref [""] 
let input_focus_on = ref ""
let refine_fic = ref ""
let key = ref "0000000000000000" 


(* Trace *)
let dump_chrono = ref true 
let dump_version = ref false 
let trace=ref false (* debug *)
let unsafe_mode=ref false (*debug *)
let memory_limit = ref 0 

(* Setting *)
let log_warning = ref true 
let tolerancy = ref "1"
    (* 0 -> crash whenever a conflict is detected in definitions *)
    (* 1 -> do not generate variants with conflict *)
    (* 2 -> ignore the instructions that generate a conflict *)

let output_file = ref ""
let keep_comments = ref true 
let metaname   = "MetaKappa "^version

let sepname = "\n"
let headline = ["This file has been automatically computed by "^metaname]

let head = "################################################################################\n"^(List.fold_right (fun a b -> a^"\n"^b) headline "")^
"MataKappa is an academic prototype for academic uses only.\nJérôme Feret LIENS (INRIA & ENS) and Russ Harmer PPS (CNRS)\n################################################################################\n"

let foot = head

let options = List.rev

[
(*2_Output*)
"--truc2",Void,"help",["Options"],Normal;
"--output",String output_file,"where to dump the result",["Options"],Normal;

(*"--memory-limit",Int memory_limit,"Limit the usage of the memory in (Mb)",["2_Memory usage"],Normal;*)

(*Debug *)
   
   "--trace",Bool trace,"to dump debuging information",["Options"],Normal;
(*   "--version",Bool dump_version,"to dump version number",["Options"],Normal;*)

(*Setting*)
   "--warn",Bool log_warning,"log warning messages, or not",["Options"],Normal;
   "--bug-tolerancy",
  Choice(["0","Crash on conflict between intstructions";
	  "1","Ignore variants with conflicting instructions";
	  "2","Ignore conflicting instructions (but warn)"]
	  ,tolerancy),
  "level of bug-taulerancy",
  ["Options"],Normal;

(*key *)

(*"--key",String key,"security key",["Reachability analysis"],Hidden*)


    ] 

