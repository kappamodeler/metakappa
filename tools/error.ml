(** 
 * error.ml 
 * Meta language for Kappa 
 * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
 * Russ Harmer PPS (CNRS)
 * 
 * Creation: June, the 6th of 2009
 * Last Moification: June, the 6th of 2009
 * 
 * Exception definitions
 * 
 * Copyright 2009,2010,2011 Institut National de Recherche en Informatique et   
 * en Automatique.  All rights reserved.  This file is distributed     
 * under the terms of the GNU Library General Public License *)


exception Syntax of (string * int)
exception Runtime of string
exception Runtime2 of string
exception Found of string
exception Too_expensive 
exception Not_handled_yet of string 


let store_error_info ((file_name:string option),(line:int option),(message:string option)) = 
  let _ = Error_handler_common.application_name := (Some "MetaKappa") in 
  let _ = Error_handler_common.file_name := file_name in 
  let _ = 
    Error_handler_common.ind := 
      (match line 
      with None -> None
      | Some i -> Some ("line "^(string_of_int i))) in
  let _ = 
    Error_handler_common.message:=message in 
  ()


let runtime context s = 
  let _ = store_error_info context in 
  raise (Runtime s)

let syntax context (s,l) = 
  let _ = store_error_info context in 
  raise (Syntax (s,l))

let warning s = Printf.printf "WARNING: %s\n" s ; flush stdout

let found context s = 
  let _ = store_error_info context in 
  raise (Found s)

