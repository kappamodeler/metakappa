(* 2009/06/20*)
(* Meta language for Kappa *)
(* Jerome Feret LIENS (INRIA/ENS/CNRS) & Russ Harmer PPS (CNRS)*)
(* Academic uses only *)
(* Exception definition *)
(* error.ml *)


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

