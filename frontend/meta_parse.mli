type token =
  | INIT_LINE
  | OBS_LINE
  | STORY_LINE
  | NEWLINE
  | MODIF_LINE
  | GEN_LINE
  | CONC_LINE
  | BEGIN_MAC_LINE
  | END_MAC_LINE
  | EXPAND_MAC_LINE
  | EOF
  | MULT
  | DIVIDE
  | ANTISLASH
  | PLUS
  | MINUS
  | COMMA
  | SEMICOLON
  | GREATER
  | SMALLER
  | SET
  | EQUAL
  | INFINITY
  | SEP
  | INSTANCE
  | DO
  | AT
  | TIME
  | KAPPA_LNK
  | KAPPA_WLD
  | KAPPA_SEMI
  | KAPPA_LRAR
  | KAPPA_RAR
  | OP_PAR
  | CL_PAR
  | OP_CONC
  | CL_CONC
  | OP_ACC
  | CL_ACC
  | INT of (int)
  | REF of (int)
  | FLOAT of (float)
  | ID of (string)
  | IS of (string)
  | KAPPA_MRK
  | LABEL of (string)
  | COMMENT of (string)

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Data_structures_metakappa.pp_parse list
