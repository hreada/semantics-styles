open Syntax;;
open State;;

type cfg =
  | AExpCfg of aexp * state
  | IntCfg of int
  | BExpCfg of bexp * state
  | BoolCfg of bool
  | StmtCfg of stmt * state
  | StateCfg of state
  | PgmCfg of pgm
;;

let eval cfg = cfg
;;
