(* Big-Step Semantics of IMP++ *)

open List;;

(* Abstract Syntax Tree of IMP++ *)

type id =
  | Id of string
;;
type aexp =
  | IntAExp of int
  | IdAExp of id
  | PlusAExp of aexp * aexp
  | DivAExp of aexp * aexp
  (* IMP++ *)
  | IncAExp of id
  | ReadAExp
;;
type bexp =
  | BoolBExp of bool
  | LessThanBExp of aexp * aexp
  | NotBExp of bexp
  | AndBExp of bexp * bexp
;;
type block =
  | EmptyBlock
  | StmtBlock of stmt
and stmt =
  | BlockStmt of block
  | AssignStmt of id * aexp
  | SeqStmt of stmt * stmt
  | IfStmt of bexp * block * block
  | WhileStmt of bexp * block
  (* IMP++ *)
  | PrintStmt of aexp
  | HaltStmt
  | SpawnStmt of block
  | DeclarationStmt of (id list)
  (* An auxiliary construct used to define declaration statements *)
  | LetStmt of id * aexp * stmt
;;
(* IMP++ *)
(* We don't use pgm anymore. We only use stmt. *)
type pgm = stmt
;;

(* Reorder (s1 s2) s3 to s1 (s2 s3) *)

let rec reorder_seq stmt = 
  let (s, should_repeat) = reorder_seq_inner stmt in
  if should_repeat then reorder_seq s else s
and reorder_seq_inner stmt =  match stmt with
| BlockStmt (b) -> (BlockStmt (reorder_seq_in_block b), false)
| SeqStmt (SeqStmt (s1, s2), s) -> (SeqStmt (s1, SeqStmt (s2, s)), true)
| SeqStmt (s1, s) -> (SeqStmt (s1, reorder_seq s), false)
| IfStmt (cond, b1, b2) -> 
    let b1_reordered = reorder_seq_in_block b1 in
    let b2_reordered = reorder_seq_in_block b2 in
    (IfStmt (cond, b1_reordered, b2_reordered), false)
| WhileStmt (cond, b) -> 
    let b_reordered = reorder_seq_in_block b in
    (WhileStmt (cond, b_reordered), false)
| SpawnStmt (b) -> 
    let b_reordered = reorder_seq_in_block b in
    (SpawnStmt (b_reordered), false)
| _ -> (stmt, false)
and reorder_seq_in_block b = match b with
| EmptyBlock -> EmptyBlock
| StmtBlock (s) -> StmtBlock (reorder_seq s)
;;


(* A desugaring function that translates declarations to let-statements. *)

let rec construct_lets xl s = match xl with
| [] -> s
| (x::xs) -> LetStmt (x, IntAExp (0), construct_lets xs s)
;;

let rec desugar stmt = let stmt_reordered = reorder_seq stmt in
  match stmt_reordered with
| BlockStmt (b) -> BlockStmt (desugar_in_block b)
| SeqStmt (DeclarationStmt (xl), s) -> construct_lets xl s
| IfStmt (cond, b1, b2) -> 
    let b1_desugared = desugar_in_block b1 in
    let b2_desugared = desugar_in_block b2 in
    IfStmt (cond, b1_desugared, b2_desugared)
| WhileStmt (cond, b) -> 
    let b_desugared = desugar_in_block b in
    WhileStmt (cond, b_desugared)
| SpawnStmt (b) -> 
    let b_desugared = desugar_in_block b in
    SpawnStmt (b_desugared)
| _ -> stmt
and desugar_in_block b = match b with
| EmptyBlock -> EmptyBlock
| StmtBlock (s) -> StmtBlock (desugar s)
;;



(* Configurations *)

type state = (id * int) list
;;

let state_lookup sigma x =
  try Some(assoc x sigma) with
  | Not_found -> None
;;

let rec state_update sigma n x =
  match sigma with 
  | [] -> []
  | (y, m)::sigma' -> if x = y then (x, n)::sigma' else (y, m)::(state_update sigma' n x)
;;

let rec state_init xs n =
  match xs with
  | [] -> []
  | x::xs -> (x,n)::(state_init xs n)
;;

type buffer = int list
;;

type cfg =
  (* Non-termination configurations *)
  | AExpCfg of aexp * state * buffer
  | BExpCfg of aexp * state * buffer
  | StmtCfg of stmt * state * buffer
  (* Termination configurations *)
  | IntCfg of int * state * buffer
  | BoolCfg of bool * state * buffer
  | StateCfg of state * buffer * buffer (* (state,inputs,outputs) *)
  (* Error configurations *)
  | ErrCfg of state * buffer (* division-by-zero *)
  (* Halting configurations *)
  | HaltingCfg of state * buffer * buffer (* (state,inputs,outputs) *)
;;


(* Big-Step Evaluation Function *)

let eval cfg = match cfg with
| AExpCfg (IntAExp (n), s, ins) -> IntCfg (n, s, ins)
(* Otherwise cases *)
| termination_cfg -> termination_cfg
;;
