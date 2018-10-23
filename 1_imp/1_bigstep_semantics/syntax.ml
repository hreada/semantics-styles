type id =
  | Id of string
;;
type aexp =
  | IntAExp of int
  | IdAExp of id
  | PlusAExp of aexp * aexp
  | DivAExp of aexp * aexp
;;
type bexp =
  | BoolBExp of bool
  | LTBExp of aexp * aexp
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
;;
type pgm =
  | Pgm of id list * stmt
;;


