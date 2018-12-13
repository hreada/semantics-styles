open Bigstep;;

let run stmt state ins =
  let initial_cfg = StmtCfg (stmt, state, ins) in
  let final_cfg = eval initial_cfg in
  print_cfg initial_cfg;
  print_string "\n  ---evals to--->\n";
  print_cfg final_cfg;
  print_string "\n\n"
;;


let stmt_assign x e = AssignStmt (x, e);;
let bexp_not e = NotBExp (e);;
let stmt_while cond block = WhileStmt (cond, block);;
let stmt_block block = BlockStmt (block);;
let block_stmt stmt = StmtBlock (stmt);;

let stmt_assign_n n = AssignStmt ("n", IntAExp (n));;
let stmt_assign_x n = AssignStmt ("x", IntAExp (n));;
let stmt_assign_y n = AssignStmt ("y", IntAExp (n));;
let stmt_assign_s n = AssignStmt ("s", IntAExp (n));;

let aexp_plus_s_n = PlusAExp (IdAExp ("s"), IdAExp ("n"));;
let aexp_minus_n_1 = PlusAExp (IdAExp ("n"), IntAExp (-1));;

let bexp_lessthan_n n = LessThanBExp (IdAExp ("n"), IntAExp (n));;
let bexp_lessthan_x n = LessThanBExp (IdAExp ("x"), IntAExp (n));;
let bexp_lessthan_y n = LessThanBExp (IdAExp ("y"), IntAExp (n));;
let bexp_lessthan_s n = LessThanBExp (IdAExp ("s"), IntAExp (n));;


let rec make_seqstmt stmts = match stmts with
| [] -> BlockStmt (EmptyBlock)
| stmt::rest_stmts -> SeqStmt (stmt, make_seqstmt rest_stmts)
;;


let test_sum_10 () =
  let initial_state = [("n",0);("s",0)] in
  let stmt =
    make_seqstmt [
      (stmt_assign_n 10);
      (stmt_assign_s 0);
      (stmt_while (bexp_not (bexp_lessthan_n 1))
        (block_stmt
          (make_seqstmt [
            (stmt_assign "s" aexp_plus_s_n);
            (stmt_assign "n" aexp_minus_n_1)])))] in
  let ins = [] in
  print_string "test_sum_10:\n";
  run stmt initial_state ins
;;

let test_sum_100 () =
  let initial_state = [("n",0);("s",0)] in
  let stmt =
    make_seqstmt [
      (stmt_assign_n 100);
      (stmt_assign_s 0);
      (stmt_while (bexp_not (bexp_lessthan_n 1))
        (block_stmt
          (make_seqstmt [
            (stmt_assign "s" aexp_plus_s_n);
            (stmt_assign "n" aexp_minus_n_1)])))] in
  let ins = [] in
  print_string "test_sum_100:\n";
  run stmt initial_state ins
;;

let test_sum_1000 () =
  let initial_state = [("n",0);("s",0)] in
  let stmt =
    make_seqstmt [
      (stmt_assign_n 1000);
      (stmt_assign_s 0);
      (stmt_while (bexp_not (bexp_lessthan_n 1))
        (block_stmt
          (make_seqstmt [
            (stmt_assign "s" aexp_plus_s_n);
            (stmt_assign "n" aexp_minus_n_1)])))] in
  let ins = [] in
  print_string "test_sum_1000:\n";
  run stmt initial_state ins
;;

let main () =
  print_string "--- Welcome to Big-Step Semantics ---\n";
  print_string "---       Xiaohong Chen           ---\n\n";
  test_sum_10();
  test_sum_100();
  test_sum_1000();
  print_string "\n---         Bye bye               ---\n";
;;

let () = main ()
;;
