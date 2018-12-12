open Bigstep;;

let run stmt state ins =
  let initial_cfg = StmtCfg (stmt, state, ins) in
  let final_cfg = eval initial_cfg in
  print_cfg initial_cfg;
  print_string "\n---evals to--->\n";
  print_cfg final_cfg
;;


let test_01 () =
  let initial_state = [("x",0)] in
  let stmt = AssignStmt ("x", IntAExp (42)) in
  let ins = [] in
  run stmt initial_state ins
;;


let main () =
  print_string "--- Welcome to Big-Step Semantics ---\n";
  print_string "---       Xiaohong Chen           ---\n";
  test_01();
  print_string "\n";
  print_string "---         Bye bye               ---\n";
;;

let () = main ()
;;
