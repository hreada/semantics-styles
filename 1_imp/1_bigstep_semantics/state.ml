open List;;
open Syntax;;

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
