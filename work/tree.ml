(* 2分探索木 *)

type ('a, 'b) t = Empty | Node of ('a, 'b) t * 'a * 'b * ('a, 'b) t

let empty = Empty

let rec insert tree k v = match tree with
    Empty -> Node (Empty, k, v, Empty)
  | Node (left, key, value, right) ->
    if k = key then Node (left, key, value, right) (* 同じ木を返す *) 
    else if k < key
      then Node ( (insert left k v), key, value, right) 
      else Node ( left, key, value, (insert right k v))

let rec search tree k = match tree with
  | Empty -> raise Not_found
  | Node (left, key, value, right) ->
    if key = k then value
    else if k < key
      then search left k
      else search right k

(* k より小さいノードの個数を返す *)
let rec count_lesser tree k = match tree with
  | Empty -> 0
  | Node (left, key, value, right) ->
    if key < k then
      1 + (count_lesser left k) + (count_lesser right k)
    else (count_lesser left k) + (count_lesser right k)

(* root のvalueを取得する *)
let root_value tree = match tree with
  | Empty -> 0
  | Node (left, key, value, right) -> value

(* tree の各ノードのvalueに1加算する *)
let rec add_one tree = match tree with
  | Empty -> Empty  
  | Node (left, key, value, right) ->
    Node (add_one left, key, value+1, add_one right)

(* insert しつつ, kより小さいノードの個数をvalueに設定する *)
let rec insert_with_lesser_count tree k v = match tree with
    Empty -> 
(*
    Printf.printf "Node=%d, value=%d\n" k v;
*)
    Node (Empty, k, v, Empty)
  | Node (left, key, value, right) ->
    if k = key then Node (left, key, v, right) (* 値を更新した木を返す *) 
    else if k < key 
      then Node ( (insert_with_lesser_count left k v), key, value+1, right)
      else Node ( left, key, v, (insert_with_lesser_count right k (value + 1))) 

(* 左側の子ノードの数をvalueに持たせる *)
let rec insert_left_node_count tree k v = match tree with
    Empty -> 
(*
    Printf.printf "Node=%d, value=%d\n" k v;
*)
    Node (Empty, k, v, Empty)
  | Node (left, key, value, right) ->
    if k = key then Node (left, key, v, right) (* 値を更新した木を返す *) 
    else if k < key 
      then Node ( (insert_left_node_count left k v), key, value+1, right)
      else Node ( left, key, value, (insert_left_node_count right k v)) 

(* 左側の子ノードの数の総和を返す *)
let search_with_sum tree k =
  let rec sub tree k count = match tree with
  | Empty -> raise Not_found
  | Node (left, key, value, right) ->
    if key = k then (count + value)
    else if k < key
      then sub left k count
      else sub right k (count + 1 + value)
  in sub tree k 0

let b = ref Empty;;
(*
b := insert_with_lesser_count !b 10 (root_value !b);;
b := insert_with_lesser_count !b 7 (root_value !b);;
b := insert_with_lesser_count !b 15 (root_value !b);;
b := insert_with_lesser_count !b 11 (root_value !b);;
*)
let root_key = 10;;
let initial_value_for_insert tree root_key insert_key =
  if root_key < insert_key then (root_value tree) else 0 ;;

b := insert_with_lesser_count !b 10 (initial_value_for_insert !b root_key 10);; 
b := insert_with_lesser_count !b 7 (initial_value_for_insert !b root_key 7);; 
b := insert_with_lesser_count !b 15 (initial_value_for_insert !b root_key 15);; 
b := insert_with_lesser_count !b 11 (initial_value_for_insert !b root_key 11);; 
b := insert_with_lesser_count !b 5 (initial_value_for_insert !b root_key 5);; 
b := insert_with_lesser_count !b 25 (initial_value_for_insert !b root_key 25);; 
(*
b := insert_with_lesser_count !b 20 (initial_value_for_insert !b root_key 20);; 
*)

let bb = ref Empty;;
bb := insert_left_node_count !bb 10 0;;
bb := insert_left_node_count !bb 7 0;;
bb := insert_left_node_count !bb 15 0;;
bb := insert_left_node_count !bb 11 0;;
bb := insert_left_node_count !bb 5 0;;

let a = ref Empty;;
a := insert_with_lesser_count !a 3 0;;
a := insert_with_lesser_count !a 1 0;;
a := insert_with_lesser_count !a 4 0;;
a := insert_with_lesser_count !a 5 0;;
a := insert_with_lesser_count !a 9 0;;
a := insert_with_lesser_count !a 2 0;;
a := insert_with_lesser_count !a 6 0;;
a := insert_with_lesser_count !a 8 0;;
a := insert_with_lesser_count !a 7 0;;

Printf.printf "--- \n";
