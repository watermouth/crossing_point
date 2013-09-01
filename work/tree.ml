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

(* insert しつつ, kより小さいノードの個数をvalueに設定する *)
let rec insert_with_lesser_count tree k v = match tree with
    Empty -> Node (Empty, k, v, Empty)
  | Node (left, key, value, right) ->
    if k = key then Node (left, key, v, right) (* 値を更新した木を返す *) 
    else if k < key 
      then Node ( (insert_with_lesser_count left k v), key, value+1, right)
      else Node ( left, key, value, (insert_with_lesser_count right k (value+1))) 
(* あとで小さいkeyを追加したときにおかしい *)

(* root のvalueを取得する *)
let root_value tree = match tree with
  | Empty -> 0
  | Node (left, key, value, right) -> value

let b = ref Empty;;
(*
b := insert_with_lesser_count !b 10 (root_value !b);;
b := insert_with_lesser_count !b 7 (root_value !b);;
b := insert_with_lesser_count !b 15 (root_value !b);;
b := insert_with_lesser_count !b 11 (root_value !b);;
*)

b := insert_with_lesser_count !b 10 0;;
b := insert_with_lesser_count !b 7 0;;
b := insert_with_lesser_count !b 15 0;;
b := insert_with_lesser_count !b 11 0;;
b := insert_with_lesser_count !b 5 0;;

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
