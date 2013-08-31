(* value 値以上の最小の要素のindex 返す *)
let find_index buffer value min_index max_index =
  (* buffer は昇順にソート済みの配列 *) 
  let rec sub value min_index max_index =
    if min_index = max_index then min_index
    else 
      let idx = (min_index + max_index ) / 2 in
      (* Printf.printf "%d\n" idx; *)
      if value <= buffer.(idx) 
      then sub value min_index idx 
      else sub value (idx+1) max_index 
  in sub value min_index max_index 
  
let calc input_file_name record_number = 
  let in_file = open_in input_file_name in
  let buffer = Array.init (record_number+1) (fun i -> i+1) in
  let points = ref 0 in
  for i=0 to record_number - 1 do
    let s = (input_line in_file) in
    let x = (int_of_string s) in
    let insert_at = find_index buffer x 0 i in 
    Printf.printf "%s:" s;
    Printf.printf "input:%d, insert_at:%d\n" x insert_at;
    points := !points + (x - 1 - insert_at);
    for j=(i+1) downto (insert_at+1) do
      buffer.(j) <- buffer.(j-1) 
    done;
(*
*)
    buffer.(insert_at) <- x
  done;
  !points 

let _ =
  let filename = Sys.argv.(1) in
  let line_number = (int_of_string Sys.argv.(2))in
  Printf.printf "%d\n" (calc filename line_number)

