let reverse ls =
  let rec helper acc ls =
    match ls with [] -> acc | h :: tail -> helper (h :: acc) tail
  in
  helper [] ls

let filter pred xs =
  let rec helper acc pred xs =
    match xs with
    | [] -> acc
    | h :: tail ->
        if pred h then helper (h :: acc) pred tail else helper acc pred tail
  in
  reverse (helper [] pred xs)

let split pred xs =
  let rec helper t_acc f_acc pred xs =
    match xs with
    | [] -> (reverse t_acc, reverse f_acc)
    | h :: tail ->
        if pred h then helper (h :: t_acc) f_acc pred tail
        else helper t_acc (h :: f_acc) pred tail
  in
  helper [] [] pred xs

let list_tail ls =
  let rec helper acc ls = match ls with [] -> acc | h :: t -> helper h t in
  helper 0 ls

let opt f s t fn =
  match (f, s, t) with
  | _, _, None -> None
  | _, None, _ -> None
  | None, _, _ -> None
  | _, _, _ -> fn f s t

let map f a =
  let rec helper f a acc =
    match a with [] -> acc | h :: t -> helper f t (f h :: acc)
  in
  reverse (helper f a [])

let map2 f list1 list2 =
  let rec helper f list1 list2 acc =
    match (list1, list2) with
    | [], [] -> acc
    | [], _ :: _ -> []
    | _ :: _, [] -> []
    | h1 :: tail1, h2 :: tail2 -> helper f tail1 tail2 (f h1 h2 :: acc)
  in
  reverse (helper f list1 list2 [])

let find el ls =
  let rec helper el ls counter =
    match ls with
    | [] -> -1
    | h :: tail -> if h = el then counter else helper el tail (counter + 1)
  in
  helper el ls 0

let cartesian list1 list2 =
  let rec helper1 ls1 acc =
    match ls1 with
    | [] -> acc
    | h :: tail ->
        helper1 tail
          (let rec helper2 ls2 current acc2 =
             match ls2 with
             | [] -> acc2
             | h :: tail -> helper2 tail current ((current, h) :: acc2)
           in
           helper2 list2 h acc)
  in
  reverse (helper1 list1 [])

[@@@ocaml.warnerror "-90"]

let concat ls =
  let rec helper acc ls =
    match ls with
    | [] -> acc
    | head :: tail ->
        helper
          (let rec helper2 acc2 current =
             match current with [] -> acc2 | h :: t -> helper2 (h :: acc2) t
           in
           helper2 acc head)
          tail
  in
  reverse (helper [] ls)

(*чисто функциональные структуры данных*)
let concat_map ls f = concat (map f ls)

let rec cartesian_N sets =
  match sets with
  | [] -> [ [] ]
  | set :: rest_sets ->
      let rest_product = cartesian_N rest_sets in
      concat (map (fun x -> map (fun y -> x :: y) rest_product) set)

let fold_left f init ls =
  let rec helper acc f ls =
    match ls with [] -> acc | h :: tail -> helper (f acc h) f tail
  in
  helper init f ls

let fold_right f lst acc =
  (fold_left (fun g x -> fun acc -> g (f x acc)) (fun x -> x) lst) acc

let fold_left_from_fold_right f acc lst =
  (fold_right (fun x g -> fun acc -> g (f acc x)) lst (fun x -> x)) acc

let print_decart_2 ls1 ls2 =
  List.iter (fun (x, y) -> Printf.printf "(%d, %d) " x y) (cartesian ls1 ls2)

let print ls = List.iter (fun x -> Printf.printf "%d " x) ls
let print_str ls = List.iter (fun x -> Printf.printf "%s " x) ls

let print_cort_n ls =
  List.iter (fun ls2 -> List.iter (fun x -> Printf.printf "%d " x) ls2) ls

let print_cort (list1, list2) =
  List.iter (fun x -> Printf.printf "%d" x) list1;
  Printf.printf ";";
  List.iter (fun x -> Printf.printf "%d" x) list2

let%expect_test "split_test_1" =
  print_cort (split (fun x -> x mod 2 = 0) [ 1; 2; 3; 5; 11; 12 ]);
  [%expect {|212;13511|}]

let%expect_test "tail_test_1" =
  Printf.printf "%d" (list_tail [ 1; 2; 3; 5; 11; 12 ]);
  [%expect {|12|}]

let%expect_test "map_test_1" =
  print
    (map
       (fun x -> x + 1)
       (map (fun x -> int_of_string x) [ "1"; "2"; "3"; "4" ]));
  [%expect {|2 3 4 5|}]

(*dz*)

let%expect_test "map2_test_1" =
  print (map2 (fun x y -> x + int_of_string y) [ 1; 2; 3 ] [ "4"; "5"; "6" ]);
  [%expect {|5 7 9|}]

let%expect_test "map2_test_2" =
  print_str
    (map2
       (fun x y -> string_of_int x ^ string_of_int y)
       [ 1; 2; 3; 8; 9111 ] [ 9; 5; 2; 16; 4448 ]);
  [%expect {|19 25 32 816 91114448|}]

let%expect_test "map2_test_3" =
  print
    (map2
       (fun x y -> if x > 0 then x mod y else y + (x mod y))
       [ 42; 36; 9; 19; -21; 13 ] [ 4; 12; 6; 11; 9; 26 ]);
  [%expect {|2 0 3 8 6 13|}]

let%expect_test "find_test_1" =
  Printf.printf "%d" (find 5 [ 1; 2; 35; 5; 331; -9 ]);
  [%expect {|3|}]

let%expect_test "find_test_2" =
  Printf.printf "%d" (find "qqq" [ "qqq"; "qqq"; "aaa"; "ddd" ]);
  [%expect {|0|}]

let%expect_test "find_test_3" =
  Printf.printf "%d" (find 'c' [ 'a'; 'b' ]);
  [%expect {|-1|}]

let%expect_test "decart_test_2_1" =
  print_decart_2 [ 1; 2; 3; 4 ] [ 6; 5; 5 ];
  [%expect
    {|(1, 6) (1, 5) (1, 5) (2, 6) (2, 5) (2, 5) (3, 6) (3, 5) (3, 5) (4, 6) (4, 5) (4, 5)|}]

let%expect_test "decart_test_2_2" =
  print_decart_2 [ 8; 5; 11; 4 ] [];
  [%expect {||}]

let%expect_test "decart_test_2_3" =
  print_decart_2 [ 11; 21; 3; 0 ] [ -8 ];
  [%expect {|(11, -8) (21, -8) (3, -8) (0, -8) |}]

let%expect_test "concat_test_1" =
  print (concat [ [ 1; 2; 3 ]; [ 4; 5; 2 ]; [ 1; 8; 0 ] ]);
  [%expect {|1 2 3 4 5 2 1 8 0|}]

let%expect_test "concat_test_2" =
  print_str (concat [ [ "aaa"; "qwerty"; "asd" ]; []; [ "ak"; "af" ] ]);
  [%expect {|aaa qwerty asd ak af|}]

let%expect_test "concat_map_test_1" =
  print
    (concat_map [ 3; 5; 11; 4 ] (fun x ->
         let rec helper vl acc =
           if vl = 0 then acc else helper (vl - 1) (vl :: acc)
         in
         helper x []));
  [%expect {|1 2 3 1 2 3 4 5 1 2 3 4 5 6 7 8 9 10 11 1 2 3 4|}]

let%expect_test "concat_map_test_2" =
  print (concat_map [] (fun x -> [ x ]));
  [%expect {||}]

let%expect_test "concat_map_test_3" =
  print_str (concat_map [ "str1"; "str2"; "str3" ] (fun s -> [ s ]));
  [%expect {|str1 str2 str3|}]

let%expect_test "fold_left_test_1" =
  print_int
    (fold_left (fun x y -> x + int_of_string y) 5 [ "12"; "66"; "-60"; "-12" ]);
  [%expect {|11|}]

let%expect_test "fold_left_test_2" =
  print_string (fold_left (fun x y -> x ^ string_of_int y) "begin: " []);
  [%expect {|begin:|}]

let%expect_test "fold_right_test_1" =
  print_int (fold_right (fun x y -> x + y) [ 1; 2; 3; 4 ] 0);
  [%expect {|10|}]

let%expect_test "fold_right_test_2" =
  print_string (fold_right (fun x y -> x ^ y) [ "qqq"; "asd"; "frek" ] "beg");
  [%expect {|qqqasdfrekbeg|}]

let%expect_test "fold_right_test_3" =
  print_int (fold_right (fun x y -> x + (x mod y)) [ 16; 5; 10; 3; 6 ] 5);
  [%expect {|22|}]

let%expect_test "fold_left_from_right_test_1" =
  print_string
    (fold_left_from_fold_right
       (fun x y -> x ^ y)
       "beg" [ "qqq"; "asd"; "frek" ]);
  [%expect {|begqqqasdfrek|}]

let%expect_test "fold_left_from_right_test_2" =
  print_int
    (fold_left_from_fold_right (fun x y -> x + (x mod y)) 5 [ 3; 4; 10; 3; 6 ]);
  [%expect {|16|}]

let%expect_test "cartesian_N_test_1" =
  List.iter
    (fun ls ->
      print_string "[";
      List.iter (fun x -> Printf.printf "%d, " x) ls;
      print_string "]; ")
    (cartesian_N [ [ 1; 2; 3 ]; [ 13; 14 ]; [ 4 ] ]);
  [%expect
    {|[1, 13, 4, ]; [1, 14, 4, ]; [2, 13, 4, ]; [2, 14, 4, ]; [3, 13, 4, ]; [3, 14, 4, ]; |}]

let%expect_test "cartesian_N_test_2" =
  List.iter
    (fun ls ->
      print_string "[";
      List.iter (fun s -> Printf.printf "%s, " s) ls;
      print_string "]; ")
    (cartesian_N
       [
         [ "str1"; "str3"; "str5" ];
         [ "str4"; "str5" ];
         [ "str12"; "str15"; "str16" ];
       ]);
  [%expect
    {|[str1, str4, str12, ]; [str1, str4, str15, ]; [str1, str4, str16, ]; [str1, str5, str12, ]; [str1, str5, str15, ]; [str1, str5, str16, ]; [str3, str4, str12, ]; [str3, str4, str15, ]; [str3, str4, str16, ]; [str3, str5, str12, ]; [str3, str5, str15, ]; [str3, str5, str16, ]; [str5, str4, str12, ]; [str5, str4, str15, ]; [str5, str4, str16, ]; [str5, str5, str12, ]; [str5, str5, str15, ]; [str5, str5, str16, ]; |}]

let%expect_test "cartesian_N_test_3" =
  List.iter
    (fun ls ->
      print_string "[";
      List.iter (fun x -> Printf.printf "%d, " x) ls;
      print_string "]; ")
    (cartesian_N [ [ 1; 2; 3 ]; [ 13; 14 ]; [] ]);
  [%expect {| |}]
