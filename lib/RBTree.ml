module type SET = sig
  type elem
  type t

  val empty : t
  val add : elem -> t -> t
  (*val member : elem -> t -> bool*)
end

module type ORDERED = sig
  type t

  val compare : t -> t -> int
end

module RedBlackSet (Element : ORDERED) : sig
  type color = R | B
  type elem = Element.t
  type t = private E | T of color * t * elem * t

  include SET with type elem := Element.t and type t := t
end = struct
  type elem = Element.t
  type color = R | B
  type t = E | T of color * t * elem * t

  let empty = E

  (*let rec member x = function
    | E -> false
    | T (_, a, y, b) -> (
        match Element.compare x y with
        | 0 -> true
        | n when n < 0 -> member x a
        | _ -> member x b)*)

  let balance = function
    | B, T (R, T (R, a, x, b), y, c), z, d
    | B, T (R, a, x, T (R, b, y, c)), z, d
    | B, a, x, T (R, T (R, b, y, c), z, d)
    | B, a, x, T (R, b, y, T (R, c, z, d)) ->
        T (R, T (B, a, x, b), y, T (B, c, z, d))
    | c, a, x, b -> T (c, a, x, b)

  let add x s =
    let rec ins = function
      | E -> T (R, E, x, E)
      | T (color, a, y, b) as s -> (
          match Element.compare x y with
          | 0 -> s
          | n when n < 0 -> balance (color, ins a, y, b)
          | _ -> balance (color, a, y, ins b))
    in
    match ins s with
    | T (_, a, y, b) -> T (B, a, y, b)
    | _ -> failwith "unreachable"
end

module S = RedBlackSet (Int)

let show_tree name (root : S.t) =
  let filename = "/tmp/demo.dot" in
  let ch = open_out filename in
  let ppf = Format.formatter_of_out_channel ch in
  let to_color = function S.B -> "black" | R -> "red" in
  let rec helper parent tree =
    match tree with
    | S.E -> ()
    | T (c, l, s, r) ->
        Format.fprintf ppf "%d [label=\"%d\",color=%s];\n" s s (to_color c);
        Format.fprintf ppf "%d -> %d;\n" parent s;
        helper s l;
        helper s r
  in
  let () =
    match root with
    | S.E -> ()
    | T (c, l, v, r) ->
        Format.fprintf ppf "digraph graphname {\n";
        Format.fprintf ppf "%d [label=\"%d\",color=%s];\n" v v (to_color c);
        helper v l;
        helper v r;
        Format.fprintf ppf "}\n%!"
  in
  close_out ch;
  let _ =
    Sys.command (Format.asprintf "dot -Tpng %s -o /tmp/%s.png" filename name)
  in
  let _ = Sys.command (Format.asprintf "xdg-open /tmp/%s.png" name) in
  ()

let () =
  let set = S.empty in
  let set = S.(set |> add 3 |> add 2 |> add 1) in
  show_tree "tree1" set;
  let set = S.(set |> add 6 |> add 5 |> add 4) in
  show_tree "tree2" set

(* https://www.cambridge.org/core/journals/journal-of-functional-programming/article/deletion-the-curse-of-the-redblack-tree/471C92AF3D431403FEE6C66FE070C492 *)
