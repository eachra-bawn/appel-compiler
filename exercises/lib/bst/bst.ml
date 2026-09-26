type key = string
type 'a tree = Leaf | Tree of 'a tree * (key * 'a) * 'a tree

let empty = Leaf

let print_tree tree =
  let rec print_tree_aux tree =
    match tree with
    | Leaf -> ()
    | Tree (l, (k, v), r) ->
        Format.printf "(%s, %d)\n" k v;
        print_branches l r
  and print_branches l r =
    match l with
    | Leaf -> print_tree_aux r
    | Tree (l, (k, v), r) as l_tree -> print_tree_aux l_tree
  in
  print_tree_aux tree

let check_tree_balance tree =
  let rec check_tree tree =
    match tree with
    | Leaf -> (0, 0)
    | Tree (l, _, r) -> (check_branch l 0, check_branch r 0)
  and check_branch sub_tree height_acc =
    match sub_tree with
    | Leaf -> height_acc
    | Tree (l, (k, _), r) ->
        max (check_branch l (height_acc + 1)) (check_branch r (height_acc + 1))
  in
  check_tree tree

let rec lookup (tree, key) =
  match tree with
  | Leaf -> failwith "Lookup failed"
  | Tree (l, (k, v), r) -> (
      if key = k then v
      else
        match lookup (l, key) with
        | exception _ -> lookup (r, key)
        | value -> value)

let rec insert (tree, key, value) =
  match tree with
  | Leaf -> Tree (Leaf, (key, value), Leaf)
  | Tree (l, (k, v), r) ->
      if key < k then Tree (insert (l, key, value), (k, v), r)
      else if key > k then Tree (l, (k, v), insert (r, key, value))
      else Tree (l, (key, value), r)

let rec member tree key =
  match tree with
  | Leaf -> false
  | Tree (l, (k, _), r) ->
      if k = key then true
      else if member l key = true then true
      else if member r key = true then true
      else false

let%expect_test "Balance 1" =
  let tree = Tree (Leaf, ("t", 1), Leaf) in
  let tree2 = insert (tree, "s", 1) in
  let tree3 = insert (tree2, "p", 1) in
  let tree4 = insert (tree3, "i", 1) in
  let tree5 = insert (tree4, "p", 1) in
  let tree6 = insert (tree5, "f", 1) in
  let tree7 = insert (tree6, "b", 1) in
  let tree8 = insert (tree7, "s", 1) in
  let tree9 = insert (tree8, "t", 1) in
  let l_bal, r_bal = check_tree_balance tree9 in
  let () = Format.printf "(%d, %d)" l_bal r_bal in
  [%expect {|(5, 0)|}]

let%expect_test "Balance 2" =
  let tree = Tree (Leaf, ("a", 1), Leaf) in
  let tree2 = insert (tree, "b", 1) in
  let tree3 = insert (tree2, "c", 1) in
  let tree4 = insert (tree3, "d", 1) in
  let tree5 = insert (tree4, "e", 1) in
  let tree6 = insert (tree5, "f", 1) in
  let tree7 = insert (tree6, "g", 1) in
  let tree8 = insert (tree7, "h", 1) in
  let tree9 = insert (tree8, "i", 1) in
  let l_bal, r_bal = check_tree_balance tree9 in
  Format.printf "(%d, %d)" l_bal r_bal;
  [%expect {|(0, 8)|}]
