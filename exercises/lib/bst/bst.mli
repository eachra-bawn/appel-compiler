open Base

type key = string

type 'a tree = Leaf | Tree of 'a tree * (key * 'a)  * 'a tree

val insert : 'a tree * key * 'a  -> 'a tree
val lookup : 'a tree * key -> 'a
