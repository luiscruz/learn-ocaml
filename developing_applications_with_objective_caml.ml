(* 
  Développement d'applications avec Objective Caml by Emmanuel Chailloux, Pascal Manoury and Bruno Pagano, published by O'Reilly France 
  http://caml.inria.fr/pub/docs/oreilly-book/
*)

(*
Write a function merge_i which takes as input two integer lists sorted in increasing order and returns a new sorted list containing the elements of the first two. 
*)
let rec merge_i list1 list2 = match (list1, list2) with
    (h1::t1, h2::t2) -> if h2 < h1 then h2::(merge_i list1 t2) else h1::(merge_i t1 list2)
    | ([],list)|(list,[]) -> list
;;
    

assert ((merge_i [1;2;5;10] [3;4;5;6]) = [1; 2; 3; 4; 5; 5; 6; 10]);;






(* Write a general function merge which takes as argument a comparison function and two lists sorted in this order and returns the list merged in the same order. The comparison function will be of type 'a -> 'a -> bool.*)
let rec merge comparison list1 list2 = match (list1, list2) with
    (h1::t1, h2::t2) -> if comparison h1 h2 then h2::(merge comparison list1 t2) else h1::(merge comparison t1 list2)
    | ([],list)|(list,[]) -> list
;;    
assert ((merge (fun x y -> x>y) [1;2;5;10] [3;4;5;6]) = [1; 2; 3; 4; 5; 5; 6; 10]);;
    
(*Apply this function to two integer lists sorted in decreasing order, then to two string lists sorted in decreasing order.*)

assert ((merge (fun x y -> x>y) [10;8;6;4] [9;7;5;3;1]) = [10; 9; 8; 7; 6; 5; 4; 3; 1]);;

(*What happens if one of the lists is not in the required decreasing order?*)

(*Write a new list type in the form of a record containing three fields: the conventional list, an order function and a boolean indicating whether the list is in that order.*)
type ('a) new_list = {
    list: 'a list;
    order_function: 'a -> 'a -> bool;
    ordered: bool;
}




(*Write the function insert which adds an element to a list of this type.

Write a function sort which insertion sorts the elements of a list.

















Write a new function merge for these lists.
*)