(*
 *  Write a function last : 'a list -> 'a option
 *  that returns the last element of a list.
 *)

let rec last = function
    | [] -> None
    | x::[] -> Some x
    | _ :: tail -> last tail;;
    
assert (last [ "a" ; "b" ; "c" ; "d" ] = Some "d");
assert (last [] = None);;

(*
 *  Find the last but one (last and penultimate)
 *  elements of a list. (easy)
 *)

let rec last_two = function
    | [] -> None
    | [_] -> None (* the 1st and 2nd patterns could have been combined *)
    | [x;y] -> Some (x,y)
    | _::tail -> last_two tail;;

assert ((last_two [ "a" ; "b" ; "c" ; "d" ]) = Some ("c", "d"));
assert ((last_two [ "a" ]) = None);;

(*
    Find the k'th element of a list. (easy)
*)

let rec at i = function
    | [] -> None
    | x::tail -> if i=1 then Some x else at (i-1) tail;;

assert ((at 3 [ "a" ; "b"; "c"; "d"; "e" ]) = Some "c");
assert ((at 3 [ "a" ]) = None);;

(*
    Find the number of elements of a list. (easy)
*)

let rec length = function
    | [] -> 0
    | _::tail -> (length tail) + 1;;

assert ((length [ "a" ; "b" ; "c"]) = 3);
assert ((length []) = 0);;

let tail_recursive_length list = 
    let rec aux n = function
        | [] -> n
        | _::tail -> aux (n+1) tail
    in aux 0 list;;

assert ((tail_recursive_length [ "a" ; "b" ; "c"]) = 3);
assert ((tail_recursive_length []) = 0);;

(*
    Reverse a list. (easy)
*)

let rec rev = function
    | [] -> []
    | h::t -> (rev t) @ [h];; (**probably, this is ineficient*)

assert ((rev ["a" ; "b" ; "c"]) = ["c"; "b"; "a"]);;

(*
    Find out whether a list is a palindrome. (easy)
*)

let is_palindrome list = 
    list = (rev list);;

assert ((is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ]) = true);
assert ((not (is_palindrome [ "a" ; "b" ])) = true );;

(*
    Flatten a nested list structure. (medium)
*)

type 'a node =
    | One of 'a 
    | Many of 'a node list;;
    
let rec flatten = function
    | [] -> []
    | One x :: t -> x::flatten(t)
    | Many l :: t -> (flatten l) @ (flatten t);;

assert (
    (flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ])
    =  ["a"; "b"; "c"; "d"; "e"]
);;

(*
    Eliminate consecutive duplicates of list elements. (medium)    
*)

let rec compress = function
| [] -> []
| [h] -> [h]
| x::y::t -> if x=y then compress(y::t) else x::(compress (y::t))
;;

(compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]);;

assert ((compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]) = ["a"; "b"; "c"; "a"; "d"; "e"]);;


print_endline "Ok."