(**
	All exercises at https://ocaml.org/learn/tutorials/99problems.html
*)

(**
 *  Write a function last : 'a list -> 'a option
 *  that returns the last element of a list.
 *)

let rec last = function
    | [] -> None
    | x::[] -> Some x
    | _ :: tail -> last tail;;
    
assert (last [ "a" ; "b" ; "c" ; "d" ] = Some "d");
assert (last [] = None);;

(**
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

(**
    Find the k'th element of a list. (easy)
*)

let rec at i = function
    | [] -> None
    | x::tail -> if i=1 then Some x else at (i-1) tail;;

assert ((at 3 [ "a" ; "b"; "c"; "d"; "e" ]) = Some "c");
assert ((at 3 [ "a" ]) = None);;

(**
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

(**
    Reverse a list. (easy)
*)

let rec rev = function
    | [] -> []
    | h::t -> (rev t) @ [h];; (*probably, this is ineficient*)

assert ((rev ["a" ; "b" ; "c"]) = ["c"; "b"; "a"]);;

(**
    Find out whether a list is a palindrome. (easy)
*)

let is_palindrome list = 
    list = (rev list);;

assert ((is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ]) = true);
assert ((not (is_palindrome [ "a" ; "b" ])) = true );;

(**
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

(**
    Eliminate consecutive duplicates of list elements. (medium)    
*)

let rec compress = function
| [] -> []
| [h] -> [h]
| x::y::t -> if x=y then compress(y::t) else x::(compress (y::t))
;;

(compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]);;

assert ((compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]) = ["a"; "b"; "c"; "a"; "d"; "e"]);;

(**
    Pack consecutive duplicates of list elements into sublists. (medium)
*)

let pack list = 
    let rec aux current acum = function
        | [] -> []
        | [x] -> (x::current)::acum
        | st::nd::t -> if st = nd
            then (aux (st::current) acum (nd::t))
            else (aux [] ((st::current)::acum) (nd::t))
        in
        List.rev (aux [] [] list)
;;

assert ((pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"])
 = [["a"; "a"; "a"; "a"];["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
 ["e"; "e"; "e"; "e"]]
);;

(**
    Run-length encoding of a list. (easy)
*)

let encode list =
    let rec aux count acum = function
    | [] -> []
    | [x] -> ((count+1),x)::acum
    | st::nd::tail -> if st = nd
        then aux (count+1) acum (nd::tail)
        else aux 0 (((count+1), st)::acum) (nd::tail)
    in List.rev (aux 0 [] list)
;;

(* As alternative, solutions suggest that other List.map could be used in combination with pack *)
         
assert ((encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"])
= [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
);;


(**
    Modified run-length encoding. (easy)
*)

type 'a rle =
    | One of 'a
    | Many of int * 'a;;
    
let mencode list =
    let aux = function
        | (1, x) -> One x
        | (n, x) -> Many (n,x)
    in
    List.map aux (encode list) 
;;
    
assert ((mencode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"])
= [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]
);;

(**
    Decode a run-length encoded list. (medium)
*)

let rec decode d = 
    let rec repeat_element x = function
        | 0 -> []
        | n -> x::(repeat_element x (n-1)) in
    match d with
        | [] -> []
        | (One x)::t -> x::(decode t)
        | Many (n,x)::t -> (repeat_element x n) @ (decode t)
;;

assert ((decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")])
= ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
);;

(**
    Run-length encoding of a list (direct solution). (medium)
*)

(* I'm skipping this -- too boring
assert ((encode_v2 ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"])
= [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")])
*)


(**
    Duplicate the elements of a list. (easy)
*)

let rec duplicate = function
| [] -> []
| h::t -> h::h::(duplicate t)
;;

assert ((duplicate ["a";"b";"c";"c";"d"])
= ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]);;

(**
    Replicate the elements of a list a given number of times. (medium)
*)

let replicate list n = 
    let rec prepend acc n x = 
        if n = 0 then acc
        else (prepend (x::acc) (n-1) x)
    in  
    let rec aux acc n = function
        | [] -> acc
        | h::t -> aux (prepend acc n h) n t
    in
    aux [] n (List.rev list)
;;
assert ((replicate ["a";"b";"c"] 3) = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"])
;;


(**
    Drop every N'th element from a list. (medium)
*)

let drop list n= 
    let rec aux n i acum = function
        | [] -> acum
        | h::t -> if i =n then (aux n 1 acum t) else (aux n (i+1) (h::acum) t) 
    in List.rev (aux n 1 [] list)
;;

assert ((drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3) = ["a"; "b"; "d"; "e"; "g"; "h"; "j"])
;;


(**
    Split a list into two parts; the length of the first part is given. (easy)
*)

let split list n =
    let rec aux acum n i = function
    | [] -> ((List.rev acum),[])
    | h::t -> 
        if i<n then (aux (h::acum) n (i+1) t) else ((List.rev (acum)), h::t)
    in aux [] n 0 list
;;

assert((split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3) = (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"]));
assert((split ["a";"b";"c";"d"] 5)=(["a"; "b"; "c"; "d"], []));
assert((split ["a";"b";"c";"d"] 0)=([],["a"; "b"; "c"; "d"]));; (*extra*)

(**
    Extract a slice from a list. (medium)
*)

let slice list i k = 
    fst (split (snd (split list i)) (k-1))
;;

assert ((slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6) = ["c"; "d"; "e"; "f"; "g"])
;;


(**
    Rotate a list N places to the left. (medium)
*)

let rotate list n = 
    let k = if n< 0 then (n+(List.length list)) else n in
    match (split list k) with
    | (a,b) -> b@a
;;

assert ((rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3) = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]);
assert ((rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2)) = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"])
;;

(**
	Remove the K'th element from a list. (easy)
*)

let remove_at n list=
	match (split list n) with
	| (a,h::b) -> a@b
	| (a,[]) -> a
;;
assert ((remove_at 1 ["a";"b";"c";"d"])=["a"; "c"; "d"]);
assert ((remove_at 0 ["a";"b";"c";"d"])=["b"; "c"; "d"]);; (*extra*)

(**
    Insert an element at a given position into a list. (easy)
*)

let rec insert_at elem n= function
    | [] -> [elem]
    | h::t as l -> if n > 0 then h::(insert_at elem (n-1) t) else elem::l
;;

assert ((insert_at "alfa" 1 ["a";"b";"c";"d"]) = ["a"; "alfa"; "b"; "c"; "d"]);
assert ((insert_at "alfa" 3 ["a";"b";"c";"d"]) = ["a"; "b"; "c"; "alfa"; "d"]);
assert ((insert_at "alfa" 4 ["a";"b";"c";"d"]) = ["a"; "b"; "c"; "d"; "alfa"]);;

(**
    Create a list containing all integers within a given range. (easy)
*)

let rec range i n =
    if i < n then i::(range (i+1) n)
    else
        if i>n then  i::(range (i-1) n)
        else [n]
;;

assert ((range 4 9) = [4; 5; 6; 7; 8; 9]);
assert ((range 9 4) = [9; 8; 7; 6; 5; 4]);;

(**
	Extract a given number of randomly selected elements from a list. (medium)
*)

let rec rand_select list n=
	let list_len = List.length list in
	let random_number = (Random.int list_len) in
	if n>0 then (List.nth list random_number)::(rand_select list (n-1)) else []
;;
(* random result -- hard to verify *)
assert (List.length (rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 3) = 3);;

(**
	Lotto: Draw N different random numbers from the set 1..M. (easy)
*)

let rec lotto_select n m= 
	if n > 0 then
		(Random.int m)::(lotto_select (n-1) m)
	else []
;;

let n= 6 in let m = 49 in
assert (List.fold_left (&&) true (List.map (fun x -> (x>=1)&&(x<=m)) (lotto_select n m)));
(assert (List.length (lotto_select n m) = n))
;;

(**
	Generate a random permutation of the elements of a list. (easy)
*)
let permutation list = 
	let rec extract acum i = function
		| [] -> (None, List.rev acum)
		| h::t ->  if i> 0 then extract (h::acum) (i-1) t else (Some h, (List.rev acum)@t) in
	let rec extract_n n list = 
		if n>0 then
			match (extract [] (Random.int n) list) with
			| (None, l) -> l
			| (Some x, new_list) -> x::(extract_n (n-1) new_list)
		else []
	in
	let list_len = (List.length list) in
	(extract_n list_len list)
;;
assert ((List.sort compare (permutation ["a"; "b"; "c"; "d"; "e"; "f"])) = (List.sort compare ["a"; "e"; "f"; "b"; "d"; "c"]))
;;

print_endline "Ok."