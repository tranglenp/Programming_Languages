(* Problem 10.*)
 
val countdown = unfold (fn n => if n = 0 then NONE else SOME(n, n-1))	
fun fold_factorial n = foldl (fn (x,v) => v*x) 1 (countdown n) 
 
  
(* Problem 11.*)
 
fun fold_map f xs = List.foldr (fn (x,v) => (f x)::v) [] xs 
 
 
(* Problem 12.*)  

fun fold_filter p xs = List.foldr (fn (x,v) => if (p x) then x::v else v) [] xs
 
 
(* Problem 13.*) 
 
fun myFoldl f v xs = 
    let val rev_xs = foldr (fn x => fn v => v@[x]) [] xs
	fun f' x v = f (x,v)
    in foldr f' v rev_xs 
    end
 
 
(* Problem 14.*)
datatype 'a tree = Leaf | Node of ('a * 'a tree * 'a tree) 

(* map over tree *)
fun tree_map f tr =
    case tr
    of Leaf => Leaf 
     | Node (x,trL,trR) => Node (f x, tree_map f trL, tree_map f trR) 
	 
(* fold over tree *)
fun tree_fold f acc tr =
    case tr
    of Leaf => acc 
     | Node (x,trL,trR) => f x (tree_fold f acc trL) (tree_fold f acc trR)	
	 
(* filter over tree *)	 
fun tree_filter p tr = 
    case tr 
    of Leaf => Leaf 
     | Node (x,trL,trR) => if (p x) 
	                   then Node (x, tree_filter p trL, tree_filter p trR)
                           else Leaf						   