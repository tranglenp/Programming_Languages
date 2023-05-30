(* Problem 1*)
fun f_opt1 xs = 
    case xs 
	of [] => NONE
	 | (x::_) => SOME x
	
fun f_opt2 xs =
    case xs
    of [] => NONE 
     | (_::xs) => SOME xs	
	
fun f_opt3 x =
    case (x < 0) 
	of true => NONE 
	 | false => SOME x
	 
val zeroOrNone = fn n => if n = 0 then SOME (SOME 0) else NONE

val someNOrNone = fn n => case n 
                          of SOME n => SOME n
                           | _      => NONE

val test_compose_opt1 = compose_opt f_opt1 f_opt2 [1,2,3] = SOME 2	
val test_compose_opt2 = compose_opt f_opt3 f_opt1 [~1,2,3] = NONE 
val test_compose_opt3 = compose_opt f_opt3 f_opt1 [1,2,3] = SOME 1 	
val test_compose_opt4 = compose_opt f_opt3 (compose_opt f_opt1 (compose_opt f_opt2 f_opt2)) [5,4,3] = SOME 3
val test_compose_opt5 = compose_opt someNOrNone zeroOrNone 0 = SOME 0
val test_compose_opt6 = compose_opt someNOrNone zeroOrNone 1 = NONE 


(* Problem 2*)
val devideOutN = fn n => do_until (fn x => x div n) (fn x => x mod n = 0)
val devideOutTwo   = devideOutN 2 
val devideOutThree = devideOutN 3 

val test_do_until1  = devideOutTwo 8 = 1
val test_do_until2  = devideOutTwo 20 = 5 
val test_do_until3  = devideOutThree 9 = 1 
val test_do_until4  = devideOutN 9 27 = 3 
val test_do_until5  = do_until (fn x => x div 3) (fn x => x mod 3 = 0) 48 = 16
val test_do_until6  = do_until (fn x => x div 2) (fn x => x mod 2 <> 0) 9 = 4
val test_do_until7  = do_until (fn x => x ^ ".") (fn x => String.size x < 9) "abcde" = "abcde...."
val test_do_until8  = do_until (fn x => x div 2) (fn x => x mod 2 <> 1) 10 = 5
val test_do_until9  = do_until (fn x => x div 2) (fn x => x mod 2 <> 1) 6 = 3
val test_do_until10 = do_until (fn x => x div 2) (fn x => x mod 2 <> 1) 4 = 1
val test_do_until11 = do_until (fn x => x div 2) (fn x => x mod 2 <> 1) 5 = 5


(* Problem 3*)
val test_fact = let val testls = map (fn n => fact n = fact' n) [0,1,2,3,4,5,6,7,8,9,10,11,12]
                in foldl (fn (x,acc) => acc andalso x) true testls 
				end 
				

(* Problem 4*)	
val test_fixed_point1 = fixed_point (fn n => n div 5) 25 = 0
val test_fixed_point2 = fixed_point (fn n => n div 3) 64 = 0
val test_fixed_point3 = fixed_point (fn n => n div 7) 17 = 0
val test_fixed_point4 = fixed_point (fn n => n div 8) 98 = 0  
val test_fixed_point5 = fixed_point (fn x => x) 17 = 17 	


(* Problem 5 *)
val test_map2_1 = map2 String.size ("abc","abcd") = (3,4)
val test_map2_2 = map2 hd ([4,5],[6]) = (4,6)
val test_map2_3 = map2 tl ([4,5],[6]) = ([5],[])
val test_map2_4 = map2 (fn n => n mod 2 = 0) (3,4) = (false,true)
val test_map2_5 = map2 (fn x => ~x) (1,2) = (~1,~2)
val test_map2_6 = map2 (fn x => x * x) (1,2) = (1,4)


(* Problem 6 *)
fun f1 n = [n,2*n,3*n]
fun f2 n = [n*2,n*3]

val test_app_all1 = app_all f1 f1 1 = [1,2,3,2,4,6,3,6,9]
   
val test_app_all2 =
    let fun f s = [s^"_1", s^"_2"]
	in app_all f f "test" = ["test_1_1","test_1_2","test_2_1","test_2_2"]
	end 
	
val test_app_all3 =
    let fun g s = [s, s^s]
	    fun f s = [String.size s,3]
	in app_all f g "test" = [4,3,8,3]
	end
	
val test_app_all4 =
    let fun g n = [devideOutTwo n, devideOutThree n]
	in app_all f2 g 24 = [6,9,16,24] 
	end		

val test_app_all5 = app_all f1 f2 2 = [4,8,12,6,12,18]


(* Problem 7*)	
val test_foldr1 = foldr (fn x => fn acc => x + acc) 0 [1,2,3,4] = List.foldr (fn (x, acc) => x + acc) 0 [1,2,3,4]
val test_foldr2 = foldr (fn x => fn v => x * v) 1 [1,2,3,4] = List.foldr (fn (x,v) => x * v) 1 [1,2,3,4] 
val test_foldr3 = foldr (fn x => fn v => (String.size x) + v) 0 ["1","2","3","4"] = List.foldr (fn (x,v) => (String.size x) + v) 0 ["1","2","3","4"] 
val test_foldr4 = foldr (fn x => fn v => x^v) "_num" ["1","2","3","4"] = List.foldr (fn (x,v) => x^v) "_num" ["1","2","3","4"]
val test_foldr5 = foldr (fn x => fn init => init andalso x >= 0) true [1,2,3,4] = List.foldr (fn (x, init) => init andalso x >= 0) true [1,2,3,4] 

val test_merge_sort1 = merge_sort [1,2,5,1,2,5,1] = [1,1,1,2,2,5,5]
val test_merge_sort2 = merge_sort [0,3,7,5,0,8,3] = [0,0,3,3,5,7,8]
val test_merge_sort3 = merge_sort [3,2,6,5,8,9,2] = [2,2,3,5,6,8,9]


(* Problem 8 *)
val test_partition1 = partition (fn x => x > 0) [3,4,~2,6,~3,5] = ([3,4,6,5],[~2,~3])
val test_partition2 = partition (fn x => x < 0) [3,4,~2,6,~3,5] = ([~2,~3],[3,4,6,5])
val test_partition3 = partition (fn x => x mod 2 = 0) [1,2,3,4,5,6,7,8] = ([2,4,6,8],[1,3,5,7])
val test_partition4 = partition (fn x => x >= 0) [1,~2,3,~4,5,6,7,~8] = ([1,3,5,6,7],[~2,~4,~8])
val test_partition5 = let val charls = partition Char.isUpper (String.explode "slhajJKVHJHhvaln")
                      in (fn (xs,ys) => String.implode (xs@ys)) charls = "JKVHJHslhajhvaln"
					  end 
			  
val test_qsort1 = qsort [8,4,5,2,9]  = [2,4,5,8,9]	
val test_qsort2 = qsort [3,2,9,4,8] = [2,3,4,8,9]
val test_qsort3 = qsort [1,~9,8,~4,4] = [~9,~4,1,4,8] 			  
					  

(* Problem 9 *)
val repeat = fn m => unfold (fn n => if n = 0 then NONE else SOME(m, n-1)) 
val test_unfold1 = countdown 5 = [5,4,3,2,1]	
val test_unfold2 = countdown 3 = [3,2,1]
val test_unfold3 = countdown 6 = [6,5,4,3,2,1]				  
val test_unfold4 = repeat 3 4 = [3,3,3,3] 
val test_unfold5 = repeat #"a" 4 = [#"a",#"a",#"a",#"a"]
val test_unfold6 = repeat ("abc",[3]) 2 = [("abc",[3]),("abc",[3])]  
val test_unfold7 = unfold (fn x => if x > 3 then NONE else SOME (x,x + 1)) 0 = [0, 1, 2, 3]
val test_unfold8 = unfold (fn _ => NONE) false = []
val test_unfold9 = unfold (fn str => if String.size str > 12 then NONE else SOME (str, "Banana" ^ str)) "" = ["", "Banana", "BananaBanana"]
