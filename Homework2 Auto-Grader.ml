(* Problem 1a*)
fun all_except_option (str, xs) =
    case xs of
        [] => NONE
        | x :: xs' => 
            if same_string(str, x)
            then SOME xs'
            else case all_except_option(str, xs') of
                NONE => NONE
              | SOME ls => SOME (x :: ls)
(*Problem 1b*)
fun get_substitutions1(xs, s) =
    case xs of
       [] => []
     | x :: xs' => case all_except_option(s, x) of
        NONE => get_substitutions1(xs', s)
      | SOME ls => ls @ get_substitutions1(xs', s)
(*Problem 1c*)
fun get_substitutions2(xs, s) =
    let
        fun aux(xs, s, acc) =
            case xs of
               [] => acc
             | x :: xs' => case all_except_option(s, x) of
                NONE => aux(xs', s, acc)
              | SOME ls => aux(xs', s, ls @ acc)
    in
        aux(xs, s, [])
    end
(*Problem 1d*)
fun similar_names (substitutions,name) =
    let 
        val {first=f, middle=m, last=l} = name
	      fun make_names xs =
	         case xs of
		           [] => []
	           | x::xs' => {first=x, middle=m, last=l}::(make_names(xs'))
    in
	      name::make_names(get_substitutions2(substitutions,f))
    end
(*Problem 2a*)
fun card_color(the_suit, the_rank) =
    case the_suit of
       Clubs => Black
     | Spades => Black
     | Diamonds => Red
     | Hearts => Red
(*Problem 2b*)
fun card_value card =
    case card of
	      (_,Jack) => 10
      | (_,Queen) => 10
      | (_,King) => 10
      | (_,Ace) => 11 
      | (_,Num n) => n
(*Problem 2c*)
fun remove_card(cs, c, e) =
    case cs of
       [] => raise IllegalMove
     | head::tail => 
        if head = c
        then tail
        else head :: remove_card(tail, c, e)
(*Problem 2d*)
fun all_same_color cs = 
    case cs of
        [] => true
      | [_] => true
      | head::neck::tail => card_color head = card_color neck 
			    andalso all_same_color(neck::tail)
(*Problem 2e*)
fun sum_cards cards =
    let
        fun aux(cards, acc) =
            case cards of
               [] => acc
             | c::cs => aux(cs, card_value c + acc)
    in
        aux(cards, 0)
    end
(*Problem 2f*)
fun score (cs,goal) = 
    let 
        val sum = sum_cards cs
    in
        (if sum >= goal then 3 * (sum - goal) else goal - sum)
	      div (if all_same_color cs then 2 else 1)
    end
(*Problem 2g*)
fun officiate (card_list, moves, goal) =
    let
        fun play(card_list, current_helds, remain_moves) =
            case remain_moves of
               [] => current_helds
             | head::tail => case head of
                Discard c => play(card_list, remove_card(current_helds, c, IllegalMove), tail)
              | Draw => case card_list of
                 [] => current_helds
               | c::cs => 
                    if sum_cards (c::current_helds) > goal
                    then c::current_helds
                    else play(cs, c::current_helds, tail)
    in
        score (play(card_list,[], moves), goal)
    end
(*Problem 31*)
fun score_challenge (held_cards, goal) =
    let
        val sum = sum_cards held_cards
        fun better_score(held_cards, sum) =
            case held_cards of
               [] => 3 * (sum - goal)
             | (the_suit, the_rank)::cs => 
                case the_rank of
                   Ace => 
                    if sum - 10 > goal
                    then better_score(cs, sum - 10)
                    else Int.min(goal - (sum - 10), 3 * (sum - goal))
                 | _ => better_score(cs, sum)
        fun pre_score(sum, goal) =
            if sum > goal
            then better_score(held_cards, sum)
            else goal - sum
    in
        if all_same_color held_cards
        then pre_score(sum, goal) div 2
        else pre_score(sum, goal)
    end
(*Problem 3a*)
fun card_value_min (the_suit, the_rank)=
    case the_rank of
       Ace => 1
     | Num n => n
     | _ => 10

fun sum_cards_min cards =
    let  
        fun aux(cards, acc) =
            case cards of
               [] => acc
             | c::cs => aux(cs, card_value_min c + acc)
    in
        aux(cards, 0)
    end

fun officiate_challenge (card_list, moves, goal) =
    let
        fun play(card_list, current_helds, remain_moves) =
            case remain_moves of
               [] => current_helds
             | head::tail => case head of
                Discard c => play(card_list, remove_card(current_helds, c, IllegalMove), tail)
              | Draw => case card_list of
                 [] => current_helds
               | c::cs => 
                    if sum_cards_min (c::current_helds) > goal
                    then c::current_helds
                    else play(cs, c::current_helds, tail)
    in
        score_challenge (play(card_list,[], moves), goal)
    end
(*Problem 3b*)
fun careful_player (card_list, goal) =
    let
        fun careful_moves(card_list, helds, moves) =
            let 
                fun remove_reach_zero (cards, goal) =
                    case cards of
                        [] => NONE
                      | c::cs => if sum_cards(remove_card (cards, c, IllegalMove)) = goal then SOME c
                                 else remove_reach_zero (cs, goal - card_value c)
            in
                case card_list of
                    [] => moves
                  | c::cs =>
                        if goal - sum_cards helds > 10
                        then careful_moves (cs, c::helds, moves @ [Draw])
                        else if goal = sum_cards helds
                        then moves
                        else case remove_reach_zero (c::helds, goal) of
                            NONE => if sum_cards (c::helds) > goal then moves
                                    else careful_moves(cs, c::helds, moves @ [Draw])
                          | SOME cd => moves @ [Discard cd, Draw]
            end
    in
        careful_moves(card_list, [], [])
    end