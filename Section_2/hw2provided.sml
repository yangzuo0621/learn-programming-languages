(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (s, slist) =
    let fun exclude xs =
	    case xs of
		[] => []
	      | x::xs' => if same_string (x, s)
			  then exclude xs'
			  else x::(exclude xs')
	val ys = exclude slist
    in
	if length slist = length ys then NONE
	else SOME ys
    end

fun get_substitutions1 (slist, s) =
    case slist of
	[] => []
      | xs::xs' => (case all_except_option (s, xs) of
		       NONE => get_substitutions1 (xs', s)
		     | SOME ys => ys @ get_substitutions1 (xs', s))

fun get_substitutions2 (slist, s) =
    let fun substitution_help (ls, acc) =
	    case ls of
		[] => acc
	      | xs::xs' => (case all_except_option (s, xs) of
				NONE => substitution_help (xs', acc)
			      | SOME ys => substitution_help (xs', acc @ ys))
    in
	substitution_help (slist, [])
    end

fun similar_names (slist, {first=first_name, middle=middle_name, last=last_name}) =
    let fun help xs =
	    case xs of
		[] => []
	      | x::xs' => {first=x, middle=middle_name, last=last_name} :: help (xs')
    in
	{first=first_name, middle=middle_name, last=last_name} :: help (get_substitutions2 (slist, first_name))
    end



(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color c =
    case c of
	(Clubs, _) => Black
      | (Spades, _) => Black
      | (Diamonds, _) => Red
      | (Hearts, _) => Red

fun card_value c =
    case c of
	(_, Num i) => i
      | (_, Ace) => 11
      | (_, _) => 10

fun remove_card (cs, c, e) =
    case cs of
	[] => raise e
      | x::cs' => if x = c then cs'
		  else x :: remove_card (cs', c, e)

fun all_same_color cs =
    case cs of
	[] => true
      | x::[] => true
      | x::y::cs' => if card_color x = card_color y
		     then all_same_color (y::cs')
		     else false

fun sum_cards cs =
    let fun sum_help (cs, sum) =
	    case cs of
		[] => sum
	      | c::cs' => sum_help (cs', sum + card_value c)
    in
	sum_help (cs, 0)
    end

fun score (cs, goal) =
    let val sum = sum_cards cs
	val preliminary_score = if sum > goal then 3 * (sum - goal) else goal - sum
    in
	if all_same_color cs
	then preliminary_score div 2
	else preliminary_score
    end

