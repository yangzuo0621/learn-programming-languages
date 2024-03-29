(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
       | Unit
       | Tuple of valu list
       | Constructor of string * valu

fun g f1 f2 p =
    let
	val r = g f1 f2
    in
	case p of
	   Wildcard          => f1 ()
	 | Variable x        => f2 x
	 | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	 | ConstructorP(_,p) => r p
	 | _                 => 0
    end

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
fun only_capitals slist =
    List.filter (fn s => Char.isUpper (String.sub(s, 0))) slist

fun longest_string1 slist =
    List.foldl (fn (a, b) => if String.size(a) > String.size(b) then a else b) "" slist

fun longest_string2 slist =
    List.foldl (fn (a, b) => if String.size(a) >= String.size(b) then a else b) "" slist

fun longest_string_helper f =
    List.foldl (fn (a, b) => if f (String.size(a), String.size(b))
			     then a
			     else b) ""

val longest_string3 = longest_string_helper (fn (x, y) => x > y)
val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f xlist =
    case xlist of
	[] => raise NoAnswer
      | x::xs' => case f x of
		      SOME v => v
		    | _ => first_answer f xs'

fun all_answers f xlist =
    let fun accumulator (acc, slist) =
	    case slist of
		[] => SOME acc
	      | x::xs' => case f x of
			      NONE => NONE
			    | SOME v => accumulator (v@acc, xs')
    in
	accumulator ([], xlist)
    end


val count_wildcards = g (fn () => 1) (fn x => 0)
val count_wild_and_variable_lengths = g (fn () => 1) String.size
fun count_some_var (x, p) =
    g (fn () => 0) (fn y => if x = y then 1 else 0) p

fun check_pat p =
    let fun extract_all_variables p =
	    case p of
		Variable x => [x]
	      | TupleP ps => List.foldl (fn (p, i) => extract_all_variables(p) @ i) [] ps
	      | ConstructorP (_, p) => extract_all_variables p
	      |  _ => []
	fun unique xs =
	    case xs of
		[] => true
	      | x::[] => true
	      | x::xs' => (not (List.exists (fn y => x = y) xs')) andalso unique xs'
    in
	unique (extract_all_variables p)
    end
	
fun match (v, p) =
    case (v, p) of
	(_, Wildcard) => SOME []
      | (_, Variable s) => SOME [(s,v)]
      | (Unit, UnitP) => SOME []
      | (Const x, ConstP y) => if x = y then SOME [] else NONE
      | (Tuple vs, TupleP ps) => if length vs = length ps
				 then all_answers match (ListPair.zip(vs, ps))
				 else NONE
      | (Constructor(s2,v), ConstructorP(s1,p)) => if s1 = s2 then match (v,p) else NONE
      | _ => NONE

fun first_match v pl =
    SOME (first_answer (fn p => match (v, p)) pl)
    handle NoAnswer => NONE

			   
(**** for the challenge problem only ****)
