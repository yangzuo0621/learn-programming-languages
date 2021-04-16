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

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
fun only_captitals slist =
    List.filter (fn s => Char.isUpper (String.sub(s, 0))) slist

fun longest_string1 slist =
    foldl (fn (a, b) => if String.size(b) >= String.size(a) then b else a) "" slist

fun longest_string2 slist =
    foldl (fn (a, b) => if String.size(a) >= String.size(b) then a else b) "" slist
