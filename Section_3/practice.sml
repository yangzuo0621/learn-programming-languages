(* compose_opt : ('b -> 'c option) -> ('a -> 'b option) -> 'a -> 'c option
   composes two functions with "optional" values. 
   If either function returns NONE, then the result is NONE. 
 *)
fun compose_opt f g x =
    case f x of
	NONE => NONE
      | SOME i => case g x of
		      NONE => NONE
		    | y => y


(* do_until : ('a -> 'a) -> ('a -> bool) -> 'a -> 'a 
   do_until f p x will apply f to x and f again to that result and so on until p x is false. 
   Example: do_until (fn x => x div 2) (fn x => x mod 2 <> 1) 
   will evaluate to a function of type int->int that divides its argument by 2 
   until it reaches an odd number. In effect, it will remove all factors of 2 its argument. 
 *)
fun do_until f p x =
    if p x
    then x
    else do_until f p (f x)

(* factorial
   Use do_until to implement factorial. 
 *)
fun factorial n =
    #1 (do_until (fn (acc, x) => (acc*x, x-1)) (fn (_, x) => x = 0) (1, n))


(* fixed_point: (''a -> ''a) -> ''a -> ''a
   Use do_until that given a function f and an initial value x applies f to x until f x = x. 
   (Notice the use of '' to indicate equality types.) 
 *)
fun fixed_point f x =
    do_until f (fn y => f y = x) (f x)


(* map2 : ('a -> 'b) -> 'a * 'a -> 'b * 'b
   that given a function that takes 'a values to 'b values and a pair of 'a values
   returns the corresponding pair of 'b values. 
 *)
fun map2 f (x1, x2) =
    (f x1, f x2)


(* app_all : ('b -> 'c list) -> ('a -> 'b list) -> 'a -> 'c list
   so that: app_all f g x will apply f to every element of the list g x
   and concatenate the results into a single list. 
   For example, for fun f n = [n, 2 * n, 3 * n], we have app_all f f 1 = [1, 2, 3, 2, 4, 6, 3, 6, 9]
 *)
fun app_all f g x =
    let fun transform f y =
	    case y of
		[] => []
	      | ys::ys' => (f ys) @ (transform f ys')
    in
	transform f (g x)
    end


(* Implement List.foldr (see http://sml-family.org/Basis/list.html#SIG:LIST.foldr:VAL). *)


(* partition : ('a -> bool) -> 'a list -> 'a list * 'a list
   where the first part of the result contains the second argument elements 
   for which the first element evaluates to true and the second part of the result contains the other second argument elements.
   Traverse the second argument only once.  
 *)


(* unfold : ('a -> ('b * 'a) option) -> 'a -> 'b list
   produces a list of 'b values given a "seed" of type 'a and a function 
   that given a seed produces SOME of a pair of a 'b value and a new seed, or NONE if it is done seeding. 
   For example, here is an elaborate way to count down from 5: 
   unfold (fn n => if n = 0 then NONE else SOME(n, n-1)) 5 = [5, 4, 3, 2, 1]
 *)


(* Use unfold and foldl to implement factorial. *)


(* Implement map using List.foldr. *)


(* Implement filter using List.foldr. *)


(* Implement foldl using foldr on functions. (This is challenging.) *)


(* Define a (polymorphic) type for binary trees where data is at internal nodes but not at leaves. 
   Define map and fold functions over such trees. 
   You can define filter as well where we interpret a "false" as meaning 
   the entire subtree rooted at the node with data that produced false should be replaced with a leaf. *)

