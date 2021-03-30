(* 
 * alternate takes a list of numbers and adds them with alternating sign. 
 * For example:
 *   alternate [1,2,3,4] = 1 - 2 + 3 - 4 = -2
 *   1-2+3-4 = 1-(2-3+4) = 1-(2-(3-4))
 *)
fun alternate (nums : int list) =
  if null nums 
  then 0 
  else hd nums - alternate(tl nums)

(* 
 * min_max takes a non-empty list of numbers, and returns a pair (min, max).
 *  min and max are the minimum and maximum of the numbers in the list.
 *)
fun min_max (xs : int list) =
  if null (tl xs) 
  then (hd xs, hd xs)
  else
    let
      val head = hd xs
      val tmp = min_max(tl xs)
      val min = if (#1 tmp) < head then (#1 tmp) else head
      val max = if (#2 tmp) > head then (#2 tmp) else head
    in
      (min, max)
    end

(*
 * cumsum takes a list of numbers and returns a list of the partial sums of those numbers. 
 * For example:
 *   cumsum [1,4,20] = [1,5,25]
 *)
fun cumsum(xs : int list) =
  if null xs then []
  else
    let fun help(ys : int list, y : int) =
      if null ys then []
      else (hd ys + y) :: help(tl ys, hd ys + y)
  in
    (hd xs) :: help(tl xs, hd xs)
  end

fun greeting(name : string option) =
  if isSome name then "Hello there, " ^ valOf name ^ "!"
  else "Hello there, you!"

(*
 * repeat repeats the integers in the first list according to the numbers indicated by the second list. 
 * For example: 
 *   repeat ([1,2,3], [4,0,3]) = [1,1,1,1,3,3,3]
 *)
fun repeat(xs : int list, ys : int list) =
  if null ys then []
  else if hd(ys) = 0
       then repeat(tl xs, tl ys)
       else hd(xs) :: repeat(xs, (hd ys - 1)::tl(ys))

(* addOpt : int option * int option -> int option
   returns SOME of their sum if they are both present 
   otherwise, returns NONE if at least one of the two arguments is NONE. 
 *)
fun addOpt(x1: int option, x2: int option) = 
  if isSome x1 andalso isSome x2
  then SOME(valOf x1 + valOf x2) 
  else NONE

(* addAllOpt : int option list -> int option
   adds those integers that are there (i.e. adds all the SOME i). 
   If the list does not contain any SOME is in it, i.e. they are all NONE or the list is empty, 
   the function should return NONE. 
   For example: 
     addAllOpt ([SOME 1, NONE, SOME 3]) = SOME 4
     addAllOpt ([NONE, NONE, NONE]) = NONE
     addAllOpt ([]) = NONE
*)
fun addAllOpt(xs : int option list) = 
  if null xs
  then NONE
  else
    let
      val head = hd xs
      val others = addAllOpt(tl xs)
    in
      if isSome head andalso isSome others then SOME(valOf head + valOf others)
      else if isSome head then head
      else if isSome others then others
      else NONE
    end

(* any : bool list -> bool
   returns true if there is at least one of them that is true, 
   otherwise returns false. 
   If the list is empty it should return false because there is no true.
*)
fun any(pres : bool list) =
  if null pres then false
  else if hd pres = true then true
  else any(tl pres)

(* all : bool list -> bool
   returns true if all of them true, 
   otherwise returns false. 
   If the list is empty it should return true because there is no false.
*)
fun all(pres : bool list) =
  if null pres then true
  else if hd pres = false then false
  else all(tl pres)

(* zip : int list * int list -> int * int
   given two lists of integers creates consecutive pairs, and stops when one of the lists is empty. 
   For example: zip ([1,2,3], [4, 6]) = [(1,4), (2,6)]. 
*)
fun zip(xs : int list, ys : int list) =
  if null xs orelse null ys
  then []
  else (hd xs, hd ys) :: zip(tl xs, tl ys)
