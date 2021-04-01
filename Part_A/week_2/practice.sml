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
   For example: zip ([1,2,3], [4,6]) = [(1,4), (2,6)]. 
*)
fun zip(xs : int list, ys : int list) =
  if null xs orelse null ys
  then []
  else (hd xs, hd ys) :: zip(tl xs, tl ys)

(* zipRecycle : int list * int list -> int * int
   creates pairs when one list is empty it starts recycling from its start until the other list completes. 
   For example: 
     zipRecycle ([1,2,3], [1, 2, 3, 4, 5, 6, 7]) = [(1,1), (2,2), (3,3), (1,4), (2,5), (3,6), (1,7)]
*)
fun zipRecycle(xs : int list, ys : int list) =
  if null ys orelse null xs 
  then []
  else
    let
      fun zip(x : int list, y : int list) =
        if null y
        then []
        else if null x
             then (hd xs, hd y) :: zip(tl xs, tl y)
             else (hd x, hd y) :: zip(tl x, tl y)
    in
      zip(xs, ys)
    end

(* zipOpt : int list * int list -> option
   return SOME of a list when the original lists have the same length
   return NONE if they do not. 
*)
fun zipOpt(xs : int list, ys : int list) =
  if null xs orelse null ys
  then NONE
  else
    let
      fun help(xs : int list, ys : int list) =
        if null xs andalso null ys then SOME []
        else if (not(null xs) andalso null ys) orelse (null xs andalso not(null ys))
        then NONE
        else
          let 
            val head = (hd xs, hd ys)
            val tmp = help(tl xs, tl ys)
          in
            if isSome tmp then SOME(head::(valOf tmp))
            else NONE
          end
    in
      help(xs, ys)
    end

(* lookup : (string * int) list * string -> int option 
   takes a list of pairs (s, i) and also a string s2 to look up. 
   It then goes through the list of pairs looking for the string s2 in the first component. 
   If it finds a match with corresponding number i, then it returns SOME i. 
   If it does not, it returns NONE. 
*)
fun lookup (dict : (string * int) list, key : string) =
  if null dict then NONE
  else if #1(hd dict) = key then SOME (#2(hd dict))
  else lookup(tl dict, key)

(* splitAt : int list -> int list * int list 
   given a list of integers creates two lists of integers, 
   one containing entries that greater and equal than threshold, the other containing entries less than threshold. 
   Relative order must be preserved: 
   All non-negative entries must appear in the same order in which they were on the original list, 
   and similarly for the negative entries. 
*)
fun splitAt(xs : int list, threshold : int) =
  let
    fun split(xs : int list, nonnegative : int list, negative : int list) =
      if null xs then (nonnegative, negative)
      else if hd xs >= threshold then split(tl xs, (hd xs)::nonnegative, negative)
      else split(tl xs, nonnegative, (hd xs)::negative)
  in
    split(rev xs, [], [])
  end
  
(* splitup : int list -> int list * int list 
   given a list of integers creates two lists of integers, 
   one containing the non-negative entries, the other containing the negative entries. 
   Relative order must be preserved: 
   All non-negative entries must appear in the same order in which they were on the original list, 
   and similarly for the negative entries. 
*)
fun splitup(xs : int list) =
  splitAt(xs, 0)

(* isSorted : int list -> boolean 
   given a list of integers determines whether the list is sorted in increasing order. 
*)
fun isSorted(xs : int list) =
  if null xs orelse null (tl xs) then true
  else hd xs <= hd(tl xs) andalso isSorted(tl xs)

(* isAnySorted : int list -> boolean 
   given a list of integers determines whether the list is sorted in either increasing or decreasing order. 
*)
fun isAnySorted(xs : int list) =
  let fun sortHelp(ys : int list, isIncreasing : bool) =
    if null ys orelse null (tl ys) then true
    else if isIncreasing = true 
         then hd ys <= hd(tl ys) andalso sortHelp(tl ys, isIncreasing)
         else hd ys >= hd(tl ys) andalso sortHelp(tl ys, isIncreasing)
  in
    if null xs orelse null (tl xs) then true
    else if hd xs = hd(tl xs) then isAnySorted(tl xs)
    else sortHelp(xs, hd xs < hd(tl xs))
  end

(* sortedMerge : int list * int list -> int list 
   takes two lists of integers that are each sorted from smallest to largest, 
   and merges them into one sorted list. 
   For example: sortedMerge ([1,4,7], [5,8,9]) = [1,4,5,7,8,9]
*)
fun sortedMerge(xs : int list, ys : int list) =
  if null xs then ys
  else if null ys then xs
  else if hd xs < hd ys
       then hd(xs) :: sortedMerge(tl xs, ys)
       else hd(ys) :: sortedMerge(xs, tl ys)
