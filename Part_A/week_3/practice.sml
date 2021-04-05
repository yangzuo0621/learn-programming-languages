(* 
 * alternate takes a list of numbers and adds them with alternating sign. 
 * For example:
 *   alternate [1,2,3,4] = 1 - 2 + 3 - 4 = -2
 *   1-2+3-4 = 1-(2-3+4) = 1-(2-(3-4))
 *)
fun alternate nums =
  case nums of 
      [] => 0
    | num :: xs' => num - alternate(xs')

(* 
 * min_max takes a non-empty list of numbers, and returns a pair (min, max).
 *  min and max are the minimum and maximum of the numbers in the list.
 *)
fun min_max xs =
  case xs of
      [] => raise List.Empty
    | x :: [] => (x, x)
    | x :: xs' => let val (min, max) = min_max(xs')
                  in
                    (Int.min(x, min), Int.max(x, max))
                  end

(*
 * cumsum takes a list of numbers and returns a list of the partial sums of those numbers. 
 * For example:
 *   cumsum [1,4,20] = [1,5,25]
 *)
fun cumsum xs =
  let 
    fun help (ys, accumulation) =
      case ys of
          [] => []
        | y :: ys' => (y + accumulation) :: cumsum(ys')
  in
    help (xs, 0)
  end

fun greeting name =
  case name of 
      NONE => "Hello there, you!"
    | SOME s => "Hello there, " ^ s ^ "!"

(*
 * repeat repeats the integers in the first list according to the numbers indicated by the second list. 
 * For example: 
 *   repeat ([1,2,3], [4,0,3]) = [1,1,1,1,3,3,3]
 *)
fun repeat(xs, ys) =
  case ys of 
      [] => []
    | 0 :: ys' => repeat(tl xs, ys')
    | y :: ys' => (hd xs) :: repeat(xs, (y-1)::ys')

(* addOpt : int option * int option -> int option
   returns SOME of their sum if they are both present 
   otherwise, returns NONE if at least one of the two arguments is NONE. 
 *)
(* fun addOpt x = 
  case x of 
      (SOME i, SOME j) => SOME(i+j)
    | _ => NONE *)

fun addOpt (SOME i, SOME j) = SOME(i+j)
  | addOpt (_, _) = NONE

(* addAllOpt : int option list -> int option
   adds those integers that are there (i.e. adds all the SOME i). 
   If the list does not contain any SOME is in it, i.e. they are all NONE or the list is empty, 
   the function should return NONE. 
   For example: 
     addAllOpt ([SOME 1, NONE, SOME 3]) = SOME 4
     addAllOpt ([NONE, NONE, NONE]) = NONE
     addAllOpt ([]) = NONE
*)
fun addAllOpt xs = 
  case xs of
      [] => NONE
    | x :: [] => x
    | NONE :: xs' => addAllOpt(xs')
    | SOME(x) :: SOME(y)::xs' => addAllOpt(SOME(x+y)::xs')
    | SOME(x) :: NONE :: xs' => addAllOpt(SOME(x)::xs')

(* any : bool list -> bool
   returns true if there is at least one of them that is true, 
   otherwise returns false. 
   If the list is empty it should return false because there is no true.
*)
fun any pres =
  case pres of
      [] => false
    | true :: xs' => true
    | _ :: xs' => any xs'

(* all : bool list -> bool
   returns true if all of them true, 
   otherwise returns false. 
   If the list is empty it should return true because there is no false.
*)
fun all ([]) = true
  | all (false :: xs') = false
  | all (_ :: xs') = all xs'

(* zip : int list * int list -> (int * int) list
   given two lists of integers creates consecutive pairs, and stops when one of the lists is empty. 
   For example: zip ([1,2,3], [4,6]) = [(1,4), (2,6)]. 
*)
fun zip(xs : int list, ys : int list) =
  if null xs orelse null ys
  then []
  else (hd xs, hd ys) :: zip(tl xs, tl ys)

(* zipRecycle : int list * int list -> (int * int) list
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
   Relative order must be preserved.
*)
fun splitAt(xs : int list, threshold : int) =
  if null xs then ([], [])
  else
    let
      val head = hd xs
      val tmp = splitAt(tl xs, threshold)
    in
      if head >= threshold
      then (head::(#1 tmp), #2 tmp)
      else (#1 tmp, head::(#2 tmp))
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

(* qsort : int list -> int list
   Takes the first element out, and uses it as the "threshold" for splitAt. 
   It then recursively sorts the two lists produced by splitAt. 
   Finally it brings the two lists together. 
*)
fun qsort(xs : int list) =
  if null xs then []
  else if null (tl xs) then xs
  else
    let
      val threshold = hd xs
      val twoParts = splitAt(tl xs, threshold)
      val greaterPart = qsort(#1 twoParts)
      val lessPart = qsort(#2 twoParts)
    in
      sortedMerge(lessPart, threshold::greaterPart)
    end

(* divide : int list -> int list * int list
   takes a list of integers and produces two lists by alternating elements between the two lists.
   For example: divide ([1,2,3,4,5,6,7]) = ([1,3,5,7], [2,4,6])
*)
fun divide (xs : int list) =
  let 
    fun divideHelp (ys : int list, index : int) =
      if null ys then ([], [])
      else 
        let
          val tmp = divideHelp(tl ys, index+1)
          val oddPart = #1 tmp
          val evenPart = #2 tmp
        in
          if index mod 2 = 1
          then ((hd ys)::oddPart, evenPart)
          else (oddPart, (hd ys)::evenPart)
        end
  in
    divideHelp(xs, 1)
  end

(* not_so_quick_sort : int list -> int list
   Given the initial list of integers, splits it in two lists using divide, 
   then recursively sorts those two lists, then merges them together with sortedMerge.  
*)
fun not_so_quick_sort(xs : int list) =
  if null xs then []
  else if null(tl xs) then xs
  else
    let
      val tmp = divide(xs)
      val firstPart = not_so_quick_sort(#1 tmp)
      val secondPart = not_so_quick_sort(#2 tmp)
    in
      sortedMerge(firstPart, secondPart)
    end

(* fullDivide : int * int -> int * int
   given two numbers k and n it attempts to evenly divide k into n as many times as possible, 
   and returns a pair (d, n2) where d is the number of times 
   while n2 is the resulting n after all those divisions. 
   Examples: 
     fullDivide (2, 40) = (3, 5)  because 2*2*2*5 = 40
     fullDivide(3, 10) = (0, 10) because 3 does not divide 10. 
*)
fun fullDivide(k : int, n : int) =
  if k = 0 orelse n = 0 then (0, 0)
  else
    let 
      fun countHelp(k : int, n2 : int, count : int) =
        if n2 mod k = 0
        then countHelp(k, n2 div k, count + 1)
        else (count, n2)
    in
      countHelp(k, n, 0)
    end

(* factorize : int -> (int * int) list
   given a number n returns a list of pairs (d, k)
   where d is a prime number dividing n and k is the number of times it fits. 
   The pairs should be in increasing order of prime factor, 
   and the process should stop when the divisor considered surpasses the square root of n. 
   If you make sure to use the reduced number n2 given by fullDivide for each next step, 
   you should not need to test if the divisors are prime: If a number divides into n, it must be prime 
   (if it had prime factors, they would have been earlier prime factors of n and thus reduced earlier). 
   Examples: 
     factorize(20) = [(2,2), (5,1)]
     factorize(36) = [(2,2), (3,2)]
     factorize(1) = [] 
*)
fun factorize(x : int) =
  let fun factorizeHelp(d : int, k : int) =
    if d = 1 then []
    else if d mod k <> 0 then factorizeHelp(d, k+1)
    else if d = k then [(k, 1)]
    else
      let
        val tmp = fullDivide(k, d)
      in
        (k, #1 tmp) :: factorizeHelp(#2 tmp, 2)
      end
  in
    factorizeHelp(x, 2)
  end

(* multiply : (int * int) list -> int
   given a factorization of a number n as described in the previous problem computes back the number n.
   So this should do the opposite of factorize. 
*)
fun multiply(pairs : (int * int) list) =
  let fun mul(x : int, count : int) =
    if count = 1 then x else x * mul(x, count - 1)
  in
    if null pairs then 1
    else mul((#1 (hd pairs)), (#2 (hd pairs))) * multiply(tl pairs)
  end

(* all_products : (int * int) list -> int list
   given a factorization list result from factorize creates a list all of possible products 
   produced from using some or all of those prime factors no more than the number of times they are available. 
   This should end up being a list of all the divisors of the number n that gave rise to the list. 
   For extra challenge, your recursive process should return the numbers in this order, as opposed to sorting them afterwards. 
   Example: 
     all_products([(2,2), (5,1)]) = [1,2,4,5,10,20]
*)
(* ugly implemetation, generateProductList is a tree recursion, and use sort and remove duplicate assistant func
   TODO: use more efficient data structure to remove duplicate elements *)
fun all_products(factors : (int*int) list) =
  let
    fun removeDuplicate(xs : int list) =
      if null xs then []
      else if null (tl xs) then xs
      else if hd(xs) = hd(tl xs) then removeDuplicate(tl xs)
      else (hd xs)::removeDuplicate(tl xs)
    
    fun generateFactorList(xs : (int*int) list) =
      if null xs then []
      else if #2 (hd xs) = 0
      then generateFactorList(tl xs)
      else 
        let
          val factor = #1 (hd xs)
          val count = #2 (hd xs)
        in
          factor :: generateFactorList((factor, count-1)::(tl xs))
        end

    fun generateProductList(xs : int list, ys : int list, product : int) =    
      if null xs then ys
      else
        let
          val x = generateProductList(tl xs, product::ys, product)
          val y = generateProductList(tl xs, (product*hd(xs))::ys, product*(hd xs))
          val x = qsort(x)
          val y = qsort(y)
        in
          removeDuplicate(sortedMerge(x, y))
        end
  in
    generateProductList(generateFactorList(factors), [], 1)
  end
