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
