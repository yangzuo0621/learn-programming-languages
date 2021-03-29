(* 
 * alternate takes a list of numbers and adds them with alternating sign. 
 * For example:
 *   alternate [1,2,3,4] = 1 - 2 + 3 - 4 = -2
 *)
fun alternate (xs : int list) =
  let
    fun help (xs : int list, sign : int) = 
      if null xs
      then 0
    else hd(xs) * sign * ~1 + help(tl(xs), sign * ~1)
  in
    help(xs, ~1)
  end

(* 
 * min_max takes a non-empty list of numbers, and returns a pair (min, max).
 *  min and max are the minimum and maximum of the numbers in the list.
 *)
fun min_max (xs : int list) =
  let
    (* a tuple contains min and max number of first two of numbers list *)
    fun min_max_tuple(x1 : int, x2 : int) = if x1 < x2 then (x1, x2) else (x2, x1)
    fun help (minNumber : int, maxNumber : int, numbers : int list) =
      if null numbers
      then (minNumber, maxNumber)
      else
        let
          val tmp = min_max_tuple(hd(numbers), hd(tl(numbers)))
          val min = if minNumber < (#1 tmp) then minNumber else (#1 tmp)
          val max = if maxNumber > (#2 tmp) then maxNumber else (#2 tmp)
        in
          help(min, max, tl(tl(numbers)))
        end
  in
    if length xs mod 2 = 1 (* list has odd length *)
    then help(hd(xs), hd(xs), tl(xs))
    else 
      let
        val tmp = min_max_tuple(hd(xs), hd(tl(xs)))
      in
        help((#1 tmp), (#2 tmp), tl(tl(xs)))
      end
  end

(*
 * cumsum takes a list of numbers and returns a list of the partial sums of those numbers. 
 * For example:
 *   cumsum [1,4,20] = [1,5,25]
 *)
fun cumsum(xs : int list) =
  let
    fun help(origin : int list, result : int list) =
      if null origin
      then result
      else if null result
           then help(tl(origin), hd(origin)::result)
           else help(tl(origin), (hd(origin) + hd(result))::result)
  in
    (* How to avoid using rev function *)
    rev(help(xs, []))
  end