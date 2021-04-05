type student_id = int
type grade = int (* must be in 0 to 100 range *)
type final_grade = { id : student_id, grade : grade option }
datatype pass_fail = pass | fail

(* 
   pass_or_fail of type {grade : int option, id : 'a} -> pass_fail
   takes a final_grade (or, as the type indicates, a more general type) and 
   returns pass if the grade field contains SOME i for an i >= 75 (else fail). 
*)
fun pass_or_fail g =
  case g of
      { grade=SOME i, id=_ } => if i >= 75
                                  then pass
                                  else fail
    | _ => fail

(* 
   has_passed of type {grade : int option, id : ’a} -> bool
   use pass_or_fail as a helper function,
   returns true if and only if the the grade field contains SOME i for an i >= 75. *)
fun has_passed g =
  case pass_or_fail g of
      pass => true
    | fail => false

(* 
   number_passed of type {grade:int option, id:'a} list -> int
   use has_passed as a helper function, 
   returns how many list elements have passing (again, >= 75) grades. 
*)
fun number_passed lists =
  case lists of
      [] => 0
    | xs :: xs' => if has_passed xs = true
                   then 1 + number_passed xs'
                   else number_passed xs'

(* 
   number_misgraded of type (pass_fail * final_grade) list -> int 
   indicates how many list elements are "mislabeled" 
   where mislabeling means a pair (pass,x) where has_passed x is false 
   or (fail,x) where has_passed x is true. 
*)
fun number_misgraded lists =
  case lists of
      [] => 0
    | (pass, x) :: xs' => if has_passed x = false
                          then 1 + number_misgraded xs'
                          else number_misgraded xs'
    | (fail, x) :: xs' => if has_passed x = true
                          then 1 + number_misgraded xs'
                          else number_misgraded xs'
