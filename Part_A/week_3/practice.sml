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
      { grade=SOME i, id=_ } => if i >= 75 then pass else fail
    | _ => fail

(* 
   has_passed of type {grade : int option, id : 'a} -> bool
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
    | xs :: xs' => (if has_passed xs = true then 1 else 0) + number_passed xs'

(* 
   number_misgraded of type (pass_fail * final_grade) list -> int 
   indicates how many list elements are "mislabeled" 
   where mislabeling means a pair (pass,x) where has_passed x is false 
   or (fail,x) where has_passed x is true. 
*)
fun number_misgraded lists =
  case lists of
      [] => 0
    | (pass, x) :: xs' => (if has_passed x = false then 1 else 0) + number_misgraded xs'
    | (fail, x) :: xs' => (if has_passed x = true then 1 else 0) + number_misgraded xs'


datatype 'a tree = leaf
                | node of { value : 'a, left : 'a tree, right : 'a tree }
datatype flag = leave_me_alone | prune_me

(* 
   tree_height of type 'a tree -> int
   evaluates to a height of this tree. 
   The height of a tree is the length of the longest path to a leaf. 
   Thus the height of a leaf is 0. 
*)
fun tree_height root =
  case root of
      leaf => 0
    | node { value, left, right } => 1 + Int.max(tree_height left, tree_height right)

(* 
   sum_tree of type int tree -> int
   evaluates to the sum of all values in the nodes. 
*)
fun sum_tree root =
  case root of
      leaf => 0
    | node { value, left, right } => value + sum_tree left + sum_tree right

(* 
   gardener of type flag tree -> flag tree
   its structure is identical to the original tree 
   except all nodes of the input containing prune_me are (along with all their descendants) 
   replaced with a leaf. 
*)
(* fun gardener root =
  case  *)

(* 
   A "natural" number is either zero, or the "successor" of a another integer.
   For example the number 1 is just SUCC ZERO, the number 2 is SUCC (SUCC ZERO), and so on. 
*)
datatype nat = ZERO | SUCC of nat

(* 
   is_positive : nat -> bool
   given a "natural number" returns whether that number is positive (i.e. not zero). 
*)
fun is_positive num =
  case num of
      ZERO => false
    | _ => true

exception Negative

(* 
   pred : nat -> nat
   given a "natural number" returns its predecessor. 
   Since 0 does not have a predecessor in the natural numbers, 
   throw an exception Negative (will need to define it first). 
*)
fun pred num =
  case num of
      ZERO => raise Negative
    | SUCC ZERO => ZERO
    | SUCC preNat => preNat

(* 
   nat_to_int : nat -> int
   given a "natural number" returns the corresponding int. 
   For example, nat_to_int (SUCC (SUCC ZERO)) = 2
   (Do not use this function for problems 13-16 -- it makes them too easy.) 
*)
fun nat_to_int num =
  case num of
      ZERO => 0
    | SUCC preNat => 1 + nat_to_int preNat

(* int_to_nat : int -> nat
   given an integer returns a "natural number" representation for it, 
   or throws a Negative exception if the integer was negative.
   (Again, do not use this function in the next few problems.) 
*)
fun int_to_nat num =
  case num of
      0 => ZERO
    | 1 => SUCC ZERO
    | x => if x < 0 then raise Negative else SUCC (int_to_nat(x-1))

(* 13.
   add : nat * nat -> nat
   perform addition. 
*)
fun add natPair =
  case natPair of
      (ZERO, ZERO) => ZERO
    | (ZERO, SUCC y) => SUCC (add(ZERO, y))
    | (SUCC x, ZERO) => SUCC (add(x, ZERO))
    | (SUCC x, SUCC y) => SUCC (SUCC (add(x, y)))

(* 14. 
   sub : nat * nat -> nat
   perform subtraction.
*)
fun sub natPair =
  case natPair of
      (ZERO, SUCC _) => raise Negative
    | (ZERO, ZERO) => ZERO
    | (x, ZERO) => x
    | (SUCC x, y) => sub (x, pred(y))

(* 15. 
   mult : nat * nat -> nat
   perform multiplication.
*)
fun mult natPair =
  case natPair of
      (ZERO, _) => ZERO
    | (_, ZERO) => ZERO
    | (x, SUCC ZERO) => x
    | (x, SUCC (SUCC y)) => add (mult(x, y), add(x, x))

(* 16. 
   less_than : nat * nat -> bool
   return true when the first argument is less than the second. 
*)
fun less_than natPair =
  case natPair of
      (ZERO, _) => false
    | (SUCC _, ZERO) => true
    | (SUCC x, SUCC y) => less_than(x, y)


datatype intSet = 
  Elems of int list (*list of integers, possibly with duplicates to be ignored*)
| Range of { from : int, to : int }  (* integers from one number to another *)
| Union of intSet * intSet (* union of the two sets *)
| Intersection of intSet * intSet (* intersection of the two sets *)

(* 17. 
   isEmpty : intSet -> bool 
   determines if the set is empty or not. 
*)
(* fun isEmpty sets =
  case sets of
      Elems [] => true
    | Union ([], []) => true
    |  *)


(* 18. 
   contains: intSet * int -> bool
   returns whether the set contains a certain element or not. 
*)

(* 19. 
   toList : intSet -> int list 
   returns a list with the set's elements, without duplicates. 
*)