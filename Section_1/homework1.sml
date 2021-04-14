fun is_older (date1 : int * int * int, date2 : int * int * int) =
    if #1 date1 < #1 date2 orelse (#1 date1 = #1 date2 andalso #2 date1 < #2 date2) orelse (#1 date1 = #1 date2 andalso #2 date1 = #2 date2 andalso #3 date1 < #3 date2)
    then true
    else false

fun number_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then 0
    else if #2 (hd dates) = month
    then 1 + number_in_month (tl dates, month)
    else number_in_month (tl dates, month)

fun number_in_months (dates: (int * int * int) list, months : int list) =
    if null months
    then 0
    else number_in_month (dates, hd months) + number_in_months (dates, tl months)

fun dates_in_month (dates: (int * int * int) list, month : int) =
    if null dates
    then []
    else if #2 (hd dates) = month
    then (hd dates) :: dates_in_month (tl dates, month)
    else dates_in_month (tl dates, month)

fun dates_in_months (dates: (int * int * int) list, months : int list) =
    if null months
    then []
    else dates_in_month (dates, hd months) @ dates_in_months (dates, tl months)

fun get_nth (ls : string list, index : int) =
    if null ls orelse index <= 0
    then ""
    else if index = 1
    then hd ls
    else get_nth (tl ls, index - 1)

fun date_to_string (date : int * int * int) =
    let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth (months, #2 date) ^ " " ^ Int.toString (#3 date) ^ ", " ^ Int.toString (#1 date)
    end

fun number_before_reaching_sum (sum : int, xs : int list) =
    let fun helper (xs : int list, acc : int, cnt : int) =
	    if null xs
	    then cnt
	    else if acc < sum
	    then helper(tl xs, acc + hd xs, cnt + 1)
	    else cnt - 1
    in
	helper (xs, 0, 0)
    end

fun what_month (day : int) =
    let val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	number_before_reaching_sum (day, days) + 1
    end

fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month (day1) :: month_range (day1 + 1, day2)

fun oldest (dates : (int * int * int) list) =
    if null dates
    then NONE
    else if null (tl dates)
    then SOME (hd dates)
    else let val d = valOf (oldest (tl dates))
	 in
	     if is_older ((hd dates), d) then SOME (hd dates) else SOME d
	 end
