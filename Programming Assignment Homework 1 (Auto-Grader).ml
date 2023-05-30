(* Problem 1*)
fun is_older(date1 : int * int * int, date2 : int * int * int) =
    if (#1 date1) < (#1 date2) 
    then true
    else if(#1 date1) > (#1 date2)
    then false
    else
        if (#2 date1) < (#2 date2)
        then true
        else if(#2 date1) > (#2 date2)
        then false
        else
            if(#3 date1) < (#3 date2)
            then true
            else false
(*Problem 2*)
fun number_in_month(dates : (int * int * int) list, month : int) =
    if null dates
    then 0
    else if #2 (hd dates) = month
    then number_in_month(tl dates, month) + 1
    else number_in_month(tl dates, month)
(*Problem 3*)
fun number_in_months (dates: (int * int * int) list, months: int list) =
    if null months
    then 0
    else if null(tl(months)) 
    then number_in_month(dates, hd(months))
    else number_in_month(dates, hd(months)) + number_in_months(dates, tl(months))
(*Problem 4*)
fun dates_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then []
    else if #2 (hd dates) = month
    then (hd dates)::dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)
(*Problem 5*)
fun dates_in_months(dates: (int * int * int) list, months: int list) =
    if null months
    then []
    else if null(tl(months)) 
    then dates_in_month(dates, hd(months))
    else dates_in_month(dates, hd(months)) @ dates_in_months(dates, tl(months))
(*Problem 6*)
fun get_nth(string_list: string list, n: int) =
    if n = 1
    then hd(string_list)
    else get_nth(tl(string_list), n-1)
(*Problem 7*)
fun date_to_string (date : int * int * int) =
    let 
        val names = ["January", "February", "March", "April", "May", "June",
		                 "July", "August", "September", "October", "November", "December"]
    in
	    get_nth(names,#2 date) ^ " " ^ Int.toString(#3 date) 
	    ^ ", " ^ Int.toString(#1 date)
    end
(*Problem 8*)
fun number_before_reaching_sum(sum: int, numbers: int list) =
    if hd(numbers) < sum
    then 1 + number_before_reaching_sum(sum - (hd(numbers)), tl(numbers))
    else 0
(*Problem 9*)
fun what_month(day: int) =
    let
        val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum(day, days_in_months) + 1
    end
(*Problem 10*)
fun month_range(day1: int, day2: int)=
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)
(*Problem 11*)
fun oldest(dates: (int * int * int) list) =
    if null dates
    then NONE
    else
        let 
            val tl_oldest = oldest(tl(dates))
        in
            if isSome tl_oldest andalso is_older(valOf tl_oldest, hd dates)
            then tl_oldest
            else SOME(hd dates)
        end
(*Problem 12*)
(* quadratic algorithm rather than sorting which is nlog n *)
fun mem(x : int, xs : int list) =
    not (null xs) andalso (x = hd xs orelse mem(x, tl xs))
fun remove_duplicates(xs : int list) =
    if null xs
    then []
    else
        let 
            val tl_ans = remove_duplicates (tl xs)
        in
            if mem(hd xs, tl_ans)
            then tl_ans
            else (hd xs)::tl_ans
        end
(*Problem 13*)
fun reasonable_date(date: int * int * int) =
    if #1 date < 1
    then false
    else if (#2 date < 1) orelse (#2 date > 12)
    then false
    else if (#3 date < 1) orelse (#3 date > 31)
    then false
    else if (#3 date = 31)
    then not (#2 date = 2 orelse (#2 date = 4) orelse (#2 date = 6) orelse (#2 date = 9) orelse (#2 date = 11))
    else if (#3 date = 29) andalso (#2 date = 2)
    then
        if not(#1 date mod 4 = 0)
        then false
        else if #1 date mod 100 = 0
        then #1 date mod 400 = 0
        else true
    else true