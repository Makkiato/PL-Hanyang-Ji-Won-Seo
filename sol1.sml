(* Question 1  *)
val temp = 4 :: [2,3]

fun merge(list1 : int list, list2 : int list) =
    if null list1 andalso null list2 then []
    else if null list1 then hd list2 :: merge(list1,tl list2)
    else if null list2 then hd list1 :: merge(tl list1,list2)
    else
        if hd list1 = hd list2 then hd list1 :: merge(tl list1, tl list2)
        else if hd list1 < hd list2 then hd list1 :: merge(tl list1,list2)
        else hd list2 :: merge(list1,tl list2)

val mergeVal = merge([1,4,7],[2,4,9])

(* Question 2  *)


fun append (xs : int list, ys: int list)=
    if null xs then ys else hd(xs)::append(tl(xs),ys);
    (* append by lecture slide *)

fun reverse(list1 : int list)=
    if null (tl list1) then hd list1 :: []
    else append(reverse(tl list1),hd list1 :: [])
   
val reverseVal = reverse([1,5,7,4])


(* Question 3  *)
fun sigma(init: int, dest: int, eval:int->int)=
        if init = dest then eval(init) else eval(init)+sigma(init+1,dest,eval)

fun sample(x: int)=
    x*x;

val resultByDefinedFunction = sigma(1,5,sample)
val resultByAnnonymousFunction = sigma(1,5,fn para => para*para*para)

(* Question 4  *)
fun digitProcess (num : int)=
        if num < 10 then num :: []
        else num mod 10 :: digitProcess (num div 10)

fun digits(num : int)=
        reverse(digitProcess num)

val digitsVal = digits(5839)

(* Question 5  *)
fun getDigitSum(num: int)=
    if num<10 then num else num mod 10+getDigitSum(num div 10)

fun additivePersistence(num: int)=
    if num<10 then 0 else additivePersistence(getDigitSum(num))+1

fun digitalRoot(num:int)=
    if num<10 then num else digitalRoot(getDigitSum(num))
    
val qFiveAdditive1 = additivePersistence(9876)
val qFiveRoot1 = digitalRoot(9876)

val qFiveAdditive2 = additivePersistence(12349)
val qFiveRoot2 = digitalRoot(12349)