(*
    2015004711 어유선

*)

(* Question1 *)
datatype expr = NUM of int
    | PLUS of expr * expr
    | MINUS of expr * expr
datatype formula = TRUE
    | FALSE
    | NOT of formula
    | ANDALSO of formula * formula
    | ORELSE of formula * formula
    | IMPLY of formula * formula
    | LESS of expr * expr
    
fun exprEval(val1 : expr) =
    case val1 of
        PLUS(a,b) => exprEval(a)+exprEval(b)
        |MINUS(a,b) => exprEval(a)-exprEval(b)
        |NUM(a) => a

fun eval (formula1 : formula) =
    case formula1 of
        TRUE => true
        |FALSE => false
        |NOT(inner1) => let fun N(in1 : formula) =
                    if (eval(in1) = true) then false else true
                in
                    N(inner1)
                end 
        |ANDALSO(inner1,inner2) =>   let fun AA(in1 : formula, in2 : formula) =
                                        if (eval(in1) = true andalso eval(in2) = true ) then true else false
                                    in
                                        AA(inner1,inner2)
                                    end 
    
        | ORELSE(inner1,inner2) =>  let fun OE(in1 : formula, in2 : formula) =
                                        if (eval(in1) = false andalso eval(in2) = false ) then false else true
                                    in
                                        OE(inner1,inner2)
                                    end
        | IMPLY(inner1,inner2) =>   let fun imply(in1 : formula, in2 : formula) =
                                        if (eval(in1) = true andalso eval(in2) = false ) then false else true
                                    in
                                        imply(inner1,inner2)
                                    end
        | LESS(inner1,inner2) => 
                                    let val val1 = exprEval(inner1)
                                    in
                                        let val val2 = exprEval(inner2)
                                        in
                                            if (val1 < val2 ) then true else false
                                        end
                                    end
val evalTest = eval(ANDALSO(ORELSE(ANDALSO(TRUE,FALSE),IMPLY(FALSE,FALSE)),LESS(PLUS(NUM(1),NUM(3)),(MINUS(NUM(5),NUM(2))))))
(*
    Test case description

    ANDALSO    =>[FALSE]
        ORELSE =>TRUE
            ANDALSO(TRUE,FALSE)    => FALSE
            ,IMPLY(FALSE,FALSE)   => TRUE
        ,LESS  =>FALSE
            PLUS(1,3)       => NUM(4)
            ,MINUS(5,2)    => NUM(3)
     
*)


(* Question2 *)

type name = string
datatype metro = STATION of name
                | AREA of name * metro
                | CONNECT of metro * metro




(*
    Area이름과 동일한 station을 제거
*)
fun removeElement(met : metro, metL : metro list)=
    if null metL then []
    else if met = hd metL then removeElement(met,tl metL)
    else hd metL :: removeElement(met,tl metL)

(*
    가장 내부에 있는 station부터 바깥으로 나오면서 area와 겹치는 station들을 리스트에서 제거한다
    최종적으로 정상적인 metro description이면 비어있는 리스트를 반환하고, 아니라면 리스트에 station이 남아있는다.
*)
fun checkStation(met : metro)=
    case met of
       STATION a => STATION a::[]
        |AREA(a,STATION b) => if a=b then [] else STATION b::[]
        |AREA(a,AREA b) =>  let val metL = checkStation(AREA b)
                            in
                                removeElement(STATION a,metL)
                            end
        |AREA(a,CONNECT b) =>  let val metL = checkStation(CONNECT b)
                            in
                                removeElement(STATION a,metL)
                            end
        
        |CONNECT(a,b) => checkStation(a) @ checkStation(b)


(*
    모든 Station의 이름을 저장한 list에서 Area가 씌워진 Station을 리스트에서 제거하여 
    빈 리스트가 되면 true 아니면 false
*)
fun checkMetro (met : metro)=
    if null (checkStation(met)) then true else false
        
(*
    과제 문서에 제시된 test case
*)
val mustTrue1 =checkMetro(AREA("a", STATION "a"))
val mustTrue2 =checkMetro(AREA("a", AREA("a", STATION "a")))
val mustTrue3 =checkMetro(AREA("a", AREA("b", CONNECT(STATION "a", STATION "b"))))
val mustTrue4 =checkMetro(AREA("a", CONNECT(STATION "a", AREA("b", STATION "a"))))

val mustFalse1 =checkMetro(AREA("a", STATION "b"))
val mustFalse2 =checkMetro(AREA("a", AREA("a", STATION "b")))
val mustFalse3 =checkMetro(AREA("a", AREA("b", CONNECT(STATION "a", STATION "c"))))
val mustFalse4 =checkMetro(AREA("a", CONNECT(STATION "a", AREA("b", STATION "c"))))

val testCase = checkMetro(AREA("a", AREA("k", CONNECT(STATION "c", AREA("c",STATION("c"))))))


(* Question3 - (1) *)



datatype 'a lazyList = nullList
    | cons of 'a * (unit -> 'a lazyList)

fun tempFun() =
    nullList

val testLL = cons(1,tempFun)

fun seq(first : int, last : int)=
    if first = last then cons(first,fn unit => nullList) else cons(first,fn unit => seq(first+1,last))

fun infSeq(first : int)=
    cons(first,fn unit => infSeq(first+1))

fun firstN(lList : 'a lazyList, num : int)=
    if num < 1 then []
    else
        case lList of
            nullList => []
            |cons(a,b) => a::firstN(b (),num-1)

fun Nth(lList : 'a lazyList, num : int)=
    if num = 1 then
        case lList of
            cons(a,b) => SOME a
            |nullList => NONE
    else if num < 1 then NONE
    else
        case lList of
            nullList => NONE
            |cons(a,b) => Nth(b (),num-1)

fun filterMultiples(lList : int lazyList, num : int)=
    if num < 1 then lList
    else
        case lList of
            nullList => nullList
            |cons(a,b) => if (a mod num) = 0 then filterMultiples(b(),num) else cons(a,fn unit => filterMultiples(b(),num))


val testSeq = seq(1,6)
val testInfSeq = infSeq(3)

val threeOfTestSeq = firstN(testSeq,3)
val fiveOfTestInfSeq = firstN(testInfSeq,5)

val seventhOfTestSeq = Nth(testSeq,7)
val fourthOfTestSeq= Nth(testSeq,4)
val eleventhOfTestInfSeq= Nth(testInfSeq,11)

val filterMultiplesOfThreeInTestSeq = filterMultiples(testSeq,3)
val printRightAbove = firstN(filterMultiplesOfThreeInTestSeq,6)



(* Question3 - (2) *)

fun sieve(lList : int lazyList)=
    case lList of
        cons(a,b) => cons(a,fn unit => sieve(filterMultiples(b(),a)))
        |nullList => nullList

fun primes () =
    let val lList = infSeq(2)
    in
        sieve(lList)
    end

val primeList = primes()
val primeFirstTen = firstN(primeList,10)
val primeTwentyth = Nth(primeList,20)