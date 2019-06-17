datatype pattern =  Wildcard
                    | Variable of string
                    | UnitP
                    | ConstP of int
                    | TupleP of pattern list
                    | ConstructorP of string * pattern
datatype valu =     Const of int
                    | Unit
                    | Tuple of valu list
                    | Constructor of string * valu


(* Question 1 *)

fun getVariableList(pat:pattern)=
        case pat of
                Variable str => [str]
                |TupleP TupP => let fun digIn(Tup) = if null (tl Tup) then getVariableList(hd Tup) else getVariableList(hd Tup)@digIn(tl Tup)
                                in
                                        digIn(TupP)
                                end

                |ConstructorP (s,p) => getVariableList(p)
                |_ => []

fun checkDistinct(sList)=
        let val existFun = List.exists(fn n => n = hd sList)
        in
                if null (tl sList) then not(existFun (tl sList)) else not(existFun (tl sList)) andalso checkDistinct(tl sList)
        end

fun check_pat(pat)=
        checkDistinct(getVariableList(pat))

val SampleConstructorP = ConstructorP ("ConName",TupleP([Variable "test3",Variable "test4"]))
val SampleConstructor = Constructor("ConName",Tuple ([Constructor("forTest3",Const 9), Tuple([Constructor("forTest4inTuple",Const 11 )])]))

val SamplePattern = TupleP([UnitP, ConstP 3, Variable "test1", Variable "test2", SampleConstructorP])
val SampleValu = Tuple([Unit,Const 3, Const 5, Const 6, SampleConstructor])

val SampleResult = getVariableList SamplePattern
val endResult = check_pat(SamplePattern)

(* Question 2 *)
fun match(valu,pat)=
        case (valu,pat) of
                (_,Wildcard)=> SOME []
                |(v,Variable s) => SOME [(s,v)]
                |(Unit,UnitP) => SOME []
                |(Const a,ConstP b) => if a=b then SOME [] else NONE
                |(Tuple Tup,TupleP TupP) =>     let fun digIn(Tup,TupP)=
                                                        case (Tup,TupP) of
                                                                (valu::restV,pat::restP)=>      let     val matchResult = match(valu,pat)
                                                                                                        val deeper = digIn(restV,restP)
                                                                                                in
                                                                                                        if(isSome matchResult andalso isSome deeper) then SOME (valOf matchResult @ valOf deeper)  else NONE
                                                                                                end
                                                                |([],pat::restP)=> NONE (*다른 길이 1*)
                                                                |(valu::restV,[])=> NONE (*다른 길이 2*)
                                                                |([],[])=> SOME [] (*동시에 고갈로 같은 길이*)
                                                in
                                                        digIn(Tup,TupP)
                                                end
                |(Constructor (s1,v),ConstructorP (s2,p)) => if s1=s2 then match(v,p) else NONE
                |(_,_) => NONE
                
(*
        Test case described just above

        val SampleConstructorP = ConstructorP ("ConName",TupleP([Variable "test3",Variable "test4"]))
        val SampleConstructor = Constructor("ConName",Tuple ([Constructor("forTest3",Const 9), Tuple([Constructor("forTest4inTuple",Const 11 )])]))

        val SamplePattern = TupleP([UnitP, ConstP 3, Variable "test1", Variable "test2", SampleConstructorP])
        val SampleValu = Tuple([Unit,Const 3, Const 5, Const 6, SampleConstructor])
*)

val testMatch = match(SampleValu,SamplePattern)




(* Question 3 *)
type name = string
datatype RSP =
        ROCK
        | SCISSORS
        | PAPER
datatype 'a strategy = Cons of 'a * (unit -> 'a strategy)
datatype tournament =
        PLAYER of name * (RSP strategy ref)
        | MATCH of tournament * tournament

fun onlyOne(one:RSP) =
        Cons(one, fn() => onlyOne(one))
fun alterTwo(one:RSP, two:RSP) =
        Cons(one, fn() => alterTwo(two, one))
fun alterThree(one:RSP, two:RSP, three:RSP) =
        Cons(one, fn() => alterThree(two, three, one))
val r = onlyOne(ROCK)
val s = onlyOne(SCISSORS)
val p = onlyOne(PAPER)
val rp = alterTwo(ROCK, PAPER)
val sr = alterTwo(SCISSORS, ROCK)
val ps = alterTwo(PAPER, SCISSORS)
val srp = alterThree(SCISSORS, ROCK, PAPER)
val rsp = alterThree(ROCK, SCISSORS, PAPER)
val prs = alterThree(PAPER, ROCK, SCISSORS)
fun next(strategyRef) =
        let val Cons(rsp, func) = !strategyRef
        in
                strategyRef := func();
                rsp
        end

fun dual(player1 : (name * (RSP strategy ref)) , player2 : (name * (RSP strategy ref)))=
        let
                val str1 = #2 player1
                val str2 = #2 player2
        in
                case (next(str1), next(str2)) of
                        (ROCK,SCISSORS) => player1
                        |(SCISSORS,PAPER) => player1
                        |(PAPER,ROCK) => player1
                        |(SCISSORS,ROCK) => player2
                        |(PAPER,SCISSORS) => player2
                        |(ROCK,PAPER) => player2
                        |(_,_) => dual(player1,player2)
        end

        
fun getWinner(t) =
        case t of
                MATCH (MATCH t1,MATCH t2) => dual(getWinner(MATCH t1),getWinner(MATCH t2))
                |MATCH (PLAYER p1, MATCH t2) => dual(p1,getWinner(MATCH t2))
                |MATCH (MATCH t1,PLAYER p2) => dual(getWinner(MATCH t1),p2)
                |MATCH (PLAYER p1,PLAYER p2) => dual(p1,p2)
                |PLAYER a => a
fun whosWinner(t)=
        PLAYER (getWinner(t))
        

val testRSP = whosWinner(MATCH(PLAYER("s", ref s),MATCH(PLAYER("rp", ref rp), PLAYER("r", ref r))))
val testRSP2 = whosWinner(MATCH(MATCH(PLAYER("rsp", ref rsp),PLAYER("ps",ref ps)),MATCH(PLAYER("srp", ref srp), PLAYER("prs", ref prs))))
        (*
                round 1 : rsp vs ps
                round 2 : srp vs prs
                round1 winner vs round 2 winner

                "srp" wins
        *)
