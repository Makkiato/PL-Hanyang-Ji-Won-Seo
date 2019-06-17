#lang racket

(provide (all-defined-out)) ;; exports the defined variables in this file.

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 



;; Problem 1

;; CHANGE (put your solutions here)
(define (racketlist->mupllist rl)(if (null? (cdr rl)) (apair (car rl) (aunit)) (apair (car rl) (racketlist->mupllist(cdr rl)))))
(define (mupllist->racketlist ml)(match ml
                                   [(struct* apair ([e1 hd]
                                                    [e2 tl]))
                                    (cons hd (mupllist->racketlist tl))]
                                   [(struct* aunit ()) null]
                                   [a a]))


;; testcases for Prob1
;;(define a (list 1 2 3 4 5))
;;(define b (racketlist->mupllist a))
;;(define c (mupllist->racketlist b))
;;(define d (mupllist->racketlist (apair 1 (aunit))))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.

;;(define (getInt e)(if (int? e) (match e [(struct* int ([num val])) val])(error "MUPL addition applied to non-number")))


(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]
      
        [(ifgreater? e)(match e [(struct* ifgreater ([e1 big]
                                                     [e2 small]
                                                     [e3 yes]
                                                     [e4 no]
                                                             ))
                                 (let
                                     ([evaledB (eval-under-env big env)]
                                      [evaledS (eval-under-env small env)])
                                   (if (> (int-num evaledB) (int-num evaledS))
                                       (eval-under-env yes env)
                                       (eval-under-env no env)
                                       )
                                   )
                                 ]
                         )
                       ]
        [(mlet? e) (match e [(struct* mlet ([var variable]
                                          [e exp]
                                          [body eval]))
                           (letrec
                               ([value (eval-under-env exp env)]
                                [newenv (append env (list (cons variable value)))])
                             (eval-under-env eval newenv))
                           ])]
        [(apair? e) (match e [(struct* apair ([e1 hd]
                                            [e2 tl]))
                                     (apair (eval-under-env hd env) (eval-under-env tl env))])]
        [(fst? e) (match e [(struct* fst ([e exp]))
                            (let ([rpair (eval-under-env exp env)])
                              (if (apair? rpair) (apair-e1 rpair)
                                  (error "not an apair") ) )])]
        [(snd? e) (match e [(struct* snd ([e exp]))   
                            (let ([rpair (eval-under-env exp env)])
                              (if (apair? rpair) (apair-e2 rpair)
                                  (error "not an apair") ) )])]
        [(aunit? e) e]
        [(isaunit? e) (match e [(struct* isaunit ([e exp]))
                                 (let ([value (eval-under-env exp env)])
                                   (if (aunit? value) (int 1) (int 0)))])]
        [(fun? e) (closure env e)]
        [(closure? e) (match e [(struct* closure ([env innerenv]
                                                  [fun fun]))
                               (eval-under-env fun env) ])]
                           

        [(call? e) (match e [(struct* call ([funexp name]
                                         [actual arg]))
                             (let ([func (eval-under-env name env)]
                                   [inarg (eval-under-env arg env)])
                                   (if (closure? func)
                                       (let* ([proc (closure-fun func)]
                                             [cloenv (closure-env func)]
                                             [recur (cons (fun-nameopt proc) func)]
                                             [argnext (cons (fun-formal proc) inarg)])
                                         (eval-under-env (fun-body proc)
                                                         (if (equal? (car recur) #f)
                                                             (append (list argnext) cloenv)
                                                             (append (list argnext recur) cloenv))))
                                       (error "bad use on fun and call")))
                             ])]
                                   
        ;; CHANGE add more cases here
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; testcases for Prob2
;;(define tig (ifgreater (int 3) (add (int 1) (int 1)) (int 3) (int 2)))
;;(define tml (mlet "five" (int 5) (add (int 3) (var "five"))))
;;(define tap (apair (var "five") (int 6)))
;;(define tenv null)
;;(define newenv (append tenv (list (cons (var-string (var "five"))  (int 5)))))
;;(define tfst (fst tap))
;;(define tsnd (snd tap))
;;(define tau (aunit))
;;(define tisau (isaunit tau))
;;(define tml2 (mlet "aunit" (aunit) (isaunit (var "aunit"))))
;;(define tfunsmall (fun "ifsmaller" "e1" (fun #f "e2" (fun #f "e3" (fun #f "e4" (ifgreater (var "e1") (var "e2") (var "e4") (var "e3")))))))
;;(define tfun (fun #f "e1" (add (var "e1") (var "five"))))
;;(define tc (call (var "ifsmaller") (int 1)))
;;(define tfc (mlet "ifsmaller" tfun tc))
;;(define mSigmaN (fun "sig" "num" (ifgreater (var "num") (int 0)
                                          (add (var "num") (call (var "sig") (add (int -1) (var "num"))) )
                                         (int 0)
                                       )))
;;(define tmf (mlet "msn" mSigmaN (call (var "msn") (int 5))))
;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))


(define (mlet* lstlst e2) (if (aunit? lstlst)
                                   e2
                                   (mlet (apair-e1 (apair-e1 lstlst)) (apair-e2 (apair-e1 lstlst)) (mlet* (apair-e2 lstlst) e2))))





(define (ifeq e1 e2 e3 e4) (ifgreater e1 e2 (ifgreater e2 e1 e3 e4) (ifgreater e2 e1 e4 e3)))

;; testcases for Prob3
;;(define tifau (ifaunit (aunit) (int 3) (int 4)))
;;(define tapair (apair (apair "five" (int 5)) (apair (apair "seven" (int 7)) (aunit))))
;;(define tpair (mupllist->racketlist tapair))
;;(define restoreapair (racketlist->mupllist tpair))

;;(define tmlstar (mlet* (apair (apair "five" (int 5))
;;                              (apair (apair "seven" (int 7))
;;                                    (apair (apair "three" (int 3))
;;                                            (aunit))))
;;                       (add (add (var "five") (var "seven")) (var "three"))))

;;(define tifeq (ifeq (int 1) (int 1) (int 1) (int 3)))


;; Problem 4

(define mupl-map
  (fun #f "applyFun"
       (fun "inList" "list"
            (ifgreater (isaunit (var "list")) (int 0)
                         (aunit)
                         (apair (call (var "applyFun") (fst (var "list")))
                                (call (var "inList") (snd (var "list"))))))))




(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "int" (call (var "map") (fun #f "arg" (add (var "int") (var "arg")))) )))
;; testcases for Prob4
;;(define tmm (eval-exp (call (call mupl-map (fun #f "arg" (add (int 3) (var "arg")))) (apair (int 3) (apair (int 5) (apair (int 7) (aunit)))))))
;;(define tmmad (call mupl-mapAddN (int 5)))


(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
