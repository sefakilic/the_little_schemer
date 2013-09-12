(define rember-f
  (lambda (test? a l)
    (cond
     ((null? l) '())
     ((test? a (car l)) (cdr l))
     (else (cons (car l) (rember-f test? a (cdr l)))))))


(rember-f eq?
          'jelly
          '(jelly beans are good))           ; (beans are good)
(rember-f equal?
          '(pop corn)
          '(lemonade (pop corn) and (cake))) ; (lemonade and (cake))

; curry-ing or schonfinkel-ing
(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define eq?-salad (eq?-c 'salad))

(eq?-salad 'salad)     ; #t
(eq?-salad 'tuna)      ; #f
((eq?-c 'salad) 'tuna) ; #f

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l) '())
       ((test? (car l) a) (cdr l))
       (else (cons (car l) ((rember-f test?) a (cdr l))))))))

(define rember-eq? (rember-f eq?))

(rember-eq? 'tuna '(tuna salad is good))           ; (salad is good)
(rember-eq? 'tuna '(shrimp salad and tuna salad))  ; (shrimp salad and salad)

((rember-f eq?) eq? '(equal? eq? eqan? eqlist? eqpair?))
                                        ; (equal? eqan? eqlist? eqpair?)

; now transform insertL to insertL-f the same way we have transformed rember into
; rember-f
(define insertL-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
       ((null? lat) '())
       ((test? (car lat) old) (cons new lat))
       (else (cons (car lat) ((insertL-f test?) new old (cdr lat))))))))

((insertL-f eq?)
 'topping
 'fudge
 '(ice cream with fudge for desert)) ; (ice cream with topping fudgefor desert)

(define insertR-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
       ((null? lat) '())
       ((test? (car lat) old) (cons old
                                  (cons new (cdr lat))))
       (else (cons (car lat) ((insertR-f test?) new old (cdr lat))))))))

((insertR-f eq?)
 'topping
 'fudge
 '(ice cream with fudge for desert)) ; (ice cream with fudge topping for desert)


; write a function insert-g that would insert either at the left or at the right
(define seqL
  (lambda (new old l)
    (cons (new (cons old l)))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((eq? (car l) old) (seq new old (cdr l)))
       (else (cons (car l)
                   ((insert-g seq) new old (cdr l))))))))

(define insertL (insert-g seqL))
(define insertR (insert-g seqR))

; define seqL without giving a name
(define insertL
  (insert-g (lambda (new old l)
              (cons new (cons old l)))))

; remember subst?
(define subst 
  (lambda (new old l) 
    (cond 
     ((null? l) '())
     ((eq? (car l) old) 
      (cons new (cdr l))) 
     (else (cons (car l) 
                 (subst new old (cdr l)))))))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst (insert-g seqS))

; what do you thing yyy is
(define yyy
  (lambda (a l)
    ((insert-g seqrem) #f a l)))
; where
(define seqrem
  (lambda (new old l)
    l))

(yyy 'sausage '(pizza with sausage and bacon))  ; (pizza with and bacon)

; The ninth commandment
; Abstract common patterns with a new function

; Do you remember value from chapter 6?

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (car (cdr nexp)) '+)
      (+ (value (car nexp))
          (value (car (cdr (cdr nexp))))))
     ((eq? (car (cdr nexp)) 'x)
      (* (value (car nexp))
         (value (car (cdr (cdr nexp))))))
     (else (expt (value (car nexp))
              (value (car (cdr (cdr nexp)))))))))

(define atom-to-function
  (lambda (x)
    (cond
     ((eq? x '+) +)
     ((eq? x 'x) *)
     (else expt))))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(atom-to-function (operator '(+ 5 3)))  ; + function

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     (else
      ((atom-to-function (operator nexp))
       (value (1st-sub-exp nexp))
       (value (2nd-sub-exp nexp)))))))

(value '(+ 5 3))  ; 8

; remember multirember
(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) a)
      (multirember a (cdr lat)))
     (else (cons (car lat)
                 (multirember a (cdr lat)))))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
       ((null? lat) '())
       ((test? (car lat) a)
        ((multirember-f test?) a (cdr lat)))
       (else (cons (car lat)
                   ((multirember-f test?) a (cdr lat))))))))

((multirember-f eq?)
                'tuna
                '(shrimp salad tuna salad and tuna)) ; (shrimp salad salad and)

(define multirember-eq? (multirember-f eq?))

; can we combine a and test?
(define eq?-tuna
  (eq?-c 'tuna))

; Perhaps we should now write multiremberT which is similar to multirember-f.
; Instead of taking test? and returning a function, multiremberT takes a function
; like eq?-tuna and a lat and then does its work.

(define multiremberT
  (lambda (test? lat)
    (cond
     ((null? lat) '())
     ((test? (car lat))
      (multiremberT test? (cdr lat)))
     (else (cons (car lat) (multiremberT test? (cdr lat)))))))

(multiremberT eq?-tuna
              '(shrimp salad tuna salad and tuna)) ; (shrimp salad salad and)


(define multirember&co
  (lambda (a lat col)
    (cond
     ((null? lat) (col '() '()))
     ((eq? (car lat) a)
      (multirember&co a
                      (cdr lat)
                      (lambda (newlat seen)
                        (col newlat
                             (cons (car lat) seen)))))
     (else
      (multirember&co a
                      (cdr lat)
                      (lambda (newlat seen)
                        (col (cons (car lat) newlat)
                             seen)))))))

(define a-friend
  (lambda (x y)
    (null? y)))

(multirember&co 'tuna
                '()
                a-friend)  ; #t

(multirember&co 'tuna
                '(tuna)
                a-friend)  ; #f

(multirember&co 'tuna
                '(and tuna)
                a-friend)  ; #f

(multirember&co 'tuna
                '(strawberries tuna and swordfish)
                a-friend) ; #f

; multirember&co looks at every atom of the lat to see wheter it is eq? to a. Those
; atoms that are not are collected in one list ls1; the others for which the answer
; is true are collected in a second list ls2. Finally, it determines the value of (f
; ls1 ls2)

(multirember&co 'tuna
                '(strawberries tuna and swordfish)
                (lambda (x y) (length x)))  ; 3

; The tenth commandment
; Build functions to collect more than one value at a time.

; Here are some old friends
(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons old
                                (cons new (multiinsertR new old (cdr lat)))))
     (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons new
                                (cons old (multiinsertL new old (cdr lat)))))
     (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) oldL)
      (cons new
            (cons oldL
                  (multiinsertLR new oldL oldR (cdr lat)))))
     ((eq? (car lat) oldR)
      (cons oldR
            (cons new
                  (multiinsertLR new oldL oldR (cdr lat)))))
     (else (cons (car lat)
                 (multiinsertLR new oldL oldR (cdr lat)))))))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
     ((null? lat) (col '() 0 0))
     ((eq? (car lat) oldL)
      (multiinsertLR&co new oldL oldR (cdr lat)
                        (lambda (newlat L R)
                          (col (cons new
                                     (cons oldL newlat))
                               (+ L 1) R))))
     ((eq? (car lat) oldR)
      (multiinsertLR&co new oldL oldR (cdr lat)
                        (lambda (newlat L R)
                          (col (cons oldR
                                     (cons new newlat))
                               L (+ R 1)))))
     (else (multiinsertLR&co new oldL oldR (cdr lat)
                          (lambda (newlat L R)
                            (col (cons (car lat) newlat) L R)))))))
  
(define a-friend
  (lambda (lat L R)
    (cons L (cons R '()))))

(define b-friend
  (lambda (lat L R)
    lat))

(multiinsertLR&co 'cranberries
                  'fish
                  'chips
                  '()
                  a-friend)
                                        ; (0 0)

(multiinsertLR&co 'salty
                  'fish
                  'chips
                  '(chips and fish or fish and chips)
                  a-friend)             ; (2 2)


; Now write the function evens-only* which removes all odd numbers from a list of
; nested lists. Here is even?
(define even?
  (lambda (n)
    (= (* (quotient n 2) 2) n)))
(even? 2) ; #t
(even? 3) ; #f

(define evens-only*
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((even? (car l))
        (cons (car l) (evens-only* (cdr l))))
       (else (evens-only* (cdr l)))))
     (else (cons (evens-only* (car l))
                 (evens-only* (cdr l)))))))

(evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2)) ; ((2 8) 10 (() 6) 2)

; Write the function evens-only*&co. It builds a nested list of even numbers by
; removing the odd ones from its argument and simultaneously multiplies the even
; numbers and sums up the odd numbers that occur in the argument.
(define evens-only*&co
  (lambda (l col)
    (cond
     ((null? l) (col l 1 0))
     ((atom? (car l))
      (cond
       ((even? (car l))
        (evens-only*&co (cdr l)
                        (lambda (newl p s)
                          (col (cons (car l) newl) (* (car l) p) s))))
       (else
        (evens-only*&co (cdr l)
                        (lambda (newl p s)
                          (col newl p (+ (car l) s)))))))
     (else (evens-only*&co (car l)
                           (lambda (al ap as)
                             (evens-only*&co (cdr l)
                                             (lambda (dl dp ds)
                                               (col (cons al dl)
                                                    (* ap dp)
                                                    (+ as ds))))))))))
  
; test evens-only*&co
(define the-last-friend
  (lambda (newl product sum)
    (cons sum
          (cons product newl))))

(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend)