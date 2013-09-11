(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))


(define rember*
  (lambda (a l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) a) (rember* a (cdr l)))
       (else (cons (car l) (rember* a (cdr l))))))
     (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

(rember* 'cup
         '((coffee) cup ((tea) cup) (and (hick)) cup))
                                        ; ((coffee) ((tea)) (and (hick)))

(rember* 'sauce
         '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))
                                        ; (((tomato)) ((bean)) (and ((flying))))

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

(lat? '(((tomato sauce))
        ((bean) sauce)
        (and ((flying)) sauce)))  ; #f

(atom? (car '(((tomato sauce))
              ((bean) sauce)
              (and ((flying)) sauce))))   ; #f


(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? old (car l)) (cons old
                                (cons new (insertR* new old (cdr l)))))
       (else (cons (car l) (insertR* new old (cdr l))))))
     (else (cons (insertR* new old (car l))
                 (insertR* new old (cdr l)))))))

))))


(insertR* 'roast
          'chuck
          '((how much (wood))
            could
            ((a (wood) chuck))
            (((chuck)))
            (if (a) ((wood chuck)))
            could chuck wood))
; ((how much (wood)) could ((a (wood) chuck roast)) (((chuck roast))) (if (a) ((wood
; chuck roast))) could chuck roast wood)

; -------------------------------------
; The first commandment (final version)

; When recurring on a list of atoms, lat, ask two questions about it: (null? lat) and
; else.
;
; When recurring on a number, n, ask two questions about it: (zero? n) and else.
; When recurring on a list of S-expressions, l, ask three questions about it: (null?
; l), (atom? (car l)), and else.
; -------------------------------------

; ------------------------------------
; The fourth commandment (final version)

; Always change at least one argument while recurring. When recurring on a list of
; atoms, lat, use (cdr lat). When recurring on a number, n, use (sub1 n). And when
; recurring on a list of S-expressions, l, use (car l) and (cdr l) if neither (null?
; l) nor (atom? (car l)) are true.

; It must be changed to be closer to termination. The changing argument must be
; tested in the termination condition:
; - when using cdr, test termination with null? and
; - when using sub1, test termination with zero?.
; ------------------------------------

(define add1
  (lambda (n)
    (+ n 1)))

(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l))
      (cond
       ((eq? (car l) a) (add1 (occur* a (cdr l))))
       (else (occur* a (cdr l)))))
     (else (+ (occur* a (car l)) (occur* a (cdr l)))))))

(occur* 'banana
        '((banana)
          (split ((((banana ice)))
                  (cream (banana))
                  sherbet))
          (banana)
          (bread)
          (banana brandy)))   ; 5

(define subst*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old) (cons new (subst* new old (cdr l))))
       (else (cons (car l) (subst* new old (cdr l))))))
     (else (cons (subst* new old (car l))
                 (subst* new old (cdr l)))))))

(subst* 'orange
        'banana
        '((banana)
          (split ((((banana ice)))
                  (cream (banana))
                  sherbet))
          (banana)
          (bread)
          (banana brandy)))

; returns ((orange)
;          (split ((((orange ice)))
;                  (cream (orange))
;                  sherbet))
;          (orange)
;          (bread)
;          (orange brandy))

(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old) (cons new
                                (cons old (insertL* new old (cdr l)))))
       (else (cons (car l) (insertL* new old (cdr l))))))
     (else (cons (insertL* new old (car l))
                 (insertL* new old (cdr l)))))))

(insertL* 'pecker
          'chuck
          '((how much  (wood)) 
            could 
            ((a  (wood)  chuck)) 
            (( (chuck))) 
            (if (a)  ((wood  chuck))) 
            could  chuck wood))

; returns ((how much  (wood)) 
;          could 
;          ((a  (wood)  pecker chuck)) 
;          (((pecker chuck))) 
;          (if (a)  ((wood  pecker chuck))) 
;          could  pecker chuck wood).

(define member*
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l)) (or (eq? a (car l)) (member* a (cdr l))))
     (else (or (member* a (car l)) (member* a (cdr l)))))))

(member* 'chips
         '((potato) (chips ((with) fish) chips)))   ; #t
(member* 'chips
         '((potato) (chips ((with) fish) (chips)))) ; #t

(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l)) (car l))
     (else (leftmost (car l))))))

(leftmost '((potato) (chips ((with) fish) (chips)))) ; potato
(leftmost '(((hot) (tuna (and))) cheese))            ; hot
;(leftmost '(((() four )) 17 (seventeen)))            ; no answer

(and (atom? (car '(mozzarella pizza)))
     (eq? (car '(mozzarella pizza)) 'pizza))             ; #f

(and (atom? (car '((mozzarella mushroom) pizza)))
     (eq? (car '((mozzarella mushroom) pizza) 'pizza)))  ; #f

(and (atom? (car '(pizza (tastes good))))
     (eq? (car '(pizza (tastes good))) 'pizza))          ; #t


(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     ((and (atom? (car l1)) (atom? (car l2)))
      (and (eq? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
     ((or (atom? (car l1)) (atom? (car l2))) #f)
     (else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

(eqlist? '(strawberry ice cream) '(strawberry ice cream))  ; #t
(eqlist? '(strawberry ice cream) '(strawberry cream ice))  ; #f
(eqlist? '(banana ((split))) '((banana) (split)))          ; #f
(eqlist? '(beef ((sausage)) (and (soda)))
         '(beef ((salami)) (and (soda))))                  ; #f
(eqlist? '(beef ((sausage)) (and (soda)))
         '(beef ((sausage)) (and (soda))))                 ; #t


; given two s-expressions, check if they are equal
(define equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2)) (eq? s1 s2))
     ((atom? s1) #f)
     ((atom? s2) #f)
     (else (eqlist? s1 s2)))))

; rewrite eqlist? using equal?
(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     (else
      (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

; The sixth commandment
; Simplify only after the function is correct.

; rember after we replace lat by a list l of s-expressions and
; a by any S-expression.
(define rember
  (lambda (s l)
    (cond
     ((null? l) '())
     ((equal? (car l) s) (cdr l))
     (else (cons (car l) (rember s (cdr l)))))))



; numbered
(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     ((eq? (car (cdr aexp)) '+)
      (and (numbered? (car aexp))
           (numbered? (car (cdr (cdr aexp))))))
     ((eq? (car (cdr aexp)) 'x)
      (and (numbered? (car aexp))
           (numbered? (car (cdr (cdr aexp))))))
     ((eq? (car (cdr aexp)) '^)
      (and (numbered? (car aexp))
           (numbered? (car (cdr (cdr aexp)))))))))

(numbered? 1)               ; #t
(numbered? '(3 + (4 ^ 5)))  ; #t
(numbered? '(2 x sausage))  ; #f

; here is the simplified version of numbered?
(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     (else (and (numbered? (car aexp))
                (numbered? (car (cdr (cdr aexp)))))))))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (car (cdr nexp)) '+)
      (o+ (value (car nexp))
          (value (car (cdr (cdr nexp))))))
     ((eq? (car (cdr nexp)) 'x)
      (x (value (car nexp))
         (value (car (cdr (cdr nexp))))))
     (else (expt (value (car nexp))
              (value (car (cdr (cdr nexp)))))))))

(value 13)             ; 13
(value '(1 + 3))       ; 4
(value '(1 + (3 ^ 4))) ; 82

; The seventh commandment
; Recur on the subparts that are of the same nature
; - on the sublists of a list
; - on the subexpressions of an arithmetic expression.

; the function value for a new kind of arithmetic expression (prefix notation)
; that is either
; - a number
; - a list of the atom + followed by two arithmetic expressions,
; - a list of the atom x followed by two arithmetic expressions, or
; - a list of the atom ^ followed by two arithmetic expressions.

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (operator nexp) '+)
      (+ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
     ((eq? (operator nexp) 'x)
      (x (value (1st-sub-exp nexp) (value (2nd-sub-exp nexp)))))
     (else
      (^ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))))))

(+ 1 3)   ; 4

; The eight commandment
; Use help functions to abstract from representations.

; Let's try another representation for numbers.
; How shall we represent zero now? -- ()
; How is one represented? -- (())
; How is two represented? -- (()())
; Got it? What's three?   -- (()()())

; a function to test for zero
(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1
  (lambda (n)
    (cdr n)))

