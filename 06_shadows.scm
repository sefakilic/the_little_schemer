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

