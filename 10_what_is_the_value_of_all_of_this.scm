(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

; Build an entry from a set of names and a list of values.
; Try to build examples with this function
(define new-entry build)

(new-entry '(appetizer entree beverage)
           '(pate boeuf vin))

(new-entry '(appetizer entree beverage)
           '(beer beer beer))

(new-entry '(beverage desert)
           '((food is) (number one with us)))

; here is the definition of lookup-in-entry
(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          entry-f)))
; where
(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
     ((null? names) (entry-f name))
     ((eq? (car names) name) (car values))
     (else (lookup-in-entry-help name
                                 (cdr names)
                                 (cdr values)
                                 entry-f)))))

; A table (also called an environment) is a list of entries. Here is one example
; (((appetizer entree beverage)
;   (pate boeuf vin))
;  ((beverage dessert)
;   ((food is) (number one with us))))


; Define the function extend-table which takes an entry and a table (possibly the
; empty one) and creates a new taable by putting the new entry in front of the old
; table
(define extend-table cons)

; lookup-in-table
(define lookup-in-table
  (lambda (name table table-f)
    (cond
     ((null? table) (table-f name))
     (else (lookup-in-entry name
                            (car table)
                            (lambda (name)
                              (lookup-in-table name
                                               (cdr table)
                                               table-f)))))))

(lookup-in-table 'entree
                 '(((entree dessert)
                    (spaghetti spumoni))
                   ((appetizer entree beverage)
                    (food tastes good)))
                 (lambda (name) '()))          ; spaghetti


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(car '(a b c))      ; a

(cons 'a
      (cons 'b
            (cons 'c '())))   ; (a b c)

(cons 'car
      (cons (cons 'quote
                  (cons (cons 'a
                              (cons 'b
                                    (cons 'c '())))
                        '()))
            '()))                          ; (car (quote (a b c)))

(car '(a b c)) ; a
(value '(car '(a b c)))


; expression to action

(define expression-to-action
  (lambda (e)
    (cond
     ((atom? e) (atom-to-action e))
     (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
     ((number? e) *const)
     ((eq? e #t) *const)
     ((eq? e #f) *const)
     ((eq? e 'cons) *const)
     ((eq? e 'car) *const)
     ((eq? e 'cdr) *const)
     ((eq? e 'null?) *const)
     ((eq? e 'eq?) *const)
     ((eq? e 'atom?) *const)
     ((eq? e 'zero?) *const)
     ((eq? e 'add1) *const)
     ((eq? e 'sub1) *const)
     ((eq? e 'number?) *const)
     (else *identifier))))

; now define the help function list-to-action
(define list-to-action
  (lambda (e)
    (cond
     ((atom? (car e))
      (cond
       ((eq? (car e) 'quote) *quote)
       ((eq? (car e) 'lambda) *lambda)
       ((eq? (car e) 'cond) *cond)
       (else *application)))
     (else *application))))

; Assuming that expression-to-action works, we can use it to define value and meaning
(define value
  (lambda (e)
    (meaning)))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

; Here is the action for constants
(define *const
  (lambda (e table)
    (cond
     ((number? e) e)
     ((eq? e #t) #t)
     ((eq? e #f) #f)
     (else (build 'primitive e)))))

; Here is the action for *quote
(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)

; *identifier
(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

; here is the initial-table
(define initial-table
  (lambda (name)
    (car '())))  ; let's hope never.

; Here is the action *lambda
(define *lambda
  (lambda (e table)
    (build 'non-primitive
           (cons table (cdr e)))))

; good idea to define some help functions
(define table-of first)
(define formals-of second)
(define body-of third)

; Here is the function evcon (cond)
(define evcon
  (lambda (lines table)
    (cond
     ((else? (question-of (car lines)))
      (meaning (answer-of (car lines)) table))
     ((meaning (question-of (car lines)) table)
      (meaning (answer-of (car lines)) table))
     (else (evcon (cdr lines) table)))))

(define else?
  (lambda (x)
    (cond
     ((atom? x) (eq? x 'else))
     (else #f))))

(define question-of first)

(define answer-of second)

; now we can write the *cond function
(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

; let's try
(*cond '(cond (coffee klatsch) (else party))
       '(((coffee) (#t))
         ((klatsch party) (5 (6)))))  ; 5


; write a function evlis that takes a list of (representations of) arguments and a
; table, and returns a list composed of the meaning of each argument.
(define evlis
  (lambda (args table)
    (cond
     ((null? args) '())
     (else
      (cons (meaning (car args) table)
            (evlis (cdr args) table))))))

; Here is *application
(define *application
  (lambda (e table)
    (apply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define function-of car)
(define arguments-of cdr)

; write primitive? and non-primitive?
(define primitive?
  (lambda (l)
    (eq? (first l) 'primitive)))

(define non-primitive?
  (lambda (l)
    (eq? (first l) 'non-primitive?)))

(define apply
  (lambda (fun vals)
    (cond
     ((primitive? fun)
      (apply-primitive (second fun) vals))
     ((non-primitive? fun)
      (apply-closure (second fun) vals)))))

; this is the definition of apply-primitive
(define apply-primitive
  (lambda (name vals)
    (cond
     ((eq? name 'cons)
      (cons (first vals) (second vals)))
     ((eq? name 'car)
      (car (first vals)))
     ((eq? name 'cdr)
      (cdr (first vals)))
     ((eq? name 'null?)
      (null? (first vals)))
     ((eq? name 'eq?)
      (eq? (first vals) (second vals)))
     ((eq? name 'atom?)
      (:atom? (first vals)))
     ((eq? name 'zero?)
      (zero? (first vals)))
     ((eq? name 'add1)
      (add1 (first vals)))
     ((eq? name 'sub1)
      (sub1 (first vals)))
     ((eq? name 'number?)
      (number? (first vals))))))

(define :atom?
  (lambda (x)
    (cond
     ((atom? x) #t)
     ((null? x) #f)
     ((eq? (car x) 'primitive) #t)
     ((eq? (car x) 'non-primitive) #t)
     (else #f))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
              (new-entry
               (formals-of closure)
               vals)
              (table-of closure)))))

; here is an example
(apply-closure '((((u v w)
                   (1 2 3))
                  ((x y z)
                   (4 5 6)))
                 (x y)
                 (cons z x))
               '((a b c)
                 (d e f)))    ; (6 a b c)


; yes, we are exhausted