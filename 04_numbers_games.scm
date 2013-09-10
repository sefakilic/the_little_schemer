(define add1
  (lambda (n)
    (+ n 1)))

(add1 67) ; 68

(define sub1
  (lambda (n)
    (- n 1)))

(sub1 5)  ; 4

(zero? 0)    ; #t
(zero? 1492) ; #f

(define o+
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (o+ (add1 n) (sub1 m))))))


(o+ 46 12) ; 58

(define o-
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (o- n (sub1 m)))))))

(o- 14 3) ; 11
(o- 17 9) ; 8
; (o- -18 25) ; no answer, there are no negative numbers

; The first commandment (first revision)

; When recurring on a list of atoms, lat, ask two questions about it: (null? lat) and
; else. When recurring on a number, n, ask two questions about it: (zero? n) and else.

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else (o+ (car tup) (addtup (cdr tup)))))))

(addtup '(3 5 2 8))     ; 18
(addtup '(15 6 7 12 3)) ; 43

; The fourth commandment (first revision)

; Always change at least one argument while recurring. It must be changed to be
; closer to termination. The changing argument must be tested in the termination
; condition:
; - when using cdr, test termination with null?
; - when using sub1, test termination with zero?

(define x
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (o+ n (x n (sub1 m)))))))

(x 12 3)  ; 36

; The fifth commandment

; When building a value with o+, always use 0 for the value of the terminating line,
; for adding 0 does not change the value of an addition.

; When building a value with x, always use 1 for the value of the terminating line,
; for multiplying 1 does not change the value of a multiplication.

; When building a value with cons, always consider () for the value of the
; terminating line.

(define tup+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else (cons (o+ (car tup1) (car tup2))
                 (tup+ (cdr tup1) (cdr tup2)))))))

(tup+ '(3 6 9 11 4) '(8 5 2 0 7))  ; (11 11 11 11 11)
(tup+ '(2 3) '(4 6))               ; (6 9)
(tup+ '(3 7) '(4 6))               ; (7 13)
(tup+ '(3 7) '(4 6 8 1))           ; (7 13 8 1)
(> 12 133)   ; #f
(> 120 11)   ; #t

(define >
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else (> (sub1 n) (sub1 m))))))

(< 4 6)  ; #t
(< 8 3)  ; #f
(< 6 6)  ; #f

(define <
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else (< (sub1 n) (sub1 m))))))

(< 4 6)
(< 8 3)
(< 6 6)

(define =
  (lambda (n m)
    (cond
     ((zero? m) (zero? n))
     ((zero? n) #f)
     (else (= (sub1 n) (sub1 m))))))

(= 3 5)  ; #f
(= 5 3)  ; #f
(= 3 3)  ; #t

; = using < and >
(define =
  (lambda (n m)
    (cond
     ((> n m) #f)
     ((< n m) #f)
     (else #t))))


(define expt
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else (x n (expt n (sub1 m)))))))

(expt 1 1)  ; 1
(expt 2 3)  ; 8
(expt 5 3)  ; 125

(define quotient
  (lambda (n m)
    (cond
     ((< n m) 0)
     (else (add1 (quotient (- n m) m))))))

(quotient 15 4)  ; 3

(define length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else (add1 (length (cdr lat)))))))

(length '(hotdogs with mustard sauerkraut and pickles))   ; 6

(define pick
  (lambda (n lat)
    (cond
     ((eq? n 1) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

(pick 4 '(lasagna spaghetti ravioli macaroni meatball))  ; macaroni

(define rempick
  (lambda (n lat)
    (cond
     ((eq? n 1) (cdr lat))
     (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(rempick 3 '(hotdogs with hot mustard)) ; (hotdogs with mustard)

(number? 'tomato)  ; #f
(number? 76)       ; #t

(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((number? (car lat)) (no-nums (cdr lat)))
     (else (cons (car lat) (no-nums (cdr lat)))))))

(no-nums '(5 pears 6 prunes 9 dates))  ; (pears prunes dates)

(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
     (else (all-nums (cdr lat))))))

(all-nums '(5 pears 6 prunes 9 dates))  ; (5 6 9)

(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2)) (= a1 a2))
     ((or (number? a1) (number? a2)) #f)
     (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     ((eq? a (car lat)) (add1 (occur a (cdr lat))))
     (else (occur a (cdr lat))))))

(occur 1 '(1 2 3 1 2))  ; 2

(define one?
  (lambda (n)
    (cond
     ((zero? n) #f)
     (else (zero? (sub1 n))))))

(one? 1)  ; #t
(one? 3)  ; #f

(define rempick
  (lambda (n lat)
    (cond
     ((one? n) (cdr lat))
     (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(rempick 3 '(lemon meringue salty pie))  ; (lemon meringue pie)
