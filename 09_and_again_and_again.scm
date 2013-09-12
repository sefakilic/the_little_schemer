(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a sorn lat) ; sorn stands for "symbol or numbrer"
    (cond
     ((number? sorn)
      (keep-looking a (pick sorn lat) lat))
     (else (eq? sorn a)))))

; pick nth element
(define pick
  (lambda (n lat)
    (cond
     ((= n 1) (car lat))
     (else (pick (- n 1) (cdr lat))))))

(looking 'caviar '(6 2 4 caviar 5 7 3))    ; #t
(looking 'caviar '(6 2 grits caviar 5 7 3)); #f
; (looking 'caviar '(7 1 2 caviar 5 6 3)) ; never stops!

; Functions like looking are called "partial" functions.
; All functions that we have seen so far are called "total", as they
; return value for any input argument.

; Can we define na shorter function that does not reach its goal for
; some of its arguments?
(define eternity
  (lambda (x)
    (eternity x)))

; shift example

; redefine first, second and build as one-liners
(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))


(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(shift '((a b) c))      ; (a (b c))
(shift '((a b) (c d)))  ; (a (b (c d)))

; align
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define a-pair?
  (lambda (x)
    (cond
     ((atom? x) #f)
     ((null? x) #f)
     ((null? (cdr x)) #f)
     ((null? (cdr (cdr x))) #t)
     (else #f))))


(define align
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora))
      (align (shift pora)))
     (else (build (first pora)
                  (align (second pora)))))))

(align '((a b) c))     ; (a (b c))
(align '((a b) (c d))) ; (a (b (c d)))

; Write a function that counts the number of atoms in align's arguments?
(define length*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else (+ (length* (car pora))
              (length* (cdr pora)))))))

(define weight*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else (+ (* (weight* (first pora)) 2)
              (weight* (second pora)))))))

(weight* '((a b) c))    ; 7
(weight* '(a (b c)))    ; 5

; Here is shuffle which is like align but uses revpair from chapter 7,
; instead of shift
(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define shuffle
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora))
      (shuffle (revpair pora)))
     (else (build (first pora)
                  (shuffle (second pora)))))))

; Let's see if shuffle is total
(shuffle '(a (b c)))    ; (a (b c))
(shuffle '(a b))        ; (a b)

; (shuffle '((a b) (c d))) ; runs forever, therefore shuffle is not total

; Is this function total?
(define C
  (lambda (n)
    (cond
     ((one? n) 1)
     (else (cond
            ((even? n) (/ n 2))
            (else (C (+ (* 3 n) 1))))))))

; no body knows. Thank you Lothar Collatz (1910-1990).

(define add1
  (lambda (n)
    (+ n 1)))
(define sub1
  (lambda (n)
    (- n 1)))
; Here is the definition of A
(define A
  (lambda (n m)
    (cond
     ((zero? n) (add1 m))
     ((zero? m) (A (sub1 n) 1))
     (else (A (sub1 n)
              (A n (sub1 m)))))))
; Thank you, Wilhelm Ackermann (1853-1946)

(A 1 0)  ; 2
(A 1 1)  ; 3
(A 2 2)  ; 7

; (A 4 2) ; Don't try this at home. It has 19,729 digits
; http://en.wikipedia.org/wiki/Ackermann_function


; At this point, the book has a great discussion about recursion and
; computability. Since examples given at this point are hypothetical, they are not
; given here.

; Stop the world - I want to get off
; Leslie Bricusse and Anthony Newley