(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

(lat? '(Jack Sprat could eat no chicken fat))  ; #t
(lat? '((Jack) Sprat could eat no chicken fat)); #f
(lat? '(Jack (Sprat could) eat no chicken fat)); #f
(lat? '())  ; #t

(lat? '(bacon (and eggs))) ; #f

; or
(or (null? '()) (atom? '(d e f g)))  ; #t
(or (null? '(a b c)) (null? '()))    ; #t
(or (null? '(a b c)) (null? '(atom))); #f

; member?
(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
               (member? a (cdr lat)))))))

(member? 'tea '(coffee tea or milk))  ; #t
(member? 'poached '(fried eggs and scrambled eggs)) ; #f
(member? 'meat '(mashed potatoes and meat gravy))   ; #t

; The first commandment
; Always ask null? as the first question in expressing any function

(member? 'liver '(bagels and lox))  ; #f
      