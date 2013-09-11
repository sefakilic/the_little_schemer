; member?
(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (equal? (car lat) a)
               (member? a (cdr lat)))))))

(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((member? (car lat) (cdr lat)) #f)
     (else (set? (cdr lat))))))

(set? '(apple peaches apple plum))    ; #f
(set? '(apples peaches pears plums))  ; #t
(set? '())                            ; #t
(set? '(apple 3 pear 4 9 apple 3 4))  ; #f

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
     (else (cons (car lat) (makeset (cdr lat)))))))

(makeset '(apple peach pear peach
           plum apple lemon peach))  ; (pear plum apple lemon peach)

; try to write makeset using multirember
; multirember gives as its final value the lat with all occurrences of a removed.
(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) a) (multirember a (cdr lat)))
     (else (cons (car lat) (multirember a (cdr lat)))))))

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else (cons (car lat) (makeset (multirember (car lat) (cdr lat))))))))

(makeset '(apple peach pear peach
           plum apple lemon peach))  ; (apple peach pear plum lemon)

(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     (else (and (member? (car set1) set2)
                (subset? (cdr set1) set2))))))

(subset? '(5 chicken wings)
         '(5 hamburgers
           2 pieces fried chicken and
           light duckling wings))        ; #t
(subset? '(4 pounds of horseradish)
         '(four pounds chicken and
           5 ounces horseradish))        ; #f

(subset? '(6 large chickens with wings)
         '(6 chickens with large wings)) ; #t

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     (else (or (member? (car set1) set2)
               (intersect? (cdr set1) set2))))))

(intersect? '(stewed tomatoes and macaroni)
            '(macaroni and cheese))             ; #t

(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2) (cons (car set1)
                                      (intersect (cdr set1) set2)))
     (else (intersect (cdr set1) set2)))))

(intersect '(stewed tomatoes and macaroni)
           '(macaroni and cheese))        ; (and macaroni)

(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2) (union (cdr set1) set2))
     (else (cons (car set1) (union (cdr set1) set2))))))

(union '(stewed tomatoes and macaroni casserole)
       '(macaroni and cheese))  ; (stewed tomatoes casserole macaroni and cheese)

; xxx returns all the atoms in set1 that are not in set2.
(define xxx
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2) (xxx (cdr set1) set2))
     (else (cons (car set1) (xxx (cdr set1) set2))))))

(xxx '(stewed tomatoes and macaroni casserole)
     '(macaroni and cheese))   ; (stewed tomatoes casserole)

(define intersectall
  (lambda (l-set)
    (cond
     ((null? (cdr l-set)) (car l-set))
     (else (intersect (car l-set)
                      (intersectall (cdr l-set)))))))

(intersectall '((a b c) (c a d e) (e f g h a b)))      ; (a)
(intersectall '((6 pears and)
                (3 peaches and 6 peppers)
                (8 pears and 6 plums)
                (and 6 prunes with some apples)))      ; (6 and)

(pair? '(pear pear))        ; #t
(pair? '(3 7))              ; #t


(define a-pair?
  (lambda (x)
    (cond
     ((atom? x) #f)
     ((null? x) #f)
     ((null? (cdr x)) #f)
     ((null? (cdr (cdr x))) #t)
     (else #f))))

(a-pair? '((2) (pair)))       ; #t

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

(define third
  (lambda (p)
    (car (cdr (cdr p)))))


; firsts
(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (car (car l)) (firsts (cdr l)))))))

; we use rel to stand for relation
; we use fun to stand fo function
(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(fun? '((4 3) (4 2) (7 6) (6 2) (3 4)))   ; #f, because 4 is repeated
(fun? '((8 3) (4 2) (7 6) (6 2) (3 4)))   ; #t
(fun? '((d 4) (b 0) (b 9) (e 5) (g 4)))   ; #f, because b is repeated

(define revrel
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else (cons (build (second (car rel)) (first (car rel)))
                 (revrel (cdr rel)))))))

(revrel '((8 a) (pumpkin pie) (got sick))) ; ((a 8) (pie pumpkin) (sick got))

; suppose we had the function revpair that reversed the two components of a pair like
; this:
(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

; redefine revrel using revpair
(define revrel
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else (cons (revpair (car rel))
                 (revrel (cdr rel)))))))

; seconds
(define seconds
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (second (car l)) (seconds (cdr l)))))))

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(fullfun? '((grape raisin)
            (plum prune)
            (stewed prune)))          ; #f
(fullfun? '((grape raisin)
            (plum prune)
            (stewed grape)))          ; #t

(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))

(one-to-one? '((chocolate chip) (doughy cookie)))   ; #t

