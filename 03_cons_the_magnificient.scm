(define rember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) a) (cdr lat))
     (else (cons (car lat) (rember a (cdr lat)))))))

(rember 'bacon '(bacon lettuce and tomato))  ; (lettuce and tomato)

(rember 'and '(bacon lettuce and tomato)) ; (bacon lettuce tomato)

; The second commandment
; Use cons to build lists

(rember 'and '(bacon lettuce and tomato))  ; (bacon lettuce tomato)
(rember 'sauce '(soy sauce and tomato sauce)) ; (soy and tomato sauce)

; firsts
(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (car (car l)) (firsts (cdr l)))))))

(firsts '((apple peach pumpkin)
          (plum pear cherry)
          (grape raisin pea)
          (bean carrot eggplant)))  ; (apple plum grape bean)
(firsts '((a b) (c d) (e f)))       ; (a c e)
(firsts '())                        ; ()
(firsts '((five plums)
          (four)
          (eleven green oranges)))  ; (five four eleven)
(firsts '(((five plums) four)
          (eleven green oranges)
          ((no) more)))             ; ((five plums) eleven (no))

; The third commandment
; When building a list, describe the first typical element, and then
; cons it onto the natural recursion.

(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons old
                                (cons new (cdr lat))))
     (else (cons (car lat) (insertR new old (cdr lat)))))))

(insertR 'topping
         'fudge
         '(ice cream with fudge for desert))
                                        ; (ice cream with fudge topping for desert)
(insertR 'jalapeno
         'and
         '(tacos tamales and salsa)) ; (tacos tamales and jalapeno salsa)

(insertR 'e
         'd
         '(a b c d f g d h)) ; (a b c d e f g d h)

(insertR 'topping
         'fudge
         '(ice cream with fudge for desert))
                                        ; (ice cream with fudge topping for desert)

; Now try insertL
(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons new lat))
     (else (cons (car lat) (insertL new old (cdr lat)))))))

; subst
(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons new (cdr lat)))
     (else (cons (car lat) (subst new old (cdr lat)))))))

(subst 'topping
       'fudge
       '(ice cream with fudge for desert)) ; (ice cream with topping for desert)

; try subst2 which replaces either first occurrence of o1 or the first occurrence of
; o2 by new
(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) '())
     ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
     (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(subst2 'vanilla
        'chocolate
        'banana
        '(banana ice cream with chocolate topping))
                                        ; (vanilla ice cream with chocolate topping)


; multirember gives as its final value the lat with all occurrences of a removed.
(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) a) (multirember a (cdr lat)))
     (else (cons (car lat) (multirember a (cdr lat)))))))

(multirember 'cup '(coffee cup tea cup and hick cup)) ; (coffee tea and hick)

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

(multiinsertR 'fried
              'chips
              '(chips and fish or fish and fried))
                               ; (chips fried and fish or fish and fried)

(multiinsertL 'fried
              'chips
              '(chips and fish or fish and fried))
                               ; (fried chips and fish or fish and fried)

; The fourth commandment
; Always change at least one argument while recurring. It must be changed to be
; closer for termination. The changing argument must be tested in the termination
; condition: when using cdr, test termination with null?.

(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
     (else (cons (car lat) (multisubst new old (cdr lat)))))))

(multisubst 'topping
            'fudge
            '(fudge cream with fudge for desert))
                                        ;(topping cream with topping for desert)


