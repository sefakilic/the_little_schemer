; atoms
'atom
'turkey
1492
'u
'*abc$

; lists and s-expressions

'(atom)
'(atom turkey or)
'((atom turkey) or)  ; two S-expressions are enclosed by parentheses

; s-expressions
; All atoms are S-expressions
; All lists are S-expressions
'xyz
'(x y z)
'((x y) z)
'(how are you doing so far)
'(((how) are) ((you) (doing so) far))
'()
'(() () () ())


; The law of car
; The primitive car is defined only for non-empty lists

(car '(a b c))              ; a
(car '((a b c) x y z))      ; (a b c)
;(car 'hotdog) ; you cannot ask the car of an atom
;(car '())     ; you cannot ask the car of the empty list

(car '(((hotdogs)) (and) (pickle) relish))      ; ((hotdogs))
(car (car '(((hotdogs)) ((and)))))              ; (hotdogs)

; cdr
(cdr '(a b c))          ; (b c)
(cdr '((a b c) x y z))  ; (x y z)
(cdr '(hamburger))      ; ()
(cdr '((x) t r))        ; (t r)
; (cdr 'hotdogs) ; you cannot ask the cdr of an atom
; (cdr ())       ; you cannot ask the cdr of the null list

; The law of cdr
; The primitive cdr is defined only for non-empty lists. The cdr of any non-empty
; list is always another list.
(car (cdr '((b) (x y) ((c)))))      ; (x y)
(cdr (cdr '((b) (x y) ((c)))))      ; (((c)))
(cdr (car '(a (b (c) d))))          ; no answer, since (car l) is an atom and cdr
                                    ; does not take an atom as an argument.

; cons
(cons 'peanut '(butter jelly))      ; (peanut butter jelly)
(cons '(banana and) '(peanut butter and jelly))
                                        ; ((banana and) peanut butter and jelly)
(cons '((help) this) '(is very ((hard) to learn)))
                                        ; (((help) this) is very ((hard) to learn))
(cons '(a b (c)) '())               ; ((a b (c)))
(cons 'a '())                       ; (a)
(cons '((a b c)) 'b)                ; no answer*
                                    ; it works in practice: (((a b c)) . b)
; The law of cons
; The primitive cons takes two arguments. The second argument to cons must be a
; list. The result is a list.

(cons 'a (car '((b) c d)))      ; (a b)
(cons 'a (cdr '((b) c d)))      ; (a c d)

; null
(null? '())        ; #t
(null? (quote ())) ; #t
(null? '(a b c))   ; #f
(null? 'spaghetti) ; you cannot ask null? of an atom
                   ; in practice, it returns #f

; the law of null?
; The primitive null? is defined only for lists?

; atom?

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(atom? 'Harry)                              ; #t
(atom? '(Harry had a heap of apples))       ; #f
(atom? (car '(Harry had a heap of apples))) ; #t
(atom? (cdr '(Harry had a heap of apples))) ; #f
(atom? (cdr '(Harry)))                      ; #f

(atom? (car (cdr '(swing low sweet cherry oat))))  ; #t
(atom? (car (cdr '(swing (low sweet) cherry oat)))); #f

; eq?
(eq? 'Harry 'Harry)      ; #t
(eq? 'margarine 'butter) ; #f
(eq? '() '(strawberry))  ; no answer. () and (strawberry) are lists.
                         ; in practice, it returns #f
(eq? 6 7)                ; no answer. 6 and 7 are numbers.
                         ; in practice, it returns #f

; The law of eq?
; The primitve eq? takes two arguments. Each must be a non-numeric atom.

(eq? (car '(Mary had a little lamb chop)) 'Mary)   ; #t
(eq? (cdr '(soured milk)) 'milk)                   ; no answer. first argument is not
                                                   ; a non-numeric atom.  in practice
                                                   ; it returns #f
(eq? (car '(beans beans we need jelly beans))
     (car (cdr '(beans beans we need jelly beans))))  ; #t








