(define (expr-compare2 x y)
    (cond 
        ((equal? x y) x)
        (else (cons 'if (list '% x y)))
    )
)

(define (expr-compare x y)
    (cond
        [(and 
            (list? x) (empty? x)
            (list? y) (empty? y)
         )
            empty
        ]
        [(and 
            (list? x) (not (empty? x)) (not (equal? 'quote (car x))) 
            (list? y) (not (empty? y)) (not (equal? 'quote (car y))) 
            (= (length x) (length y))
            (or (and (equal? 'if (car x)) (equal? 'if (car y))) (and (not (equal? 'if (car x))) (not (equal? 'if (car y)))))
         )
            (if (and (list? (car x)) (list? (car y)))
                (append (list (expr-compare (car x) (car y))) (expr-compare (list-tail x 1) (list-tail y 1)))
                (append (expr-compare (car x) (car y)) (expr-compare (list-tail x 1) (list-tail y 1)))
            )
        ]
        [else 
            (cond 
                [(equal? x y) 
                    (list x)
                ]
                [
                (and
                    (equal? '#t x)
                    (equal? '#f y)
                )
                    (list '%)
                ]
                [
                (and
                    (equal? '#f x)
                    (equal? '#t y)
                )
                    (list (list 'not '%))
                ]
                [else
                    (list (list 'if '% x y))
                ]
            )
        ]
    )
)

(define (test ff)
    (eval `(let ([x ff] [y 3]), '(+ x y)))
)