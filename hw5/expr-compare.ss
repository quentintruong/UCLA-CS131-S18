; First, write a Scheme procedure (expr-compare x y) that implements the specification described above
(define (expr-compare x y)
    (expr-compare-decision x y '%)
)

(define (expr-compare-decision x y decision)
    (let ([output (expr-compare-decision-helper x y decision (bound-variable-searcher x y))])
        (if (equal? (length output) 1)
            (car output)
            output
        )
    )
)

(define (expr-compare-decision-helper x y decision bindings)
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
                (append (list (expr-compare-decision-helper (car x) (car y) decision bindings)) (expr-compare-decision-helper (cdr x) (cdr y) decision bindings))
                (append (expr-compare-decision-helper (car x) (car y) decision bindings) (expr-compare-decision-helper (cdr x) (cdr y) decision bindings))
            )
        ]
        [else 
            (cond 
                [(equal? (replace x bindings) (replace y bindings)) 
                    (list (replace x bindings))
                ]
                [
                (and
                    (equal? '#t x)
                    (equal? '#f y)
                )
                    (list decision)
                ]
                [
                (and
                    (equal? '#f x)
                    (equal? '#t y)
                )
                    (list (list 'not decision))
                ]
                [else
                    (list (list 'if decision (replace x bindings) (replace y bindings)))
                ]
            )
        ]
    )
)

(define (replace symbol bindings)
    (if (assoc symbol bindings)
        (second (assoc symbol bindings))
        symbol
    )
)

(define (bound-variable-searcher x y)
    (cond
        [(and 
            (list? x) (not (empty? x)) (not (equal? 'quote (car x))) 
            (list? y) (not (empty? y)) (not (equal? 'quote (car y))) 
            (= (length x) (length y))
            (or (and (equal? 'if (car x)) (equal? 'if (car y))) (and (not (equal? 'if (car x))) (not (equal? 'if (car y)))))
         )
            (cond 
                [(and 
                    (equal? 'let (car x)) 
                    (equal? 'let (car y))
                 )
                    (append (grabber-let (second x) (second y)) (bound-variable-searcher (cdr x) (cdr y)))
                ]
                [(and 
                    (equal? 'lambda (car x)) 
                    (equal? 'lambda (car y))
                 )
                    (append (grabber-lambda (second x) (second y)) (bound-variable-searcher (cdr x) (cdr y)))
                ]
                [else
                    (append (bound-variable-searcher (car x) (car y)) (bound-variable-searcher (cdr x) (cdr y)))
                ]
            )
        ]
        [else 
            empty
        ]
    )
)

(define (grabber-let x y)
    (cond 
        [(and
            (list? x) (not (empty? x))
            (list? y) (not (empty? y))
        )
            (if (equal? (caar x) (caar y))
                (grabber-let (cdr x) (cdr y))
                (append (list   (list (caar x) 
                                    (string->symbol (string-append (symbol->string (caar x)) "!" (symbol->string (caar y))))
                                )
                                (list (caar y) 
                                    (string->symbol (string-append (symbol->string (caar x)) "!" (symbol->string (caar y))))
                                )
                        )
                        (grabber-let (cdr x) (cdr y))
                )
            )
        ]
        [else
            empty
        ]
    )
)

(define (grabber-lambda x y)
    (cond 
        [(and
            (list? x) (not (empty? x))
            (list? y) (not (empty? y))
        )
            (if (equal? (car x) (car y))
                (grabber-lambda (cdr x) (cdr y))
                (append (list   (list (car x) 
                                    (string->symbol (string-append (symbol->string (car x)) "!" (symbol->string (car y))))
                                )
                                (list (car y) 
                                    (string->symbol (string-append (symbol->string (car x)) "!" (symbol->string (car y))))
                                )
                        )
                        (grabber-lambda (cdr x) (cdr y))
                )
            )
        ]
        [else
            empty
        ]
    )
)

; Second, write a Scheme procedure (test-expr-compare x y) that tests your implementation of expr-compare by using eval to evaluate the expression x, 
; and to evaluate the expression returned by (expr-compare x y) in the same context except with % bound to #t, 
(define (test-expr-compare x y)
    (and 
        (equal? (eval (expr-compare-decision x y #t)) (eval x))
        (equal? (eval (expr-compare-decision x y #f)) (eval y))
    )
)

; Third, define two Scheme variables test-expr-x and test-expr-y that contain data 
; that can be interpreted as Scheme expressions that test expr-compare well
(define test-expr-x '(let ([a (lambda (b) (+ b b))]) (if c (cons (a 1) (list 1)) (cons (a 1) (list f d)))))
(define test-expr-y '(let ([z (lambda (b) (+ b b))]) (if c (cons (z 2) (list 2)) (cons (z 2) (list f p)))))


