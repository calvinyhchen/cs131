(define (bind-para x y)
    (string->symbol 
        (string-append (symbol->string x) "!" (symbol->string y) ) 
    )
)

(define (match-let-var x y)
    (cond
        ((and (equal? x '()) (equal? y '()) )  (values '() '())  )
        ((equal? (length x) (length y) )
            (if (equal? (caar x) (caar y))
                (match-let-var (cdr x) (cdr y))
                (let-values (( (newx newy) (match-let-var (cdr x) (cdr y)) ))
                    (let ( (bind (bind-para (caar x) (caar y))) )
                        (values
                            (cons (caar x) (cons bind newx)) ; (x (bind x) ...)
                            (cons (caar y) (cons bind newy)) ; (y (bind y) ...)
                        )
                    )
                )
            )           
        )
        (else (values '() '()) )
    )
)

(define (update var mlst)
    (if (equal? mlst '()) 
        '()                              
        (if (equal? var (car mlst) ) 
            (cadr mlst)                   
            (update var (cdr mlst))
        )
    )
) 

(define (get-match var mlst)
    (let ((ismember (update var mlst)))
        (if (equal? ismember '())  
            var         ; original var
            ismember    ; update var
        )
    )
)

(define (build-new-var var mlst)
    (if (equal? var '())
        '()
        (let ((res (get-match (caar var) mlst)))
            (if (equal? res (caar var))
                (append (cons (car var) '()) (build-new-var (cdr var) mlst))
                (append (cons (cons res (cdar var)) '() )  (build-new-var (cdr var) mlst))
            )

        )        
    )
)

(define (build-new-val val mlst)
    (if (equal? val '())
        '()
        (let ((res (get-match (cadar val) mlst)))
            (if (equal? res (cadar val))
                (append (cons (car val) '()) (build-new-val (cdr val) mlst))
                (append (cons (cons (caar val) (cons res '())) '()) (build-new-val (cdr val) mlst))
            )
        )        
    )
)

(define (build-new-expr expr mlst)
    (if (list? expr)
        (cond
            ((equal? expr '()) '() )

            ; (let (variables) expr)
            ((equal? (car expr) 'let)
                (cons 'let
                    (append (cons (build-new-val (cadr expr) mlst) '())   (build-new-expr (cddr expr) '())  )
                )
            )

            ; (lambda variables expr)
            ((equal? (car expr) 'lambda)
                (cons 'lambda (build-new-expr (cdr expr) mlst) )
            )
            ; quote
            ((equal? (car expr) 'quote)
                expr
            ) 

            (else
                (cons (build-new-expr (car expr) mlst) (build-new-expr (cdr expr) mlst))
            )            
        )
        (get-match expr mlst)
    )
)




(define (let-compare x y)
    (let-values (( (matchx matchy) (match-let-var (cadr x) (cadr y)) ))
        ; (let (variables) expr)
        (let ((newxvar (build-new-var  (cadr x) matchx))  
             (newxexpr (build-new-expr (caddr x) matchx))  )
            (let ((newyvar (build-new-var (cadr y) matchy)) 
                 (newyexpr (build-new-expr (caddr y) matchy))  )
                (cons 'let (expr-compare                                ; (expr-compare newx newy)
                                (cons newxvar ( cons newxexpr '())) 
                                (cons newyvar ( cons newyexpr '())) 
                            )       
                )
            )
        )
    )
)

(define (match-lambda-var x y)
    (cond
        ((and (equal? x '()) (equal? y '()) )  (values '() '())  )
        ((equal? (length x) (length y) )
            (if (equal? (car x) (car y))
                (match-lambda-var (cdr x) (cdr y))
                (let-values (( (newx newy) (match-lambda-var (cdr x) (cdr y)) ))
                    (let ( (bind (bind-para (car x) (car y))) )
                        (values
                            (cons (car x) (cons bind newx))
                            (cons (car y) (cons bind newy))
                        )
                    )
                )
            )           
        )
        (else (values '() '()) )
    )    
)



(define (lambda-compare x y)
    ; (lambda variables expr)
    (let-values (( (matchx matchy) (match-lambda-var (cadr x) (cadr y)) ))
        (let ((newxvar (build-new-expr  (cadr x) matchx))  
             (newxexpr (build-new-expr (caddr x) matchx))  )
            (let ((newyvar (build-new-expr (cadr y) matchy)) 
                 (newyexpr (build-new-expr (caddr y) matchy)))
                (cons 'lambda (expr-compare                             ; (expr-compare newx newy)
                                (cons newxvar ( cons newxexpr '())) 
                                (cons newyvar ( cons newyexpr '())) 
                            )       
                )
            )
        )
    )
)

(define (match-car x y)
    (cond
        ( (or (equal? x 'quote) (equal? y 'quote)) #f )
        ( (or (equal? x 'let) (equal? y 'let)) #f )
        ( (or (equal? x 'lambda) (equal? y 'lambda)) #f )
        ( (or (equal? x 'if) (equal? y 'if)) #f )
        ( else #t )
    )
)


(define (expr-compare x y)
    (cond
        ((equal? x y) x )
        ((and (eq? x #t) (eq? y #f)) (quasiquote %) )
        ((and (eq? x #f) (eq? y #t)) (quasiquote (not %)) )
        ((and (list? x) (list? y))   
            (if (eq? (length x) (length y) )
                (if (eq? (car x) (car y) ) 
                    (cond
                        ; quote
                        ((equal? (car x) 'quote)   
                            (if  ( equal? (cdr x) (cdr y) ) 
                                (quote x)
                                (quasiquote (if % (unquote x) (unquote y)))
                            ) 
                        )

                        ; let
                        ((equal? (car x) 'let) (let-compare x y) )

                        ; lambda
                        ((equal? (car x) 'lambda) (lambda-compare x y) )

                        (else 
                            ( cons (expr-compare (car x) (car y)) (expr-compare (cdr x) (cdr y)) ) 
                        )
                    )
                    ( if (match-car (car x) (car y))
                        ( cons (expr-compare (car x) (car y)) (expr-compare (cdr x) (cdr y)) )
                        ( quasiquote (if % (unquote x) (unquote y)) )
                    )
                )
                ( quasiquote (if % (unquote x) (unquote y)) )
            ) 
        )
        (else 
            (quasiquote (if % (unquote x) (unquote y))) 
        )
    )
)

(define (test-expr-compare x y)
    (and
        (equal? 
            (eval x)
            (eval (cons 'let (cons '((% #t)) (cons (expr-compare x y) '() ))) ) ; (let ((% #t)) expr)
        )
        (equal? 
            (eval y)
            (eval (cons 'let (cons '((% #f)) (cons (expr-compare x y) '() ))) ) ; (let ((% #f)) expr)
        )
    )
)

(define test-expr-x 
    '(list
        (let ((a 0) (b 1)) 
            (list 
                (quote (a b))
                (cons a b)
                (if b b a)
                (let ((y b) (x a)) (+ x y))
                ((lambda (x y) (+ x y)) 1 2)
                ((lambda (c) (+ b c)) b)
                ((lambda (c) (+ (let ((a 3)) a ) c)) b )
                (let ((b 2))
                    (eqv?
                        ((lambda (c) (+ (let ((c 3)) c ) c)) b )
                        b
                    )
                )
            )
        )
        (quote (a b)) 
        #f    
    )
)

(define test-expr-y
    '(list
        (let ((b 0) (a 1))
            (list 
                (quote (b a))
                (list a b)
                (if a a b)
                (let ((x a) (y b)) (- x y))
                ((lambda (y x) (+ y x)) 1 2)
                ((lambda (c) (+ a c)) a)
                ((lambda (c) (+ (let ((c 3)) c ) c)) a )
                (let ((a 2))
                    (eq? 
                        ((lambda (c) (+ (let ((c 3)) c ) c)) a )
                        a
                    )
                )
            )
        )
        (quote (b a))
        #t
    )
)

; (expr-compare test-expr-x test-expr-y)
; (test-expr-compare test-expr-x test-expr-y)

; (load "expr-compare.ss") 

