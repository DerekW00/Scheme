(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
  (map (lambda (lst) (cons first lst)) rests))

(define (zip pairs)
   (cons (map (lambda (lst) (car lst)) pairs)
     (cons (map (lambda (lst) (car (cdr lst))) pairs) nil)))



;; Returns a list of two-element lists
(define (enumerate s)


    (define (helper s counter)(
      if (null? s)()
      (cons (cons counter (cons (car s) nil))
        (helper (cdr s) (+ 1 counter)))))
        (helper s 0) )



;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)

 (cond
 ((null? denoms) ())
 ((= total 0) '(()))
  ((> (car denoms) total) (list-change total (cdr denoms)))
  (else (append (cons-all(car denoms) (list-change(- total (car denoms)) denoms))
    (list-change total (cdr denoms))
    )))


  )



;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr) expr
         ; BEGIN PROBLEM 19
         ; END PROBLEM 19
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))

           (cons form (cons params (let-to-lambda body)))

           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))

      (cons
        (cons 'lambda
          (cons
            (car
              (zip (let-to-lambda values)))
              (let-to-lambda body)))
              (cadr (zip (let-to-lambda values)))
           )

           ))
        (else

         (map let-to-lambda expr)
         
         )))
