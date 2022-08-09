#lang racket
(provide (all-defined-out))

#;(define (divisible-by-x?) (λ(n)
  (define helper (λ(y)
   (print (% y n))))
   )
)

#;(define (divisible k)
  (λ(x)
    (= (remainder x k) 0)))
; nO 1 RACHKETS HAS NO %
#;(define (divisible-by-x? k)
  (λ(x)
    (= (remainder x k) 0)))
;2
; use lambda to define a function
#;(define (function-9 lambda)
  (lambda 9) 
)

;3
#;(define my-map 
  (λ(lambda lst)
    (if (null? lst)   ;check this line properly
    ;(null? lst)
        null
        (cons (lambda (car lst)) (my-map lambda (cdr lst)))
    )
  )
)

;4
#;(define pair-up
  (λ(lst1 lst2)
    (if (or (null? lst1) (null? lst2))
        null
        (cons (list (car lst1) (car lst2)) (pair-up (cdr lst1) (cdr lst2)))
    )
  )
)

;5
#;((define helper1
  (λ (lambda lst)
    (cond
      [(null? lst) null]
      [(lambda (first lst))
         (cons (first lst) (helper1 lambda (rest lst)))]
      [else (helper1 lambda (rest lst))]
     )
   )
)

(define helper2
  (λ (lambda lst)
    (cond
      [(null? lst) null]
      [(not (lambda (first lst)))
         (cons (first lst) (helper2 lambda (rest lst)))]
      [else (helper2 lambda (rest lst))]
     )
   )
)
(define classify
  (lambda(lambda lst)
    (list (helper1 lambda lst) (helper2 lambda lst))
  )
)
)
;6
#;(define is-member?
  (λ(x lst)
    (if (null? lst)
        #f
        ( if (equal? x (car lst))
             #t
             (is-member? x (cdr lst))
        )
    )
  )
)


;7
#;(define my-sorted?
    (λ(lambda lst)
      ;if it passes the first then go on to the next if else return false
      ; if statement works in an: if then next format
      (if (null? (cdr lst))
          #t
          (if (lambda (car lst) (car (cdr lst)))
              (my-sorted? lambda (cdr lst))
              #f
          )
      )
    )
)

;8
#;(define my-flatten
    (λ(lst)
      (cond
        [(null? lst) null] ; null/empty/'()
        [(pair? (car lst))  ;pair?(#f if '()) vs list? (#t if '())
         (append (my-flatten (car lst)) (my-flatten (cdr lst)))]
        [else (cons (car lst) (my-flatten (cdr lst)))]
      )
    )
)

;9
#;((define upper-threshold
  (λ(lst x)
    ( helper (lambda(v)(< v x)) lst)
  )
)
(define helper
  (λ (lambda lst)
    (cond
      [(null? lst) null]
      [(lambda (first lst))
         (cons (first lst) (helper lambda (rest lst)))]
      [else (helper lambda (rest lst))]
     )
   )
))
;OR

#;(define upper-threshold
  (λ(lst x)
    ;> (let ([fumt (lambda(v)(< v 3))]) (fumt 5))
    ; #f
    ;(let ([lambda (lambda(v)(< v x))]) 
    (cond
      [(null? lst) null]
      [((lambda(v)(< v x)) (first lst))
         (cons (first lst) (upper-threshold (lambda(v)(< v x)) (rest lst)))]
      [else (upper-threshold (rest lst) (lambda(v)(< v x)))]
     )
      ;)
  )
)

;10
#;(define inc_helper(lambda(x)
       (if(not (= x 3))
          (add1 x)
          null)))

#;(define inc (let ([x 0])
         (display (inc_helper(x)))
         ;(display x)
        ))
(define funct (let fac ([n 10])
                (if (zero? n)
                    1
                    (* n (fac (sub1 n))))))


#;(define inc (lambda() (let ([x 0])
    (cond [(not (= x 3)) (add1 x)]
          [ ]))))


#;(define my-list-ref(lambda (lst n)
    (letrec ([nth-cdr
          (lambda(n)
            (cond
              [(null? lst) (error '"ERROR:Index out of bounds")]
              [(zero? n) lst]
              [else
               (set! lst (cdr lst))
               (nth-cdr (sub1 n))])
            )])
     (car (nth-cdr n)))
))

;11
(define (deep-reverse lst)
  (if (list? lst)
      (my-reverse (map deep-reverse lst))
      lst
  )
)
(define my-reverse
  (lambda(lst)
         (if(null? lst)
         null
         (append (my-reverse (cdr lst)) (list (car lst)))
         )
  )
)
