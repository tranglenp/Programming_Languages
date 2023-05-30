; --- problem 1 ---
(define (palindromic xs)
  (add-lists xs (reverse xs)))


; --- problem 2 (a) ---
(define (stream-to-list stream n)
  (cond [(= n 0) null]
        [#t (cons (car (stream)) (stream-to-list (cdr (stream)) (- n 1)))]))

(define nats
  (letrec ([aux (λ(x) (λ()(cons x (aux (+ x 1)))))])
    (aux 0)))

(define (fact n)
  (cond [(zero? n) 1]
        [#t (* n (fact (- n 1)))]))

(define facts
  (letrec ([aux (λ(x y) (λ()(cons x (aux (* x y) (+ y 1)))))])
      (aux 1 1)))


; --- problem 2 (b) ---
(define (fibo n)
  (cond [(< n 2) n]
        [#t  (+ (fibo (- n 1)) (fibo (- n 2)))]))
		
(define fibonaci
  (letrec ([aux (λ(x y) (λ()(cons x (aux y (+ x y)))))])
      (aux 0 1)))


; --- problem 3 ---
(define (f-cond f n-max)
  (λ(n) (cond [(>= n n-max) #f]
              [#t (f n)])))

; main
(define (stream-until f s)
  (cond [(false? (f (car (s)))) null]
        [#t (cons (f (car (s))) (stream-until f (cdr (s))))]))


; --- problem 4 ---
(define (stream-map f s) (λ() (cons (f (car(s))) (stream-map f (cdr (s))))))


; --- problem 5 ---
(define (zip xs ys)
  (cond [(or (null? xs) (null? ys)) null]
        [#t (cons (list (car xs) (car ys)) (zip (cdr xs) (cdr ys)))]))

(define (stream-zip s1 s2) (λ() (cons (list (car(s1)) (car(s2))) (stream-zip (cdr(s1)) (cdr(s2))))))

