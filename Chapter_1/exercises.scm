; 5 + 4 + (2 − (3 − (6 + 4/5 )))
; ------------------------------
;       3(6 − 2)(2 − 7)

(define dividend (+ 5 4
                      ( - 2
                          (- 3
                             ( + 6
                                 ( / 4 5))))))

(define divisor (* 3 (- 6 2) (- 2 7)))

; Exercise 1.3: Define a procedure that takes three numbers
; as arguments and returns the sum of the squares of the two
; larger numbers.

(define (abs x)
  (if (> 0 x)
      (* -1 x)
      x))

(define (square x) (* x x))
(define (greater a b) (if (> a b) a b))
(define (sum-greatest-squares a b c)
  (+
    (square (greater a b))
    (square (greater b c))))

(define (square-root x)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (average a b)
    (/ (+ a b) 2))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (define (good-enough? guess)
    (<
     (abs (/
       (- guess (improve guess))
       guess))
     0.00001))
  (sqrt-iter 1.0)
)

;Newton's method for cube roots
;x/y^2 + 2 * y
;-------------
;      3

(define (cube-root x)
  (define (improve guess)
    (/
     (+ (/ x (square guess))
        (* 2 guess))
     3))
  (define (good-enough? guess)
    (<
     (abs (- guess (improve guess)))
     0.0001))
  (define (cube-root-iter guess)
    (if (good-enough? guess)
        guess
        (cube-root-iter (improve guess))))
  (cube-root-iter 1.0)
)

(display (cube-root 27))
(newline)
(display (square-root 9))






















