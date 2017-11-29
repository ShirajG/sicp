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

; Ackermann Function, work through it via substitution
; Good video: https://www.youtube.com/watch?v=i7sm9dzFtEI
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))
; (A 1 10);
; (A 0 (A 1 9));
;...2^10

; (A 2 4);
; (A 1 (A 2 3))
; (A 1 (A 1 (A 2 2)))
; (A 1 (A 1 (A 1 (A 2 1))))
; (A 1 (A 1 (A 1 2)))
; (A 1 (A 1 (A 0 (A 1 1))))
; (A 1 (A 1 4))
; (A 1 (A 0 (A 1 3)))
; (A 1 (A 0 (A 0 (A 1 2))))
; (A 1 (A 0 (A 0 (A 0 (A 1 1)))))
; (A 1 16)
; (A 0 (A 1 15))
;...2^16

; (A 3 3)
; (A 2 (A 3 2))
; (A 2 (A 2 (A 3 1)))
; (A 2 (A 2 2))
; (A 2 (A 1 (A 2 1)))
; (A 2 (A 1 2))
; (A 2 (A 0 (A 1 1)))
; (A 2 (A 0 2))
; (A 2 4)
; ...2^16 as per before

; (define (f n) (A 0 n)) => 2n
; (define (g n) (A 1 n)) => 2^n
; (define (h n) (A 2 n)) => 2^...(n times)
  ; ie: (A 2 4) => 2^2^2^2


; Recursive Version
(define (func1 n)
  (if (< n 3)
    n
    (+ (func1 (- n 1))
       (* 2 (func1 (- n 2)))
       (* 3 (func1 (- n 3))))))

;Pascals Triangle
;1
;1,1
;1,2,1
;1,3,3,1
;1,4,6,4,1
;1,5,10,10,5,1

(define (pascals-triangle row col)
  (cond ((= row 1) 1)
        ((= col 1) 1);first el in row is always 1
        ((= row col) 1);last element in a row is always 1
        (else (+ (pascals-triangle (- row 1) col)
                 (pascals-triangle (- row 1) (- col 1))))))

; 1.13: Prove that Fib(n) is the closest integer
; to (φ^n)/√5, where φ = (1 + √5)/2.
; Hint: Let ψ = (1 − √5)/2.
; Use induction and the definition of the Fibonacci numbers
; to prove that Fib(n) = (φ^n − ψ^n)/√5.





