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

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

; Update good-enough? to use a delta to judge
(define (good-enough? guess x)
  (<
   (abs (/
     (- guess (improve guess x))
     guess))
   0.0001))

;Newton's method for cube roots
;x/y^2 + 2 * y
;-------------
;      3

(define (improve-cube guess x)
  (/
   (+ (/ x (square guess))
      (* 2 guess))
   3))

(define (cube-good-enough? guess x)
  (<
   (abs (- guess (improve-cube guess x)))
   0.001))

(define (cube-root guess x)
  (if (cube-good-enough? guess x)
      guess
      (cube-root (improve-cube guess x) x)))

(cube-root 1.0 1000000)
