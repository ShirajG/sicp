(define (abs x)
  (if (> 0 x)
      (* -1 x)
      x))

(define (identity x)
  (* 1 x))

(define (increment x)
  (+ 1 x))

(define (even? x)
  (= (remainder x 2) 0))

(define (square x) (* x x))
(define (squares list)
  (map square list))

(define (switch-word word)
  (cond ((or (equal? word 'I) (equal? word 'me)) 'you)
        ((equal? word 'you) 'me)
        (else word)))

(define (switch-first-word sentence)
  (cond ((equal? (first sentence) 'me) (cons 'I (cdr sentence)))
        (else sentence)))

(define (switch sentence)
  (switch-first-word (map switch-word sentence)))

(switch '(you think this is a test for me))
(define (greater a b) (if (> a b) a b))
(define (sum-greatest-squares a b c)
  (+
    (square (greater a b))
    (square (greater b c))))

(define (ordered? nums)
  (cond ((null? (cdr nums)) #t)
        ((> (car nums) (car (cdr nums))) #f)
        (else (ordered? (cdr nums)))))

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
  (sqrt-iter 1.0))

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
  (cube-root-iter 1.0))

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

; Not a math major here, going to skip this.
; Looks like this wants you to prove the Binet formula???

; 1.16: Iterative algorithm for computing exponentiation using
; successive squaring
; Given hint: (b^n/2)^2 = (b^2)^n/2
(define (exponent b n)
        (exponent-iter b n 1))
; use a to track state, ab^n should always == the initial b^n
(define (exponent-iter b n a)
  (cond ((= n 0) a)
        ((even? n)(exponent-iter (square b) (/ n 2) a ))
        (else (exponent-iter b (- n 1) (* a b)))))

(exponent 2 6)

(define (ends-e sentence)
  (filter
   (lambda (word)
     (cond ((equal? (last word) 'e) #t)
           (else #f)))
   sentence))

(define (ends-e sentence)
  (cond ((empty? sentence) sentence)
        ((equal? (last (first sentence)) 'e)
         (cons (first sentence) (ends-e (bf sentence))))
        (else (ends-e (bf sentence)))))

(ends-e '(please put the salami above the blue elephant))

;; Section 1.3
;; Ex. 1.31
;; The sum procedure is only the simplest of a vast num-
;; ber of similar abstractions that can be captured as higher-
;; order procedures. Write an analogous procedure called
;; product that returns the product of the values of a
;; function at points over a given range. Show how to de-
;; fine factorial in terms of product . Also use product
;; to compute approximations to π using the formula:
;; π/4 =  2/3 * 4/3 * 4/5 * 6/5 * 6/7 * 8/7
;; t1 -> 2(floor of (t)/2) + 2)/(floor of (t+1)/2) + 2 )

;; Start at a, use next to generate next num, continue until a hits end

(define (product a fn next end)
  (if (> a end)
       1
      (* (fn a) (product (next a) fn next end))))

;; factorial in terms of product

(define (factorial x)
  (product 1 identity increment x))
(factorial 4)

;; pi using the formula given

(define (pi)
  (define (compute-pi-term x)
    (define (numerator x)
      (+ (* 2 (floor (/ x 2))) 2))
    (define (denominator x)
      ( + 3 (* 2 (floor (/ (- x 1) 2)))))
    (/ (numerator x) (denominator x)))
  (* 4.0 (product 1 compute-pi-term increment 10000)))
(pi)

;; Ex. 1.32a
;; Show that sum and product (Exercise 1.31) are both
;; special cases of a still more general notion called accumulate
;; that combines a collection of terms, using some gen-
;; eral accumulation function:
;; (accumulate combiner null-value term a next b)
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner a (accumulate combiner null-value term (next a) next b))))

(accumulate * 1 identity 8 increment 10)
(accumulate + 0 identity 0 increment 10)

























;;
