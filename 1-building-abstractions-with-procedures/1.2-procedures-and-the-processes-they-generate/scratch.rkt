#lang sicp
;; vim: ft=racket
;; REPL command: racket -i -p neil/sicp -l xrepl

;; Alternatively, run this in a REPL session to load the neil/sicp package:
;; (require (planet "neil/sicp"))

(define (factorial n)
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

(define (factorial n)
  (define (fact-iter product counter max-count)
    (if (> counter max-count)
      product
      (fact-iter (* counter product) (+ counter 1) max-count)))
  (fact-iter 1 1 n))

(factorial 6)

(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
9

(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
9

;; Ackermann's function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (dec x)
                 (A x (dec y))))))

(A 1 10)
(A 0 (A 1 9))
(A 0 (A 0 (A 1 8)))
(A 0 (A 0 (A 0 (A 1 7))))
(A 0 (A 0 (A 0 (A 0 (A 1 6)))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
(A 0 (A 0 (A 0 (A 0 (A 0 32)))))
(A 0 (A 0 (A 0 (A 0 64))))
(A 0 (A 0 (A 0 128)))
(A 0 (A 0 256))
1024

(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)))
(A 1 (A 1 (A 0 (A 1 1))))
(A 1 (A 1 (A 0 2)))
(A 1 (A 1 4))
(A 1 (A 0 (A 1 3)))
(A 1 (A 0 (A 0 (A 1 2))))
(A 1 (A 0 (A 0 (A 0 (A 1 1)))))
(A 1 (A 0 (A 0 (A 0 2))))
(A 1 (A 0 (A 0 4)))
(A 1 (A 0 8))
(A 1 16)
;; etc.
65536

(A 3 3)
(A 2 (A 3 2))
(A 2 (A 2 2))
(A 2 (A 1 (A 2 1)))
(A 2 (A 1 2))
(A 2 (A 0 (A 1 1)))
(A 2 (A 0 2))
(A 2 4)
;; etc.
;; 65536

(A 0 n) ;; => 2n
(A 1 n) ;; => 2^n
;; 0 => 0
;; 1 => 2
;; 2 => (A 0 (A 1 1)) => (A 0 2) => 4
;; 3 => (A 0 (A 1 2)) => (A 0 (A 0 (A 1 1))) => (A 0 (A 0 2)) => (A 0 4) => 8
(A 2 n) ;; => 2^(something...? math is hard)
;; 0 => 0 (2^0)
;; 1 => 2 (2^1)
;; 2 => (A 1 (A 2 1)) => (A 1 2) => 2^2 => 4
;; 3 => (A 1 (A 2 2)) => (A 1 4) => 2^4 => 16
;; 4 => (A 1 (A 2 3)) => (A 1 16) => 2^16 => 65536

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(define (count-change amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (count-change amount
                               (- kinds-of-coins 1))
                 (count-change (- amount
                                  (first-denomination kinds-of-coins))
                               kinds-of-coins)))))

(count-change 100 5)

(define (f n)
  (if (< n 3)
    n
    (+ (f (- n 1))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3))))))

(f 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (expt b n)
  (if (= n 0)
    1
    (* b (expt b (- n 1)))))

(expt 2 4)

(define (expt b n)
  (define (expt-iter b counter product)
    (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))
  (expt-iter b n 1))

(define (square x)
  (* x x))

(define (even? n)
  (= (remainder n 2) 0))

(even? 23)
(even? 24)

(define (fast-expt b n)
  (cond ((= n 0)   1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else      (* b (fast-expt b (- n 1))))))

(fast-expt 2 19)
(fast-expt 2 1000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Euclid's Algorithm for finding the greatest common denominator
(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(gcd 16 28)
(gcd 206 40)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (divides? a b)
  (= (remainder b a) 0))

(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n)   test-divisor)
          (else                        (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(prime? 0)
(prime? 1)
(prime? 2)
(prime? 3)
(prime? 4)
(prime? 5)

;;; The Fermat test of primality

;; a helper function that computes the exponential of a number modulo another
;; number
(define (expmod base exp m)
  (cond ((= exp 0)
         1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(fermat-test 79)

(define (fast-prime? n times)
  (cond ((= times 0)     true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else            false)))

(fast-prime? 241 10)

