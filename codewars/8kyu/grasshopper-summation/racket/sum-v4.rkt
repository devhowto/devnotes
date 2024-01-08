#lang racket
(provide sum)

;;;;
;; Sums all whole numbers from 1 to n.
;;
;; ASSUME: n >= 1.
;;
;; sum :: Int -> Int
;;
(define sum
  (λ (n) (quotient (* n (add1 n)) 2)))
