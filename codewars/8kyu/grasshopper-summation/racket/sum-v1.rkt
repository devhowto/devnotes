#lang racket/base
(provide sum)

;;;;
;; Sums all whole numbers from 1 to n.
;;
;; ASSUME: n >= 1.
;;
;; sum :: Int -> Int
;;
(define sum
  (λ (n [acc 1])
    (cond
      [(= n 1) acc]
      [else (sum (- n 1) (+ acc n))])))
