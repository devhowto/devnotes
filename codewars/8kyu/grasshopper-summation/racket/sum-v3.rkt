#lang racket/base
(provide sum)

;;;;
;; Sums all whole numbers from 1 to n.
;;
;; ASSUME: n >= 1.
;;
;; sum :: Int -> Int
;;
(define (sum to)
  (if (zero? to)
      0
      (+ to (sum (sub1 to)))))
