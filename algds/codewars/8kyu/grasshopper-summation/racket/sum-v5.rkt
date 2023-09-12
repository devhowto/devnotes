#lang racket
(provide sum)

;;;;
;; Sums all whole numbers from 1 to n.
;;
;; ASSUME: n >= 1.
;;
;; Recursive tail call.
;;
;; sum :: Int -> Int
;;
(define sum
  (λ (n)
    (apply + (range (+ n 1)))))
