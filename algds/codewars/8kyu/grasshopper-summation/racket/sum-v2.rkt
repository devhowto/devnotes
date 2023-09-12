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
(define (sum to)
  (let go ([n to] [acc 1])
    (cond
      [(= n 1) acc]
      [else (go (- n 1) (+ acc n))])))

