#lang racket

(provide invert)

;;;;
;; Inverts all integer values in the list.
;;
;; - T.C: O(n).
;; - S.C: O(1).
;;
;; invert :: [Int] -> [Int]
;;
(define invert
  (λ (xs) (map - xs)))

sc
