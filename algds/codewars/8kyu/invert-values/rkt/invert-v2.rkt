#lang racket

(provide invert)

;;;;
;; Inverts all integer values in the list.
;;
;; invert :: [Int] -> [Int]
;;
;; - T.C: O(n).
;; - S.C: O(1).
;;
;; Not tail-call.
;;
(define (invert xs)
  (if (empty? xs) '()
      (cons (- (car xs))
            (invert (cdr xs)))))

