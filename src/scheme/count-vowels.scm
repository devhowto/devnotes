(import srfi-13)

;;;;
;; Count the number of vowels in s.
;;
(define (count-vowels str)
  (string-fold (lambda (c acc)
                 (if (or (string-index "aeiou" c)
                         (string-index "AEIOU" c))
                     (+ acc 1)
                     acc))
               0
               str))

