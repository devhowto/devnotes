#lang racket/base
(provide drop-vowels)

;;;;
;; Checks whether c is a vowel.
;;
;; vowel? :: char -> char
;;
(define (vowel? c)
  (cond
    [(eq? (char-downcase c) #\a) #t]
    [(eq? (char-downcase c) #\e) #t]
    [(eq? (char-downcase c) #\i) #t]
    [(eq? (char-downcase c) #\o) #t]
    [(eq? (char-downcase c) #\u) #t]
    [else #f]))

;;;;
;; Drop all vowels from s.
;;
;; NOTE: y is not considered a vowel in this implementation.
;;
;; drop-vowels :: String -> String
;;
(define (drop-vowels s)
  (list->string
   (filter
    (λ (c) (not (vowel? c)))
    (string->list s))))
