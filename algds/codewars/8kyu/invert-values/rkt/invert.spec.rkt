#lang racket
(require rackunit "./invert-v2.rkt")

(test-case
    "invert"

  (check-equal?
   (invert '())
   '()
   "empty list")

  (check-equal?
   (invert '(0))
   '(0)
   "list containing 0")

  ;;
  ;; -0 is eq? 0
  ;;

  (check-equal?
   (invert '(-0))
   '(0)
   "list containing -0")

  (check-equal?
   (invert '(1))
   '(-1)
   "single positive element")

  (check-equal?
   (invert '(-1))
   '(1)
   "single negative element")

  (check-equal?
   (invert '(-1 2 -3 4))
   '(1 -2 3 -4)
   "mixed positive and negative elements"))
