;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

;;;;
;; sum :: Num... -> Num
;;
;; Takes zero or more numeric arguments and returns their sum.
;;
;; As 0 (zero) is the identity of addition, this function returns
;; 0 if no arguments are provided.
;;
(define (sum . args)
  (let go ((lst args) (acc 0))
    (cond
     ((null? lst) acc)
     (else (go (cdr lst) (+ (car lst) acc))))))

(sum 1 1.2 3)
;=> 6

(sum)
;=> 0

(sum 7)
;=> 7

(sum 1 2)
;=> 3

(sum 1 2 3 4 5)
;=> 15
