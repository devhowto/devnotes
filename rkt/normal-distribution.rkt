#lang racket

;;
;; See:
;;
;; • https://en.wikipedia.org/wiki/Normal_distribution
;; • https://www.khanacademy.org/math/probability/xa88397b6:analyze-quantitative/normal-distributions-a2ii/v/ck12-org-normal-distribution-problems-empirical-rule
;;

;;;;
;; Converts `n` to a string keeping two decimal places.
;;
(define (fmt n)
  (real->decimal-string n 2))

;;;;
;; Generates the left values of a normal distribution number line.
;;
(define (left m d)
  (let run ([n 4] [acc '()])
    (cond
      [(= n 0) acc]
      [else
       (run (sub1 n)
            (append acc (list (fmt (- m (* d n))))))])))

;;;;
;; Generates the right values of a normal distribution number line.
;;
(define (right m d)
  (let run ([n 4] [acc '()])
    (cond
      [(= n 0) acc]
      [else
       (run (sub1 n)
            (cons (fmt (+ m (* d n))) acc))])))

;;;;
;; Generates a “normal distribution number line”.
;;
(define (normal-distribution mean standard-deviation)
  (append (left mean standard-deviation)
          (list (string-append "<" (fmt mean) ">"))
          (right mean standard-deviation)))


(format "~A" (normal-distribution 12.5 2.4))
