#lang racket/base

(require rackunit "./sum-v5.rkt")

(check-eq? (sum 1) 1 "from 1 to 1")

;; 3
(check-eq? (sum 2) (+ 2 1) "from 1 to 2")

;; 6
(check-eq? (sum 3) (+ 3 2 1) "from 1 to 3")

(check-eq? (sum 32) 528 "from 1 to 32")

(check-eq? (sum 64) 2080 "from 1 to 64")
