#lang racket/base
(require rackunit)
(require "./vowel-rem-v1.rkt")

(check-equal? (drop-vowels "") "" "empty string")

(check-equal?
 (drop-vowels "aeiou")
 ""
 "only lowercase, only vowels")

(check-equal?
 (drop-vowels "AEIOU")
 ""
 "only uppercase, only vowels")

(check-equal?
 (drop-vowels "hello")
 "hll"
 "lowercase, vowels and consonants ")

(check-equal?
 (drop-vowels "HElLo WOrlD")
 "HlL WrlD"
 "mixed case, vowels and consontants")
