;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;! Instructions:
;;! 1. Read the contents of this file, and carry out the tasks within
;;!    below.
;;! 2. Do not create, modify or delete any line that begins with ";;!!", such
;;!    as these lines. These are markers that we use to segment your file into
;;!    parts to facilitate grading.

(require 2htdp/image)
(require 2htdp/universe)

;;! Problem 1
;;
;; Design a data definition for a WritingImplement that is either a pen, pencil, or quill.
;;
;; Estimated Portion of Lab: 15%

;; A Writing Implement is one of the following:
;; - pencil
;; - pen
;; - quill
;; A WritingImplement is something that you use to write

(define WRITING-IMPLEMENT-PEN "pencil")
(define WRITING-IMPLEMENT-PENCIl "pen")
(define WRITING-IMPLEMENT-QUILL "quill")

(define (writing-implement-temp w)
  (cond
        [(string=? WRITING-IMPLEMENT-PEN w)...]
        [(string=? WRITING-IMPLEMENT-PENCIl w)...]
        [(string=? WRITING-IMPLEMENT-QUILL w)...]))

;;! Problem 2
;;
;; Design a function maybe-duplicate-string that takes in a String and a Boolean.
;; The function should return a String with two copies of the original String when the input Boolean is #true.
;; If the input Boolean is #false, the function should return the original String unchanged.
;;
;; Estimated Portion of Lab: 25%

;; maybe-duplicate-string: String Boolean -> String
;; Returns string with two copies of original string if the two input strings are the same,
;; if not returns original string
(define (maybe-duplicate-string string bool)
  (cond
    [(boolean=? bool #false) string]
    [(boolean=? bool #true)(string-append string string)]))

;; Test Cases
(check-expect (maybe-duplicate-string "food" #true) "foodfood")
(check-expect (maybe-duplicate-string "pencil" #false) "pencil")
(check-expect (maybe-duplicate-string "hello" #false) "hello")

;;! Problem 3
;;
;; Design a data definition for an Address that includes a street number, street & city.
;;
;; Estimated Portion of Lab: 35%

;; An address is a (make-address street-number street city)
;; Represents a street address
;; - street-number represents the street number of the address
;; - street represents the street name of the address
;; - city represents the city of the address

;; make-address: Number String String -> Address
;; address?: Any -> Boolean
;; address-street-number: Address -> Number
;; address-street: Address -> String
;; address-city: Address -> String

(define-struct address [street-number street city])

(define ADDRESS-1 (make-address 291 "St. Botolph Street" "Boston"))
(define ADDRESS-2 (make-address 796 "Columbus Avenue" "Boston"))
(define ADDRESS-3 (make-address 5 "Cook Road" "New Jersey"))

(define (address-temp a)
  (... (address-street-number a) ...
       ... (address-street a) ...
       ... (address-city a) ...))

;;! Problem 4
;;
;; Consider the following data definition for Weight:

(define-struct weight [pounds ounces])
;; A Weight is a (make-weight Number Number[0-15])
;; Interpretation: a weight in imperial measure, where:
;; - pounds is the number of pounds
;; - ounces is the number of ounces, which should never be over 15 since 16 ounces are 1 pound
(define WEIGHT0 (make-weight 0 0))
(define WEIGHT1 (make-weight 0 15))
(define WEIGHT2 (make-weight 12 4))

(define (weight-temp w)
  (... (weight-pounds w) ... (weight-ounces w) ...))

;; Design a function total-ounces that takes as input a Weight and returns the
;; total number of ounces it represents (as just a number).
;;
;; Estimated Portion of Lab: 25%

;; total-ounces : Weight -> Number
;; Returns the total number of ounces it represents given a Weight
(define (total-ounces weight)
  (+ (* 16 (weight-pounds weight)) (weight-ounces weight)))

;; Test Cases
(check-expect (total-ounces (make-weight 0 0)) 0)
(check-expect (total-ounces (make-weight 0 15)) 15)
(check-expect (total-ounces (make-weight 5 0)) 80)
(check-expect (total-ounces (make-weight 5 1)) 81)