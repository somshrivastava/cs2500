;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 10-9-los-abs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; A List-of-String (LoS) is one of:
;; - '()
;; - (cons String LoS)
;; respresents a list of strings

(define LOS-0 '())
(define LOS-1 (cons "cat" LOS-0))
(define LOS-2 (cons "dog" LOS-1))
(define LOS-3 (cons "bird" LOS-2))
(define LOS-4 (cons "dog" LOS-3))

;; make the template for a list of string

#;(define (los-temp los)
  (cond [(empty? los) ...]
        [(cons? los) ...(first los)...
                     ...(los-temp (rest los))...]))

;; prefix-with-from : LoS -> LoS
;; prefixes every string with "From: "
(check-expect (prefix-with-from '()) '())
(check-expect (prefix-with-from LOS-2) (list "From: dog" "From: cat"))
(define (prefix-with-from los)
  (prefix-with los "From: "))

;; prefix-with-to : LoS -> LoS
;; prefixes every string with "To: "
(check-expect (prefix-with-to '()) '())
(check-expect (prefix-with-to LOS-2) (list "To: dog" "To: cat"))
(define (prefix-with-to los)
  (prefix-with los "To: "))

;; prefix-with : LoS String -> LoS
;; prefixes every string with the given string
(check-expect (prefix-with '() "Dear ") '())
(check-expect (prefix-with LOS-2 "Dear ") (list "Dear dog" "Dear cat"))
(define (prefix-with los str)
  (cond [(empty? los) '()]
        [(cons? los) (cons (string-append str (first los))
                           (prefix-with (rest los) str))]))












