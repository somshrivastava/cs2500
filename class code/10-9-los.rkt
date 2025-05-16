;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 10-9-los) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A ListOfStrings (LoS) is one of
; - '()
; - (cons String LoS)
; Interpretation: represents a list of strings
(define LOS-0 '())
(define LOS-1 (cons "cat" LOS-0)) 
(define LOS-2 (cons "dog" LOS-1))
(define LOS-3 (cons "bird" LOS-2))

(define (los-temp los) 
  (...(cond [(empty? los) ...] 
            [(cons? los)
             ...(first los) ...
             ...(los-temp (rest los)) ...])))

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

;; find-and-replace: LoS String String -> LoS
;; replaces all occurrances of the first string with the second
(check-expect (find-and-replace LOS-0 "cat" "dog") LOS-0)
(check-expect (find-and-replace LOS-3 "kitty" "dog") LOS-3)
(check-expect (find-and-replace LOS-3 "cat" "dog")
              (cons "bird" (cons "dog" (cons "dog" '()))))
(check-expect (find-and-replace (cons "cat" LOS-3) "cat" "dog")
              ( cons "dog" (cons "bird" (cons "dog" (cons "dog" '())))))
(define (find-and-replace los old new) 
  (cond [(empty? los) los] 
        [(cons? los)
         (if (string=? old (first los))
             (cons new (find-and-replace (rest los) old new))
             (cons (first los) (find-and-replace (rest los) old new)))]))













