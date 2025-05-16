;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 10-21-andmap-ormap) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; my-ormap: (X) [X -> Boolean] [List-of X] -> Boolean
;; check if the list has at least one item in the list that passes the test
(check-expect (my-ormap even? (list 1 2 3 4)) #true)
(check-expect (my-ormap even? (list 1 3)) #f)
(check-expect (my-ormap even? (list )) #f)
(define (my-ormap p? alox)
  (cond [(empty? alox) #f]
        [(cons? alox) (or (p? (first alox))
                          (my-ormap p? (rest alox)))]))

;; my-andmap: (X) [X -> Boolean] [List-of X] -> Boolean
;; check all items in the list pass the test
(check-expect (my-andmap even? (list 1 2 3 4)) #false)
(check-expect (my-andmap odd? (list 1 3)) #t)
(check-expect (my-andmap even? (list )) #t)
(define (my-andmap p? alox)
  (cond [(empty? alox) #t]
        [(cons? alox) (and (p? (first alox))
                           (my-andmap p? (rest alox)))]))