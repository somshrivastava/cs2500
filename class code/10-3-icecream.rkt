;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 10-3-icecream) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(define-struct icecream [flavor foundation])

;; An ICC is one of:
;; -  String
;; - (make-icecream String ICC)
;; represents an ice cream cone with an arbitrary number
;;            of scoops of ice cream
;; where an ICC can be a string decribing the cone, or
;; a make-icecream with a scoop on top and an ICC under it

(define IC-1 "sugar cone")
(define IC-2 (make-icecream "cinnamon" IC-1))
(define IC-3 (make-icecream "cookie dough" IC-2))
(define IC-4
  (make-icecream "vanilla"
                 (make-icecream "chocolate"
                                (make-icecream  "strawberry"
                                                "waffle cone"))))

#;(define (icc-temp icc)
  (cond [(string? icc) ...]
        [(icecream? icc) ... (icecream-flavor icc) ...
                         ... (icc-temp (icecream-foundation icc)) ...]))

;; scoop-count: ICC -> Natural
;; count the number of scoops in the given ICC
;(check-expect (scoop-count IC-1) 0)
(check-expect (scoop-count IC-3) 2)
;(check-expect (scoop-count IC-4) 3)
(define (scoop-count icc)
  (cond [(string? icc) 0]
        [(icecream? icc)  (+ 1 
                             (scoop-count (icecream-foundation icc)))]))

;; has-flavor? : ICC String -> Boolean
;; does the given ICC have a given flavor?
(check-expect (has-flavor? IC-1 "chocolate") #false)
(check-expect (has-flavor? IC-3 "chocolate") #false)
(check-expect (has-flavor? IC-4 "chocolate") #true)
(define (has-flavor? icc a-flavor)
  (cond [(string? icc) #false]
        [(icecream? icc) (or (string=? a-flavor (icecream-flavor icc))
                             (has-flavor? (icecream-foundation icc) a-flavor))]))
                                                           


