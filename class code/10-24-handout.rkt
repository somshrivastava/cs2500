;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 10-24-handout) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; compose-fun: [Y -> Z] [X -> Y] -> [X -> Z]
;; compose the two given functions f and g
(check-expect ((compose-fun add1 sqr) 2) 5)
(check-expect ((compose-fun string-length number->string) 2) 1)
(define (compose-fun f g)
  (local ([define (h x) (f (g x))])
    h))

;; compose-fun2: [Y -> Z] [X -> Y] -> [X -> Z]
;; compose the two given functions f and g
(check-expect ((compose-fun2 add1 sqr) 2) 5)
(check-expect ((compose-fun2 string-length number->string) 2) 1)
(define (compose-fun2 f g)
  (lambda (x) (f (g x))))

;; sum-even: [List-of Number] -> Number
;; sum the even numbers in the list
(check-expect (sum-even (list 1 2 3 4 5 6)) 12)
(check-expect (sum-even (list )) 0)
(define (sum-even lon)
  (local ([define (helper new result)
             (if (even? new)
               (+ new result)
               result)])
    (foldr helper 0 lon)))

;; sum-even.v2: [List-of Number] -> Number
;; sum the even numbers in the list (using lambda instead of local)
(check-expect (sum-even.v2 (list 1 2 3 4 5 6)) 12)
(check-expect (sum-even.v2 (list )) 0)
(define (sum-even.v2 lon)
    (foldr (λ (new result)
             (if (even? new)
                 (+ new result)
                 result))
           0
           lon))

;; compose-all: [List-of [X -> X]] X -> X
;; compose a list of functions
(check-expect (compose-all (list sqr add1 sqrt) 4) 9)
(check-expect (compose-all (list ) 4) 4)
(define (compose-all lofx n)
  (local [(define (helper f x) (f x))]
    (foldr helper n lofx)))
    
;; compose-all.v2: [List-of [X -> X]] -> [X -> X]
;; produces a new function that computes (f (g (h x))) for any x
(check-expect ((compose-all.v2 (list add1 sqr sub1)) 3) 5)
(define (compose-all.v2 lofx)
  (local [(define (identity x) x)]
    (foldr compose identity lofx)))





