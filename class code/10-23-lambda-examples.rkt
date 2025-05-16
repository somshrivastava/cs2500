;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 10-23-lambda-examples) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; usd->eur : [List-of Number] Number -> [List-of Number]
; Converts USD to EUR with the given exchange rate
(check-expect (usd->eur '() 2.0) '())
(check-expect (usd->eur (list 1 2 3) 2.0) (list 2.0 4.0 6.0))
(define (usd->eur lon rate)
    (map (local [; convert : Number -> Number
          ; Converts the USD amount to EUR
          ; If rate is 7.0, given 1.0 would produce 7.0 
          (define (convert n) (* n rate))] convert) lon))


; usd->eur : [List-of Number] Number -> [List-of Number]
; Converts USD to EUR with the given exchange rate
(check-expect (usd->eur2 '() 2.0) '())
(check-expect (usd->eur2 (list 1 2 3) 2.0) (list 2.0 4.0 6.0))
(define (usd->eur2 lon rate)
    (map (λ (n) (* n rate)) lon))

; double-squares : Nat -> [List-of Nat]
; produce the first n double squares
(check-expect (double-squares 0) '())
(check-expect (double-squares 1) (list 0))
(check-expect (double-squares 4) (list 0 2 8 18))
(define (double-squares n)
     (build-list n (λ (x) (* 2 (sqr x)))))
