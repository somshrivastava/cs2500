;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 9-5-simple-functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; add-ten : Number -> Number 
;; Adds ten to the given number 
(define (add-ten x)
  (+ x 10))

(add-ten 5)

;; area-of-circle: PosReal -> PosReal
;; computes the area of a circle of the given radius
(define (area-of-circle radius)
  (* pi (sqr radius)))

(area-of-circle 46)