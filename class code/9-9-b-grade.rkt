;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 9-9-b-grade) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; b-range? : PosReal -> Boolean
;; is the given number in the B range (80-89 inclusive)?
(define (b-range? grade)
  (and (>= grade 80) (<= grade 89)))

(check-expect (b-range? 98) #false)
(check-expect (b-range? 98.6) #false)
(check-expect (b-range? 85) #true)
(check-expect (b-range? 76) #false)
(check-expect (b-range? 80) #true)
(check-expect (b-range? 89) #true)