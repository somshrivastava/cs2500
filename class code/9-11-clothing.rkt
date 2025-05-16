;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 9-11-clothing) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Design the data to represent an MBTA subway line.

#|
1. The Data Definition 
2. Interpretation
3. (Data) Examples
4. Template
|#

; A Comfort is a real number in [0,3]
; Interpretation:  The a "Comfort Score" where 0 is very uncomfortable, 3 is very comfortable.
 
(define COMFORT-0.1 0.1)
(define COMFORT-3.0 3.0)
 
(define (comfort-temp c)
  (... c ...))

; A Clothing is one of:
; - "t-shirt"
; - "sweater"
; - "coat"
; - "snow suit"
; Interpretation:  An article of clothing that a person might wear

(define CLOTHING-TSHIRT "t-shirt")
(define CLOTHING-SWEATER "sweater")
(define CLOTHING-COAT "coat")
(define CLOTHING-SNOWSUIT "snow suit")


; clothing->comfort : Clothing -> Comfort
; Returns the comfort score for the given clothing choice
 
(check-expect (clothing->comfort CLOTHING-TSHIRT) 3.0)
(check-expect (clothing->comfort CLOTHING-COAT) 1.0)
(check-expect (clothing->comfort CLOTHING-SNOWSUIT) 0.0)
(check-expect (clothing->comfort CLOTHING-SWEATER) 2.0)

(define (clothing->comfort c)
   (cond
      [(string=? c CLOTHING-TSHIRT) 3.0]
      [(string=? c CLOTHING-SWEATER) 2.0]
      [(string=? c CLOTHING-COAT) 1.0]
      [(string=? c CLOTHING-SNOWSUIT) 0.0]))

