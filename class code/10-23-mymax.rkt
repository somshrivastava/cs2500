;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 10-23-mymax) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; mymax : [NEList-of Number] -> Number
; Finds the biggest number in the list
(check-expect (mymax (list 1)) 1)
(check-expect (mymax (list 1 4 3 5)) 5)
(check-expect (mymax (list 5 3 4 1)) 5)
(define (mymax nelon) 
  (cond
    [(empty? (rest nelon)) (first nelon)]
    [(cons? (rest nelon))
      (if (> (first nelon) (mymax (rest nelon))) 
          (first nelon)
          (mymax (rest nelon)))]))

(define (mymax2 nelon) 
  (cond [(empty? (rest nelon)) (first nelon)]
        [(cons? (rest nelon))
          (local [(define MYMAX (mymax2 (rest nelon)))] 
            (if (> (first nelon) MYMAX)
                (first nelon)
                 MYMAX))]))
