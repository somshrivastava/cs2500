;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 10-23-y-posn-mover) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;Write the function, y-posn-mover:

;; [Number -> Number] -> [Posn -> Posn]
;; Returns a function that uses the given operator 
;; to move the y coordinate of the posn
(define (y-posn-mover op) 
  (λ (p) (make-posn (posn-x p) (op (posn-y p)))))
  
(define up-1   (y-posn-mover add1))
(define down-2 (y-posn-mover (lambda (height) (- height 2))))

(check-expect (up-1   (make-posn 3 9))  (make-posn 3 10))
(check-expect (down-2 (make-posn 3 9))  (make-posn 3  7))
(check-expect (map down-2 (list (make-posn 7 0) (make-posn 50 50)))
              (list (make-posn 7 -2) (make-posn 50 48)))
