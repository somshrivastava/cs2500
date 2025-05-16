;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 9-18-posn) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(define POSN-1 (make-posn 3 4))
(define POSN-2 (make-posn 0 0))
(define POSN-3 (make-posn 89.2 pi))

;; Position -> ?
;; (define (posn-temp p)
   ;;  (... (posn-x p) ... (posn-y p)))

;; dist-to-0 : Position -> PosReal
;; computes the distance to the origin of the given Position
(define (dist-to-0 p)
    (sqrt (+ (sqr (posn-x p)) (sqr (posn-y p)))))
    
;; Test Cases
(check-expect (dist-to-0 POSN-1) 5)
(check-expect (dist-to-0 POSN-2) 0)

;; move-posn : Position Number Number -> Position
;; produces a new Position that is shifted by the given dx and dy
(define (move-posn p x y)
    (make-posn (+ (posn-x p) x) (+ (posn-y p) y)))

;; Test Cases
(check-expect (move-posn (make-posn 3 4) 5 -3) (make-posn 8 1))
(check-expect (move-posn (make-posn -3 4) 5 -7) (make-posn 2 -3))
(check-expect (move-posn (make-posn -3 -4) 5 -7) (make-posn 2 -11))