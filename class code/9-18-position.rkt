;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 9-18-position) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Position is a (make-posn Number Number)
; interpretation: represents a point on the Cartesian plane with
; an x and y position

(define POSN-1 (make-posn 3 4)) 
(define POSN-2 (make-posn 0 0)) 
(define POSN-3 (make-posn 89.2 pi))
;(define POSN-4 (make-posn 0 (make-posn 5 7))) not a valid Position

;; Position -> ?
(define (posn-temp p)
  (... (posn-x p) ... (posn-y p)))

;; dist-to-0: Position -> PosReal
;; computes the distance to the origin of the given Position
(check-expect (dist-to-0 POSN-1) 5)
(check-expect (dist-to-0 POSN-2) 0)
(define (dist-to-0 p)
  (sqrt (+ (sqr (posn-x p)) (sqr (posn-y p)))))

;; move-posn: Position Number Number -> Position
;; produces a new Position that is shifted by the given dx and dy
(check-expect (move-posn POSN-1 1 2) (make-posn 4 6))








