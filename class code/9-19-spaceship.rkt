;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 9-19-spaceship) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A Spaceship is a (make-ship Position String Number)
(define-struct ship (location color time-elapse))
;; Interpretation: represents a spaceship in the game
;; location is the position of the center of the ship on the board
;; time-elapsed is measured in seconds

(define (spaceship-temp aship)
    ... (posn-temp (ship-location aship))
    ... (ship-color aship)
    ... (ship-time-elapsed aship))

(define ship1 (make-ship (make-posn 30 40) "red" 30))

;; move-spaceship : Spaceship Number Number -> Spaceship
;; produces a new spaceship that is shifted by dx and dy

(define (move-spaceship sp dx dy)
  (make-ship (move-location (ship-locaiton aship) dx dy)
             (ship-color aship)
             (ship-time-elapsed aship)))

;; Test Cases
(check-expect (move-spaceship SHIP1 0 3)
              (make-ship (make-posn 30 43) "red" 30))