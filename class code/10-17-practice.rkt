;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 10-17-practice) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))


;; filter: (X) [X -> Boolean] [List-of X] -> [List-of X]
;; get the items that pass the test

;; map: (X Y) [X -> Y] [List-of X] -> [List-of Y]
;; maps (applies) a function to every member of the list

;; foldr : (X Y) [X Y -> Y] Y [List-of X] -> Y
;; combine the items in the list


;; count-sheep: [List-of Position] -> Number
;; count the posns that are within bounds in the list (50 x 50)
(check-expect (count-sheep (list (make-posn 30 40) (make-posn 400 60))) 1)
(check-expect (count-sheep '()) 0)
(define (count-sheep alop)
  ; [Position Number -> Number] Number [List-of Position] -> Number
  (foldr count-if 0 alop))

;; count-if : Position Number -> Number
;; add 1 to the count so far if the posn is in bounds
(check-expect (count-if (make-posn 30 40) 2) 3)
(check-expect (count-if (make-posn 300 40) 2) 2)
(define (count-if p n)
  (if (in? p) (add1 n) n))


;; herd: [List-of Position] -> [List-of Position]
;; shifts all posns by 5 and 10
(check-expect (herd (list (make-posn 30 40) (make-posn 400 60)))
              (list (make-posn 35 50) (make-posn 405 70)))
(check-expect (herd '()) '())
(define (herd alop)
  ;[Position -> Position] [List-of Position] -> [List-of Position]
  (map move-posn alop))



;; wolves: [List-of Position] -> [List-of Position]
;; eliminates all posns from a list that are not within a 50 by 50 square
(check-expect (wolves (list (make-posn 30 40) (make-posn 400 60)))
              (list (make-posn 30 40)))
(check-expect (wolves '()) '())
(define (wolves alop)
  ;[Position -> Boolean] [List-of Position] -> [List-of Position]
  (filter in? alop))

; A Position is a (make-posn Number Number)
; a 2D-position on the Cartesian plane
(define posn-0 (make-posn 0 0))
(define posn-1 (make-posn 3 4))
(define (posn-templ p)
  (... (posn-x p) ... (posn-y p) ...))
 
; A [List-of Position] is one of:
;; - '()
;; - (cons Position LoP)
; a list of Positions

(define LOP-0 '())
(define LOP-1 (list posn-0))
(define LOP-2 (list posn-1 posn-0))

(define (lop-templ lop)
  (... (cond [(empty? lop) ...]
             [(cons?  lop) ... (posn-templ (first lop))
                           ... (lop-templ (rest lop)) ...])))











;; in? : Position -> Boolean
;; is the position in a 50x50 area?
(check-expect (in? (make-posn 30 40)) #true)
(check-expect (in? (make-posn 300 40)) #false)
(define (in? p)
  (and (<= 0 (posn-x p) 50)
       (<= 0 (posn-y p) 50))) 





;; move-posn: Position -> Position
;; move the position by 5 and 10
(check-expect (move-posn (make-posn 1 2)) (make-posn 6 12))
(define (move-posn p)
  (make-posn (+ 5 (posn-x p)) (+ 10 (posn-y p))))











