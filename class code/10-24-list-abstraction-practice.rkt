;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 10-24-list-abstraction-practice) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; ormap : (X) [X -> Boolean] [List-of X] -> Boolean
;; check if the list contains at least one item that passes the test

;; andmap : (X) [X -> Boolean] [List-of X] -> Boolean
;; check if everything in the list passes the test

;; filter: (X) [X -> Boolean] [List-of X] -> [List-of X]
;; get the items that pass the test

;; map: (X Y) [X -> Y] [List-of X] -> [List-of Y]
;; maps (applies) a function to every member of the list

;; foldr : (X Y) [X Y -> Y] Y [List-of X] -> Y
;; combine the items in the list

;; in? : Position -> Boolean
;; is the position in a 50x50 area?
(define (in? p)
  (and (<= 0 (posn-x p) 50)
       (<= 0 (posn-y p) 50)))

;; count-sheep: [List-of Position] -> Number
;; count the posns that are within bounds in the list (50 x 50)
(check-expect (count-sheep (list (make-posn 30 40) (make-posn 400 60))) 1)
(check-expect (count-sheep '()) 0)
(define (count-sheep alop)
  (local [;; Position Number -> Number
          ;; adds 1 to the count so far if the position is in bounds
          ;; given: (make-posn 30 40) and 2 -> 3
          ;; given: (make-posn 30 400) and 2 -> 2
          (define (count-if p n)
            (if (in? p) (add1 n) n))]
    ;; [Position Number -> Number] Number [List-of Position] -> Number
    (foldr count-if 0 alop)))

;; count-sheep2: [List-of Position] -> Number
;; count the posns that are within bounds in the list (50 x 50)
(check-expect (count-sheep2 (list (make-posn 30 40) (make-posn 400 60))) 1)
(check-expect (count-sheep2 '()) 0)
(define (count-sheep2 alop)
  (length (filter (λ (p) (in? p)) alop)))

;; count-sheep3: [List-of Position] -> Number
;; count the posns that are within bounds in the list (50 x 50)
(check-expect (count-sheep3 (list (make-posn 30 40) (make-posn 400 60))) 1)
(check-expect (count-sheep3 '()) 0)
(define (count-sheep3 alop)
  (foldr + 0 (map (λ (p) (if (in? p) 1 0)) alop)))

;; [List-of [List-of Number]] -> [List-of Number]
;; sums the numbers in each list
(check-expect (sum-lists (list (list 1 2) (list 3 4))) (list 3 7))
(check-expect (sum-lists '()) '())
(define (sum-lists alol)
  (map (λ (lst) (foldr + 0 lst)) alol)) 







