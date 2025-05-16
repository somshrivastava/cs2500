;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 11-4-mci-practice-done) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; Exercise: Design the function prizes that takes two parallel lists
;; (one of names, the other of prize amounts) and produces  a list that
;; announces who gets what. If we run out of prizes, the person gets 0.

; prizes : [List-of String] [List-of Number] -> [List-of String]
; Awards each person in the first list a prize amount from the second;
; if run out of prizes, person gets 0 :(
 
(check-expect (prizes (list "alice" "bob" "carol") (list 100 50 10))
              (list "alice gets $100"
                    "bob gets $50"
                    "carol gets $10"))
 
(check-expect (prizes (list "alice" "bob" "carol" "dan") (list 100 50))
              (list "alice gets $100"
                    "bob gets $50"
                    "carol gets $0"
                    "dan gets $0"))
 
(check-expect (prizes (list "alice" "bob") (list 100 50 10))
              (list "alice gets $100"
                    "bob gets $50"))
(check-expect (prizes (list ) (list )) (list ))
(check-expect (prizes (list ) (list 10 )) (list ))
(check-expect (prizes (list "bob") (list )) (list  "bob gets $0")) 

(define (prizes.v1 los lon)
  (local [;; award: String Number -> String
          ;; computes the prize amount for the given person
          (define (award s n)
            (string-append s " gets $" (number->string n)))]
    (cond [(and (empty? los) (empty? lon)) '()]
          [(and (empty? los) (cons? lon)) '()]
          [(and (cons? los) (empty? lon))
           (cons (award (first los) 0)
                 (prizes.v1 (rest los) '()))]
          [(and (cons? los) (cons? lon))
           (cons (award (first los) (first lon))
                 (prizes.v1 (rest los) (rest lon)))])))

;; this version simplifies into just 3 cond clauses
(define (prizes los lon)
  (local [;; award: String Number -> String
          ;; computes the prize amount for the given person
          (define (award s n)
            (string-append s " gets $" (number->string n)))]
    (cond [(empty? los) '()]
          [(and (cons? los) (empty? lon))
           (cons (award (first los) 0)
                 (prizes (rest los) '()))]
          [(and (cons? los) (cons? lon))
           (cons (award (first los) (first lon))
                 (prizes (rest los) (rest lon)))]))) 

;; Exercise: Design the function make-uno-deck that accepts a
;; natural number representing the highest numeric card value
;; and a list of strings, representing the colors, and generates a
;; deck of Uno cards. There should be one card with each color and
;; number. Do so first via templates and then with list abstractions.

(define-struct card [number color])
; An UnoCard is a (make-card Nat String)

(define UNOCARD-1 (make-card 1 "red"))
(define UNOCARD-2 (make-card 2 "blue"))
 
(define (unocard-temp uc)
  (... (card-number uc) ...
       (card-color uc) ...))
 
; make-uno-deck : Nat [List-of String] -> [List-of UnoCard]
; Makes a deck of Uno cards, with one card of each color and number
(define CARDLIST (list (make-card 1 "red") (make-card 2 "red") (make-card 3 "red")
                    (make-card 1 "green") (make-card 2 "green") (make-card 3 "green"))) 
(check-expect (make-uno-deck 3 (list "red" "green")) CARDLIST) 
(define (make-uno-deck num colors)
  (local [;;make-all-nums : String Nat -> [List-of UnoCard]
          ;; makes the cards for a color and n and below
          (define (make-all-nums s n)
            (cond [(zero? n) '()]
                  [(positive? n) (cons
                                  (make-card (add1 (- num n)) s)
                                  (make-all-nums s (sub1 n)))]))
          ;;make-all-colors : [List-of String] -> [List-of UnoCard]
          ;;makes a deck of uno cards for each color for all nums
          (define (make-all-colors los)
            (cond [(empty? los) '()]
                  [(cons? los)
                   (append (make-all-nums (first los) num)
                           (make-all-colors (rest los)))]))] 
    (make-all-colors colors)))

; this version uses list abstractions
; make-uno-deck.v2 : Nat [List-of String] -> [List-of UnoCard]
; Makes a deck of Uno cards, with one card of each color and number 
(check-expect (make-uno-deck.v2 3 (list "red" "green")) CARDLIST)
(define (make-uno-deck.v2 n colors)
  (local [;; make-all-nums: String -> [List-of UnoCard]
          ;; make a deck of uno cards for all nums 1..n and the given color
          ;; 3, "red" -> (list (make-card 1 "red")
          ;;                   (make-card 2 "red")
          ;;                   (make-card 2 "red"))
          (define (make-all-nums c)
            (build-list n (λ (i) (make-card (add1 i) c))))]
    ;; [[List-of UnoCard] [List-of UnoCard] -> [List-of UnoCard]]
    ;;                    [List-of UnoCard] [List-of [List-of UnoCard]] ->
    ;;                    [List-of UnoCard] 
    (foldr append '() (map make-all-nums colors))))