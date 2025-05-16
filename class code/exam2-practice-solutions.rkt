;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname exam2-f24-practice-solutions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct app [name size leaky?])
;; An App is a (make-app String PosInt Boolean)
;; Interpretation: representing a phone app with a name,
;; a size measured in megabytes and whether it leaks sensitive
;; data to a third party

;; A [List-of App] is one of:
;; - '()
;; - (cons App [List-of App])
;; Interpretation: represents a list of apps on a smart phone

(define CALENDAR (make-app "Calendar" 10 #true))
(define WEATHER (make-app "Weather" 5 #false))

(define LOP-0 '())
(define LOP-1 (list CALENDAR))
(define LOP-2 (list WEATHER CALENDAR))

;; Design a function, get-space-hogs, which accepts a [List-of App]
;; and a number of megabytes as inputs and produces a [List-of App]
;; that has only the Apps with at least that number of megabytes.
;; solve this without list abstractions

#;(define (app-temp app)
  ... (app-name app) ...
  ... (app-size app) ...
  ... (app-leaky? app) ...)

#;(define (lop-temp lop)
  (cond [(empty? lop) ...]
        [(cons? lop) ... (app-temp (first lop)) ...
                     ... (lop-temp (rest lop)) ...]))

;; get-space-hogs : [List-of App] PosInt -> [List-of App]
;; get the apps whose sizes are at least as big as the given number
(check-expect (get-space-hogs LOP-0 5) LOP-0)
(check-expect (get-space-hogs LOP-2 7) LOP-1)
(define (get-space-hogs lop mg)
  (cond [(empty? lop) '()]
        [(cons? lop) (if (at-least-size-of? (first lop) mg)
                         (cons (first lop)
                               (get-space-hogs (rest lop) mg))
                         (get-space-hogs (rest lop) mg))]))

;; at-least-size-of? : App PosInt -> Boolean
;; is the size of the app at least the given number of mb?
(check-expect (at-least-size-of? CALENDAR 5) #true)
(check-expect (at-least-size-of? CALENDAR 17) #false)
(define (at-least-size-of? app mb)
  (>= (app-size app) mb))

(define-struct game [team1 team2 team1-score team2-score])
;; A Game is a (make-game String String NaturalNumber NaturalNumber)
;; interpretation: represents a game between two teams with the final scores
;;                 team1 is the name of the first team
;;                 team2 is the nmae of the second team
;;                 team1-score is the final score for team1
;;                 team2-score is the final score for team2

;; Example of a list of games:
(define HOCKEY (list (make-game "Northeastern" "Connecticut" 2 1)
                     (make-game "Northeastern" "Boston University" 5 2)
                     (make-game "Northeastern" "Michigan" 2 3)))

;; 1. Design a function to count the number of games won by team1 in a list of games
;; (use a list abstraction, bonus points for using only one )

;; count-wins: [List-of Game] -> Natural
;; count the number of wins for team1
(check-expect (count-wins '()) 0)
(check-expect (count-wins HOCKEY) 2)
(define (count-wins log)
  (foldr (lambda (x y)
           (if (> (game-team1-score x) (game-team2-score x))
               (add1 y)
               y)) 
       0 log))

;; Design a function to determine if any of the games in a list of games was won by team2
;; (use a list abstraction)

;; any-wins-team2? : [List-of Game] -> Boolean
;; did team2 win any games in the list of games?
(check-expect (any-wins-team2? '()) #false)
(check-expect (any-wins-team2? HOCKEY) #true)
(define (any-wins-team2? log)
  (ormap (λ (g) (> (game-team2-score g) (game-team1-score g))) log))

;; Design a function that gets a list of the names of team1 from a list of games.
;; (use a list abstraction)

;; team-names : [List-of Game] -> [List-of String]
;; gets a list of the names of team1 from a list of games
(check-expect (team1-names '()) '())
(check-expect (team1-names HOCKEY)
              (list "Northeastern" "Northeastern" "Northeastern"))
(define (team1-names log)
  (map game-team1 log))

;; Study the function definition fo r zoobee and give it a signature.
;; Be as general as possible.

;; zoobee: (X) [List-of X] [X -> Number] Number -> Number
(define (zoobee a b c)
  (cond [(empty? a) c]
        [(cons? a) (+ (b (first a))
                      (zoobee (rest a) b c))]))