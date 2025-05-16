;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 10-21-wof-next) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; CONSTANTS
(define IMAGE-TEXT-SIZE 40)
(define IMAGE-COLOR "red")
(define BACKGROUND (empty-scene (* IMAGE-TEXT-SIZE 15) (* IMAGE-TEXT-SIZE 7)))

;;World state defs
(define-struct guessed [char])
(define-struct unguessed [char])

; A Game1String is one of:
; - (make-guessed 1String)
; - (make-unguessed 1String)
; A 1String in the word, either it has been guessed already or it is still hidden
 
(define G1S-1 (make-guessed "a"))
(define G1S-2 (make-unguessed "a")) 
 
(define (g1s-temp g1s)
  (...
   (cond
     [(guessed? g1s) (... (guessed-char g1s) ...)]
     [(unguessed? g1s) (... (unguessed-char g1s) ...)])))
 
; A WorldOfFortune (WoF) is a [List-of Game1String]
; The current state of the game
 
(define WOF-0 (list (make-unguessed "h")
                    (make-unguessed "e")
                    (make-unguessed "l")
                    (make-unguessed "l")
                    (make-unguessed "o"))) 
 
(define WOF-1 (list (make-unguessed "h")
                    (make-guessed "e")
                    (make-unguessed "l")
                    (make-unguessed "l")
                    (make-unguessed "o")))
 
(define WOF-2 (list (make-unguessed "h")
                    (make-guessed "e")
                    (make-guessed "l")
                    (make-guessed "l")
                    (make-unguessed "o")))
 
(define WOF-3 (list (make-guessed "h")
                    (make-guessed "e")
                    (make-guessed "l")
                    (make-guessed "l")
                    (make-unguessed "o")))
 
(define WOF-4 (list (make-guessed "h")
                    (make-guessed "e")
                    (make-guessed "l")
                    (make-guessed "l")
                    (make-guessed "o")))

;; guessing-game: String -> WoF
;; launch the world of fortune game
(define (guessing-game s)
  (big-bang (map make-unguessed (explode s))
    [to-draw draw-wof]
    [on-key handle-key]
    [stop-when all-guessed? draw-wof]))

;; draw-wof : [List-of Game1String] -> Image
;; renders the game at its current state
(check-expect (draw-wof WOF-0)
              (overlay (text "?????" IMAGE-TEXT-SIZE IMAGE-COLOR) BACKGROUND))
(check-expect (draw-wof WOF-3)
              (overlay (text "hell?" IMAGE-TEXT-SIZE IMAGE-COLOR) BACKGROUND))
(check-expect (draw-wof '())
              (overlay (text "" IMAGE-TEXT-SIZE IMAGE-COLOR) BACKGROUND))
(define (draw-wof wof)
   (overlay (text (get-string wof) IMAGE-TEXT-SIZE IMAGE-COLOR) BACKGROUND))

;; get-string: [List-of Game1String] -> String
;; get the string that represents the current state of the game
(check-expect (get-string WOF-0) "?????")
(check-expect (get-string WOF-4) "hello")
(define (get-string wof)
  ;;  [Game1String String -> String] String [List-of Game1String] -> String
  (foldr get-char "" wof))

;; get-char: Game1String String -> String
;; represent the Game1String as a "?" or a character
(check-expect (get-char G1S-1 "t") "at")
(check-expect (get-char G1S-2 "") "?")
(define (get-char g1s str)
   (cond
     [(guessed? g1s) (string-append (guessed-char g1s) str)]
     [(unguessed? g1s) (string-append "?" str)]))





;; all-guessed? : [List-of Game1String] -> Boolean
;; have all of the characters been guessed?
(check-expect (all-guessed? WOF-4) #t)
(check-expect (all-guessed? WOF-3) #f)
(check-expect (all-guessed? '()) #t)
(define (all-guessed? wof)
  ;; [Game1String -> Boolean] [List-of Game1String] -> Boolean
  (andmap guessed? wof))

;; handle-key: [List-of Game1String] KeyEvent -> [List-of Game1String]
;; checks if the key press matches any unguessed characters and makes the guessed struct if so
(check-expect (handle-key WOF-3 "x") WOF-3)
(check-expect (handle-key WOF-3 "o") WOF-4)
(define (handle-key wof key)
  (local [;; check-key: Game1String -> Game1String
          ;; check if the key matches the character if the game1String is unguessed
         ;(check-expect (check-key G1S-1 "k") G1S-1)
         ;(check-expect (check-key G1S-2 "a") G1S-1)
         ;(check-expect (check-key G1S-2 "k") G1S-2)
          (define (check-key g1s)
            (cond
              [(guessed? g1s) g1s]
              [(unguessed? g1s) (if (string=? key (unguessed-char g1s))
                                    (make-guessed (unguessed-char g1s))
                                    g1s)]))]
  ;;  [Game1String -> Game1String] [List-of Game1String] -> [List-of Game1String]
  (map check-key wof)))  

 
 #|
;; check-key: Game1String -> Game1String
;; check if the key matches the character if the game1String is unguessed
(check-expect (check-key G1S-1 "k") G1S-1)
(check-expect (check-key G1S-2 "a") G1S-1)
(check-expect (check-key G1S-2 "k") G1S-2)
(define (check-key g1s)
   (cond
     [(guessed? g1s) g1s]
     [(unguessed? g1s) (if (string=? key (unguessed-char g1s))
                           (make-guessed (unguessed-char g1s))
                           g1s)]))


|#



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
















