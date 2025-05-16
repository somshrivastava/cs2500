;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname 9-16-word-pro) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CONSTANTS ;;;;;;

(define BACKGROUND (empty-scene 600 100 "yellow"))
(define WORD-SIZE 20)
(define WORD-COLOR "black")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; WORLD STATE ;;;;;;

; A WP-World is a String 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; EVENT HANDLERS ;;;;;;

; word-processor : WP-World -> WP-World
; Runs a simplified word processor
(define (word-processor initial-word)
  (big-bang initial-word
    [to-draw draw-word]
    [on-key key-wp]))

; draw-word : WP-World -> Image
; Draws the current word
(check-expect (draw-word "") BACKGROUND)
(check-expect (draw-word "hello")
              (overlay (text "hello" WORD-SIZE WORD-COLOR) BACKGROUND))
(define (draw-word s) 
  (overlay (text s WORD-SIZE WORD-COLOR) BACKGROUND)) 

; key-wp : WP-World KeyEvent -> WP-World
; Adds character represented by the key event to the end of s
(check-expect (key-wp "hello wor" "l") "hello worl")
(check-expect (key-wp "" "x") "x")
(check-expect (key-wp "hello" "\b") "hell")
(check-expect (key-wp "" "\b") "")
(define (key-wp ws key)
  (cond [(key=? key "\b") (remove-last ws)]
        [else (string-append ws key)]))

;; remove-last : WP-World -> WP-World
;; removes the last character from the given string,
;; unless it is empty
(check-expect (remove-last "hello") "hell")
(check-expect (remove-last "") "")
(define (remove-last ws)
  (cond [(string=? ws "") ""]
        [else 
         (substring ws 0 (sub1 (string-length ws)))]))









