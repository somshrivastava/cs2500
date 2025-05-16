;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 9-12-big-bang) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image) 
(require 2htdp/universe)

(define SKY-WIDTH 300) 
(define SKY-HEIGHT 200) 
(define RADIUS 25)
(define SUN (circle RADIUS "solid" "yellow"))
(define MOON (circle RADIUS "solid" "gray"))
(define SKY 
      (rectangle SKY-WIDTH SKY-HEIGHT "solid" "light blue"))
(define DARK-SKY 
      (rectangle SKY-WIDTH SKY-HEIGHT "solid" "black"))
(define DIM-SKY 
      (rectangle SKY-WIDTH SKY-HEIGHT "solid" "blue"))

;; A Moon-State is a Real Number

;; main: Moon-State -> Moon-State
;; launches the eclipse animation
(define (main x-moon)
  (big-bang x-moon
    [to-draw draw-eclipse]
    [on-tick next-moon]
    [on-key restart]
    [on-mouse move-x]
    [stop-when end last-scene]))

     
;; draw-eclipse : Natural -> Image
;; draws the sun and the moon onto the sky
(check-expect (draw-eclipse 60)
              (place-image MOON 60 (/ SKY-HEIGHT 2)
               (place-image SUN (/ SKY-WIDTH 2) (/ SKY-HEIGHT 2)
                            SKY))) 
(define (draw-eclipse x-moon)
  (place-image MOON x-moon (/ SKY-HEIGHT 2)
               (place-image SUN (/ SKY-WIDTH 2) (/ SKY-HEIGHT 2)
                            SKY)))


;; next-moon: Moon-State -> Moon-State
;; produces the next moon state which is 2 pixels to the right
(check-expect (next-moon 89) 91)
(define (next-moon x-moon)
  (+ 2 x-moon))


;; restart: Moon-State KeyEvent -> Moon-State
;; moon goes back to 0 if space bar is pressed
(check-expect (restart 130 " ") 0)
(check-expect (restart 130 "r") 130)
(define (restart x-moon key)
  (if (key=? key " ") 0 x-moon))

;; move-x: Moon-State Integer Integer MouseEvent -> Moon-State
;; move the moon position to the x of the mouse click
(check-expect (move-x 60 100 60 "button-down") 100)
(check-expect (move-x 60 100 60 "drag") 60)
(define (move-x x-moon x y me)
  (if (mouse=? "button-down" me)
      x
      x-moon))


;; end: Moon-State -> Boolean
;; ends the animation when the moon hits the right side
(check-expect (end 325) #true)
(check-expect (end 31) #false)
(define (end x-moon)
  (>= x-moon (+ RADIUS SKY-WIDTH)))

;; last-scene: Moon-State -> Image
;; draws the last scene when the world ends
(check-expect (last-scene 40)
              (overlay (text "DONE!" 30 "red") SKY))
(define (last-scene x-moon)
  (overlay (text "DONE!" 30 "red") SKY))
                       


