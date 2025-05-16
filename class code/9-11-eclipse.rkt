;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 9-11-eclipse) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

;; draw-eclipse : Natural -> Image
;; draws the sun and the moon onto the sky
(define (draw-eclipse x-moon)
  (place-image MOON x-moon (/ SKY-HEIGHT 2)
               (place-image SUN (/ SKY-WIDTH 2) (/ SKY-HEIGHT 2)
                            SKY)))

;; draw-eclipse.v2 : Natural -> Image
;; draws the sun and the moon onto the sky
(define (draw-eclipse.v2 x-moon)
  (place-image MOON x-moon (/ SKY-HEIGHT 2)
               (place-image SUN (/ SKY-WIDTH 2) (/ SKY-HEIGHT 2)
                            (if (= x-moon (/ SKY-WIDTH 2))
                                DARK-SKY
                                SKY))))

;; draw-eclipse.v3 : Natural -> Image
;; draws the sun and the moon onto the sky
(define (draw-eclipse.v3 x-moon)
  (place-image MOON x-moon (/ SKY-HEIGHT 2)
               (place-image SUN (/ SKY-WIDTH 2) (/ SKY-HEIGHT 2)
                            (cond
                              [(<= (abs (- x-moon (/ SKY-WIDTH 2))) 5) DARK-SKY]
                              [(< (abs (- x-moon (/ SKY-WIDTH 2))) (* 2 RADIUS)) 
                                DIM-SKY]
                              [else SKY])))) 

(animate draw-eclipse.v3) 
