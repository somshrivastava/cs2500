;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 9-5-sunset) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define SUN (circle 30 "solid" "yellow"))
(define SKY (rectangle 300 200 "solid" "lightblue"))

;(place-image SUN 150 120 SKY)
;(place-image SUN 150 150 SKY)
;(place-image SUN 150 170 SKY)

;; draw-sun : Number -> Image
;; draws the sun onto the sky at the given y value
(define (draw-sun y)
  (place-image SUN 150 (sqr y) SKY))

(animate draw-sun)

   