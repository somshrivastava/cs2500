;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 9-16-traffic-light) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image) 
(require 2htdp/universe)

;;;;; Constants ;;;;;

(define RADIUS 100)

;;;;; World Definitions ;;;;;

;; A TrafficLight is one of:
;; - "red"
;; - "green"
;; - "yellow"
;; represents a traffic light that changes color

(define RED "red")
(define GREEN "green")
(define YELLOW "yellow")

;; add a template for TrafficLight
;; trafficLight-temp : TrafficLight -> ?
;; (define (trafficLight-temp tL)
;;     (cond
;;        [(string=? tL RED) ...]
;;        [(string=? tL GREEN) ...]
;;        [(string=? tL YELLOW) ...]))




;;;;; Event Handlers ;;;;;

;; launch-tl : TrafficLight -> TrafficLight
;; launches the traffic light animation
(define (launch-tl tl)
  (big-bang tl
    [to-draw draw-light]
    [on-tick change-light .3]))

;; draw-light : TrafficLight -> Image
;; draw the current state of the traffic light
(check-expect (draw-light "red") (circle RADIUS "solid" RED))
(define (draw-light tl)
  (circle RADIUS "solid" tl)
  #;(cond
   [(string=? tl RED) (circle RADIUS "solid" RED)]
   [(string=? tl GREEN) (circle RADIUS "solid" GREEN)]
   [(string=? tl YELLOW) (circle RADIUS "solid" YELLOW)])) 


;; change-light : TrafficLight -> TrafficLight
;; changes the light at each tick
(check-expect (change-light RED) GREEN)
(check-expect (change-light YELLOW) RED)
(check-expect (change-light GREEN) YELLOW)
(define (change-light tl)
     (cond
        [(string=? tl RED) GREEN]
        [(string=? tl GREEN) YELLOW]
        [(string=? tl YELLOW) RED]))




