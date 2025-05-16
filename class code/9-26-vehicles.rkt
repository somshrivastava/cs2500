;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 9-26-vehicles) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

;; An Auto is a (make-auto Integer PosReal)
(define-struct auto (doors mileage))
;; interp. 2-door cars hold 2 passengers,
;;         4 or more doors holds 5 passengers

(define CAR1 (make-auto 2 30))

;; A Bus is a (make-bus Integer String)
(define-struct bus (seats route))
;; interp. represents a bus with a number of seats and the route name

(define THE-39 (make-bus 45 "huntington"))

;; A Train is a (make-train Integer String Boolean)
(define-struct train (cars line express?))
;; interp. each car holds 100 passengers

(define WORC-FRAM (make-train 6 "Worcester Framingham" #true))

;; A Vehicle is one of:
;; - Auto
;; - Bus
;; - Train
;; interp. represents a vehicle that can be a car, bus or train

;; can-hold? : Vehicle PosInt -> Boolean
;; can the vehicle hold the given number of passengers?
(check-expect (can-hold? CAR1 3) #false)
(check-expect (can-hold? CAR1 2) #true)
(check-expect (can-hold? THE-39 38) #true)
(check-expect (can-hold? THE-39 50) #false)
(check-expect (can-hold? WORC-FRAM 120) #true)
(check-expect (can-hold? WORC-FRAM 650) #false)
(define (can-hold? v n)
  (cond [(auto? v) (auto-holds? v n)]
        [(bus? v) (bus-holds? v n)]
        [(train? v) (train-holds? v n)]))

;; Auto PosInt -> Boolean
;; can the auto hold the given number of passengers?
(check-expect (auto-holds? CAR1 3) #false)
(check-expect (auto-holds? CAR1 2) #true)
(define (auto-holds? a n)
  (or (and (= (auto-doors a) 2) (<= n 2))
      (and (>= (auto-doors a) 4) (<= n 5))))

;; Bus PosInt -> Boolean
;; can the bus hold the given number of passengers?
(check-expect (bus-holds? THE-39 38) #true)
(check-expect (bus-holds? THE-39 50) #false)
(define (bus-holds? b n)
  (>= (bus-seats b) n))


;; Train PosInt -> Boolean
;; can the train hold the given number of passengers?
(check-expect (train-holds? WORC-FRAM 120) #true)
(check-expect (train-holds? WORC-FRAM 650) #false)
(define (train-holds? t n)
  (>= (* 100 (train-cars t)) n))

