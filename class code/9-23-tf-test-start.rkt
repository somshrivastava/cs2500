;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 9-23-tf-test-start) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A TFQuestion is a (make-tfq String Boolean)
(define-struct tfq [prompt answer])
;; represents a question in a true/false test

(define TFQ-1 (make-tfq "fundies 1 is the best course" #true)) 
(define TFQ-2 (make-tfq "The weather is cold today" #false)) 
(define TFQ-3 (make-tfq "Well-designed code comes from 
                                following the design  recipe" #true)) 
(define TFQ-4 (make-tfq "The shape of the data determines 
                                  the shape of the function" #true)) 
(define TFQ-5 (make-tfq "I can produce functioning, well-designed code
                     faster by not following the design recipe" #false))

;; tfq-temp : TFQuestion -> ?
#;(define (tfq-temp q)
  ... (tfq-prompt q) ...
  ... (tfq-answer q) ...)


;; A TFTest is a (make-tft TFQuestion TFQuestion
;;                         TFQuestion TFQuestion TFQuestion)
(define-struct tft [q1 q2 q3 q4 q5])
;; represents a true/false test with 5 questions

(define TFT (make-tft TFQ-1 TFQ-2 TFQ-5 TFQ-4 TFQ-3))


;; tft-temp : TFTest -> ?
#;(define (tft-temp test)
  ...(tfq-temp (tft-q1 test))...
  ...(tfq-temp (tft-q2 test))...
  ...(tfq-temp (tft-q3 test))...
  ...(tfq-temp (tft-q4 test))...
  ...(tfq-temp (tft-q5 test))...)


;; passed? : TFTest Boolean -> Boolean
;; can we pass the test by plugging in
;; the given Boolean to each question?

(check-expect (passed? TFT #true) #true)
(check-expect (passed? TFT #false) #false)
(define (passed? test answer?)
  (>= (+ (score (tft-q1 test) answer?)
         (score (tft-q2 test) answer?)
         (score (tft-q3 test) answer?)
         (score (tft-q4 test) answer?)
         (score (tft-q5 test) answer?)) 3))

;; score: TFQuestion Boolean -> [1, 0]
;; scores the given question based on the given answer
(check-expect (score TFQ-1 #true) 1)
(check-expect (score TFQ-1 #false) 0)
(check-expect (score TFQ-2 #true) 0)
;;(define (score q answer?)
  ;;(if (boolean=? (tfq-answer))))

(define-struct event (title start-time hours location host))
;; An Event is a (make-event String Number Number String String)
;; Represents an event where:
;; - title is the name of the event
;; - start-time is the hour at which the event starts
;; - hours is the number of hours is lasts
;; - location is the place the event is hosted
;; - host is the person hosting the event

;; make-event : String Number Number String String -> Event
;; event? : Any -> Boolean
;; event-title : Event -> String
;; event-start-time : Event -> Number
;; event-hours : Event -> Number
;; event-location : Event -> String
;; event-host : Event -> String

(define EVENT (make-event "Event 1" 5 5 "Boston" "Som"))

(define (event-temp e)
  (... (event-title e) ...
       (event-start-time e) ...
       (event-hours e) ...
       (event-location e) ...
       (event-host) ...))

;; move-event-location : Event String -> Event
;; moves the location of the event to a new location

(define (move-event-location Event new-location)
  (make-event (event-title Event)
              (event-start-time Event)
              (event-hours Event)
              new-location
              (event-host Event)))

