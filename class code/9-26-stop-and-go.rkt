;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 9-26-stop-and-go) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)
 
;; STEP 1: global constants
(define RADIUS 200)
(define SPEED (quotient RADIUS 5))
(define BALL (circle RADIUS "solid" "red"))
(define WIDTH (* RADIUS 5))
(define MID RADIUS)
(define BACKGROUND (empty-scene WIDTH (* 2 RADIUS)))
(define TXT (text "press any key" 33 "black"))
 
;; STEP 2: world state data designs
(define-struct running (x dir))
;; A Running is a (make-running Number Direction)
;; interpretation: represents a moving ball with an x-position and a direction
 
(define-struct resting (x dir))
;; A Resting is a (make-resting Number Direction)
;; interpretation: represents a moving ball with an x-position and a direction
 
;; Direction is one of:
;; -- -1
;; -- +1
;; interpretation: -1 means right to left, +1 is left to right
 
;; SG is one of:
;; -- "press any key"
;; -- Resting
;; -- Running
;; interpretation: "press any key" means the game hasn't started,
;; (make-resting x d) means the ball is resting and will move in
;; direction d when restarted, and
;; (make-running x d) means the ball is at x moving in d
 
(define SG1 "press any key")
(define SG2 (make-running 200 1))
(define SG3 (make-resting 200 -1))
 
 
;; SG -> ?
#;(define (sg-temp asg)
  (cond [(string? asg) ...]
        [(resting? asg) ...(resting-temp asg)]
        [(running? asg) ...(running-temp asg)]))
 
;; Resting -> ?
#;(define (resting-temp aresting)
  ...(resting-x aresting) ... (resting-dir aresting))
 
;; Running -> ?
#;(define (running-temp arunning)
  ...(running-x arunning) ... (running-dir arunning))
 
 
;; STEP 3: Event handlers
;; Anything -> SG
;; launches the program. the user can input any value and it's ignored since
;; the world state always starts with the string "press any key"
(define (main _)
  (big-bang "press any key"
            [to-draw render-sg]
            [on-tick next-ball]
            [on-key start-pause]))
       
 
;; render-sg: SG -> Image
;; render the current stop-and-go state as an image
(check-expect (render-sg SG1)
              (place-image TXT (/ WIDTH 2) MID BACKGROUND))
(check-expect (render-sg SG2)
              (place-image BALL 200 MID BACKGROUND))
(check-expect (render-sg SG3)
              (place-image BALL 200 MID BACKGROUND))
(define (render-sg asg)
  (cond [(string? asg) (place-image TXT (/ WIDTH 2) MID BACKGROUND)]
        [(resting? asg) (resting-render asg)]
        [(running? asg) (running-render asg)]))
 
;; resting-render: Resting -> Image
;; draws the resting ball onto the background
(check-expect (resting-render SG3)
              (place-image BALL 200 MID BACKGROUND))
(define (resting-render aresting)
  (place-image BALL (resting-x aresting) MID BACKGROUND))
 
;; running-render: Running -> Image
;; draws the running ball onto the background
(check-expect (running-render SG2)
              (place-image BALL 200 MID BACKGROUND))
(define (running-render arunning)
  (place-image BALL (running-x arunning) MID BACKGROUND))
 
;; start-pause: SG KeyEvent -> SG
;; start or pause or re-start ball
(check-expect (start-pause SG2 "f")
              (make-resting 200 1))
(check-expect (start-pause SG3 " ")
              (make-running 200 -1))
(check-expect (start-pause "press any key" "c")
              (make-running 0 1))
(define (start-pause asg akey)
  (cond [(string? asg) (key-handler-string asg)]
        [(resting? asg) (key-handler-resting asg)]
        [(running? asg) (key-handler-running asg)]))

;; key-handler-string: String -> Running
;; switches to the second state if a key is pressed
(check-expect (key-handler-string SG1) (make-running 0 1))
(define (key-handler-string s)
  (make-running 0 1))
 
;; key-handler-running: Running -> Resting
;; switches from running to resting ball if a key is pressed
(check-expect (key-handler-running SG2)
              (make-resting 200 1))
(define (key-handler-running arunning)
  (make-resting (running-x arunning) (running-dir arunning)))

;; key-handler-resting: Resting -> Running
;; switches from resting to running ball if a key is pressed
(check-expect (key-handler-resting SG3)
              (make-running 200 -1))
(define (key-handler-resting aresting)
  (make-running (resting-x aresting) (resting-dir aresting)))
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; next-ball: SG -> SG
;; move a running ball in the appropriate direction or flip it if it hits a border,
;; leave others alone
(check-expect (next-ball SG1) SG1)
(check-expect (next-ball SG2) (make-running (+ 200 SPEED) 1))
(check-expect (next-ball SG3) SG3)
(check-expect (next-ball (make-running -1 -1)) (make-running 0 1))
(check-expect (next-ball (make-running (add1 WIDTH) 1)) (make-running WIDTH -1))
(define (next-ball asg)
  (cond [(string? asg) asg]
        [(resting? asg) asg]
        [(running? asg) (if (<= 0 (running-x asg) WIDTH)
                            (move-ball asg)
                            (flip-direction asg))]))


 
;; move-ball: Running -> Running
;; move the ball in its direction
(check-expect (move-ball SG2) (make-running (+ 200 SPEED) 1))
(check-expect (move-ball (make-running 150 -1)) (make-running (- 150 SPEED) -1))
(define (move-ball arunning)
  (make-running (+ (running-x arunning)
                   (* SPEED (running-dir arunning)))
                (running-dir arunning)))
 
;; flip-direction: Running -> Running
;; flips direction of the ball
(check-expect (flip-direction (make-running -1 -1))
              (make-running 0 1))
(check-expect (flip-direction (make-running WIDTH 1))
              (make-running WIDTH -1))
(define (flip-direction arunning)
  (if (< (running-x arunning) 0)
      (make-running 0 1)
      (make-running WIDTH -1)))
 
 
 
 
 
 
 
 
