;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;! Instructions:
;;! 1. Read the contents of this file, and fill in [TODO] items that appear
;;!    below.
;;! 2. Do not create, modify or delete any line that begins with ";;!", such
;;!    as these lines. These are markers that we use to segment your file into
;;!    parts to facilitate grading.
;;! 3. Any time you see the word "design", it means follow the appropriate
;;!    design recipe (function or data).

(require 2htdp/image)
(require 2htdp/universe)

;;! Problem 1

;; TODO: Design the function string-starts-with? which takes two
;; Strings and returns a Boolean indicating whether the first
;; string begins with the second. Be sure to follow all the steps
;; of the Design Recipe for functions.

;; When you are testing your function, make sure you test the case
;; where the first string is shorter than the second. For example
;; (string-starts-with? "fundies" "fun") should return #true but
;; (string-starts-with? "fun" "fundies") should return #false.

;; string-starts-with? : String -> Boolean
;; checks whether the first string starts with the second string
(define (string-starts-with? string1 string2)
  (if (> (string-length string2) (string-length string1))
      #false
      (string=? (substring string1 0 (string-length string2)) string2)))

;; Test Cases
(check-expect (string-starts-with? "fundies" "fun") #true)
(check-expect (string-starts-with? "fun" "fundies") #false)
(check-expect (string-starts-with? "fun" "fun") #true)

;; STOP AND SWITCH PARTNERS!
;;
;; Make sure you share your Problem 1 code with your partner before switching.

;;! Problem 2

;; You are running a robot portrait studio where artists can come to draw robot
;; models, but the problem is that the robot models keep falling asleep! In this lab,
;; you will design a simulator that allows you to wake the robots up when they
;; fall asleep, so your artists stop getting mad at you...
;;
;; - A robot can either be asleep or awake. Your program state will be able to
;;   represent these two states.
;;
;; - The user can wake up an asleep robot by pressing the "w"
;;   key on their keyboard.
;;
;; - Unfortunately, robots are prone to falling asleep: every 5 seconds, it seems
;;   that some process inside the robots causes them to fall asleep, even if they
;;   were just woken up!


;;! An RobotState is one of:
;;! - "asleep"
;;! - "awake"
;;! Interpretation: states of a robot in the portrait studio

;;! Part A

;; TODO: finish the Design Recipe for data for RobotState
;; (so provide examples and a template called rs-temp)

;; Examples: "asleep", "awake"
(define AWAKE "awake")
(define ASLEEP "asleep")

(define (rs-temp state)
  (cond
    [(string=? state "asleep") ...]
    [(string=? state "awake") ...]))

;;! Part B

;; In this part, you will define the two faces of your robot. We have provided
;; the sleeping face as the constant ASLEEP-FACE. You should design a face for
;; the woken up robot (who might be surprised, unhappy, or simply alert!) with
;; the constant AWAKE-FACE. You're welcome to re-use or modify our face components.


(define BACKGROUND (square 150 "solid" "transparent"))

(define EMOJI-COLOR (color 255 205 84))

(define FACE (circle 75 "solid" (color 255 205 84)))
(define FACE-MIDDLE (/ (image-width FACE) 2))
(define NEUTRAL-EXPRESSION
  (rectangle 50 5 "solid" "black"))

(define SLEEPING-EYE
  (place-image
   (rotate 180 (wedge 15 180 "solid" EMOJI-COLOR))
   (* 37.5 0.6) 5
   (rotate 180 (wedge (* 37.5 0.6) 180 "solid" "black"))))

(define ASLEEP-FACE
  (overlay
   (place-image
    (rectangle 110 4 "solid" EMOJI-COLOR)
    FACE-MIDDLE (- FACE-MIDDLE 35)
    (place-image
     SLEEPING-EYE
     (- FACE-MIDDLE 30) (- FACE-MIDDLE 25)
     (place-image
      SLEEPING-EYE
      (+ FACE-MIDDLE 30) (- FACE-MIDDLE 25)
      (place-image NEUTRAL-EXPRESSION
                   FACE-MIDDLE (+ FACE-MIDDLE 25)
                   FACE))))
   BACKGROUND))

;; TODO: define constant AWAKE-FACE with an image with an awake robot.


(define SHOCKED-EXPRESSION
  (ellipse 35 25 "solid" "black"))

(define AWAKE-EYE
  (place-image
   (circle 5 "solid" "white")
   15 15
   (circle 15 "solid" "black")))

(define AWAKE-FACE
  (overlay
    (place-image
     AWAKE-EYE
     (- FACE-MIDDLE 30) (- FACE-MIDDLE 25)
     (place-image
      AWAKE-EYE
      (+ FACE-MIDDLE 30) (- FACE-MIDDLE 25)
      (place-image SHOCKED-EXPRESSION
                   FACE-MIDDLE (+ FACE-MIDDLE 25)
                   FACE)))
   BACKGROUND))

;;! Part C
;;
;; Design a function, robot-draw, that takes a RobotState and produces an Image,
;; where the robot is either asleep or awake depending on the state.

;; robot-draw : RobotState -> Image
;; returns the robot face given the robot state
(define (robot-draw RobotState)
  (cond
    [(string=? RobotState AWAKE) AWAKE-FACE]
    [(string=? RobotState ASLEEP) ASLEEP-FACE]))

;; Test Cases
(check-expect (robot-draw AWAKE) AWAKE-FACE)
(check-expect (robot-draw ASLEEP) ASLEEP-FACE)

;;! Part D
;;
;; Design a function, robot-key, that takes a RobotState and a KeyEvent, and when
;; the key 'w' is pressed, if the RobotState is ASLEEP, changes it to AWAKE. If
;; any other key is pressed, the state should not change.

;; robot-key : "w" -> state change
;; Changes the state when the "w" key is pressed
(define (robot-key RobotState key)
  (if (string=? key "w")
      AWAKE
      RobotState))

;; Test Cases
(check-expect (robot-key AWAKE "w") AWAKE)
(check-expect (robot-key ASLEEP "w") AWAKE)
(check-expect (robot-key AWAKE "s") AWAKE)
(check-expect (robot-key ASLEEP "s") ASLEEP)

;;! Part E
;;
;; Design a function, robot-tick, that takes a RobotState and, if it is AWAKE, makes
;; it ASLEEP again.

;; robot-tick : RobotState -> RobotState
;; makes the robot sleep if it is awake
(define (robot-tick RobotState)
  (if (string=? RobotState AWAKE)
      ASLEEP
      RobotState))

;; Test Cases
(check-expect (robot-tick AWAKE) ASLEEP)
(check-expect (robot-tick ASLEEP) ASLEEP)

;;! Part F
;;
;; Design a function robot-studio-simulator (but do not write check-expects)
;; that calls big-bang with all the handlers above, given a starting state
;; as input. Note that for `on-tick`, you will want to include the optional second
;; argument that is the time interval between when it is called.


;; To test your simulator, run (robot-studio-simulator ASLEEP) in the interactions
;; window. If you do put it that line in the definitions window, be SURE to comment
;; it out before submitting, as otherwise your program will run forever and the
;; autograder will be unable to process it.

;; robot-studio-simulator : RobotState -> Image
;; returns the robot given the state
(define (robot-studio-simulator RobotState)
  (big-bang RobotState
    (to-draw robot-draw)
    (on-key robot-key)
    (on-tick robot-tick 5)))