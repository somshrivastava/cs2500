;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lab7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Boston Potholes

;; Consider “Street Bump,” a project by the city of Boston to crowdsource data on potholes
;; (https://www.boston.gov/transportation/street-bump). The smartphone app automatically detects
;; potholes using data from the smartphone’s sensors and sends the data to the city. For our
;; purposes, we will represent the data gathered on a drive using the following data definitions:

(define-struct pothole [x y size urgent?])
;; A Pothole is a (make-pothole Decimal Decimal Decimal Boolean)
;; Interpretation: a datapoint collected about a pothole measuring the pothole's latitudinal and
;; longitudinal position, size (diameter in feet), and whether repairs are urgently needed.

;; Note: you can use the following website to explore the latitude and longitudes of each
;; pothole: https://www.latlong.net/

(define POTHOLE-EX-1 (make-pothole 42.3491 -71.0417 2 #true))
(define POTHOLE-EX-2 (make-pothole 42.3555 -71.0566 0.7 #false))
(define POTHOLE-EX-3 (make-pothole 42.3497 -71.0797 4 #true))
(define POTHOLE-EX-4 (make-pothole 42.3650 -71.0543 1.2 #true))

(define (pothole-temp pd)
  (... (pothole-x pd) ... (pothole-y pd) ... (pothole-size pd) ... (pothole-urgent? pd) ...))

;; A DriveData is one of:
;; - '()
;; - (cons Pothole DriveData)
;; Interpretation: a collection of informatation about potholes collected on a drive.

(define DD-EX-1 '())
(define DD-EX-2 (cons POTHOLE-EX-2 (cons POTHOLE-EX-1 '())))
(define DD-EX-3
  (cons POTHOLE-EX-4 (cons POTHOLE-EX-3 (cons POTHOLE-EX-2 (cons POTHOLE-EX-1 DD-EX-1)))))

;; drive-data-template : DriveData -> ...
(define (drive-data-template bd)
  (cond
    [(empty? bd) ...]
    [(cons? bd)
     (... (pothole-temp (first bd)) ...
          (drive-data-template (rest bd)) ...)]))

;;! Part A

;; Write a function called `urgent-potholes`, which consumes a DriveData and returns a DriveData with
;; all the urgent potholes.

;; Estimated Portion of Lab: 20%

;; urgent-potholes : DriveData -> DriveData
;; consumes a DriveData and returns a DriveData with all the urgent potholes
(define (urgent-potholes bd)
  (cond
    [(empty? bd) '()]
    [(cons? bd) (if (pothole-urgent? (first bd))
                    (cons (first bd) (urgent-potholes (rest bd)))
                    (urgent-potholes (rest bd)))]))

;; Test Cases
(check-expect (urgent-potholes DD-EX-1) '())
(check-expect (urgent-potholes DD-EX-2) (cons POTHOLE-EX-1 '()))
(check-expect (urgent-potholes DD-EX-3) (cons POTHOLE-EX-4 (cons POTHOLE-EX-3 (cons POTHOLE-EX-1 '()))))


;; STOP, SUBMIT YOUR WORK SO FAR TO GRADESCOPE, AND SWITCH WHO IS TYPING.

;;! Part B

;; INTERPRETIVE QUESTIONS
;; -- Consider the fact that our measure of "urgency" is a Boolean. It seems as though urgency
;; being "True" or "False" doesn't actually provide much valuable information. For example, imagine
;; you're trying to create an order in which to repair potholes around Boston. However, we might have
;; tens or hundreds of potholes that are all merely marked "Urgent," and we would have no way to
;; meaningfully rank them. Propose another field to measure the urgency of a pothole's repair. How
;; will this field be calculated? In 2-3 sentences, explain why your proposal effectively captures the
;; idea of "urgency."

;; Estimated Portion of Lab: 20%


#|

Rather than having a boolean to represent the urgency of the pothole, we can store a number ranging
from 1 to 10 that represents the level of urgency of the pothole. With such a proposal, the
construction team will be able to prioritize potholes that have a higher level of urgency rather
than those that are not as urget. Therefore, this better captures the idea of "urgency".

|#


;; When Street Bump was an active initiative from 2011-2014, people making <$30,000/year
;; were around half as likely to own a smartphone that could download the Street Bump app
;; (https://www.pewresearch.org/internet/2011/07/11/overview-of-smartphone-adoption/).
;; Smartphone ownership is still skewed across socioeconomic lines
;; (https://www.pewresearch.org/internet/fact-sheet/mobile/?tabItem=64e32376-5a21-4b1d-8f8b-5f92406db984).
;; In 2-3 sentences, explain how Street Bump may have worsened existing inequalities in Boston's
;; infrastructure.

;; Estimated Portion of Lab: 10%

#|

Street Bump may have worsened inequalities in Boston's infrastructure because the app was unable to be
downloaded by lower income communities. This may have prevented these groups from having access to
reporting potholes and other problems with infrastructure. This would lead to a disproportionate
addressing of infrastructural issues between higher and lower income communities.

|#

;;! Part C

;; After seeing this data, the city of Boston decides to allocate additional resources to fix the
;; potholes. They need to determine the quantity of cement they will need to fix the potholes.
;; Write a function called total-pothole-size, which consumes a DriveData and returns the sum
;; of the diameters (in feet) of all of the potholes.

;; Estimated Portion of Lab: 20%

;; total-pothole-size : DriveData -> Number
;; consumes a DriveData and returns the sum of the diameters of all of the potholes
(define (total-pothole-size bd)
  (cond
    [(empty? bd) 0]
    [(cons? bd) (+ (pothole-size (first bd)) (total-pothole-size (rest bd)))]))

;; Test Cases
(check-expect (total-pothole-size DD-EX-1) 0)
(check-expect (total-pothole-size DD-EX-2) 2.7)
(check-expect (total-pothole-size DD-EX-3) 7.9)


;;! Part D

;; Finally, write a function called `average-pothole`, which consumes a
;; DriveData and returns the average diameter (in feet) of its potholes. It
;; should return a MaybeNumber, defined below, and if there are no potholes,
;; return #false.
;;
;; Note: do not use the built-in `length` function.

;; A MaybeNumber is one of:
;; - #false
;; - Number
;; Interpretation: a Number or #false, if the number is unknown/impossible.
(define MNF #false)
(define MN0 0)
(define MN10 10.0)
(define (mn-temp mn)
  (cond [(boolean? mn) ...]
        [(number? mn) (... mn ...)]))

;; Estimated Portion of Lab: 30%

;; average-pothole : DriveData -> MaybeNumber
;; consumes a DriveData and returns the average diameter of its potholes
(define (average-pothole bd)
  (cond
    [(empty? bd) #false]
    [(cons? bd) (/ (total-pothole-size bd) (length bd))]))

(check-expect (average-pothole DD-EX-1) #false)
(check-expect (average-pothole DD-EX-2) 1.35)
(check-expect (average-pothole DD-EX-3) 1.975)

