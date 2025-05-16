;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname HW6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Instructions
;; 1. Do not create, modify or delete any line that begins with ";;!". These are
;;    markers that we use to segment your file into parts to facilitate grading.
;; 2. You must follow the _design recipe_ for every problem. In particular,
;;    every function you define must have at least three check-expects (and
;;    more if needed).
;; 3. You must follow the Style Guide:
;;    https://pages.github.khoury.northeastern.edu/2500/2024F/style.html
;; 4. You must submit working code. In DrRacket, ensure you get on errors
;;    when you click Run. After you submit on Gradescope, you'll get instant
;;    feedback on whether or Gradescope can run your code, and your code must
;;    run on Gradescope to receive credit from the autograder.
;; 5. On some problems, you can get automated feedback on your in-progress work
;;    from FeedBot, a system developed by the course staff. When you submit your
;;    assignment, you will see a link to the FeedBot report along with the autograder
;;    feedback. Only a certain number of submissions will get this, and submissions
;;    close together will not receive the feedback.

;;! Problem 1

;; Consider the three functions below (we have deliberately omitted tests and purpose
;; statements):

;; flip: [List-of Boolean] -> [List-of Boolean]
(define (flip lob)
  (cond
    [(empty? lob) '()]
    [(cons? lob) (cons (not (first lob)) (flip (rest lob)))]))

;; until-zero: [List-of Number] -> [List-of Number]
(define (until-zero lon)
  (cond
    [(empty? lon) '()]
    [(cons? lon) (if (= (first lon) 0) '() (cons (first lon) (until-zero (rest lon))))]))

;; words-until-period: [List-of String] -> [List-of String]
(define (words-until-period los)
  (cond
    [(empty? los) '()]
    [(cons? los)
     (if (string=? (first los) ".") '() (cons (first los) (words-until-period (rest los))))]))

;;! Part A

;; It is possible to design a list abstraction that can be used to simplify two
;; of the three functions defined above. Define that list abstraction. NOTE: you
;; do not need to include a signature, or tests for this. While a purpose isn't
;; required, thinking about it may be helpful.

;;!! IMPORTANT: Write your response BELOW this line:

;; processes a list by applying a transformation function to each element 
;; until the stopping condition is met, and returns the list of transformed elements.
(define (iterate-until lst stop-condition)
  (cond
    [(empty? lst) '()]
    [(cons? lst) (if (stop-condition (first lst))
                     '()
                     (cons (first lst) (iterate-until (rest lst) stop-condition)))]))

;;! Part B

;; Use the list abstraction you designed in Part A to rewrite the functions
;; above that you can. Do not modify the code above. Instead, write your
;; functions here and call them flip/v2, until-zero/v2, or
;; words-until-period/v2. Follow the full design recipe.

;;!! IMPORTANT: Write your response BELOW this line:

;; stop-at-zero?: Number -> Boolean
;; returns true if the given number is zero, else false
(define (stop-at-zero? x) (= x 0))

;; Test Cases
(check-expect (stop-at-zero? 5) #false)
(check-expect (stop-at-zero? 0) #true)
(check-expect (stop-at-zero? -3) #false)

;; until-zero/v2: [List-of Number] -> [List-of Number]
;; returns a list of numbers until a zero is found
(define (until-zero/v2 lon)
  (iterate-until lon stop-at-zero?))

;; Test Cases
(check-expect (until-zero/v2 '(1 2 3 0 4 5)) '(1 2 3))
(check-expect (until-zero/v2 '(0 1 2 3)) '())
(check-expect (until-zero/v2 '(1 2 3 4 5)) '(1 2 3 4 5))
(check-expect (until-zero/v2 '()) '())

;; stop-at-period?: String -> Boolean
;; returns true if the given string is a period
(define (stop-at-period? x) (string=? x "."))

;; Test Cases
(check-expect (stop-at-period? ".") #true)
(check-expect (stop-at-period? "hello") #false)
(check-expect (stop-at-period? "") #false)

;; words-until-period/v2: [List-of String] -> [List-of String]
;; returns a list of words until a period is found
(define (words-until-period/v2 los)
  (iterate-until los stop-at-period?))

;; Test Cases
(check-expect (words-until-period/v2 '("hello" "world" "." "again")) '("hello" "world"))
(check-expect (words-until-period/v2 '("." "hello" "world")) '())
(check-expect (words-until-period/v2 '("this" "is" "a" "test")) '("this" "is" "a" "test"))
(check-expect (words-until-period/v2 '()) '())

;;! Problem 2

;; In HW4, you designed a data definition that combined a Neighborhood with a
;; count of people who used public transit in that neighborhood. In this
;; problem, we are going to use a very similar data definition: this time add
;; the total number of commuters of your neighborhood, not just the number who
;; commute via public transit. We'll use this to build a simulation that can
;; infer future information about a neighborhood based on its known data.

;;! Part A

;; Please copy your data definition from HW4, adjusting it if necessary based on
;; feedback you received, and adding a field for the total _number_ of
;; commuters. Adjust examples appropriately, looking up accurate numbers for
;; your chosen neighborhoods.
;;
;; Name (or rename) your data definition NeighborhoodCommuters

;;!! IMPORTANT: Write your response BELOW this line:

;; A Neighborhood is one of:
;; - "East Boston"
;; - "West Roxbury"
;; - "Hyde Park"
;; - "Brighton"
;; - "Dorchester"
;; Interpretation: A neighborhood is one of 23 named neighborhoods in Boston
;; Examples: "East Boston", "West Roxbury", "Hyde Park"

(define EAST-BOSTON "East Boston")
(define WEST-ROXBURY "West Roxbury")
(define HYDE-PARK "Hyde Park")
(define BRIGHTON "Brighton")
(define DORCHESTER "Dorchester")

;; neighborhood-template : Neighborhood -> ?
(define (neighborhood-template n)
  (cond
    [(string=? n EAST-BOSTON) ...]
    [(string=? n WEST-ROXBURY) ...]
    [(string=? n HYDE-PARK) ...]
    [(string=? n BRIGHTON) ...]
    [(string=? n DORCHESTER) ...]))

;; A NeighborhoodCommuters is a (make-neighborhood-commuters Neighborhood Number Number)
(define-struct neighborhood-commuters [name public-transit commuters])
;; Represents a NeighborhoodCommuters where:
;; - name is the Neighborhood
;; - public transit is the number of commuters who use public transit in the neighborhood
;; - commuters is the total number of commuters in the neighborhood

;; make-neighborhood-commuters : Neighborhood Number Number -> NeighborhoodCommuters
;; neighborhood-commuters? : Any -> Boolean
;; neighborhood-commuters-name : NeighborhoodCommuters -> Neighborhood
;; neighborhood-commuters-public-transit : NeighborhoodCommuters -> Number
;; neighborhood-commuters-commuters : NeighborhoodCommuters -> Number

(define EAST-BOSTON-NC (make-neighborhood-commuters EAST-BOSTON 14110 27151))
(define WEST-ROXBURY-NC (make-neighborhood-commuters WEST-ROXBURY 2919 18483))
(define HYDE-PARK-NC (make-neighborhood-commuters HYDE-PARK 4548 19365))
(define BRIGHTON-NC (make-neighborhood-commuters BRIGHTON 11007 33229))
(define DORCHESTER-NC (make-neighborhood-commuters DORCHESTER 22294 64311))

;; NeighborhoodCommuters -> ?
(define (neighborhood-commuters-temp neighborhood-commuters)
    (...(neighborhood-template (neighborhood-commuters-name neighborhood-commuters))
        ...(neighborhood-commuters-public-transit neighborhood-commuters)
        ...(neighborhood-commuters-commuters neighborhood-commuters)))

;;! Part B

;; Define a constant named _exactly_ `MY-NC-EXAMPLE` that has an example of your
;; data definition. You should have more than one example above; this one can
;; just be defined as a synonym of one of the above.

;;!! IMPORTANT: Write your response BELOW this line:

(define MY-NC-EXAMPLE EAST-BOSTON-NC)

;;! Part C

;; We now want to represent multiple neighborhoods. Define a data definition for
;; a collection of NeighborhoodCommuters called BostonCommuters. Note: use a
;; list!

;;!! IMPORTANT: Write your response BELOW this line:

;; A BostonCommuters (BC) is one of:
;; - '()
;; - (cons NeighborhoodCommuters BC)
;; Interpretation: represents multiple neighborhoods within Boston

(define BC-1 '())
(define BC-2 (cons EAST-BOSTON-NC BC-1))
(define BC-3 (cons DORCHESTER-NC BC-2))
(define BC-4 (cons HYDE-PARK-NC (cons BRIGHTON-NC (cons WEST-ROXBURY-NC (cons DORCHESTER-NC BC-2)))))

;; bc-temp : BC -> ?
(define (bc-temp bc) 
  (...(cond [(empty? bc) ...] 
            [(cons? bc)
             ...(first bc) ...
             ...(bc-temp (rest bc)) ...])))

;;! Part D

;; Define a constant named _exactly_ `MY-BC-EXAMPLE` that has an example of your
;; data definition. You should have more than one example above; this one can
;; just be defined as a synonym of the above.

;;!! IMPORTANT: Write your response BELOW this line:

(define MY-BC-EXAMPLE BC-3)

;;! Part E

;; We want to use the known information about commuters who take public transportation
;; in a neighborhood to derive the aggregate CO2 emissions at 10, 25, and 40 years
;; into the future.

;; Design a function `simulate-emissions` that takes a NeighborhoodCommuters and
;; a number (representing years into the future) and returns the total amount of CO2
;; emissions after the specified number of years _caused by commuting_. You are
;; welcome to use the following simplified notes / assumptions:
;;
;; - You can consider all people who do not commute via public transit to
;;   commute via car. If one of your neighborhoods is Longwood, perhaps choose a
;;   different one!
;;
;; - A report from a few years ago stated the average commute time _by car_
;;   in the Boston area was 40 mins, and another report we found said the average
;;   distance of such commutes was 20 miles.
;;
;; - The average car emmits 400g of CO2 per mile driven. Feel free to ignore the
;;   (likely) improvement of this average over time, due to electrification.
;;
;; - You can ignore change in population over time for your simulation. Just assume
;;   the value you calculate for the first year is the same for the subsequent ones.
;;
;; - You can assume commuters go to work every day!

;;!! IMPORTANT: Write your response BELOW this line:

;; simulate-emissions : NeighborhoodCommuters Number -> Number
;; computes the total CO2 emissions from people that are commuting by car in a given neighborhood
;; over a specified number of years
(define (simulate-emissions nc years)
  (* (- (neighborhood-commuters-commuters nc)
        (neighborhood-commuters-public-transit nc))
     40      ;; miles per commute two ways
     400     ;; grams of CO2 per mile
     365     ;; days per year
     years))

;; Test Cases
(check-expect (simulate-emissions EAST-BOSTON-NC 10) 761594400000) 
(check-expect (simulate-emissions WEST-ROXBURY-NC 25) 2272344000000) 
(check-expect (simulate-emissions DORCHESTER-NC 40) 9815171200000) 

;;! Part F

;; INTERPRETIVE QUESTION
;;
;; Imagine we're members of the Environmental Justice, Resiliency, and Parks committee
;; (https://www.boston.gov/departments/city-council/environmental-justice-resiliency-and-parks)
;; in Boston's city council. We want to fund the electrification of buses to
;; communities who are most harmed by air pollution from gas buses. We need to
;; find where pollutants are distributed.
;;
;; Our data currently tracks what percentage of neighborhood commuters take
;; public transportation (for this question, let's assume they're all taking
;; gas-powered buses).
;;
;; In 2-3 sentences, explain why our data does not track where air pollutants
;; from buses are distributed. What sort of information would we need to
;; estimate where gas buses spread air pollutants?

;;!! IMPORTANT: Write your response BELOW this line:

#|

Our data only tracks the percentage of people commuting via public transportation, but does not
account for the actual bus routes. To estimate where gas buses spread air pollutants, we need data
on the bus routes so that we know where exactly these buses are travelling and spreading the air
pollutants. This will help us know what communities are actually most harmed by air pollution
from gas buses.

|#

;;! Part G

;; Now, design a new draw function, `draw-boston-emissions`, that takes
;; a BostonCommuters, a scale (0-1), and a number of years into the future and
;; draws all of the neighborhoods colored-coded by the amount of CO2 emissions
;; at that time caused by commuting.

;; Hint: You will need to design a helper function that associates a color with the amount of
;; CO2 emissions in a specific neighborhood, and should re-use past drawing functions you've implemented.

;;!! IMPORTANT: Write your response BELOW this line:

;; get-color : Number -> Color
;; takes the emissions and returns a red color; the higher the emissions, the darker the red
(define (get-color emissions)
  (make-color (min 255 (max 0 (floor (/ emissions 20000000000)))) 0 0))

;; Test Cases
(check-expect (get-color 0) (make-color 0 0 0))
(check-expect (get-color 20000000000) (make-color 1 0 0))
(check-expect (get-color 4000000000000) (make-color 200 0 0))
(check-expect (get-color 60000000000000) (make-color 255 0 0))

;; draw-neighborhood : NeighborhoodCommuters Number Number -> Image
;; returns an image of the neighborhood given the neighborhood-commuters, a scale, and the number
;; of years into the future
(define (draw-neighborhood nc sc yrs)
  (cond
    [(string=? (neighborhood-commuters-name nc) EAST-BOSTON)
     (place-image (rectangle (* sc 50) (* sc 50) "solid" (get-color (simulate-emissions nc yrs)))
                  (* sc 175)
                  (* sc 25)
                  (rectangle (* sc 200) (* sc 250) "solid" "transparent"))]
    [(string=? (neighborhood-commuters-name nc) WEST-ROXBURY)
     (place-image (rectangle (* sc 55) (* sc 80) "solid" (get-color (simulate-emissions nc yrs)))
                  (* sc 20)
                  (* sc 180)
                  (rectangle (* sc 200) (* sc 250) "solid" "transparent"))]
    [(string=? (neighborhood-commuters-name nc) HYDE-PARK)
     (place-image (rectangle (* sc 45) (* sc 70) "solid" (get-color (simulate-emissions nc yrs)))
                  (* sc 70)
                  (* sc 225)
                  (rectangle (* sc 200) (* sc 250) "solid" "transparent"))]
    [(string=? (neighborhood-commuters-name nc) BRIGHTON)
     (place-image (rectangle (* sc 40) (* sc 40) "solid" (get-color (simulate-emissions nc yrs)))
                  (* sc 35)
                  (* sc 45)
                  (rectangle (* sc 200) (* sc 250) "solid" "transparent"))]
    [(string=? (neighborhood-commuters-name nc) DORCHESTER)
     (place-image (rectangle (* sc 60) (* sc 70) "solid" (get-color (simulate-emissions nc yrs)))
                  (* sc 150)
                  (* sc 130)
                  (rectangle (* sc 200) (* sc 250) "solid" "transparent"))]))

;; Test Cases
(check-expect 
  (draw-neighborhood EAST-BOSTON-NC 1 2)
  (place-image (rectangle 50 50 "solid" (get-color (simulate-emissions EAST-BOSTON-NC 2)))
               175
               25
               (rectangle 200 250 "solid" "transparent")))
(check-expect 
  (draw-neighborhood WEST-ROXBURY-NC 0.5 5)
  (place-image (rectangle 27.5 40 "solid" (get-color (simulate-emissions WEST-ROXBURY-NC 5)))
               10
               90
               (rectangle 100 125 "solid" "transparent")))
(check-expect 
  (draw-neighborhood BRIGHTON-NC 1 10)
  (place-image (rectangle 40 40 "solid" (get-color (simulate-emissions BRIGHTON-NC 10)))
               35
               45
               (rectangle 200 250 "solid" "transparent")))

;; draw-boston-emissions : BostonCommuters Number Number -> Image
;; takes a boston-commuters, a scale, and a number of years into the future, and draws only
;; the neighborhoods provided, color-coded by the amount of CO2 emissions after that time,
;; positioning them based on a rough geographic layout
(define (draw-boston-emissions bc scale years)
  (cond
    [(empty? bc) (rectangle (* scale 200) (* scale 250) "solid" "white")] 
    [else
     (overlay
      (draw-neighborhood (first bc) scale years)
      (draw-boston-emissions (rest bc) scale years))]))

;; Test Cases
(check-expect (draw-boston-emissions BC-1 1 10) (rectangle 200 250 "solid" "white"))
(check-expect (draw-boston-emissions BC-2 0.5 5)
              (overlay (draw-neighborhood EAST-BOSTON-NC 0.5 5)
                       (rectangle 100 125 "solid" "white")))
(check-expect (draw-boston-emissions BC-3 1 10)
              (overlay (draw-neighborhood DORCHESTER-NC 1 10)
                       (overlay (draw-neighborhood EAST-BOSTON-NC 1 10)
                                (rectangle 200 250 "solid" "white"))))

;;! Part H

;; INTERPRETIVE QUESTION

;; Our estimate of commuters' CO2 emissions is based on public transportation
;; data. However, public transportation commuters on average produce less than
;; half as much CO2 compared to other commuters.
;; (https://www.transit.dot.gov/sites/fta.dot.gov/files/docs/PublicTransportationsRoleInRespondingToClimateChange2010.pdf)
;;
;; In 2-3 sentences, identify which commuters we're leaving out of our
;; calculation. If we were to rank neighborhoods by total carbon emissions, how
;; might our calculation give an incomplete picture?

;;!! IMPORTANT: Write your response BELOW this line:

#|

In our calculations, we ONLY take into account commuters by car. We exclude public transit commuters
or any other sorts of commuters like walkers or cyclists causing our calculations to be an
innacurate representation of the neighborhoods. If we were to rank neighborhoods by total carbon
emissions, we would have an incomplete picture because we fail to consider other sources of carbon
emissions like deforestation, industry factories, and waste disposal. By not considering these
sources, we have an even further innacurate representation of total carbon emissions.

|#

;;! Part I

;; Finally, let's visualize these changes over time. Design a function
;; `visualize-boston-emissions` that takes a single argument--a number of years in the
;; future--and draws a collection of neighborhoods of your choice, colored according to
;; the amount of emissions.
;;
;; NOTE: in the Interactions window, you can use `animate` from 2htdp/universe
;; to run this function, but do not put it in the file as it may cause the
;; autograder not to run.

;;!! IMPORTANT: Write your response BELOW this line:

;; visualize-boston-emissions : Number -> Image
;; takes a number of years and returns a collection of neighborhoods
(define (visualize-boston-emissions years)
  (draw-boston-emissions BC-4 0.5 years))

;; Test Cases
(check-expect (visualize-boston-emissions 10) (draw-boston-emissions BC-4 0.5 10))
(check-expect (visualize-boston-emissions 15) (draw-boston-emissions BC-4 0.5 15))
(check-expect (visualize-boston-emissions 20) (draw-boston-emissions BC-4 0.5 20))

(animate visualize-boston-emissions)