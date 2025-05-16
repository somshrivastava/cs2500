;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; Purpose: Practice with self-referential data.

;; Instructions
;; 1. Do not create, modify or delete any line that begins with ";;!". These are
;;    markers that we use to segment your file into parts to facilitate grading.
;; 2. You must follow the design recipe for every problem. In particular,
;;    every function you define must have at least three check-expects (and
;;    more if needed).
;; 3. You must follow the Style Guide:
;;    https://pages.github.khoury.northeastern.edu/2500/2024F/style.html
;; 4. You must submit working code. In DrRacket, ensure you get no errors
;;    when you click Run. After you submit on Gradescope, you'll get instant
;;    feedback on whether Gradescope can run your code, and your code must
;;    run on Gradescope to receive credit from the autograder.
;; 5. On some problems, you can get automated feedback on your in-progress work
;;    from FeedBot, a system developed by the course staff. When you submit your
;;    assignment, you will see a link to the FeedBot report along with the autograder
;;    feedback. Only a certain number of submissions will get this, and submissions
;;    close together will not receive the feedback.


;;! Problem 1

;; We want to track student diversity by recording age and racial data for an incoming class
;; of first-year students. Consider the following data definitions:

;; Demographic is one of:
;; - "International"
;; - "American Indian or Alaska Native"
;; - "Asian"
;; - "Black or African American"
;; - "Hispanic or Latinx"
;; - "Native Hawaiian or Pacific Islander"
;; - "Two or More Races"
;; - "White"
;; - "Race and Ethnicity Unknown"
;; which represents responses to a mutltiple-choice question about citizenship, ethnicity,
;; and race at Northeastern: https://diversity.northeastern.edu/resources/data/our-demographics/

(define (demo-template r)
  (cond
    [(string=? r "International") ...]
    [(string=? r "American Indian or Alaska Native") ...]
    [(string=? r "Asian") ...]
    [(string=? r "Black or African American") ...]
    [(string=? r "Hispanic or Latinx") ...]
    [(string=? r "Native Hawaiian or Pacific Islander") ...]
    [(string=? r "Two or More Races") ...]
    [(string=? r "White") ...]
    [(string=? r "Race and Ethnicity Unknown") ...]))

(define-struct student [name age demographic legacy? next])
;;! A Responses is one of:
;;! - "end of responses"
;;! - (make-student String Integer Demographic Boolean Responses)
;;! Interpretation: Student responses to a demographic survey.

(define RESPONSES-EX-1 "end of responses")
(define RESPONSES-EX-2 (make-student "Alice" 18 "International" #false "end of responses"))
(define RESPONSES-EX-3
  (make-student "Alice"
                18
                "International"
                #false
                (make-student "Bob" 19 "Two or More Races" #true "end of responses")))
(define RESPONSES-EX-4
  (make-student "Waldo"
                21
                "International"
                #false
                "end of responses"))

;;! Part A

;; Write the template for Responsses.

;;!! IMPORTANT: Write your response BELOW this line:

; responses-temp : Response -> ?
(define (responses-temp r)
  (cond
    [(and (string? r) (string=? "end of responses" r)) ...]
    [(student? r) (... (student-name r) ...
                   ... (student-age r) ...
                   ... (demo-template (student-demographic r)) ...
                   ... (student-legacy? r) ...
                   ... (reponses-template (student-next r)))]))

;;! Part B
;; INTERPRETIVE QUESTION
;;
;; In order to meaningfully track student diversity, it's important that our
;; data allow students to accurately share the demographic information with
;; which they identify. Consider the structure of our data definition. In 2-3
;; sentences, describe how using a single-picklist (i.e., Demographic is exactly
;; one of the options above) might undermine a student's ability to accurately
;; self-identify. Why might a student want to choose more than one?

;;!! IMPORTANT: Write your response BELOW this line:

#|

Using a single-picklist for demographic data forces students to select only one option, which can 
limit their ability to fully represent their diverse identities. Many students may identify with 
multiple demographic categories, such as being of mixed race or belonging to various cultural groups.
By restricting them to one choice, the data collected could oversimplify their identities and fail 
to capture the complexity of their lived experiences. One option may misidentify or not fully 
specify their identity to the extent they want. For example, if a student was Hispanic and African 
American, they would not have an option.

|#

;;! Part C
;; Design a function called count-over-twenty that counts the number
;; of students over the age of twenty in a series of Responses.

;;!! IMPORTANT: Write your response BELOW this line:

;; count-over-twenty : Responses Integer -> Integer
;; counts the number of students over the age of twenty in a series of responses
(define (count-over-twenty responses)
  (cond
    [(and (string? responses) (string=? "end of responses" responses)) 0]
    [(student? responses) 
      (if (> (student-age responses ) 20)
          (+ 1 (count-over-twenty (student-next responses)))
          (count-over-twenty (student-next responses)))]))

;; Test Cases
(check-expect (count-over-twenty RESPONSES-EX-1) 0)
(check-expect (count-over-twenty RESPONSES-EX-2) 0)
(check-expect (count-over-twenty RESPONSES-EX-3) 0)
(check-expect (count-over-twenty RESPONSES-EX-4) 1)

;;! Part D

;; Design a predicate called waldo-responded? that determines if a student named
;; "Waldo" is in a series of Responses.

;;!! IMPORTANT: Write your response BELOW this line:

;; waldo-responded? : Responses -> Boolean
;; determines if a student named "Waldo" is in a series of Responses
(define (waldo-responded? responses)
  (cond
    [(and (string? responses) (string=? "end of responses" responses)) #false]
    [(student? responses) (or (string=? (student-name responses) "Waldo")
                      (waldo-responded? (student-next responses)))]))

;; Test Cases
(check-expect (waldo-responded? RESPONSES-EX-1) #false)
(check-expect (waldo-responded? RESPONSES-EX-2) #false)
(check-expect (waldo-responded? RESPONSES-EX-3) #false)
(check-expect (waldo-responded? RESPONSES-EX-4) #true)

;;! Part E

;; Design a function, `legacy-students`, that takes in a Reponses and produces a Responses
;; containing only the legacy students.

;;!! IMPORTANT: Write your response BELOW this line:

;; legacy-students : Responses Responses -> Responses
;; takes in a Responses and produces a Responses with only legacy students
(define (legacy-students responses)
  (cond
    [(and (string? responses) (string=? "end of responses" responses)) responses] 
    [(student? responses)
     (if (student-legacy? responses)
         (make-student (student-name responses)
                       (student-age responses)
                       (student-demographic responses)
                       (student-legacy? responses)
                       (legacy-students (student-next responses)))
         (legacy-students (student-next responses)))]))

;; Test Cases
(check-expect (legacy-students RESPONSES-EX-1) "end of responses")
(check-expect (legacy-students RESPONSES-EX-2) "end of responses")
(check-expect (legacy-students RESPONSES-EX-3) (make-student "Bob" 19 "Two or More Races" #true "end of responses"))

;;! Problem 2

;; This problem has a partially-completed data definition that represents a
;; workout sequence.

(define-struct cardio [rest])
(define-struct strength [rest])
(define-struct flexibility [rest])
;;! A Workout is one of:
;;! - (make-cardio Workout)
;;! - (make-strength Workout)
;;! - (make-flexibility Workout)
;;! - "done"
;;! Interpretation: A list of exercises in a long workout.

;;! Part A

;; Give three examples of Workouts.

;;!! IMPORTANT: Write your response BELOW this line:

(define WORKOUT-1 (make-cardio "done"))
(define WORKOUT-2 (make-cardio (make-strength "done")))
(define WORKOUT-3 (make-cardio (make-strength (make-flexibility "done"))))

;;! Part B

;; Write the template for Workouts.

;;!! IMPORTANT: Write your response BELOW this line:

;; workout-temp : Workout -> ?
(define (workout-temp workout)
  (cond [(and (string? workout) (string=? "done" workout)) ...]
        [(cardio? workout) (... (workout-temp (cardio-rest workout)) ...)]
        [(strength? workout) (... (workout-temp (strength-rest workout)) ...)]
        [(flexibility? workout) (... (workout-temp (flexibility-rest workout)) ...)]))

;;! Part C

;; Design a function called recovery-sequence that generates a "recovery" sequence for a
;; given Workout. In the recovery sequence, cardio exercises become flexibility exercises,
;; strength exercises become cardio exercises, and flexibility exercises become strength
;; exercises. If you are "done", the recovery sequence ends.

;;!! IMPORTANT: Write your response BELOW this line:

;; recovery-sequence : Workout Workout -> Workout
;; generates a recovery sequence where cardio exercises become flexibility exercises,
;; strength exercises become cardio exercises, and flexibility exercises become strength
;; exercises
(define (recovery-sequence workout)
  (cond
    [(and (string? workout) (string=? "done" workout)) workout]
    [(cardio? workout)
     (make-flexibility (recovery-sequence (cardio-rest workout)))]
    [(strength? workout)
     (make-cardio (recovery-sequence (strength-rest workout)))]
    [(flexibility? workout)
     (make-strength (recovery-sequence (flexibility-rest workout)))]))

;; Test Cases
(check-expect (recovery-sequence WORKOUT-1) (make-flexibility "done"))
(check-expect (recovery-sequence WORKOUT-2) (make-flexibility (make-cardio "done")))
(check-expect (recovery-sequence WORKOUT-3) (make-flexibility (make-cardio (make-strength "done"))))