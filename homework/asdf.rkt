;; Purpose: Practice with self-referential data.

;; Instructions
;; 1. Do not create, modify or delete any line that begins with ";;!". These are
;;    markers that we use to segment your file into parts to facilitate grading.
;; 2. You must follow the design recipe for every problem. In particular,
;;    every function you define must have at least three check-expects (and
;;    more if needed).
;; 3. You must follow the Style Guide:
;;    https://pages.github.khoury.northeastern.edu/2500/2024F/style.html
;; 4. You must submit working code. In DrRacket, ensure you get no errors
;;    when you click Run. After you submit on Gradescope, you'll get instant
;;    feedback on whether Gradescope can run your code, and your code must
;;    run on Gradescope to receive credit from the autograder.
;; 5. On some problems, you can get automated feedback on your in-progress work
;;    from FeedBot, a system developed by the course staff. When you submit your
;;    assignment, you will see a link to the FeedBot report along with the autograder
;;    feedback. Only a certain number of submissions will get this, and submissions
;;    close together will not receive the feedback.


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

(define RESPONSES-EX-4 (make-student "Test" 25 "International" #false "end of responses"))
(define RESPONSES-EX-5
  (make-student "Alice"
                18
                "International"
                #false
                (make-student "Waldo" 15 "Two or More Races" #true "end of responses")))

;;! Part A

;; Write the template for Responses.

;;!! IMPORTANT: Write your response BELOW this line:



;; response-template: Response -> ?

#; (define (reponses-template r)
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

;; A single-picklist might undermine a student's ability to accurately self-identify because
;; it restricts the amount of options a student has to identify themselves within a singular
;; option of the list. A student may feel that they identify with 2 or more specific options or even
;; an option not in the list. This is because one option may
;; misidentify or not fully specify their identity to the extent they want if, for example,
;; a student was Asian and African American then they would have no option. 



;;! Part C
;; Design a function called count-over-twenty that counts the number
;; of students over the age of twenty in a series of Responses.

;;!! IMPORTANT: Write your response BELOW this line:


(check-expect (count-over-twenty RESPONSES-EX-1) 0)
(check-expect (count-over-twenty RESPONSES-EX-2) 0)
(check-expect (count-over-twenty RESPONSES-EX-4) 1)

;; count-over-twenty: Response -> Number
;; Counts the number of students over the age of 20 in a series of Responses.

(define (count-over-twenty r)
  (cond
    [(and (string? r) (string=? "end of responses" r)) 0]
    [(student? r)
     (if (> (student-age r) 20)
         (+ 1 (count-over-twenty (student-next r)))
         (count-over-twenty (student-next r)))]))

;;! Part D

;; Design a predicate called waldo-responded? that determines if a student named
;; "Waldo" is in a series of Responses.

;;!! IMPORTANT: Write your response BELOW this line:

(check-expect (waldo-responded? RESPONSES-EX-1) #false)
(check-expect (waldo-responded? RESPONSES-EX-2) #false)
(check-expect (waldo-responded? RESPONSES-EX-5) #true)

;; waldo-responded?: Response-> Boolean
;; Determines if a student "Waldo" is in a series of Responses, true if "Waldo" is and false if not.

(define (waldo-responded? r)
  (cond
    [(and (string? r) (string=? "end of responses" r)) #false]
    [(student? r) (or (string=? (student-name r) "Waldo") (waldo-responded? (student-next r)))]))

;;! Part E

;; Design a function, `legacy-students`, that takes in a Reponses and produces a Responses
;; containing only the legacy students.

;;!! IMPORTANT: Write your response BELOW this line:

(check-expect (legacy-students RESPONSES-EX-1) "end of responses")
(check-expect (legacy-students RESPONSES-EX-2) "end of responses")
(check-expect (legacy-students RESPONSES-EX-5)
              (make-student "Waldo" 15 "Two or More Races" #true "end of responses"))

;; legacy-students: Response-> Response
;; Takes in a Response and produces a Response that contains only legacy students


(define (legacy-students r)
  (cond
    [(and (string? r) (string=? "end of responses" r)) r] 
    [(student? r)
     (if (student-legacy? r)
         (make-student
          (student-name r)
          (student-age r)
          (student-demographic r)
          (student-legacy? r)
          (legacy-students (student-next r)))
         (legacy-students (student-next r)))]))

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

(define WORKOUT-1 "done")
(define WORKOUT-2 (make-cardio "done"))
(define WORKOUT-3 (make-strength
                   (make-cardio
                    "done")))
  

;;! Part B

;; Write the template for Workouts.

;;!! IMPORTANT: Write your response BELOW this line:

(define (workout-template w)
  (cond
    [(and (string? w) (string=? "done" w)) ...]
    [(cardio? w) (... (workout-template (workout-rest w))...)]
    [(strength? w) (...(workout-template (workout-rest w))...)]
    [(flexibility? w) (...(workout-template (workout-rest w))...)]))
  

;;! Part C

;; Design a function called recovery-sequence that generates a "recovery" sequence for a
;; given Workout. In the recovery sequence, cardio exercises become flexibility exercises,
;; strength exercises become cardio exercises, and flexibility exercises become strength
;; exercises. If you are "done", the recovery sequence ends.

;;!! IMPORTANT: Write your response BELOW this line:


;; recovery-sequence: Workout-> Workout
;; Generates a "recovery" sequence for a Workout where cardio exercises become flexibility exercises,
;; strenth exercises become cardion exercises, and flexibility exerecises become strength exercises.
;; If "done", the recovery sequence ends.


(check-expect (recovery-sequence WORKOUT-1) "done")
(check-expect (recovery-sequence WORKOUT-2) (make-flexibility "done"))
(check-expect (recovery-sequence WORKOUT-3) (make-cardio (make-flexibility "done")))


(define (recovery-sequence w)
  (cond
    [(and (string? w) (string=? "done" w)) w]
    [(cardio? w) (make-flexibility (recovery-sequence (cardio-rest w)))]
    [(strength? w) (make-cardio (recovery-sequence (strength-rest w)))]
    [(flexibility? w) (make-strength (recovery-sequence (flexibility-rest w)))]))