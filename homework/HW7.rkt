;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname HW7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

;; The objective in this problem is to define the following functions.
;; We have given their signatures, purpose statements, and check-expects.

(define-struct pair [first second])
;; A [Pair X] is a (make-pair X X) representing a pair of values of type X
;; - first is the first item in the pair
;; - second is the second item in the pair

;; strings-or-odds : [List-of [Pair Number]] -> [List-of [Pair String]]
;; For each pair converts the first item to a string and the second to "odd".
(check-expect (strings-or-odds (list (make-pair 53 23) (make-pair 40 11)))
              (list (make-pair "53" "odd") (make-pair "40" "odd")))
(check-expect (strings-or-odds (list (make-pair 20 30) (make-pair 0 1) (make-pair 3 4)))
              (list (make-pair "20" "odd") (make-pair "0" "odd") (make-pair "3" "odd")))
(check-expect (strings-or-odds '()) '())

;; alternate-case : [List-of [Pair String]] -> [List-of [Pair String]]
;; Uppercase the first item of each pair. Leaves the second item unchanged.
(check-expect (alternate-case (list (make-pair "hello" "world") (make-pair "this" "Is")))
              (list (make-pair "HELLO" "world") (make-pair "THIS" "Is")))
(check-expect
 (alternate-case (list (make-pair "one" "two") (make-pair "three" "FOUR") (make-pair "five" "six")))
 (list (make-pair "ONE" "two") (make-pair "THREE" "FOUR") (make-pair "FIVE" "six")))
(check-expect (alternate-case (list (make-pair "apple" "banana")))
              (list (make-pair "APPLE" "banana")))

;; flip-or-keep-boolean : [List-of [Pair Boolean]] -> [List-of [Pair Boolean]]
;; Flip the first item of each pair, keep the second unchanged.
(check-expect (flip-or-keep-boolean (list (make-pair #true #true) (make-pair #true #true)))
              (list (make-pair #false #true) (make-pair #false #true)))
(check-expect (flip-or-keep-boolean (list (make-pair #false #false) (make-pair #false #false)))
              (list (make-pair #true #false) (make-pair #true #false)))
(check-expect (flip-or-keep-boolean (list (make-pair #true #false) (make-pair #false #true)))
              (list (make-pair #false #false) (make-pair #true #true)))

;; However, you must not directly use the list template when you define them!
;;
;; Instead, first design a list abstraction (following the list template), then
;; use that abstraction to design the three functions.

;;!! IMPORTANT: Write your response BELOW this line:

;; pairs-list-abstraction : (X Y) [List-of X] [X -> Y] -> [List-of Y]
;; applies a given function to each element in a list of pairs
(define (pairs-list-abstraction f lst)
  (cond
    [(empty? lst) empty]
    [(cons? lst) (cons (f (first lst)) (pairs-list-abstraction f (rest lst)))]))

(check-expect (pairs-list-abstraction convert-string-or-odds (list (make-pair 53 23) (make-pair 40 11)))
              (list (make-pair "53" "odd") (make-pair "40" "odd")))
(check-expect (pairs-list-abstraction uppercase (list (make-pair "hello" "world")
                                                      (make-pair "this" "Is")))
              (list (make-pair "HELLO" "world") (make-pair "THIS" "Is")))
(check-expect (pairs-list-abstraction flip (list (make-pair #true #true) (make-pair #true #true)))
              (list (make-pair #false #true) (make-pair #false #true)))

;; convert-string-or-odds : [Pair Number] -> [Pair String]
;; takes a pair of numbers, makes the first element a string and the second element "odd"
(define (convert-string-or-odds pair) 
  (make-pair (number->string (pair-first pair)) "odd"))

;; Test Cases
(check-expect (convert-string-or-odds (make-pair 53 23)) (make-pair "53" "odd"))
(check-expect (convert-string-or-odds (make-pair 40 11)) (make-pair "40" "odd"))
(check-expect (convert-string-or-odds (make-pair 0 1)) (make-pair "0" "odd"))
(check-expect (convert-string-or-odds (make-pair 3 4)) (make-pair "3" "odd"))

(define (strings-or-odds lst)
  (pairs-list-abstraction convert-string-or-odds lst))

;; uppercase : [Pair String] -> [Pair String]
;; takes a pair of strings, makes the first element all uppercase and the second element unchanged
(define (uppercase pair)
  (make-pair (string-upcase (pair-first pair)) (pair-second pair)))

;; Test Cases
(check-expect (uppercase (make-pair "hello" "world")) (make-pair "HELLO" "world"))
(check-expect (uppercase (make-pair "this" "Is")) (make-pair "THIS" "Is"))
(check-expect (uppercase (make-pair "one" "two")) (make-pair "ONE" "two"))
(check-expect (uppercase (make-pair "apple" "banana")) (make-pair "APPLE" "banana"))

(define (alternate-case lst)
  (pairs-list-abstraction uppercase lst))

;; flip : [Pair Boolean] -> [Pair Boolean]
;; takes a pair of booleans, flips the first element and the second element unchanged
(define (flip pair)
  (make-pair (not (pair-first pair)) (pair-second pair)))

;; Test Cases
(check-expect (flip (make-pair #true #true)) (make-pair #false #true))
(check-expect (flip (make-pair #false #false)) (make-pair #true #false))
(check-expect (flip (make-pair #true #false)) (make-pair #false #false))
(check-expect (flip (make-pair #false #true)) (make-pair #true #true))

(define (flip-or-keep-boolean lst)
  (pairs-list-abstraction flip lst))

;;! Problem 2

;; Recall our student demographic survey from HW5:

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

;; In this problem, you will be asked to update this data definition. You should create
;; new versions of the relevant structs/functions by naming them `.../v2`, `.../v3`, etc.

;;! Part A

;; The university has decided that the nested responses have become too unwieldy
;; to parse. Replace the definition with a new data structure that represents an
;; arbitrary number of survey results using a list. Consider how the definition
;; of a student should change.

;;!! IMPORTANT: Write your response BELOW this line:

(define-struct student/v2 [name age demographic legacy?])

;; A Student/v2 is a (make-student/v2 String Integer Demographic Boolean)
;; Interpretation: A survey response by a student at Northeastern
;; - name is their name
;; - age is their age
;; - demographic is their demographic
;; - legacy? is whether they have legacy status

;; make-student/v2 : String Integer Demographic Boolean -> Student
;; student/v2? : Any -> Boolean
;; student/v2-name : Student/v2 -> String
;; student/v2-age : Student/v2 -> Integer
;; student/v2-demographic : Student/v2 -> Demographic
;; student/v2-legacy? : Student/v2 -> Boolean

(define STUDENT/V2-1 (make-student/v2 "Alice" 18 "International" #false))
(define STUDENT/V2-2 (make-student/v2 "Bob" 19 "Two or More Races" #true))
(define STUDENT/V2-3 (make-student/v2 "Som" 23 "International" #true))

(define (student/v2-temp s)
  (... (student/v2-name s) ...
       (student/v2-age s) ...
       (demo-template (student/v2-demographic s)) ...
       (student/v2-legacy? s) ...))

;; A Responses/v2 is a [List-of Student/v2]:
;; Interpretation: a list of student responses without any nesting

(define RESPONSES/V2-1 '())
(define RESPONSES/V2-2 (list STUDENT/V2-1))
(define RESPONSES/V2-3 (list STUDENT/V2-1 STUDENT/V2-2 STUDENT/V2-3))

(define (responses/v2-temp responses)
  (... (cond [(empty? responses) ...]
             [(cons? responses) ... (student/v2-temp (first responses))
                           ... (responses/v2-temp (rest responses)) ...])))

;;! Part B

;; Given our concerns about accuracy from HW 5, the university has decided to
;; further modify the survey so students can select multiple options of
;; Demographic in response to the question about citizenship, ethnicity, and
;; race. Revise the Student and Responses data definitions accordingly, providing `.../v3` versions.

;;!! IMPORTANT: Write your response BELOW this line:

;; A LoD is a [List-of Demographic]:
;; Interpretation: a list of demographics for a student

(define LOD-1 '())
(define LOD-2 (list "Asian"))
(define LOD-3 (list "International" "Race and Ethnicity Unknown"))
(define LOD-4 (list "Black or African American" "American Indian or Alaska Native"))

(define (lod-temp lod)
  (... (cond [(empty? lod) ...]
             [(cons? lod) ... (demo-template (first responses))
                           ... (lod-temp (rest responses)) ...])))

(define-struct student/v3 [name age demographics legacy?])

;; A Student/v3 is a (make-student/v3 String Integer LoD Boolean)
;; Interpretation: A survey response by a student at Northeastern
;; - name is their name
;; - age is their age
;; - demographics is their demographics
;; - legacy? is whether they have legacy status

;; make-student/v3 : String Integer LoD Boolean -> Student
;; student/v3? : Any -> Boolean
;; student/v3-name : Student/v3 -> String
;; student/v3-age : Student/v3 -> Integer
;; student/v3-demographics : Student/v3 -> LoD
;; student/v3-legacy? : Student/v3 -> Boolean

(define STUDENT/V3-1 (make-student/v3 "Alice" 18 LOD-2 #false))
(define STUDENT/V3-2 (make-student/v3 "Bob" 19 LOD-3 #true))
(define STUDENT/V3-3 (make-student/v3 "Som" 23 LOD-4 #false))

(define (student/v3-temp s)
  (... (student/v3-name s) ...
       (student/v3-age s) ...
       (lod-temp (student/v3-demographics s)) ...
       (student/v3-legacy? s) ...))

;; A Responses/v3 is a [List-of Student/v3]:
;; a list of student responses with a list of demographics rather than just one demographic

(define RESPONSES/V3-1 '())
(define RESPONSES/V3-2 (list STUDENT/V3-1))
(define RESPONSES/V3-3 (list STUDENT/V3-1 STUDENT/V3-2 STUDENT/V3-3))

(define (responses/v3-temp responses)
  (... (cond [(empty? responses) ...]
             [(cons? responses) ... (student/v3-temp (first responses))
                           ... (responses/v3-temp (rest responses)) ...])))

;;! Part C (PART 1)
;; INTERPRETIVE QUESTIONS
;;
;; When collecting demographic data, we want to strike an effective balance
;; between allowing for accurate self-expression from respondents and collecting
;; data in a way that allows for meaningful analysis. In an effort to enhance
;; students' abilities to self-express, we're considering adding an "Other"
;; option, in which students can submit a free text response to self-identify.

;; -- Universities track student demographic data (in part) to evaluate how
;; their efforts to support a diverse student body have been effective or
;; ineffective over time. In 1-2 sentences, explain how adding the "Other"
;; option might complicate analysis.

;;!! IMPORTANT: Write your response BELOW this line:

#|

Adding an "Other" option with a free text response can complicate analysis because it introduces
variability in how students self-identify, leading to a wide range of unique responses that are
difficult to categorize and quantify. This can make it challenging to perform consistent comparisons
over time or identify trends within specific demographic groups.

|#

;;! Part C (PART 2)
;; -- In 2-3 sentences, how might adding the "Other" option benefit students?
;; How might it benefit the university?

;;!! IMPORTANT: Write your response BELOW this line:

#|

Adding the "Other" option can benefit students by allowing them to express their identities more
accurately, especially if existing categories do not fully represent them. For the
university, this option can provide qualitative insights into the various backgrounds of the student
body, potentially highlighting the need for additional support or resources for underrepresented
groups.

|#

;;! Part D

;; You've been tasked to identify how many students put down "Race and Ethnicity
;; Unknown", as that might indicate students didn't feel the existing options
;; allowed them to express their identity. Design a function
;; `count-unknown-students` that returns the number of such students, given the
;; latest version of your data definition as input. Consider which list
;; abstraction(s) would be useful here (you are not required to use one, but
;; may).

;;!! IMPORTANT: Write your response BELOW this line:

;; count-unknown-students : Responses/v3 -> Number
;; returns the number of students who have "Race and Ethnicity Unknown" in their list of demographics
(define (count-unknown-students responses)
  (length (filter has-unknown-demographic? responses)))

;; Test Cases
(check-expect (count-unknown-students RESPONSES/V3-1) 0)
(check-expect (count-unknown-students RESPONSES/V3-2) 0)
(check-expect (count-unknown-students RESPONSES/V3-3) 1)

;; has-unknown-demographic? : Student/v3 -> Boolean
;; returns true if the student's demographics list includes "Race and Ethnicity Unknown"
(define (has-unknown-demographic? student)
  (contains-race-and-ethnicity-unknown? (student/v3-demographics student)))

;; Test Cases
(check-expect (has-unknown-demographic? STUDENT/V3-1) #false)
(check-expect (has-unknown-demographic? STUDENT/V3-2) #true)
(check-expect (has-unknown-demographic? STUDENT/V3-3) #false)

;; contains-race-and-ethnicity-unknown? : [List-of Demographic] -> Boolean
;; returns true if "Race and Ethnicity Unknown" is in the list of demographics
(define (contains-race-and-ethnicity-unknown? demographics)
  (cond
    [(empty? demographics) #false]
    [(cons? demographics) (if (string=? (first demographics) "Race and Ethnicity Unknown")
                              #true
                              (contains-race-and-ethnicity-unknown? (rest demographics)))]))

;; Test Cases
(check-expect (contains-race-and-ethnicity-unknown? LOD-1) #false)
(check-expect (contains-race-and-ethnicity-unknown? LOD-2) #false)
(check-expect (contains-race-and-ethnicity-unknown? LOD-3) #true)
(check-expect (contains-race-and-ethnicity-unknown? LOD-4) #false)

;;! Part E
;; INTERPRETIVE QUESTION
;;
;; Read the following article: https://tinyurl.com/bdza4nt7
;;
;; Imagine you're working for a real estate company, and your boss wants to
;; target advertisements using the demographic data collected above. Write a 2-3
;; sentence memo to your boss explaining why you can't advertise on the basis of
;; racial demographic data, referring to the article in your answer. Explain why
;; the advertisements your boss proposed have the potential to violate the
;; provisions of the Fair Housing Act. Offer an alternative to your boss's
;; proposal.

;;!! IMPORTANT: Write your response BELOW this line:

#|

Using racial demographic data for targeted advertisements can violate the laws of
the Fair Housing Act, as the article highlights that it is illegal to discriminate in housing
advertisements based on race or color. Targeting ads based on race could be considered discriminatory
and result in legal consequences for our company. Instead, we should focus on
criteria like location or interests that isn't discriminatory so that our marketing
efforts follow the laws and are inclusive.

|#

;;! Part F

;; Since this personal racial data is sensitive and the university only needs
;; the demographic data in aggregate, the university has decided they would like
;; to anonymize the data for privacy purposes. Design a data definition for a
;; student response that does not include the student's name. Then write a
;; function, `anonymize`, that converts from a series of responses containing
;; student names to your new data definition.

;;!! IMPORTANT: Write your response BELOW this line:

(define-struct student/v4 [age demographics legacy?])

;; A Student/v4 is a (make-student/v4 Integer [List-of Demographic] Boolean)
;; Interpretation: A survey response by a student at Northeastern
;; - age is their age
;; - demographics is their demographics
;; - legacy? is whether they have legacy status

;; make-student/v4 : Integer [List-of Demographic] Boolean -> Student
;; student/v4? : Any -> Boolean
;; student/v4-age : Student/v4 -> Integer
;; student/v4-demographics : Student/v4 -> [List-of Demographic]
;; student/v4-legacy? : Student/v4 -> Boolean

(define STUDENT/V4-1 (make-student/v4 18 (list "Asian") #false))
(define STUDENT/V4-2 (make-student/v4 19
                                      (list "International" "Race and Ethnicity Unknown") #true))
(define STUDENT/V4-3 (make-student/v4 23
                                      (list "Black or African American"
                                            "American Indian or Alaska Native") #false))

(define (student/v4-temp s)
  (... (student/v4-age s) ...
       (lod-temp (student/v4-demographic s)) ...
       (student/v4-legacy? s) ...))

;; A Responses/v4 is a [List-of Student/v4]:
;; a list of student responses without their name

(define RESPONSES/V4-1 '())
(define RESPONSES/V4-2 (list STUDENT/V4-1))
(define RESPONSES/V4-3 (list STUDENT/V4-1 STUDENT/V4-2 STUDENT/V4-3))

(define (responses/v4-temp responses)
  (... (cond [(empty? responses) ...]
             [(cons? responses) ... (student/v3-temp (first responses))
                           ... (responses/v3-temp (rest responses)) ...])))

;; anonymize : Responses/v3 -> Responses/v4
;; converts a list of student responses with names to anonymized student responses without names
(define (anonymize responses)
  (map convert-to-anonymous responses))

;; Test Cases
(check-expect (anonymize RESPONSES/V3-1) RESPONSES/V4-1)
(check-expect (anonymize RESPONSES/V3-2) RESPONSES/V4-2)
(check-expect (anonymize RESPONSES/V3-3) RESPONSES/V4-3)

;; convert-to-anonymous : student/v3 -> student/v4
;; converts a student/v3 to a student/v4 by removing the name
(define (convert-to-anonymous student)
  (make-student/v4 (student/v3-age student)
                   (student/v3-demographics student)
                   (student/v3-legacy? student)))

;; Test Cases
(check-expect (convert-to-anonymous STUDENT/V3-1) STUDENT/V4-1)
(check-expect (convert-to-anonymous STUDENT/V3-2) STUDENT/V4-2)
(check-expect (convert-to-anonymous STUDENT/V3-3) STUDENT/V4-3)