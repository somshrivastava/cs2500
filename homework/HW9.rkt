;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname HW9) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Usual Instructions:
;; 1. Do not create, modify or delete any line that begins with ";;!". These are
;;    markers that we use to segment your file into parts to facilitate grading.
;; 2. You must follow the _design recipe_ for every problem. In particular,
;;    every function you define must have at least three check-expects (and
;;    more if needed).
;; 3. You must follow the Style Guide:
;;    https://pages.github.khoury.northeastern.edu/2500/2024F/style.html
;; 4. You must submit working code. In DrRacket, ensure you get no errors
;;    when you click Run. After you submit on Gradescope, you'll get instant
;;    feedback on whether or not Gradescope can run your code, and your code must
;;    run on Gradescope to receive credit from the autograder.
;; 5. On some problems, you can get automated feedback on your in-progress work
;;    from FeedBot, a system developed by the course staff. When you submit your
;;    assignment, you will see a link to the FeedBot report along with the autograder
;;    feedback. Only a certain number of submissions will get this, and submissions
;;    close together will not receive the feedback.

;;! HW9

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New Instructions                                                           ;;
;; 1. You must use list abstractions to receive credit. Do not write directly ;;
;;    recursive functions.                                                    ;;
;; 2. You may use `lambda` if you wish.                                       ;;
;; 3. Many problems have provided signatures and purpose statements that you  ;;
;;    should not modify.                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This homework refers to the following data definitions.

;;! A Category is one of:
;;! - "Personal"
;;! - "Work"
;;! - "Academic"
;;! Interpretation: a category of tasks in a task list.
(define PERSONAL "Personal")
(define WORK "Work")
(define ACADEMIC "Academic")

(define (category-template cat)
  (cond
    [(string=? cat PERSONAL) ...]
    [(string=? cat WORK) ...]
    [(string=? cat ACADEMIC) ...]))

(define-struct task [category description priority])
;; A Task is (make-task Category String Number)
;; Interpretation: A task in a task list, with its category, description, and
;; priority. Lower numbers are more urgent.
(define EX-ASSIGNMENT (make-task ACADEMIC "Finish HW9" 0))
(define EX-LIBRARY (make-task WORK "Finishing shelving books in Snell" 10))
(define EX-PERSONAL (make-task PERSONAL "Do laundry this time" 20))

(define (task-template t)
  (... (category-template (task-category t)) ... (task-description t) ... (task-priority t) ...))

;;! Problem 1

;; Design a function called priority-zero that consumes a list of tasks and
;; only produces those with priority 0.

;;! priority-zero : [List-of Task] -> [List-of Task]
;;! Produces a list of tasks with priority 0.

;;!! IMPORTANT: Write your response BELOW this line:

;; priority-zero : [List-of Task] -> [List-of Task]
;; produces a list of tasks with priority 0
(define (priority-zero lot)
  (filter (lambda (task) (= (task-priority task) 0)) lot))

;; Test Cases
(check-expect (priority-zero '()) '())
(check-expect (priority-zero (list EX-ASSIGNMENT EX-LIBRARY EX-PERSONAL)) (list EX-ASSIGNMENT))
(check-expect (priority-zero (list EX-LIBRARY EX-PERSONAL)) '())

;;! Problem 2

;; Design a function called priority<= that consumes a priority number and
;; a list of tasks and produces only those with priority less than or equal
;; to the given number.

;;! priority<= : Number [List-of Task] -> [List-of Task]
;;! Produces a list of tasks with priority less than or equal to the given
;;! number.

;;!! IMPORTANT: Write your response BELOW this line:

;; priority<= : Number [List-of Task] -> [List-of Task]
;; produces a list of tasks with priority less than or equal to the given number
(define (priority<= num lot)
  (filter (lambda (task) (<= (task-priority task) num)) lot))

;; Test Cases
(check-expect (priority<= 0 '()) '())
(check-expect (priority<= 10 (list EX-ASSIGNMENT EX-LIBRARY EX-PERSONAL)) (list EX-ASSIGNMENT EX-LIBRARY))
(check-expect (priority<= 5 (list EX-LIBRARY EX-PERSONAL)) '())

;;! Problem 3

;;! Part A
;; Design a function called prioritize that consumes a category and a list of
;; tasks, and sets the priority of all tasks in the given category to 0. The
;; produced list should contain all tasks in the original list.

;;! prioritize : Category [List-of Task] -> [List-of Task]
;;! Produces a list of tasks where the priority of every task that matches the category given, is set to 0

;;!! IMPORTANT: Write your response BELOW this line:

;; prioritize : Category [List-of Task] -> [List-of Task]
;; produces a list of tasks where the priority of every task that matches the category given,
;; is set to 0
(define (prioritize category lot)
  (map (lambda (task) (if (string=? (task-category task) category)
                          (make-task (task-category task) (task-description task) 0)
                          task)) lot))

;; Test Cases
(check-expect (prioritize PERSONAL (list EX-ASSIGNMENT EX-LIBRARY EX-PERSONAL))
              (list EX-ASSIGNMENT EX-LIBRARY (make-task PERSONAL "Do laundry this time" 0)))
(check-expect (prioritize WORK (list EX-ASSIGNMENT EX-LIBRARY EX-PERSONAL))
              (list EX-ASSIGNMENT (make-task WORK "Finishing shelving books in Snell" 0) EX-PERSONAL))
(check-expect (prioritize "Other" (list EX-ASSIGNMENT EX-LIBRARY EX-PERSONAL))
              (list EX-ASSIGNMENT EX-LIBRARY EX-PERSONAL))

;;! Part B
;;
;; INTERPRETIVE QUESTION
;;
;; The data definition provided had a numeric score for Priority. It is quite common
;; for software systems to use numeric scores for things like this, though it comes at
;; a risk of losing information that a more nuanced representation could capture.
;;
;; One approach is, rather than storing a single numeric score, to store scores
;; for various attributes, and then combine them via a formula. For example,
;; when thinking of tasks, you might have separate scores for:
;;
;; - Duration: a low score task is short, a high score task is long
;; - Urgency: a low score task needs to be done soon, a high score task can be done later
;; - Enjoyment: a low score task is very enjoyable, a high score task is unpleasant.
;;
;; This will allow users to more accurately describe the attributes that might
;; contribute to the priority, and might enable different filtering (i.e., I
;; could ask for only short tasks, or only tasks that would be very enjoyable).
;; But, often this separation also requires us to determine what weighting we
;; give, by default, to the separate aspects.
;;
;; Your FIRST task is to assign "weights" to each of the three aspects above: they
;; should be percentages that sum to 100 (or fractions that sum to 1). Explain
;; how you determined the weights in a few sentances.
;;
;; Your SECOND task is to propose another scoring criterion that could
;; contribute to priority. You do not need to alter your weights, but you do
;; need to justify why having a score for your proposed criteria could be useful
;; when determining task priority.


;;!! IMPORTANT: Write your response BELOW this line:

#|

TASK 1

Duration: 30%
Urgency: 50%
Enjoyment: 20%

The primary factor that determines the priority of a class is urgency. That's why it has the most 
weightage. Second is the duration of the assignment and then the enjoyment of the assignment.
Enjoyment is not as important as urgency and duration when considering priority although it should be
taken into account.

TASK 2

Adding an "Impact" criteria would be valuable. This score would be aboue how much completing the task
will impact your overall progress. A task with high impact would be more prioritized than one with
lower impact.

|#

;;! Problem 4

;; Design a predicate called any-work? that determines if any task in a list
;; is a Work task.

;;! any-work? : [List-of Task] -> Boolean
;;! Determines if any task in the given list is a work task.

;;!! IMPORTANT: Write your response BELOW this line:

;; any-work? : [List-of Task] -> Boolean
;; determines if any task in the given list is a work task
(define (any-work? lot)
  (ormap (lambda (task) (string=? (task-category task) WORK)) lot))

;; Test Cases
(check-expect (any-work? '()) #false)
(check-expect (any-work? (list EX-ASSIGNMENT EX-PERSONAL)) #false)
(check-expect (any-work? (list EX-ASSIGNMENT EX-LIBRARY EX-PERSONAL)) #true)

;;! Problem 5

;; Design a function called search-work that consumes a string and a list of tasks
;; and produces the descriptions of the Work tasks that contain the given string.

;;! search-work : String [List-of Task] -> [List-of String]
;;! Produces the list of descriptions of Work tasks that contain the given string.

;;!! IMPORTANT: Write your response BELOW this line:

;; search-work : String [List-of Task] -> [List-of String]
;; Produces the list of descriptions of Work tasks that contain the given string.
(define (search-work search lot)
  (map (lambda (task) (task-description task))
       (filter (lambda (task) (and (string=? (task-category task) WORK)
                                   (string-contains? search (task-description task)))) lot)))

;; Test Cases
(check-expect (search-work "shelving" (list EX-ASSIGNMENT EX-LIBRARY EX-PERSONAL))
              (list "Finishing shelving books in Snell"))
(check-expect (search-work "Finishing" (list EX-ASSIGNMENT EX-LIBRARY EX-PERSONAL))
              (list "Finishing shelving books in Snell"))
(check-expect (search-work "project" (list EX-ASSIGNMENT EX-LIBRARY EX-PERSONAL))
              '())

;;! Problem 6

;; Design a function called average-priority that consumes a list of tasks and
;; produces the average priority of all tasks in the given list.

;;! average-priority : [List-of Task] -> Number
;;! Produces the average priority of all tasks in the given list.

;;!! IMPORTANT: Write your response BELOW this line:

;; average-priority : [List-of Task] -> Number
;; produces the average priority of all tasks in the given list
(define (average-priority lot)
  (if (empty? lot)
      0
      (/ (foldr (lambda (task acc) (+ (task-priority task) acc)) 0 lot)
         (length lot))))

;; Test Cases
(check-expect (average-priority '()) 0)  
(check-expect (average-priority (list EX-ASSIGNMENT EX-LIBRARY EX-PERSONAL)) 10) 
(check-expect (average-priority (list EX-ASSIGNMENT EX-ASSIGNMENT)) 0) 

;;! Problem 7

;; Design a function called group-tasks-by-category that consumes a list of
;; tasks and groups them into sublists based on their category. The produced
;; list should contain three sublists corresponding to the categories "Personal",
;; "Work", and "Academic". Each sublist should contain all tasks that belong
;; to its respective category. If there are no tasks in a particular category,
;; the corresponding sublist should be empty. The order of the
;; list of categories is provided below.

;;! group-tasks-by-category : [List-of Task] -> [List-of [List-of Task]]
;;! Groups tasks into sublists based on their category.

(define CATEGORIES (list PERSONAL WORK ACADEMIC))

;;!! IMPORTANT: Write your response BELOW this line:

;; group-tasks-by-category : [List-of Task] -> [List-of [List-of Task]]
;; groups tasks into sublists based on their category
(define (group-tasks-by-category lot)
  (map (lambda (category)
         (filter (lambda (task) (string=? (task-category task) category)) lot)) CATEGORIES))

;; Test Cases
(check-expect (group-tasks-by-category '())
              (list '() '() '()))
(check-expect (group-tasks-by-category (list EX-ASSIGNMENT EX-LIBRARY EX-PERSONAL))
              (list (list EX-PERSONAL)        
                    (list EX-LIBRARY)         
                    (list EX-ASSIGNMENT)))    
(check-expect (group-tasks-by-category (list EX-LIBRARY EX-PERSONAL))
              (list (list EX-PERSONAL)        
                    (list EX-LIBRARY)
                    '()))