;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname exam) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct worker [name title])
(define-struct manager [name title reports])
;; An Employee is one of:
;; - (make-worker String String)
;; - (make-manager String String Reports)
;; Interpretation: represents people working at a company, where everyone has a name and title, and managers also have a list of employess that report to them.
;; A Reports is a [List-of Employee]
;; Interpretation: a list of employess that report ot someone

(define E1 (make-worker "Jo" "Data Analyst"))
(define E2 (make-worker "El" "Support Technician"))
(define E3 (make-worker "Lee" "Support Technician"))
(define E4 (make-manager "May" "Team Lead" (list E1 E2)))
(define E5 (make-manager "Fred" "Dept Head" (list E4 E3)))
(define E6 (make-manager "Roo" "CEO" (list E5)))

;; a Worker is a (make-worker String String)
;; Interpretation: someone who works at a company
(define W1 (make-worker "Jo" "Data Analyst"))
(define W2 (make-worker "El" "Support Technician"))
(define W3 (make-worker "Lee" "Support Technician"))
(define (worker-temp w)
  (... (worker-name w) ... (worker-title w) ...))

(check-expect (all-workers E1) (list W1))
(check-expect (all-workers E4) (list (make-worker "May" "Team Lead") W1 W2))

;; all-workers : Employee -> [List-of Worker]
;; returns everyone who works at a company, nmanagers, and workers alike
(define (all-workers e)
  (local [
          (define (all-workers-reports rs)
            (foldr append '() (map all-workers rs)))]
    (cond [(worker? e) (list (make-worker (worker-name e) (worker-title e)))]
          [(manager? e) (cons (make-worker (manager-name e) (manager-title e))
                              (all-workers-reports (manager-reports e)))])))

;; DESIGN A FUNCTION EMPLOYEES-WITH-TITLE THAT TAKES AN EMPLOYEE AND A STRING AND RETURNS A LIST OF THE NAMES OF ALL PEOPLE
;; WITH THAT STRING AS THEIR TITLE. MUST USE LIST ABSTRACTIONS.

(check-expect (employees-with-title E1 "Data Analyst") (list "Jo"))
(check-expect (employees-with-title E4 "Team Lead") (list "May"))
(check-expect (employees-with-title E4 "Support Technician") (list "El"))
(check-expect (employees-with-title E6 "Support Technician") (list "El" "Lee"))
(check-expect (employees-with-title E6 "Intern") (list))
(check-expect (employees-with-title E6 "Team Lead") (list "May"))
(check-expect (employees-with-title E2 "Support Technician") (list "El"))

;; employees-with-title : Employee String -> [List-of String]
;; some purpose bruh
(define (employees-with-title e title)
  (map worker-name 
       (filter (lambda (w) (string=? (worker-title w) title))
               (all-workers e))))
