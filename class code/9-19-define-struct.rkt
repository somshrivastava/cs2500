;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname define-struct) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct student [firstname lastname nuid])

;; A Student is a (make-student String String Number)
;; Interpretation: A student at Northeastern
;; - firstname is their first name
;; - lastname is thier last name
;; nuid is their NUID

;; make-student : String String Number -> Student
;; student? : Any -> Boolean
;; student-firstname : Student -> String
;; student-lastname : Student -> String
;; student-nuid : Student -> Number

(define STUDENT-1 (make-student "Alice" "Doe" 1234))

(define (student-temp s)
  (... (student-firstname s) ...
       (student-lastname s) ...
       (student-nuid s) ...))

;; letter-header : Student -> String
;; writes a letter header
(check-expect (letter-header STUDENT-1) "Dear Alice")
(define (letter-header student)
  (string-append "Dear " (student-firstname student)))