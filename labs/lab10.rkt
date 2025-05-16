;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lab 10

;;! Part A

;; Design the function pay-rates which, given a list of names as strings
;; and a list of hourly rates as numbers, produces a list of strings
;; in order which describes how much money each person makes.

;; The list of names will be the same size or longer than the list of
;; rates; people with no corresponding wage are paid $15/hour (minimum
;; wage in Boston in 2024). Two tests are provided for clarity.

(check-expect
 (pay-rates
  (list "Archer" "Bella" "Charlie")
  (list 30 20 19))
 (list "Archer is paid $30/hour"
       "Bella is paid $20/hour"
       "Charlie is paid $19/hour"))

(check-expect
 (pay-rates (list "Alex" "Robin") (list 25))
 (list "Alex is paid $25/hour" "Robin is paid $15/hour"))

;; pay-rates : [List-of String] [List-of Number] -> [List-of String]
;; given a list of names as strings and a list of hourly rates as numbers, produces a list of strings
;; in order which describes how much money each person makes
(define (pay-rates los lon)
  (cond [(and (empty? los) (empty? lon)) '()]
        [(and (cons? los) (empty? lon))
         (cons (string-append (first los)
                              " is paid $15/hour")
               (pay-rates (rest los) '()))]
        [(and (cons? los) (cons? lon))
         (cons (string-append (first los)
                              " is paid $"
                              (number->string (first lon))
                              "/hour")
               (pay-rates (rest los) (rest lon)))]))

;; STOP AND SWITCH WHO IS TYPING. CODE WALKS WILL BEGIN NOW!

;;! Part B

;; INTERPRETIVE QUESTION
;;
;; "Wage disparity", "pay disparity", or a "pay gap" all refer to a phenomenon
;; where people are paid different amounts for doing the same work. It is a
;; serious issue in many industries, and explicitly prohibited by executive
;; order for US Government contractors
;; https://www.dol.gov/agencies/ofccp/about/executive-order-11246-history. In
;; particular, this can show up systemically as women being paid less than men
;; for similar work (a gender pay gap), people of color being paid less than
;; white people (a racial pay gap), etc.
;;
;; Critically, not all wage *differences* are wage *disparities*. Those words,
;; examples of moral language, convey important meaning. Different wages may
;; be justified -- for example, if someone is doing a job that requires more training, expertise, or
;; experience, or they have a higher level of responsibility. With only the
;; information in Part A, it would be impossible to assess if there are any
;; wage *disparities*, since all we know are names and wages.
;;
;; If you were to be tasked with reviewing wages for *disparities*, what
;; additional information would you like to have for each person? Please include
;; at least two additional fields, and justify why they would be useful. Keep
;; your response to 2-3 sentences. Longer responses may not receive full credit.

#|

Two additional fields to include would be years of experience and the position in which they work.
These would be useful because those are two indicators of why somebody may be paid more or less
compared to someone else.

|#

;;! Part C

;; Design the function `map-func` which, given a function and a list,
;; returns a new list containing the results of applying the function to each element of the list.
;; The implementation must exclusively use the `foldr` list abstraction.
;; Some tests have been supplied for clarity.

(check-expect (map-func add1 '()) '())
(check-expect (map-func add1 (list 1 2 3)) (list 2 3 4))
(check-expect (map-func string-upcase (list "a" "b" "c")) (list "A" "B" "C"))

;; map-func : (X Y) [X -> Y] [List-of Any] -> [List-of Any]
;; given a function and a list, returns a new list containing the results of applying the function to
;; each element of the list
(define (map-func f ls)
  (foldr (lambda (x y) (cons (f x) y)) '() ls))

;;! Part D
;;
;; Design the function `prefix`, which takes a list `lp` of prefixes (each
;; prefix is a string) and a list `l` of strings.
;; It produces a list of lists of strings. Each list of strings in
;; the output is obtained by using one of the given prefixes and inserting
;; them as the first element into `l`. We've provided one test case for clarity:

(check-expect (prefix (list "p1" "p2") (list "A" "B" "C"))
              (list (list "p1" "A" "B" "C")
                    (list "p2" "A" "B" "C")))
;; prefix : [List-of String] [List-of String] -> [List-of String]
;; returns a list where each element in the first list is now a list with the second list attached to
;; it
(define (prefix lp l)
  (map (lambda (x) (cons x l)) lp))

;;! Part E
;;
;; Design a function `intersect`, which, given two lists and an equality predicate, produces
;; a new list that computes the intersection of two lists.
;; The intersection should consist of all elements from the first list that appear in the second,
;; as determined by the equality predicate.
;; The equality predicate defines what it means for an element from the first list to be "the same"
;; as an element from the second list. This is the notion of sameness your intersection function
;; should use to determine whether an element in one list appears in the other.

(check-expect (intersect (list 1 2 3 4) (list 3 4 5 6) =) (list 3 4))
(check-expect (intersect (list "apple" "banana" "apple") (list "apple" "cherry") string=?)
              (list "apple" "apple"))
(check-expect (intersect (list "APPLE" "banana" "cherry" "OrAnGe") (list "apple" "banana" "orange")
                         (lambda (s1 s2) (string=? (string-downcase s1) (string-downcase s2))))
              (list "APPLE" "banana" "OrAnGe"))

;; intersect : [List-of Any] [List-of Any] (X -> Boolean) -> [List-of Any]
;; given two lists and an equality predicate, produces a new list that computes the intersection of
;; two lists
(define (intersect l1 l2 equality-pred)
  (filter (lambda (x) (ormap (lambda (y) (equality-pred x y)) l2)) l1))