;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lab8) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;;! Problem 1
;;
;; Design a data definition CallInfo that is either a phone number (represented as a
;; number), or the string "do not call"; this data represents information
;; collected by a marketing company.
;;
;; Estimated Portion of Lab: 20%

;; A CallInfo is either a
;; - Number
;; - "do not call"
;; Interpretation: CallInfo represents information collected by a marketing company

(define CALLINFO-1 "do not call")
(define CALLINFO-2 0000000000)
(define CALLINFO-3 1111111111)

;; callinfo: CallInfo -> ?
(define (callinfo-temp ci)
  (cond
    [(number?) ...]
    [(and (string?) (string=? "do not call")) ...]))

;;! Problem 2
;;
;; Consider the following data definition:

(define-struct person (name next))
;; A Names is one of:
;; - "end"
;; - (make-person String Names)
;; Interpretation: a sequence of names
(define N0 "end")
(define N1 (make-person "Alan" N0))
(define N2 (make-person "Zoe" N1))
(define N3 (make-person "Bill" N2))

(define (names-temp n)
  (cond [(string? n) ...]
        [(person? n) (... (person-name n)
                      ... (names-temp (person-next n)) ...)]))

;; Consider the following two functions:

;; count-anames : Names -> Number
;; count how many names start with "A"
(check-expect (count-anames N0) 0)
(check-expect (count-anames N1) 1)
(check-expect (count-anames N3) 1)
(define (count-anames n)
  (local (; a? : String -> Boolean
          ; does the string start with "A"
          (define (a? s)
            (and (> (string-length s) 0)
                 (string=? (substring s 0 1) "A"))))
  (cond [(string? n) 0]
        [(person? n) (if (a? (person-name n))
                         (+ 1 (count-anames (person-next n)))
                         (count-anames (person-next n)))])))

;; count-znames : Names -> Number
;; count how many names start with "Z"
(check-expect (count-znames N0) 0)
(check-expect (count-znames N1) 0)
(check-expect (count-znames N3) 1)
(define (count-znames n)
  (local (; z? : String -> Boolean
          ; does the string start with "Z"
          (define (z? s)
            (and (> (string-length s) 0)
                 (string=? (substring s 0 1) "Z"))))
  (cond [(string? n) 0]
        [(person? n) (if (z? (person-name n))
                         (+ 1 (count-znames (person-next n)))
                         (count-znames (person-next n)))])))

;; Design an abstraction called `count-names` (you can skip purpose statement & tests) that uses the
;; names template, then create new versions of the above functions (with the same signature)
;; that use the abstraction, named `count-anames/v2` and `count-znames/v2`.

;; Estimated Portion of Lab: 35%

(define (count-names n l)
  (local (;; l? : String -> Boolean
          ;; does the string start with given letter?
          (define (l? s)
            (and (> (string-length s) 0)
                 (string=? (substring s 0 (string-length l)) l))))
  (cond [(string? n) 0]
        [(person? n) (if (l? (person-name n))
                         (+ 1 (count-names (person-next n) l))
                         (count-names (person-next n) l))])))

;; count-anames/v2 : Names -> Number
;; returns the number of names that start with "A"
(define (count-anames/v2 names)
  (count-names names "A"))

;; Test Cases
(check-expect (count-anames/v2 N1) 1)
(check-expect (count-anames/v2 N2) 1)
(check-expect (count-anames/v2 N3) 1)

;; count-znames/v2 : Names -> Number
;; returns the number of names that start with "Z"
(define (count-znames/v2 names)
  (count-names names "Z"))

;; Test Cases
(check-expect (count-znames/v2 N1) 0)
(check-expect (count-znames/v2 N2) 1)
(check-expect (count-znames/v2 N3) 1)

;;! Problem 3
;;
;; Design a function `total-distance`, that takes a [ListOf Posn] as input and
;; returns the sum of all individual distances to the origin (i.e., (make-posn 0
;; 0)) of each of the Posns.
;;
;; Note that the distance between two points (x1,y1)
;; and (x2,y2) is sqrt((x2-x1)^2 + (y2-y1)^2).
;;
;; If the input list is empty, this should be 0. You may use a list
;; abstraction, but do not have to. We have provided signature, purpose, and
;; tests:
;;
;; Estimated Portion of Lab: 25%

;; total-distance : [ListOf Posn] -> Number
;; returns sum of all distances to the origin
(define (total-distance l)
  (cond
    [(empty? l) 0]
    [(cons? l)
     (+ (sqrt
         (+ (sqr (posn-x (first l)))
            (sqr (posn-y (first l)))))
        (total-distance (rest l)))]))

;; Test Cases
(check-expect (total-distance '()) 0)
(check-expect (total-distance (list (make-posn 3 4))) 5)
(check-expect (total-distance (list (make-posn 3 4) (make-posn 5 12))) 18)

;;! Problem 4
;;
;; Design a function, `non-zeros`, that takes a [ListOf Number] and returns only
;; those numbers that are non-zero. Note: you must use a list abstraction!
;; Hint: the `zero?` function might be helpful.
;;
;; Estimated Portion of Lab: 20%

;; notzero? : Number -> Boolean
;; checks whether the number is not zero
(define (notzero? n) (not (zero? n)))

;; Test Cases
(check-expect (notzero? 0) #f)
(check-expect (notzero? 2) #t)
(check-expect (notzero? 3) #t)

;; non-zeros : [List-of Number] -> [List-of Number]
;; returns all the numbers that are non-zero
(define (non-zeros l)
  (filter notzero? l))

;; Test Cases
(check-expect (non-zeros '()) '())
(check-expect (non-zeros (list 0 1 2 3 4)) (list 1 2 3 4))
(check-expect (non-zeros (list 0 1 2 0 0 3 5)) (list 1 2 3 5))