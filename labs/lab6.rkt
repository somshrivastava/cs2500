;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; Customer Research

;;! Problem 1

;; We've been asked to assist with filtering a large set of market research data to identify
;; potential customers for a clothing brand. We've been given records in the following format:

(define-struct customer [age zip-code yearly-income next-customer])
;;! A Spreadsheet is one of:
;;! - "end of spreadsheet"
;;! - (make-customer Number Number Number Spreadsheet)
;;! Interpretation: a spreadsheet of results from a market research survey

(define (spreadsheet-template s)
  (cond
    [(and (string? s) (string=? s "end of spreadsheet")) ...]
    [else
     (... (customer-age s) ...
          (customer-zip-code s) ...
          (customer-yearly-income s) ...
          (spreadsheet-template (customer-next-customer s)) ...)]))

(define SS-EX-1 "end of spreadsheet")
(define SS-EX-2 (make-customer 25 02101 500000 SS-EX-1))
(define SS-EX-3 (make-customer 30 02110 75000 SS-EX-2))

;;! Part A

;; Write two more examples below.

;; Estimated Portion of Lab: 20%

(define SS-EX-4 (make-customer 35 02115 50000 SS-EX-3))
(define SS-EX-5 (make-customer 15 02214 75000 SS-EX-4))

;;! Part B

;; Design a function young-customers that takes a Spreadsheet and returns a Spreadsheet
;; containing only the customers who are under 30 years old.

;; Estimated Portion of Lab: 30%

;; young-customers : Spreadsheet -> Spreadsheet
;; Takes a spreadsheet and returns a spreadsheeting containing only the customers who are under
;; 30 years old

(define (young-customers spreadsheet)
  (cond
    [(and (string? spreadsheet) (string=? spreadsheet "end of spreadsheet")) "end of spreadsheet"]
    [else
     (cond [(< (customer-age spreadsheet) 30) (make-customer (customer-age spreadsheet)
                                                   (customer-zip-code spreadsheet)
                                                   (customer-yearly-income spreadsheet)
                                                   (young-customers (customer-next-customer
                                                                     spreadsheet)))]
           [else (young-customers (customer-next-customer spreadsheet))])]))

;; Test Cases
(check-expect (young-customers SS-EX-1) SS-EX-1)
(check-expect (young-customers SS-EX-2) SS-EX-2)
(check-expect (young-customers SS-EX-3) SS-EX-2)
(check-expect (young-customers SS-EX-4) SS-EX-2)
(check-expect (young-customers SS-EX-5) (make-customer 15 02214 75000 SS-EX-2))

;; STOP, SUBMIT YOUR WORK SO FAR TO GRADESCOPE, AND SWITCH WHO IS TYPING.

;;! Part C

;; INTERPRETIVE QUESTIONS
;; Imagine a political action group wants to share advertisements on YouTube with voters of
;; a certain political party. Although YouTube users don't explicitly share their political
;; affiliation, YouTube could feasibly analyze a user's watch history to determine their
;; political affiliation and target ads accordingly.

;; This is the idea of data inference: using one or many pieces of information to infer some
;; other piece of information. Data inference tools are being marketed for hiring employees,
;; detecting shoppers' moods, and predicting criminal behavior
;; (https://www.nytimes.com/2019/04/21/opinion/computational-inference.html).

;; Carefully consider our customer data. What might we be able to infer about our users with this data?
;; In 2-3 sentences, explain how that assumption could be problematic.
;; Estimated Portion of Lab: 20%

#|

Using this data, we may be inferring the average income in specific areas using the zipcode of the
customer. This data could be skewed left or right due to an extremely high or low income customer,
causing us to misinterpret the average income in an area. This generalization that could be made by
us about the average income of the zip code is problematic because it leads us to NOT FAIRLY
represent the true average income of the city.

|#

;;! Part D

;; Write a function, `total-fen/ken-income`, that takes in a Spreadsheet and produces
;; the total sum of incomes of customers who live in the Fenway and Kenmore neighborhoods
;; (zip codes 02115 and 02214).

;; Estimated Portion of Lab: 30%

; total-fen/ken-income: Spreadsheet -> Number
; Returns the total sum of incomes of customers who live in Fenway and Kenmore neighborhoods

(define (total-fen/ken-income spreadsheet)
  (cond
    [(and (string? spreadsheet) (string=? spreadsheet "end of spreadsheet")) 0]
    [(or (= (customer-zip-code spreadsheet) 02115) (= (customer-zip-code spreadsheet) 02214))
            (+ (customer-yearly-income spreadsheet) (total-fen/ken-income (customer-next-customer spreadsheet)))]
    [else (total-fen/ken-income (customer-next-customer spreadsheet))]))

;; Test Cases

(check-expect (total-fen/ken-income SS-EX-1) 0)
(check-expect (total-fen/ken-income SS-EX-2) 0)
(check-expect (total-fen/ken-income SS-EX-3) 0)
(check-expect (total-fen/ken-income SS-EX-4) 50000)
(check-expect (total-fen/ken-income SS-EX-5) 125000)