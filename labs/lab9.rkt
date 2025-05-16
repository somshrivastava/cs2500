;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab9) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lab 9

;; For each problem below use pre-defined list abstraction(s) when appropriate.
;; As a reminder, they include (but are not limited to):
;;
;; - map
;; - filter
;; - andmap
;; - ormap
;; - foldr
;; - foldl
;;
;; It is encouraged that you use `lambda` in these problems when applicable.

;; Just because we now have cool abstractions doesn't mean you should forget
;; about the design recipe and following templates (which particularly come up
;; for helpers that you pass as input to the list abstractions)!

;; Lab Topic: ATM Transactions

(define-struct datetime [year month day hours minutes seconds])
;; A DateTime is a (make-datetime Number Number Number Number Number Number)
;; Represents the date & time (ignoring leap years) where:
;; - year is between 1990 and 2024
;; - month is between 1 and 12
;; - day is between 1 and up to 31 (specifically, 28 when month is 2,
;; 30 when month is 4, 6, 9, or 11, and 31 otherwise)
;; – hours is between 0 and 23
;; – minutes is between 0 and 59
;; – seconds is between 0 and 59

(define DATETIME-EX-1 (make-datetime 2024 10 10 0 0 0))
(define DATETIME-EX-2 (make-datetime 2024 10 15 23 59 59))
(define DATETIME-EX-3 (make-datetime 2024 10 17 12 30 45))
(define DATETIME-EX-4 (make-datetime 2023 12 3 12 30 47))

;; datetime-temp : DateTime -> ...
(define (datetime-temp dt)
  (... (datetime-year dt) ...
       (datetime-month dt) ...
       (datetime-day dt) ...
       (datetime-hours dt) ...
       (datetime-minutes dt) ...
       (datetime-seconds dt) ...))

;; A ATMLocation is one of:
;; - "Bank of America"
;; - "Citizens Bank"
;; - "Sunoco"
;; - "MGM Music Hall"
;; - "Huntington Wine & Spirits"
;; - "TD Garden"
;; - "Stop & Shop"
;; - "Fenway Park"
;; Interpretation: the location of an ATM

(define LOCATION-BOFA "Bank of America")
(define LOCATION-CITIZENS "Citizens Bank")
(define LOCATION-SUNOCO "Sunoco")
(define LOCATION-MGM "MGM Music Hall")
(define LOCATION-HUNT "Huntington Wine & Spirits")
(define LOCATION-TD "TD Garden")
(define LOCATION-STOP "Stop & Shop")
(define LOCATION-FENWAY "Fenway Park")

;; location-temp : ATMLocation -> ...
(define (location-temp l)
  (cond
    [(string=? l LOCATION-BOFA) ...]
    [(string=? l LOCATION-CITIZENS) ...]
    [(string=? l LOCATION-SUNOCO) ...]
    [(string=? l LOCATION-MGM) ...]
    [(string=? l LOCATION-HUNT) ...]
    [(string=? l LOCATION-TD) ...]
    [(string=? l LOCATION-STOP) ...]
    [(string=? l LOCATION-FENWAY) ...]))

(define-struct transaction [datetime atm-id account-number amount location is-withdraw])
;; A Transaction is a (make-transaction DateTime Number Number Number ATMLocation Boolean)
;; Interpretation: a transaction with a date & time, ATM ID, account number, dollar amount,
;; location, and a boolean indicating if it was a withdrawal.

(define TRANSACTION-EX-1 (make-transaction DATETIME-EX-1 5678 4789 100 LOCATION-CITIZENS #t))
(define TRANSACTION-EX-2 (make-transaction DATETIME-EX-2 5678 86754 75 LOCATION-MGM #f))
(define TRANSACTION-EX-3 (make-transaction DATETIME-EX-3 5678 1234 2500 LOCATION-HUNT #t))
(define TRANSACTION-EX-4 (make-transaction DATETIME-EX-4 5678 1234 2000 LOCATION-TD #t))
(define TRANSACTION-EX-5 (make-transaction DATETIME-EX-4 5678 1234 1000 LOCATION-FENWAY #t))

;; transaction-temp : Transaction -> ...
(define (transaction-temp t)
  (... (datetime-temp (transaction-datetime t)) ...
       (transaction-atm-id t) ...
       (transaction-account-number t) ...
       (transaction-amount t) ...
       (location-temp (transaction-location t)) ...
       (transaction-is-withdraw t) ...))

;; An ATMHistory is a [Listof Transaction]
;; Interpretation: a list of transactions that have occurred at an ATM. All ATM ID numbers should
;; be the same within a list of one ATM's transactions.

(define ATM-HISTORY-EX-1 '())
(define ATM-HISTORY-EX-2 (list TRANSACTION-EX-1 TRANSACTION-EX-2))
(define ATM-HISTORY-EX-3 (list TRANSACTION-EX-1 TRANSACTION-EX-2 TRANSACTION-EX-3 TRANSACTION-EX-4))

;;! Problem 1

;;! Part A

;; Design a function `large-withdrawals`, that takes an ATMHistory, and returns only the
;; withdrawals over $1000. Consider how list abstractions can be used to simplify this problem.

;; Estimated Portion of Lab: 20%

;; large-withdrawals : ATMHistory -> ATMHistory
;; takes an ATMHistory and returns only the withdrawals over $1000
(define (large-withdrawals ath)
  (filter (lambda (t) (and (transaction-is-withdraw t) (> (transaction-amount t) 1000))) ath))

;; Test Cases
(check-expect (large-withdrawals ATM-HISTORY-EX-1) '())
(check-expect (large-withdrawals ATM-HISTORY-EX-2) '())
(check-expect (large-withdrawals ATM-HISTORY-EX-3) (list TRANSACTION-EX-3 TRANSACTION-EX-4))

;; STOP AND SWITCH WHO IS TYPING. CODE WALKS WILL BEGIN NOW!

;;! Part B

;; Design a function `sum-transactions` that takes an ATM's transaction history and
;; adds all the transaction amounts up.

;; Estimated Portion of Lab: 20%

;; sum-transactions : ATMHistory -> Number
;; takes an ATM's transaction history and returns the sum of all trasnactions
(define (sum-transactions l)
  (foldr + 0 (map transaction-amount l)))

;; Test Cases
(check-expect (sum-transactions ATM-HISTORY-EX-1) 0)
(check-expect (sum-transactions ATM-HISTORY-EX-2) 175)
(check-expect (sum-transactions ATM-HISTORY-EX-3) 4675)

;;! Part C

;; Design a function called `transactions-within-time` that takes an ATM's transaction
;; history, a start datetime, and an end datetime, and returns a list of transactions
;; that occurred between the two, including those made at the start and end datetimes.
;; Assume that the second datetime is greater than the first, and that the transactions
;; only go back to 1990. You may use the provided function `seconds-since-1990`.

;; Estimated Portion of Lab: 20%

(define SECONDS-PER-YEAR 31536000)
(define SECONDS-PER-MONTH 2592000)
(define SECONDS-PER-DAY 86400)
(define SECONDS-PER-HOUR 3600)
(define SECONDS-PER-MINUTE 60)

;; seconds-since-1990 : DateTime -> Number
;; converts a datetime to the number of seconds since 1/1/1990 at 12:00am
(define (seconds-since-1990 dt)
  (+ (* (- (datetime-year dt) 1990) SECONDS-PER-YEAR)
     (* (datetime-month dt) SECONDS-PER-MONTH)
     (* (datetime-day dt) SECONDS-PER-DAY)
     (* (datetime-hours dt) SECONDS-PER-HOUR)
     (* (datetime-minutes dt) SECONDS-PER-MINUTE)
     (datetime-seconds dt)))

;; transactions-within-time : ATMHistory Datetime Datetime -> ATMHistory
;; Returns a list of transactions within the start and end datetime
(define (transactions-within-time ath start end)
  (filter (lambda (x) (and (<= (seconds-since-1990 start)
                               (seconds-since-1990
                                (transaction-datetime x)))
                           (>= (seconds-since-1990 end)
                               (seconds-since-1990 (transaction-datetime x))))) ath))

;; Test Cases
(check-expect (transactions-within-time ATM-HISTORY-EX-1 DATETIME-EX-1 DATETIME-EX-2) '())
(check-expect (transactions-within-time ATM-HISTORY-EX-2 DATETIME-EX-1 DATETIME-EX-2)
              ATM-HISTORY-EX-2)
(check-expect (transactions-within-time ATM-HISTORY-EX-3 DATETIME-EX-4 DATETIME-EX-2)
              (list TRANSACTION-EX-1 TRANSACTION-EX-2 TRANSACTION-EX-4))

;;! Part D

;; INTERPRETIVE QUESTIONS
;; -- Temporary Assistance for Needy Families (TANF) provides temporary financial assistance to
;; low or no-income families to help pay for food, shelter, utilities, and other expenses. Benefits
;; are loaded onto Electronic Benefits Transfer (EBT) cards. In 2014, Maine's governor Paul LePage
;; released a list of 3,650 transactions in which TANF recipients withdrew cash from ATMs in smoke
;; shops, liquor stores, or out of state locations. In an effort to convince people that TANF
;; recipients are defrauding taxpayers, LePage fallaciously used these data to argue that TANF
;; recipients were spending their benefits on alcohol, tobacco, etc.

;; Why is LePage's argument inaccurate? Write 2-3 sentences explaining why a fraud-detecting
;; algorithm can't reach this conclusion.

;; Estimated Portion of Lab: 10%

#|

LePage's argument is inaccurate because the presence of ATMs in certain locations does not indicate
how the withdrawn cash is spent. The data only shows where the withdrawal took place, not what the
money was used for, so assuming misuse of funds from these transactions is misleading.

|#

;; -- Errors in fraud detection can prevent people from accessing their money, cause stigma, and
;; otherwise burden already vulnerable populations. No matter how well-designed an algorithm is,
;; it's likely that errors will still occur. What sort of recourse should be offered to people who were
;; falsely flagged? In 2-3 sentences, make 1 technical proposal and 1 non-technical proposal to
;; address the potential harms caused by incorrect fraud classifications.

;; Estimated Portion of Lab: 10%

#|

A technical proposal could involve implementing a transparent review system where flagged
transactions go through further analysis by human reviewers. A non-technical proposal might include
providing recipients with an appeal process that allows them to contest any flagged
transactions without penalty, reducing stigma.

|#


;; Part E

;; We want to write a program that searches an ATM's transaction history for fraudulent activity.
;; Design a function that take in an ATM's transaction history and an account number to
;; investigate. This function should return true if any of the transactions are deemed
;; "fraudulent." We leave it to you to define what constitutes a fraudulent transaction, but be
;; sure to clearly document your choices, given the thinking you've done in Part D.

;; Estimated Portion of Lab: 20%

;; fraudulent : ATMHistory Number -> Boolean
;; Returns true if any of the transactions are deemed fradulent
(define (fraudulent? atmh acctnum)
  (ormap (lambda (a) (or (> (transaction-amount a) 2000)
                         (not (= (transaction-account-number a) acctnum)))) atmh))

;; Test Cases
(check-expect (fraudulent? ATM-HISTORY-EX-1 1234) #f)
(check-expect (fraudulent? ATM-HISTORY-EX-2 4789) #t)
(check-expect (fraudulent? ATM-HISTORY-EX-3 1234) #t)
