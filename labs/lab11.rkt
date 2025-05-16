;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
2
;;! Lab 11

;; Consider the following data definitions:

;; A PhoneId is a String
;; Interpretation: a unique identifier for a person's phone
;; Examples:
(define PHONE1 "p1")
(define PHONE2 "p2")
;; Template:
(define (phoneid-temp pi) (... pi ...))

;; A TagId is a String
;; Interpretation: a unique identifier for a tracking tag
;; Examples:
(define TAG1 "t1")
(define TAG2 "t2")
(define TAG3 "t3")
;; Template:
(define (tagid-temp ti) (... ti ...))


(define-struct owner [phoneid tagid])
;; An Owner is a (make-owner PhoneId TagId)
;; Interpretation: a record of a particular phone _owning_ a tracking tag.
;; There should only ever be one phone per tag, though can be many tags per phone.
;; Examples:
(define OWNER1 (make-owner PHONE1 TAG1))
(define OWNER2 (make-owner PHONE1 TAG2))
(define OWNER3 (make-owner PHONE2 TAG3))
;; Template:
(define (owner-temp o) (... (phoneid-temp (owner-phoneid o)) ... (tagid-temp (owner-tagid o)) ...))


;; A Time is a Natural
;; Interpretation: a time represented by the number of seconds since Jan 1, 2024.
;; Examples:
(define TIME1 10)
(define TIME2 20)
(define TIME3 30)
;; Template:
(define (time-temp t) (... t ...))

(define-struct encounter [phoneid tagid time posn])
;; An Encounter is a (make-encounter PhoneId TagId Time Posn)
;; Interpretation: a record of a tag being seen by a given phone at a given time and location.
;; Note that location is represented as X,Y coordinates for simplicity.
;; Examples:
(define ENCOUNTER1 (make-encounter PHONE1 TAG1 TIME1 (make-posn 0 0)))
(define ENCOUNTER2 (make-encounter PHONE2 TAG1 TIME2 (make-posn 100 100)))
(define ENCOUNTER3 (make-encounter PHONE1 TAG2 TIME3 (make-posn 0 0)))
;; Template:
(define (encounter-temp e)
  (... (phoneid-temp (encounter-phoneid e)) ... (tagid-temp (encounter-tagid e)) ... (time-temp (encounter-time e)) ... (encounter-posn e) ...))

;; The goal of this lab will be to build many functions related to tracking tags.


;;! Problem 1

;; Design a function `tag-activity-count` that takes a TagId, a [ListOf Encounter],
;; a start Time, and an end Time, and calculates how many times the tag has been
;; encountered between the two times, including encounters at exactly the start and
;; end times. Assume that the second time is greater than the first.

;; tag-activity-count : TagId [List-of Encounter] Time Time -> Number
;; Returns the number of times a specific tag has been encountered within a given time range (inclusive)
(define (tag-activity-count tagid loe start end)
  (length (filter (lambda (e) (and (string=? tagid (encounter-tagid e))
                                   (>= (encounter-time e) start)
                                   (<= (encounter-time e) end))) loe)))

;; Test Cases
(check-expect (tag-activity-count TAG1 (list ENCOUNTER1 ENCOUNTER2) TIME1 TIME2) 2)
(check-expect (tag-activity-count TAG1 (list ENCOUNTER1 ENCOUNTER2 ENCOUNTER3) TIME1 TIME2) 2)
(check-expect (tag-activity-count TAG2 (list ENCOUNTER1 ENCOUNTER2 ENCOUNTER3) TIME1 TIME3) 1)

;; STOP AND SWITCH WHO IS TYPING. CODE WALKS WILL BEGIN NOW!

;;! Problem 2

;; Design a function `tag-encounter-position` that identifies all positions (0-indexed) where
;; a specific TagId appears within a list of Encounters. If the tag id does not occur, the list may be empty.
;; One test is provided to clarify how the function is supposed to work:

;; tag-encounter-position : TagId [List-of Encounter] -> [List-of Natural]
;; Returns the positions (starting at 0) of the given tag id in the list of encounters
(define (tag-encounter-position tagid loe)
  (local [
          (define (helper lst i)
            (cond [(empty? lst) '()]
                  [(cons? lst) (if (string=? tagid (encounter-tagid (first lst)))
                                    (cons i (helper (rest lst) (add1 i)))
                                    (helper (rest lst) (add1 i)))]))
          ]
    (helper loe 0)))

;; Test Cases
(check-expect (tag-encounter-position TAG1 (list ENCOUNTER1 ENCOUNTER3 ENCOUNTER2 ENCOUNTER1 ENCOUNTER3 ENCOUNTER1))
              (list 0 2 3 5))
(check-expect (tag-encounter-position TAG1 (list ENCOUNTER1 ENCOUNTER3 ENCOUNTER2 ENCOUNTER1))
              (list 0 2 3))
(check-expect (tag-encounter-position TAG1 (list ENCOUNTER3 ENCOUNTER2 ENCOUNTER1))
              (list 1 2))


;;! Problem 3

;; Design a function, `lost?` that takes a TagId, a [ListOf Owner], and a
;; [ListOf Encounter] and returns if the tag has been lost, where we define that
;; a tag is lost if the tag is encountered, but never by its owner. You may
;; assume that the given TagId appears exactly once in the [ListOf Owner].
;;
;; HINT: This problem requires several steps, which you probably want to write
;; helpers, or at least define local constants for:
;; - (1) encounters with the tag
;; - (2) what is the tag owner
;; - (3) the tag owner does not appear in (1)

;; lost? : TagId [List-of Owner] [List-of Encounter] -> Boolean
;; returns if the tag has been lost, where we define that
;; a tag is lost if the tag is encountered, but never by its owner. You may
;; assume that the given TagId appears exactly once in the [ListOf Owner]
(define (lost? tagid loo loe)
  (local [
          (define (tag-owner loo tagid) (filter (lambda (o) (string=? tagid (owner-tagid o))) loo))
          (define (encounters-with-tag loe tagid) (filter (lambda (e) (string=? tagid (encounter-tagid e))) loe))
          (define (encounters-with-tag-and-owner encounters-with-tag tag-owner)
            (if (> (length tag-owner) 0)
                (filter (lambda (e) (string=? (owner-phoneid (first tag-owner))
                                          (encounter-phoneid e))) encounters-with-tag)
                '()))
          ]
    (and (not (empty? (encounters-with-tag loe tagid)))
         (empty? (encounters-with-tag-and-owner (encounters-with-tag loe tagid) (tag-owner loo tagid))))))

;; Test Cases
(check-expect (lost? TAG1 (list OWNER1 OWNER2) (list ENCOUNTER1 ENCOUNTER2)) #false)
(check-expect (lost? TAG1 (list OWNER2 OWNER3) (list ENCOUNTER1 ENCOUNTER2)) #true)
(check-expect (lost? TAG2 (list OWNER2 OWNER3) (list ENCOUNTER1 ENCOUNTER2)) #false)

;;! Problem 4

;; INTERPRETIVE QUESTION

;; One challenge with location tags is that can easily be used maliciously, most
;; prominently, for stalking. In this case, abusive ex-partners (or other
;; people) secretly put tags in their target's car, or in some item the person
;; they are stalking has, and then use the location mechanism to allow them to
;; track them.
;;
;; While there are some mitigations: the tags often will emit an audible beep
;; after separated from their owner for long enough (but those speakers can be
;; intentionally damaged or removed), and given the right software support,
;; might notify a non-owner that they are near, the risk of abuse was only taken
;; seriously well after the products were initially released, and indeed, after
;; lawsuits filed alleging the particular use of these tools for stalking.

;; Please read these articles:
;; https://freedium.cfd/https://onezero.medium.com/cybersecurity-workers-need-to-learn-from-victims-9db34f3db198
;; https://apnews.com/article/apple-airtags-stalking-lawsuits-e59166988920c4ba1e82956ea85c1677

;; While plenty of technical solutions for location tags have been proposed, in
;; this problem we'd like you to consider several non-technical solutions, and
;; describe which you think is most promising, in 3-4 sentences.
;;
;; 1. All Location Tags must be registered, and anyone who has a criminal record, or
;; has had a restraining order for domestic abuse, harassment, or stalking cannot
;; have one.
;;
;; 2. Location Tags cannot work remotely, via other phones -- they legally are
;; only allowed to broadcast to the owners phone via Bluetooth, so limited in
;; range to about 30 feet.
;;
;; 3. To get location results (beyond the 30 feet from Bluetooth), you must
;; request the location from a public safety agency, and ask them to help locate
;; the item for you, for a nominal fee (say, $5). You must provide a description
;; of what you are looking for, and they will look for you, never directly
;; providing the location info to you.

#|

We think that Solution 1 is the most promising non-technical solution. It seems pretty reasonable to
have something as powerful as a tracking device to be registered under government records. There
already exists restrictions on criminals from buying firearms and other stuff, so adding location
tags to this list of restrictions seems pretty resonable. By keeping it registered, we can track tags
back to their owners if they are lost or stolen by criminals.

|#