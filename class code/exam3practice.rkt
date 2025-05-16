;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname exam3practice) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; QUESTION 1

(define-struct book (title author year))
;; A Book is a (make-book String String Number)
;; - title is the title of the book
;; - author is the name of the author
;; - year is the publication year
;; Interpretation: Represents a book.

(define EX-BOOK1 (make-book "Gatsby" "Fitzgerald" 1925))
(define EX-BOOK2 (make-book "1984" "Orwell" 1949))
(define EX-BOOK3 (make-book "Mockingbird" "Lee" 1960))

(check-expect (tba (list EX-BOOK1 EX-BOOK2 EX-BOOK3) "Orwell") (list "1984 (1949)"))

;; tba : [List-of Book] String -> [List-of String]
;; tba "titles by author" produces a list of strings formatted as "Title (Year)"
;; for books by the given author.
(define (tba lob str)
  (map (lambda (x) (string-append (book-title x) " (" (number->string (book-year x)) ")"))
       (filter (lambda (x) (string=? (book-author x) str)) lob)))

;; QUESTION 2

(define-struct fruit-tree [name count])
(define-struct group [orchards])
;; An Orchard is one of:
;; - (make-fruit-tree String Number)
;; - (make-group OrchardList)
;; Interpretation
;; - (make-fruit-tree name count) represents a single fruit-growing tree with the
;; given number of fruit.
;; - (make-group orchards) represents a group of trees and/or orchards.

;; An OrchardList is a [List-of Orchard]
(define APPLE1 (make-fruit-tree "Apple" 30))
(define APPLE2 (make-fruit-tree "Apple" 5))
(define ORANGE (make-fruit-tree "Orange" 20))
(define ORCHARD (make-group (list APPLE1 APPLE2 ORANGE)))

(check-expect (countf APPLE1 "Apple") 30)
(check-expect (countf ORANGE "Apple") 0)
(check-expect (countf ORCHARD "Apple") 35)

;; countf : Orchard String -> Number
;; counts the total number of fruit of the given kind.
(define (countf orchard str)
  (cond [(fruit-tree? orchard) (if (string=? str (fruit-tree-name orchard)) (fruit-tree-count orchard) 0)]
        [(group? orchard) (foldr + 0 (map (lambda (x) (countf x str)) (group-orchards orchard)))]))

;; QUESTION 3

#|

It turns out that not all the trees in the orchard bear fruit. Modify the data definition for Orchard to
include non-fruiting trees, and then write the templates for Orchard. Non-fruiting trees do have names,
which you should represent. You may omit the interpretation and examples.

|#

(define-struct fruitless-tree [name])
;; An Orchard is one of:
;; - (make-fruitless-tree String)
;; - (make-fruit-tree String Number)
;; - (make-group OrchardList)
;; Interpretation
;; - (make-fruitless-tree name) represents a single fruitless tree
;; - (make-fruit-tree name count) represents a single fruit-growing tree with the
;; given number of fruit.
;; - (make-group orchards) represents a group of trees and/or orchards.

;; An OrchardList is a [List-of Orchard]

(define (orchard-temp o)
  (cond [(fruitless-tree? o) (... (fruitless-tree-name o) ...)]
        [(fruit-tree? o) (... (fruit-tree-name o) ...
                         ... (fruit-tree-count o) ...)]
        [(group? o) (... (orchardlist-temp (group-orchards o)) ...)]))

(define (orchardlist-temp ol)
  (cond [(empty? ol) ...]
        [(cons? ol) ((orchard-temp (first ol)) ... (orchardlist-temp (rest ol)))]))

;; Design a function orange-within-3-levels? that takes in an Orchard
;; and returns #true if an orange tree is encountered within 3 levels
;; of the Orchard, #false otherwise.
;; Some tests are given for clarity. You only need to do the signature and code steps
;; of the design recipe.

(define PEAR (make-fruit-tree "Pear" 2))
(define APPLE (make-fruit-tree "Apple" 2))

(define APPLE-GROUP (make-group (list APPLE APPLE)))
(define ORANGE-GROUP (make-group (list ORANGE)))
(define PEAR-GROUP (make-group (list PEAR PEAR ORANGE-GROUP)))

(define LARGE-ORCHARD-1 (make-group (list PEAR APPLE APPLE-GROUP PEAR-GROUP)))
(define LARGE-ORCHARD-2 (make-group (list PEAR APPLE APPLE-GROUP)))
(define LARGE-ORCHARD-3 (make-group (list ORANGE APPLE APPLE-GROUP)))
(define LARGE-ORCHARD-4 (make-group (list APPLE LARGE-ORCHARD-1)))

(check-expect (orange-within-3-levels? ORANGE) #true)
(check-expect (orange-within-3-levels? LARGE-ORCHARD-3) #true)
(check-expect (orange-within-3-levels? LARGE-ORCHARD-1) #false)
(check-expect (orange-within-3-levels? LARGE-ORCHARD-2) #false)
(check-expect (orange-within-3-levels? LARGE-ORCHARD-4) #false)

;; orange-within-3-levels? : Orchard -> Boolean
;; checks whether there is an orange within 3 levels and returns true, else returns false
(define (orange-within-3-levels? o)
  (local [
          (define (helper lst acc)
            (if (<= acc 3)
                (cond [(empty? lst) 0]
                  [(cons? lst)
                   (cond [(fruitless-tree? (first lst)) (if (string=? (fruitless-tree-name (first lst)) "Orange") #true (helper (rest lst) acc))]
                         [(fruit-tree? (first lst)) (if (string=? (fruit-tree-name (first lst)) "Orange") #true (helper (rest lst) acc))]
                         [(group? (first lst)) (helper (group-orchards o) (add1 acc))])])
                #false))
          ]
    (cond [(fruitless-tree? o) (string=? (fruitless-tree-name o) "Orange")]
                  [(fruit-tree? o) (string=? (fruit-tree-name o) "Orange")]
                  [(group? o) (helper (group-orchards o) 0)])))

(define (orange-within-3-levels? orch)
  (local [; search-3-levels : Orchard Nat -> Boolean
          ; Searches three levels for an orange; returns False
          ; if the orange is not found, otherwise returns True
          (define (search-3-levels cur_orch levels-left)
            (cond 
              [(zero? levels-left) #false]
              [(fruitless? cur_orch) (string=? (fruitless-name cur_orch) "Orange")]
              [(fruit-tree? cur_orch) (string=?  (fruit-tree-name cur_orch) "Orange")]
              [(group? cur_orch) (ormap
                                  (lambda (o) (search-3-levels o (sub1 levels-left)))
                                  (group-orchards cur_orch))]))]
    (search-3-levels orch 3)))