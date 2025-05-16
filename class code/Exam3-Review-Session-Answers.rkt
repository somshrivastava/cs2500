;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Exam3-Review-Session-Answers) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; --------- Question 1

(define-struct book (title author year))

;; A Book is a (make-book String String Number)
;; - title is the title of the book
;; - author is the name of the author
;; - year is the publication year
;; Interpretation: Represents a book.

(define EX-BOOK1 (make-book "Gatsby" "Fitzgerald" 1925))
(define EX-BOOK2 (make-book "1984" "Orwell" 1949))
(define EX-BOOK3 (make-book "1985" "Orwell" 1949))
(define EX-BOOK4 (make-book "Mockingbird" "Lee" 1960))

; Complete the following function design (definition and check-expects). 
; You must use list abstractions.
;; tba : [List-of Book] String -> [List-of String]
;; tba "titles by author" produces a list of strings formatted as "Title (Year)"
;; for books by the given author.

(define LOB (list EX-BOOK1 EX-BOOK2 EX-BOOK3 EX-BOOK4))

(check-expect (tba LOB "NAA") (list))
(check-expect (tba LOB "Orwell") (list "1984 (1949)" "1985 (1949)"))
(check-expect (tba LOB "Fitzgerald") (list "Gatsby (1925)"))
(check-expect (tba (list) "Lee") (list))

(check-expect (tba2 LOB "NAA") (list))
(check-expect (tba2 LOB "Orwell") (list "1984 (1949)" "1985 (1949)"))
(check-expect (tba2 LOB "Fitzgerald") (list "Gatsby (1925)"))
(check-expect (tba2 (list) "Lee") (list))

(define (tba lob str)
  (map (lambda (book) (string-append (book-title book) " (" (number->string (book-year book)) ")"))
       (filter (lambda (book) (string=? (book-author book) str)) lob)))

(define (tba2 lob str)
  (foldr (lambda (book acc)
           (if (string=? (book-author book) str)
           (cons (string-append (book-title book) " (" 
                                (number->string (book-year book)) ")")
                 acc)
           acc))
         (list) lob))

;; --------- Question 2

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
(define ORCHARD2 (make-group (list APPLE1 ORCHARD)))

; Templates deliberately omitted. Complete the following function design (definition 
; and check-expects).

;; countf : Orchard String -> Number
;; Counts the total number of fruit of the given kind.

(check-expect (countf APPLE1 "Apple") 30)
(check-expect (countf APPLE1 "Orange") 0)
(check-expect (countf ORCHARD "Apple") 35)

(check-expect (countf2 APPLE1 "Apple") 30)
(check-expect (countf2 APPLE1 "Orange") 0)
(check-expect (countf2 ORCHARD "Apple") 35)

(define (countf orc name)
  (cond
    [(fruit-tree? orc) (if (string=? name (fruit-tree-name orc)) (fruit-tree-count orc) 0)]
    [(group? orc) (foldr (lambda (subtree acc) (+ acc (countf subtree name))) 0 (group-orchards orc))]))

(define (countf2 orc name)
  (cond
    [(fruit-tree? orc) (if (string=? name (fruit-tree-name orc)) (fruit-tree-count orc) 0)]
    [(group? orc) (count-orchard-list (group-orchards orc) name)]))

(define (count-orchard-list orchard-list name)
  (cond
    [(empty? orchard-list) 0]
    [(cons? orchard-list) (+ (countf2 (first orchard-list) name) 
                             (count-orchard-list (rest orchard-list) name))]))

;; --------- Question 3

; It turns out that not all the trees in the orchard bear fruit. Modify the data 
; definition for Orchard to include non-fruiting trees, and then write the templates 
; for Orchard. Non-fruiting trees do have names, which you should represent. You may 
; omit the interpretation and examples.


(define-struct fruitless [name])

;; An Orchard is one of:
;; - (make-fruitless String)
;; - (make-fruit-tree String Number)
;; - (make-group OrchardList)

;; Interpretation
;; - (make-fruitless name) represents a non-fruiting tree
;; - (make-fruit-tree name count) represents a single fruit-growing tree with the
;; given number of fruit.
;; - (make-group orchards) represents a group of trees and/or orchards.

;; An OrchardList is a [List-of Orchard]

(define (orchard-templ orch)
  (cond [(fruitless? orch) (... (fruitless-name orch) ...)]
        [(fruit-tree? orch) (... (fruit-tree-name orch) ... (fruit-tree-count orch) ...)]
        [(group? orch) (... (orchardlist-templ (group-orchards orch)) ...)]))

(define (orchardlist-templ ol)
  (cond [(empty? ol) ...]
        [(cons? ol) (... (orchard-templ (first ol)) ... (orchardlist-templ (rest ol)) ...)]))

;; --------- Extra Problem

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


; orange-within-3-levels? : Orchard -> Boolean
; Searches through the given orchard and returns true if the 
; orchard contains an orange within 3 levels

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