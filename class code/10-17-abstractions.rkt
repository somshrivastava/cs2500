;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 10-17-abstractions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; need to add tests!

; do-to-all : (X Y) [List-of X] [X -> Y] -> [List-of Y]
; apply an operation to every member of the list, returning a list of the results
(define (do-to-all lon f) 
  (cond [(empty? lon) '()]
        [(cons? lon)
         (cons (f (first lon))
               (do-to-all (rest lon) f))]))

; short-msgs : [List-of String] -> [List-of String]
; Keeps just the strings that are < SHORT characters
(define SHORT 14)
(define (short-msgs los) 
  (keep-if los short?))

; polite-msgs : [List-of String] -> [List-of String]
; Keeps just the strings that start with "dear"
(define POLITE "dear")  
(define (polite-msgs los) 
  (keep-if los polite?))

; short? : String -> Boolean
; Determines if a string is short
(check-expect (short? "cat") #true)
(check-expect (short? "this is not a short string") #false)
(define (short? s)
  (< (string-length s) SHORT))

; polite? : String -> Boolean
; Determines if a string starts with "dear"
;(check-expect (polite? STR-S) #false)
;(check-expect (polite? STR-LP) #true)
(define (polite? s)
  (and (>= (string-length s) (string-length POLITE))
       (string=? (substring s 0 (string-length POLITE)) POLITE)))

; keep-if : (X) [List-of X] [X -> Boolean] -> [List-of X]
; Keeps just the strings that the function returns true for
(define (keep-if lox p?) 
   (cond [(empty? lox) '()]
         [(cons? lox) 
            (if (p? (first lox))
                (cons (first lox) (keep-if (rest lox) p?)) 
                (keep-if (rest lox) p?))])) 

;; find-sum: [List-of Number] -> Number
;; sum the numbers in the given list
(check-expect (find-sum (list )) 0)
(check-expect (find-sum (list 1 2 3)) 6)
(define (find-sum lon) 
  (collapse lon 0 +))

;; find-product: [List-of Number] -> Number
;; multiply the numbers in the given list
(check-expect (find-product (list )) 1)
(check-expect (find-product (list 1 2 3 4)) 24)
(define (find-product lon) 
  (collapse lon 1 *))

; collapse : (X Y) [List-of X] Y [X Y -> Y] -> Y
; Collapses a list given a base-case and pairwise function
(check-expect (collapse '() 0 +) 0)
(check-expect (collapse (list 2 3 4) 0 +) 9)
(check-expect (collapse '() 1 *) 1)
;(check-expect (collapse LON-3 1 *) 24)
(define (collapse lon base f) 
    (cond [(empty? lon) base]
          [(cons? lon)
            (f (first lon)
               (collapse (rest lon) base f))]))

;; add-length: String Number -> Number
;; add the length of s to n
(define (add-length s n)
  (+ (string-length s) n))

;; dist-to-0: Position -> PosReal
;; computes the distance to the origin of the given Position
;(check-expect (dist-to-0 POSN-1) 5)
;(check-expect (dist-to-0 POSN-2) 0)
(define (dist-to-0 p)
  (sqrt (+ (sqr (posn-x p)) (sqr (posn-y p)))))

;; greater-dist: Position Number -> Number
;; get the greater of the dist to 0 of p, or n
(define (greater-dist p n)
  (max (dist-to-0 p) n))