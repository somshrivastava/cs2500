;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 10-9-lon) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A ListOfNumbers (LoN) is one of
; - '()
; - (cons Number LoN)
; Interpretation: represents a list of numbers
(define LON-0 '())
(define LON-1 (cons 3 LON-0)) 
(define LON-2 (cons 5 LON-1))


(define (lon-temp lon) 
  (...(cond [(empty? lon) ...] 
            [(cons? lon)
             ...(first lon) ...
             ...(lon-temp (rest lon)) ...])))

;; double-list: LoN -> LoN
;; doubles each number in the given list
(check-expect (double-list LON-0) LON-0)
(check-expect (double-list LON-2) (cons 10 (cons 6 '())))
(define (double-list lon) 
  (cond [(empty? lon) '()] 
        [(cons? lon)
         (cons (* 2 (first lon)) 
               (double-list (rest lon)))]))

;; sqrt-all: LoN -> LoN
;; takes the square root of each number in the given list
(check-expect (sqrt-all LON-0) LON-0)
(check-within (sqrt-all LON-2) (cons (sqrt 5) (cons (sqrt 3) '())) .001)
(define (sqrt-all lon) 
  (cond [(empty? lon) '()] 
        [(cons? lon)
         (cons (sqrt (first lon)) 
               (sqrt-all (rest lon)))]))

;; sqr-all: LoN -> LoN
;; takes the square root of each number in the given list
(check-expect (sqr-all LON-0) LON-0)
(check-expect (sqr-all LON-2) (cons 25 (cons 9 '())))
(define (sqr-all lon) 
  (cond [(empty? lon) '()] 
        [(cons? lon)
         (cons (sqr (first lon)) 
               (sqr-all (rest lon)))]))


;; do-to-all : LoN ??? -> LoN
; Applies the second argument to each of the numbers
(check-expect (do-to-all LON-0 add1) '())
(check-expect (do-to-all LON-2 add1) (cons 6 (cons 4 '())))
(check-expect (do-to-all LON-2 sqr) (cons 25 (cons 9 '())))
(check-within (do-to-all LON-2 sqrt) (cons (sqrt 5) (cons (sqrt 3) '())) .0001)
(define (do-to-all lon f) 
  (cond
    [(empty? lon) '()]
    [(cons? lon)
     (cons (f (first lon))
           (do-to-all (rest lon) f))]))
