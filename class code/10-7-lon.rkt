;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 10-7-lon) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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