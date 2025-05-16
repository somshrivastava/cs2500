;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 10-16-keep-if) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; keep-if : (X) [List-of X] [X -> Boolean] -> [List-of X]
; Keeps just the strings that the function returns true for
(define (keep-if lox p?) 
   (cond [(empty? lox) '()]
         [(cons? lox) 
            (if (p? (first lox))
                (cons (first lox) (keep-if (rest lox) p?)) 
                (keep-if (rest lox) p?))]))
