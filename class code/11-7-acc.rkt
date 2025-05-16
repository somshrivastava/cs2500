;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 11-7-acc) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; rel->abs : [List-of Number] -> [List-of Number]
;; convert the relative distances to absolute distances
(check-expect (rel->abs (list 10 50 20)) (list 10 60 80))
(check-expect (rel->abs '()) '())
(define (rel->abs lon)
  (cond [(empty? lon) '()]
        [(cons? lon) (cons (first lon)
                           (map (λ (x) (+ x (first lon))) (rel->abs (rest lon))))]))

;; add-to-all : Number [List-of Number] -> [List-of Number]
;; add the given number to every member of the list
(check-expect (add-to-all 2 (list 1 2 3)) (list 3 4 5))
(check-expect (add-to-all 2 (list)) (list ))
(define (add-to-all n lon)
  (cond [(empty? lon) '()]
        [(cons? lon) (cons (+ n (first lon))
                           (add-to-all n (rest lon)))]))

;; rel->abs.v2 : [List-of Number] Number -> [List-of Number]
;; convert the relative distances to absolute distances
(check-expect (rel->abs.v2 (list 10 50 20) 0) (list 10 60 80))
(check-expect (rel->abs.v2 '() 0) '())
(define (rel->abs.v2 lon running-total)
  (cond [(empty? lon) '()]
        [(cons? lon) (cons (+ (first lon) running-total)
                           (rel->abs.v2 (rest lon) (+ (first lon) running-total)))]))

;; rel->abs.v3 :  [List-of Number] -> [List-of Number]
;; convert the relative distances to absolute distances
(check-expect (rel->abs.v3 (list 10 50 20)) (list 10 60 80))
(check-expect (rel->abs.v3 '()) '())
(define (rel->abs.v3 lon0)
  (local [; [List-of Number] Number -> [List-of Number]
          ; accumulator: keeps track of the distance traveled so far
          ;(check-expect (rel->abs.v2 (list 10 50 20) 0) (list 10 60 80))
          ;(check-expect (rel->abs.v2 '() 0) '())
          (define (r->a.acc lon running-total)
            (cond [(empty? lon) '()]
                  [(cons? lon) (cons (+ (first lon) running-total)
                                     (r->a.acc (rest lon)
                                                  (+ (first lon) running-total)))]))]
    (r->a.acc lon0 0)))

;; my-foldl : (X Y) [X Y -> Y] Y [List-of X] -> Y
;; collapse the list from left to right
(check-expect (my-foldl + 0 (list 1 2 3)) 6)
(check-expect (my-foldl cons '() (list 1 2 3)) (list 3 2 1))
(define (my-foldl func base lox)
  (cond [(empty? lox) base]
        [(cons? lox) (my-foldl func (func (first lox) base) (rest lox))]))

;; sum : [List-of Number] -> Number
;; sum the numbers in the list
(define (sum lon)
  (cond [(empty? lon) 0]
        [(cons? lon) (+ (first lon) (sum (rest lon)))]))

;; sum.v2 : [List-of Number] Number -> Number
;; sum the numbers in the list
;; accumulator: sum of the numbers encountered so far
(define (sum.v2 lon acc)
  (cond [(empty? lon) acc]
        [(cons? lon) (sum.v2 (rest lon) (+ (first lon) acc))]))

(sum (list 1 2 3))
(+ 1 (sum (list 2 3)))
(+ 1 (+ 2 (sum (list 3))))
(+ 1 (+ 2 (+ 3 (sum '()))))
(+ 1 (+ 2 (+ 3 0)))
(+ 1 (+ 2 3))
(+ 1 5)
6

(sum.v2 (list 1 2 3) 0)
(sum.v2 (list 2 3) 1)
(sum.v2 (list 3) 3)
(sum.v2 (list ) 6)
6