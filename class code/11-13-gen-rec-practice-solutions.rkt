;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 11-13-gen-rec-practice-solutions) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; N>=1 N>=1 -> N
;; finds the greatest common divisor of the two numbers
(check-expect (gcd-struct 12 34) 2)
(check-expect (gcd-struct 7 13) 1)
(check-expect (gcd-struct 9 24) 3)
(define (gcd-struct n m)
  (local [;; N>=1 -> N
          (define (find-divisor i)
            (cond [(= i 1) 1]
                  [else (if (and (= (remainder n i) 0)
                                 (= (remainder m i) 0))
                            i
                            (find-divisor (sub1 i)))]))]
    (find-divisor (min m n))))

;; idea: Euclid's algorithm: suppose a > b, then gcd(a, b) = gcd(b, a mod b)

;; N>=1 N>=1 -> N
;; finds the greatest common divisor of the two numbers
;; terminates: recurs on the smaller number and the remainder of dividing the
;;             the smaller and larger numbers
(check-expect (gcd-gen 12 34) 2)
(check-expect (gcd-gen 7 13) 1)
(check-expect (gcd-gen 9 24) 3)
(define (gcd-gen n m)
  (cond [(> m n) (gcd-gen m n)]
        [(= m n) n]
        [(= (modulo n m) 0) m]
        [else (gcd-gen m (modulo n m))]))

;; find-root: [Number->Number] Number Number Number -> Number
;; finds a number that is close to the root of the function
(check-within (find-root sin 2 4 .001) pi .001)
(check-within (find-root identity -1 1 .001) 0 .001)

(define (find-root f lo hi delta)
  (local [(define width (- hi lo))
          (define num-windows (floor (/ width delta)))
          (define (mk-window i) (+ lo (* i delta)))
          (define intervals (build-list num-windows mk-window))
          ;; Number -> Boolean
          ;; is this window close enough to 0?
          (define (good window)
            (<= (* (f window) (f (+ window delta))) 0))]
    (first (filter good intervals))))

;; idea: binary search
;; find-root.v2: [Number->Number] Number Number Number -> Number
;; finds a number that is close to the root of the function
;; terminates: recurs on a smaller space because mid is between lo and hi
(define (find-root.v2 f lo hi delta)
  (cond [(<= (- hi lo) delta) lo]
        [else (local [(define mid (/ (+ hi lo) 2))]
                      (if (<= (* (f lo) (f mid)) 0)
                          (find-root.v2 f lo mid delta)
                          (find-root.v2 f mid hi delta)))]))  














          