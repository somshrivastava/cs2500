;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 11-7-s-exp) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; An Atom is one of:
;; - String
;; - Number
;; - Boolean
;; interpretation: represents a simple data type

(define A1 "abc")
(define A2 "hello")
(define A3 1234)
(define A4 #t)

#;(define (atom-temp a)
  (cond [(string? a) ...]
        [(number? a) ...]
        [(boolean? a) ...]))

;; atom? : Any -> Boolean
;; is the given an Atom?
(check-expect (atom? A1) #t)
(check-expect (atom? (make-posn 2 3)) #f)
(define (atom? a)
  (or (string? a) (number? a) 
      (boolean? a)))

;; atom=? : Atom Atom -> Boolean
;; are the given atoms the same?
(check-expect (atom=? A1 A2) #f)
(check-expect (atom=? A1 A1) #t)
(define (atom=? a1 a2)
  (cond [(string? a1) (and (string? a2) (string=? a1 a2))]
        [(number? a1) (and (number? a2) (= a1 a2))]
        [(boolean? a1) (and (boolean? a2) (boolean=? a1 a2))]))

;; An S-exp is one of:
;; - Atom
;; - [List-of S-exp]
;; represents an atom or a list of s-expression

(define S1 45)
(define S2 (list A1 A2 A3))
(define S3 '("abc" (("a" 13) ("ab" "cd")) #t))

#;(define (s-exp-temp s)
  (cond [(atom? s) ...]
        [(list? s) ... (s-exp-list-temp s)...]))

#;(define (s-exp-list-temp slist)
  (cond [(empty? slist) ...]
        [(cons? slist) ...(s-exp-temp (first slist))...
                       ...(s-exp-list-temp (rest slist)) ...]))

;; contains-atom? : S-exp Atom -> Boolean
;; does the S-exp contain the given atom?
(check-expect (contains-atom? A1 A1) #true)
(check-expect (contains-atom? S2 A3) #true)
(check-expect (contains-atom? S3 13) #true)
(check-expect (contains-atom? A1 A3) #false)
(check-expect (contains-atom? S2 'world) #false)
(check-expect (contains-atom? S3 "abcd") #false) 
(define (contains-atom? s a)
  (cond [(atom? s) (atom=? s a)]
        [(list? s) (s-exp-in-list? s a)]))

;; s-exp-in-list? : [List-of S-exp] Atom -> Boolean
;; is the atom in the list?
(check-expect (s-exp-in-list? S2 A3) #true)
(check-expect (s-exp-in-list? S3 13) #true)
(check-expect (s-exp-in-list? S2 'world) #false)
(check-expect (s-exp-in-list? S3 "abcd") #false)
(define (s-exp-in-list? slist a)
  (cond [(empty? slist) #false]
        [(cons? slist) (or (contains-atom? (first slist) a)
                           (s-exp-in-list? (rest slist) a))]))

;; contains-atom?.v2 : S-exp Atom -> Boolean
;; does the S-exp contain the given atom?
(check-expect (contains-atom?.v2 A1 A1) #true)
(check-expect (contains-atom?.v2 S2 A3) #true)
(check-expect (contains-atom?.v2 S3 13) #true)
(check-expect (contains-atom?.v2 A1 A3) #false)
(check-expect (contains-atom?.v2 S2 'world) #false)
(check-expect (contains-atom?.v2 S3 "abcd") #false) 
(define (contains-atom?.v2 s a)
  (cond [(atom? s) (atom=? s a)]
        [(list? s) (ormap (λ (x) (contains-atom?.v2 x a)) s)]))

;; flatten : S-exp -> [List-of Atom]
;; flattens the s-expression into one flat list
(check-expect (flatten A1) (list 'abc))
(check-expect (flatten S3) (list 'abc 'a 13 "ab" "cd" #t)) 
(define (flatten s)
  (cond [(atom? s) (list s)]
        [(list? s) (foldr (λ (item list-so-far)
                            (append (flatten item) list-so-far))
                          '()
                            s)])) 

;; atom->string : Atom -> String
;; represent the atom as a string
(check-expect (atom->string 'a) "a")
(check-expect (atom->string "a") "\"a\"")
(check-expect (atom->string 123) "123")
(check-expect (atom->string #true) "#true")
(define (atom->string a)
  (cond [(symbol? a) (symbol->string a)]
        [(string? a) (string-append "\"" a "\"")]
        [(number? a) (number->string a)]
        [(boolean? a) (boolean->string a)]))

;; s-exp->string : S-exp -> String
;; represent the s-expression as a string
(check-expect (s-exp->string '(abc ((a 13) ("ab" "cd")) #t))
              "(abc ((a 13) (\"ab\" \"cd\")) #true)")
(check-expect (s-exp->string '()) "()")
(define (s-exp->string s)
  (cond [(atom? s) (atom->string s)]
        [(list? s) (string-append "(" (slist->string.v2 s) ")")])) 

#|
;; list->string : [List-of S-exp] -> String
;; represent all of the parts of the list as a string
(check-expect (slist->string '(abc ((a 13) ("ab" "cd")) #t))
              "abc ((a 13 ) (\"ab\" \"cd\" ) ) #true ")
(check-expect (slist->string '()) "")
(define (slist->string slist)
  (foldr (λ (s string-so-far)
           (string-append (s-exp->string s) " " string-so-far))
         ""
         slist))
|#

;; list->string.v2 : [List-of S-exp] -> String
;; represent all of the parts of the list as a string
(check-expect (slist->string.v2 '(abc ((a 13) ("ab" "cd")) #t))
              "abc ((a 13) (\"ab\" \"cd\")) #true")
(check-expect (slist->string.v2 '()) "")
(define (slist->string.v2 slist)
  (if (empty? slist) ""
      (local [(define ITEM1 (s-exp->string (first slist)))
              (define REST (rest slist))]
        (if (empty? REST) ITEM1
            (string-append ITEM1
                           (foldr (λ (s string-so-far)
                                    (string-append " "(s-exp->string s) string-so-far))
                                  ""
                                  REST))))))