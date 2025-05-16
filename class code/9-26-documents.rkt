;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 9-26-documents) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

;; A Document is one of:
;; - Book
;; - Magazine
;; - Pamphlet
;; interpretation: represents a document that can be read

;; A Book is a (make-book String String)
(define-struct book (title author))
;; interpretation: represents a book with a title and author

;; A Magazine is a (make-magazine String Date Integer)
(define-struct magazine (name date issue))
;; interpretation: represents a magazine with a name,
;;                      date published and issue number

;; A Pamphlet is a (make-pamphlet String String)
(define-struct pamphlet (name subject))
;; interpretation: represents a pamphlet with a name and a subject

;; student to do: add examples

;; Document -> ?
(define (doc-temp adoc)
  (... (cond [(book? adoc) ...(book-temp adoc)]
             [(magazine? adoc) ...(magazine-temp adoc)]
             [(pamphlet? adoc) ...(pamphlet-temp adoc)])))

;; Book -> ?
(define (book-temp abook)
  (...(book-title abook) ... (book-author abook)))

;; Magazine -> ?
(define (magazine-temp amag)
  (... (magazine-name amag)
       ... (magazine-date amag)
       ...(magazine-issue amag)))

;; Pamphlet -> ?
(define (pamphlet-temp p)
  (... (pamphlet-name p) ... (pamphlet-subject p)))


