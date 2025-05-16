;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 11-6-docs) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define-struct section [title parts])
(define-struct paragraph [text])
;; A Doc is one of:
;; - (make-section String Docs)
;; - (make-paragraph String)
;; Interpretation: A document in a word processor is a:
;; - (make-section title parts) is a section with the given title and parts.
;; - (make-paragraph text) is a paragraph with the given text.

;; A Docs is a [List-of Doc].
;; Interpretation: a sequence of documents that go in a section.

(define DOC-EXAMPLE-1 (make-paragraph "This is a paragraph."))
(define DOC-EXAMPLE-2
  (make-section "Section 1" 
                (list DOC-EXAMPLE-1
                      (make-section "Section 1.1"
                                    (list (make-paragraph "Subsection text"))))))
(define DOCS-EXAMPLE-1 (list DOC-EXAMPLE-1 DOC-EXAMPLE-2))
(define DOCS-EXAMPLE-2 '()) ; lame example, but technically correct

;; count-sections: Doc -> Nat
;; count the number of sections in the given Doc
(check-expect (count-sections DOC-EXAMPLE-1) 0)
(check-expect (count-sections DOC-EXAMPLE-2) 2)
(define (count-sections doc)
  (cond [(section? doc) (+ 1 (count-sections-lod (section-parts doc)))]
        [(paragraph? doc) 0]))

;; count-sections-lod: Docs -> Nat
;; count the number of sections in the given [List-of Doc]
(check-expect (count-sections-lod DOCS-EXAMPLE-1) 2)
(check-expect (count-sections-lod DOCS-EXAMPLE-2) 0)
(define (count-sections-lod lod)
  (foldr (λ (x y) (+ (count-sections x) y)) 0 lod)
  #;(cond [(empty? lod) 0]
        [(cons? lod) (+ (count-sections (first lod))
                        (count-sections-lod (rest lod)))]))


