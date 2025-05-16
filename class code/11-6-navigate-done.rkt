;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 11-6-navigate-done) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; ---------------------------------------------------------------------------------------------------
;; PROBLEM: design the function 'navigate'. It consumes a Tree
;; and a list of directions ("left", "right"). It extracts the sub-tree
;; at the specified places, that is, for a 'growth',
;; it goes straight (because there is no choice), for a 'fork',
;; it follows the next direction, 
;; and if there are no more directions, it returns the given tree. 

;; structure type definitions 
(define-struct leaf ())
(define-struct growth (next))
(define-struct fork (left right))

;; data definitions 
;; Tree is one of: 
;; -- (make-leaf)
;; -- (make-growth Tree)
;; -- (make-fork Tree Tree)

;; Direction is one of: 
;; -- "left" 
;; -- "right" 

;; data examples
(define LF (make-leaf))
(define T1 (make-growth LF))
(define T2 (make-fork T1 T1))
(define T3 (make-growth T2))

(define LOD-0 '())
(define LOD-1 (list "left" "right"))
(define LOD-2 (list "left" "right" "left"))

(define (lod-tree-template tree lod)
  (cond [(and (empty? lod) (leaf? tree)) ...]
        [(and (empty? lod) (growth? tree)) ...(growth-next tree)
                                           ...(lod-tree-template lod (growth-next tree))]
        [(and (empty? lod) (fork? tree))]
        [(and (cons? lod) (leaf? tree)) ...]
        [(and (cons? lod) (growth? tree))]
        [(and (cons? lod) (fork? tree))]))

;; navigate : Tree [List-of Direction] -> Tree
;; navigate the tree using the given list of directions
(check-expect (navigate LF LOD-0) LF)
(check-expect (navigate LF LOD-2) LF)
(check-expect (navigate T1 LOD-0) T1)
(check-expect (navigate T2 LOD-1) LF)
(check-expect (navigate T3 LOD-1) LF)
(define (navigate tree lod)
  (cond [(and (empty? lod) (leaf? tree)) tree]
        [(and (empty? lod) (growth? tree)) tree]
        [(and (empty? lod) (fork? tree)) tree]
        [(and (cons? lod) (leaf? tree)) tree]
        [(and (cons? lod) (growth? tree)) (navigate (growth-next tree) lod)]
        [(and (cons? lod) (fork? tree))
         (if (string=? (first lod) "left")
             (navigate (fork-left tree) (rest lod))
             (navigate (fork-right tree) (rest lod)))]))

;; navigate.v2 : Tree [List-of Direction] -> Tree
;; navigate the tree using the given list of directions
(check-expect (navigate.v2 LF LOD-0) LF)
(check-expect (navigate.v2 LF LOD-2) LF)
(check-expect (navigate.v2 T1 LOD-0) T1)
(check-expect (navigate.v2 T2 LOD-1) LF)
(check-expect (navigate.v2 T3 LOD-1) LF)
(define (navigate.v2 tree lod)
  (cond [(empty? lod) tree]
        [(and (cons? lod) (leaf? tree)) tree]
        [(and (cons? lod) (growth? tree)) (navigate.v2 (growth-next tree) lod)]
        [(and (cons? lod) (fork? tree))
         (if (string=? (first lod) "left")
             (navigate.v2 (fork-left tree) (rest lod))
             (navigate.v2 (fork-right tree) (rest lod)))]))
 