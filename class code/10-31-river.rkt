;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 10-31-river) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct merge [width left right])
(define-struct stream [flow-rate])
;; A River is one of
;; - (make-merge Nat River River)
;; - (make-stream Nat)
;; interpretation: represents a tree-shaped system of small streams
;; that drain into large rivers.
;; flow rate of a stream is cubic meters per second
;; width of a river is measured in meters at its widest point

(define STONY-BROOK (make-stream 20))
(define CANTERBURY-BROOK (make-stream 15))
(define CHARLES-RIVER (make-merge 1000 STONY-BROOK CANTERBURY-BROOK))


;; Design a function to make every merge point wider in a river network
;; by a given amount. You may omit tests and the purpose statement.







;; Design a function to calculate the total volume of water that flows into
;; a river network. You may omit tests and the purpose statement.

;; total-volume : River -> Number
(define (total-volume river)
  (cond
    [(merge? river) (+ (total-volume (merge-left river)) 
                       (total-volume (merge-right river)))]
    [(stream? river) (stream-flow-rate river)]))



;; Design a function to calculate the number of merge points
;; in a river network. You may omit tests and the purpose statement.

;; total-merges : River -> Number
(define (total-merges river)
  (cond
    [(merge? river) (+ 1 (total-merges (merge-left river))
                         (total-merges (merge-right river)))]
    [(stream? river) 0]))





;; Design a function to make every merge point wider in a river network
;; by a given amount. You may omit tests and the purpose statement.
;; widen-river : Nat River -> River
(define (widen-river n river)
  (cond
    [(merge? river) (make-merge (+ n (merge-width river))
                                (widen-river (merge-left river))
                                (widen-river (merge-right river)))]
    [(stream? river) river]))




;; Design a function that caps the flow of every stream to at most 10.
;; You may omit tests and the purpose statement.

;; cap-flow : River -> River
(define (cap-flow river)
  (cond
    [(merge? river) (make-merge (merge-width river)
                                (cap-flow (merge-left river))
                                (cap-flow (merge-right river)))]
    [(stream? river) (make-stream (min 10 (stream-flow-rate river)))]))




;; 1) Write two abstractions for processing river networks,
;; and 2)use them as appropriate to rewrite the previous functions.
;; You may omit tests and the purpose statement.

;; river-fold : (X X Number -> X) (Number -> X) River -> X
(define (river-fold f-merge f-stream river)
  (cond
    [(merge? river) (f-merge (merge-width river)
                             (river-fold f-merge f-stream (merge-left river))
                             (river-fold f-merge f-stream (merge-right river)))]
    [(stream? river) (f-stream (stream-flow-rate river))]))

(define (total-merges-f-merge w l r)
  (+ 1 l r))

(define (total-merges-f-stream n)
  0)

;; total-merges : River -> Number
(define (total-merges.v2 river)
  (river-fold total-merges-f-merge total-merges-f-stream river))

(define (total-volume-f-merge w l r)
  (+ l r))

(define (total-volume-f-stream n)
  n)

;; total-volume : River -> Number
(define (total-volume.v2 river)
  (river-fold total-volume-f-merge total-volume-f-stream river))

;; river-map : (Number -> Number) (Number -> Number) River -> River
(define (river-map f-merge f-stream river)
  (cond 
    [(merge? river) (make-merge (f-merge (merge-width river))
                                (river-map f-merge f-stream (merge-left river))
                                (river-map f-merge f-stream (merge-right river)))]
    [(stream? river) (make-stream (f-stream (stream-flow-rate river)))]))

;; widen-river : Number River -> River
(define (widen-river.v2 n river)
  (local [(define (add-n m) (+ n m))]
    (river-map identity widen-river river)))

(define (cap-flow-n n)
  (min 10 n))

;; cap-flow : River -> River
(define (cap-flow.v2 river)
  (river-map cap-flow-n identity river))