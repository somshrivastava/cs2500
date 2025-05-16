;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname HW10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; Usual Instructions:
;; 1. Do not create, modify or delete any line that begins with ";;!". These are
;;    markers that we use to segment your file into parts to facilitate grading.
;; 2. You must follow the _design recipe_ for every problem. In particular,
;;    every function you define must have at least three check-expects (and
;;    more if needed).
;; 3. You must follow the Style Guide:
;;    https://pages.github.khoury.northeastern.edu/2500/2024F/style.html
;; 4. You must submit working code. In DrRacket, ensure you get no errors
;;    when you click Run. After you submit on Gradescope, you'll get instant
;;    feedback on whether or not Gradescope can run your code, and your code must
;;    run on Gradescope to receive credit from the autograder.
;; 5. On some problems, you can get automated feedback on your in-progress work
;;    from FeedBot, a system developed by the course staff. When you submit your
;;    assignment, you will see a link to the FeedBot report along with the autograder
;;    feedback. Only a certain number of submissions will get this, and submissions
;;    close together will not receive the feedback.

;;! HW10
;; Consider the following data definition for binary trees:

(define-struct leaf [val])
(define-struct node [left right])

;; A [BinTree-of X] is one of:
;; - (make-leaf X)
;; - (make-node [BinTree-of X] [BinTree-of X])
;; Interpretation - a binary tree of values of type X which is either:
;; - a leaf containing a value
;; - or a node containing two binary trees

;; The goal of this homework will be to write abstractions for working with
;; binary trees, similar to the list abstractions you are familiar with.

;;! Problem 1

;;! Part A
;; Finish the data design for BinTree by writing examples and a template.

;;!! IMPORTANT: Write your response BELOW this line:

(define LEAF0 (make-leaf 0))
(define LEAF5 (make-leaf 5))
(define LEAF10 (make-leaf 10))
(define TREE1 (make-node LEAF5 LEAF10))
(define TREE2 (make-node TREE1 LEAF5))
(define TREE3 (make-node LEAF5 TREE2))

(define LEAF-A (make-leaf "A"))
(define LEAF-B (make-leaf "B")) 
(define STRING-TREE1 (make-node LEAF-A LEAF-B))
(define STRING-TREE2 (make-node STRING-TREE1 LEAF-A)) 
(define STRING-TREE3 (make-node LEAF-A STRING-TREE2))

;; bintree-template : [BinTree-of X] -> ?
(define (bintree-template bintree)
  (cond
    [(leaf? bintree) (... (leaf-val bintree))] 
    [(node? bintree) 
     (... (bintree-template (node-left bintree)) 
          (bintree-template (node-right bintree)))]))

;;! Part B
;;
;; Design a function called tree-zero that takes a binary tree and replaces all
;; leaf values with 0; the structure of the tree should remain the same.

;;!! IMPORTANT: Write your response BELOW this line:

;; tree-zero : [BinTree-of X] -> [BinTree-of Number]
;; replaces all leaf values in the binary tree with 0
(define (tree-zero bintree)
  (cond
    [(leaf? bintree) (make-leaf 0)]
    [(node? bintree) 
     (make-node (tree-zero (node-left bintree))
                (tree-zero (node-right bintree)))]))

;; Test Cases
(check-expect (tree-zero LEAF5) LEAF0)
(check-expect (tree-zero LEAF-A) LEAF0)
(check-expect (tree-zero TREE1) (make-node LEAF0 LEAF0))
(check-expect (tree-zero TREE2) (make-node (make-node LEAF0 LEAF0) LEAF0))
(check-expect (tree-zero STRING-TREE2) (make-node (make-node LEAF0 LEAF0) LEAF0))

;;! Part C
;;
;; Design a function tree-contains? that takes a String and a [BinTree-of
;; String] that returns whether the String exists as a leaf.

;;!! IMPORTANT: Write your response BELOW this line:

;; tree-contains? : String [BinTree-of String] -> Boolean
;; takes a string and a [BinTree-of String] and returns whether the string exists as a leaf
(define (tree-contains? str bintree)
  (cond [(leaf? bintree) (string=? (leaf-val bintree) str)]
        [(node? bintree) (or (tree-contains? str (node-left bintree))
                             (tree-contains? str (node-right bintree)))]))

;; Test Cases
(check-expect (tree-contains? "A" LEAF-A) #true)
(check-expect (tree-contains? "C" LEAF-A) #false)
(check-expect (tree-contains? "B" STRING-TREE2) #true)

;;! Part D
;;
;; Design a function tree-sum that takes a [BinTree-of Number] and returns the
;; sum of all the numbers in the tree.

;;!! IMPORTANT: Write your response BELOW this line:

;; tree-sum : [BinTree-of Number] -> Number
;; takes a [BinTree-of Number] and returns the sum of all the numbers in the tree
(define (tree-sum bintree)
  (cond [(leaf? bintree) (leaf-val bintree)]
        [(node? bintree) (+ (tree-sum (node-left bintree)) (tree-sum (node-right bintree)))]))

;; Test Cases
(check-expect (tree-sum TREE1) 15)
(check-expect (tree-sum TREE2) 20)
(check-expect (tree-sum TREE3) 25)

;;! Part E
;;
;; While trees have more structure, like lists, they still contain elements. To
;; demonstrate this similarity, design a function called tree-flatten that takes
;; a binary tree and converts it into a list. When flattening a node, the resulting list
;; should contain all leaves on the left side of the node before the leaves
;; on the right side of the node.

;; For example, the flattening the tree below should result in `(list 1 2 3 4 5 6)`:
;;           *
;;          / \
;;         *   6
;;        / \
;;       1   *
;;          / \
;;         *   5
;;        / \
;;       *   4
;;      / \
;;     2   3

;;!! IMPORTANT: Write your response BELOW this line:

;; tree-flatten : [BinTree-of X] -> [List-of X]
;; takes a binary tree and converts it into a list
(define (tree-flatten bintree)
  (cond [(leaf? bintree) (list (leaf-val bintree))]
        [(node? bintree) (append (tree-flatten (node-left bintree))
                                 (tree-flatten (node-right bintree)))]))

(check-expect (tree-flatten TREE1) (list 5 10))
(check-expect (tree-flatten TREE2) (list 5 10 5))
(check-expect (tree-flatten TREE3) (list 5 5 10 5))

;;! Part F

;; INTERPRETIVE QUESTION
;;
;; In some families -- maybe including yours -- people want to be able to
;; understand who their ancestors are and how they're related to their them.
;;
;; Family trees can be useful for that purpose. Family trees represent familial
;; relationships in certain ways. For example, a node represents a parent, and a
;; child represents... well, a child.
;;
;; Often, but not always, family trees represent (a) legally-recognized
;; relationships and (b) relationships in which the children are directly
;; genetically related to their parents. That means that children (or parents!)
;; resulting from any sort of relationship that doesn't meet both conditions (a)
;; and (b) might not be included in a family tree.
;;
;; Over time, ideas about what a "family" is, and about which familial relations
;; should be openly acknowledged and documented have changed. That means that a
;; family tree is both a "biological" record and a cultural one. Determining
;; citizenship (who is a citizen of a nation? who can apply for citizenship
;; based on family relationships?), finances (whose debts are you responsible
;; for paying?), medical decision-making authority (what decisions can someone
;; make on someone else's behalf?), and inheritance (who will automatically
;; inherit if there is no will?) all depend on definitions of who counts as
;; "family" or "relatives".
;;
;; As a result, decisions about who is a member of a family doesn't have only
;; emotional and psychological consequences; it also has political and legal
;; ones. Someone might be subjected to harms or eligible for benefits depending
;; on whether a law or policy considers them to be part of "the same family" as
;; someone else. What our language means, therefore, is quite important.
;;
;; If you had to design a data definition for a family tree, what kinds of
;; relation do you think it should represent? In other words, what counts as
;; "being part of the same family"? Please include a justification of your
;; answer in 2 - 3 sentences.


#|

If I were to design a data definition for a family tree, I would include both biological and legally
recognized relationships, while also accounting for adoptive and step relationships. This inclusive
approach acknowledges that family does not have to be genetically tied, but can be legally tied as
well.

|#

;;! Problem 2

;;! Part A
;;
;; Design a function called tree-map that takes a function and a binary tree and
;; applies the function to each value in the tree. The function should return a
;; new tree with the same structure as the original tree, but with the values
;; replaced by the result of applying the function to the value that was
;; originally there.

;;!! IMPORTANT: Write your response BELOW this line:

;; tree-map : (X Y) (X -> Y) [BinTree-of X] -> [BinTree-of Y]
;; takes a function and a binary tree and applies the function to each value in the tree
(define (tree-map f bintree)
  (cond
    [(leaf? bintree) (make-leaf (f (leaf-val bintree)))]
    [(node? bintree) 
     (make-node (tree-map f (node-left bintree))
                (tree-map f (node-right bintree)))]))

;; Test Cases
(check-expect (tree-map add1 LEAF5) (make-leaf 6)) 
(check-expect (tree-map sub1 TREE1) (make-node (make-leaf 4) (make-leaf 9)))
(check-expect (tree-map string-downcase LEAF-A) (make-leaf "a")) 
(check-expect (tree-map string-downcase STRING-TREE1) (make-node (make-leaf "a") (make-leaf "b")))

;;! Part B
;;
;; Design a function called tree-andmap that takes a predicate and determines whether all values in the
;; tree satisfy the predicate.

;;!! IMPORTANT: Write your response BELOW this line:

;; tree-andmap : (X -> Boolean) [BinTree-of X] -> Boolean
;; takes a predicate and binary tree and determines whether all values in the tree satisfy the
;; predicate
(define (tree-andmap predicate bintree)
  (cond
    [(leaf? bintree) (predicate (leaf-val bintree))]
    [(node? bintree) 
     (and (tree-andmap predicate (node-left bintree))
          (tree-andmap predicate (node-right bintree)))]))

;; Test Cases
(check-expect (tree-andmap number? TREE1) true)
(check-expect (tree-andmap (lambda (x) (< x 7)) TREE3) false)
(check-expect (tree-andmap string? STRING-TREE1) true)
(check-expect (tree-andmap (lambda (x) (equal? x "A")) STRING-TREE3) false)

;;! Part C
;;
;; Design a function called tree-ormap that takes a predicate and determines whether any values in the
;; tree satisfy the predicate.

;;!! IMPORTANT: Write your response BELOW this line:

;; tree-ormap : (X -> Boolean) [BinTree-of X] -> Boolean
;; takes a predicate and binary tree and determines whether any of the values in the tree satisfy
;; the predicate
(define (tree-ormap predicate bintree)
  (cond
    [(leaf? bintree) (predicate (leaf-val bintree))]
    [(node? bintree) 
     (or (tree-ormap predicate (node-left bintree))
         (tree-ormap predicate (node-right bintree)))]))

;; Test Cases
(check-expect (tree-ormap string? TREE1) false)
(check-expect (tree-ormap (lambda (x) (< x 7)) TREE3) true)
(check-expect (tree-ormap number? STRING-TREE1) false)
(check-expect (tree-ormap (lambda (x) (equal? x "A")) STRING-TREE3) true)

;;! Part D

;; Design a function tree-fold that acts like a fold over a tree. This takes in a function
;; to apply to leaf values, and a function to combine the results of folding subtrees.
;; These two functions should be used to compress a given tree down to
;; a single resulting value. It should have the following signature:

;; tree-fold : (X -> Y) (Y Y -> Y) [BinTree-of X] -> Y

;; where `(X -> Y)` is the function for the leaf, and
;; `(Y Y -> Y)` is the function to combine the results of folding subtrees.

;; A test is provided for clarity.

(check-expect (tree-fold string-upcase string-append
                         (make-node (make-leaf "hello") (make-leaf "world"))) "HELLOWORLD")

;;!! IMPORTANT: Write your response BELOW this line:

;; tree-fold : (X -> Y) (Y Y -> Y) [BinTree-of X] -> Y
;; takes in a function to apply to leaf values, and a function that combines the results of folding
;; subtrees
(define (tree-fold lf cb bintree)
  (cond
    [(leaf? bintree) (lf (leaf-val bintree))]
    [(node? bintree) 
     (cb (tree-fold lf cb (node-left bintree))
         (tree-fold lf cb (node-right bintree)))]))

;; Test Cases
(check-expect (tree-fold string-upcase string-append STRING-TREE2) "ABA")
(check-expect (tree-fold number->string string-append TREE1) "510")
(check-expect (tree-fold string-downcase string-append STRING-TREE1) "ab")

;;! Problem 3
;; Now we will use the tree-abstractions we defined in Problem 2 to reimplement the solutions in
;; Problem 1

;;! Part A
;;
;; Reimplement the tree-zero function using tree abstractions from problem 2; call it `tree-zero/v2`

;;!! IMPORTANT: Write your response BELOW this line:

;; tree-zero/v2 : [BinTree-of X] -> [BinTree-of Number]
;; replaces all leaf values in the binary tree with 0
(define (tree-zero/v2 bintree)
  (tree-map (lambda (x) 0) bintree))

;; Test Cases
(check-expect (tree-zero/v2 LEAF5) LEAF0)
(check-expect (tree-zero/v2 LEAF-A) LEAF0)
(check-expect (tree-zero/v2 TREE1) (make-node LEAF0 LEAF0))
(check-expect (tree-zero/v2 TREE2) (make-node (make-node LEAF0 LEAF0) LEAF0))
(check-expect (tree-zero/v2 STRING-TREE2) (make-node (make-node LEAF0 LEAF0) LEAF0))

;;! Part B
;;
;; Reimplement the tree-contains? function using tree abstractions from problem 2; call it `tree-contains?/v2`.

;;!! IMPORTANT: Write your response BELOW this line:

;; tree-contains?/v2 : String [BinTree-of String] -> Boolean
;; takes a string and a [BinTree-of String] and returns whether the string exists as a leaf
(define (tree-contains?/v2 str bintree)
  (tree-ormap (lambda (x) (string=? x str)) bintree))

;; Test Cases
(check-expect (tree-contains?/v2 "A" LEAF-A) #true)
(check-expect (tree-contains?/v2 "C" LEAF-A) #false)
(check-expect (tree-contains?/v2 "B" STRING-TREE2) #true)

;;! Part C
;;
;; Reimplement the tree-sum function using tree abstractions from problem 2; call it `tree-sum/v2`.

;;!! IMPORTANT: Write your response BELOW this line:

;; tree-sum/v2 : [BinTree-of Number] -> Number
;; takes a [BinTree-of Number] and returns the sum of all the numbers in the tree
(define (tree-sum/v2 bintree)
  (tree-fold (lambda (x) x) + bintree))

;; Test Cases
(check-expect (tree-sum/v2 TREE1) 15)
(check-expect (tree-sum/v2 TREE2) 20)
(check-expect (tree-sum/v2 TREE3) 25)

;;! Part D
;;
;; Reimplement the tree-flatten function using tree abstractions from problem 2;
;; call it `tree-flatten/v2`.

;;!! IMPORTANT: Write your response BELOW this line:

;; tree-flatten/v2 : [BinTree-of X] -> [List-of X]
;; takes a binary tree and converts it into a list
(define (tree-flatten/v2 bintree)
  (tree-fold list append bintree))

(check-expect (tree-flatten/v2 TREE1) (list 5 10))
(check-expect (tree-flatten/v2 TREE2) (list 5 10 5))
(check-expect (tree-flatten/v2 TREE3) (list 5 5 10 5))