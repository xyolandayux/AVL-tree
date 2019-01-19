;; AVL Tree Code Version 1.1
;; Copyright Gordon V. Cormack, 2008, 2010
;; For use by students in CS145 Fall 2016, 2017, 2018
;; Any other use, or publication, prohibited
;; Implements a binary search tree with the operations
;;   empty            ;; an empty tree
;;   (node-left t)    ;; left subtree of non-empty tree
;;   (node-right t)   ;; right subtree of non-empty tree
;;   (node-key t)     ;; number labeling the root of t
;;   (insertavl t n)  ;; add number n to t, if not present
;;   (deleteavl t n)  ;; delete n from t, if present
;;   (listavl t)      ;; ordered list of elements in t
;;   (sizeavl t)      ;; the number of elements in t
;;
;; Running times as function of N, the size of the tree.
;;   node-left, node-right, node-key, sizeavl:  O(1)
;;   insertavl, deleteavl: O(log N)
;;   listavl: O(N)

;; Example
;;
;; (listavl 
;;   (deleteavl 
;;     (insertavl (insertavl (insertavl empty 3) 5) 7) 
;;     3))  ;; returns (5 7)

#lang scheme
(provide node-key node-left node-right) ;; node operations
(provide insertavl deleteavl listavl sizeavl)    ;; avl operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this is the implementation of the various AVL tree operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct node (key sz height left right))

(define (sizeavl t) (if (empty? t) 0 (node-sz t)))

(define (height t) (if (empty? t) 0 (node-height t)))

(define (build-node d L R) (make-node d (+ 1 (sizeavl L) (sizeavl R))(add1 (max (height L) (height R))) L R))

(define (subst-L t p) (build-node (node-key t) p (node-right t)))

(define (subst-R t p) (build-node (node-key t) (node-left t) p))

(define (raise_right t)
  (define tt (subst-R t (node-left (node-right t))))
  (subst-L (node-right t) tt))

(define (raise_left t)
  (define tt (subst-L t (node-right (node-left t))))
  (subst-R (node-left t) tt))

(define (rebalance t)
  (cond
    [(empty? t) t]
    [(> (height (node-left t)) (add1 (height (node-right t))))
       (raise_left 
         (if (> (height (node-right (node-left t))) (height (node-left (node-left t))))
             (subst-L t (raise_right (node-left t))) t))]
    [(> (height (node-right t)) (add1 (height (node-left t))))
       (raise_right 
         (if (> (height (node-left (node-right t))) (height (node-right (node-right t))))
             (subst-R t (raise_left (node-right t))) t))]
    [else t]))

(define (insertavl t d)
  (cond
    [(empty? t) (build-node d empty empty)]
    [(= d (node-key t)) t]
    [(< d (node-key t)) (rebalance (subst-L t (insertavl (node-left t) d)))]
    [else (rebalance (subst-R t (insertavl (node-right t) d)))]))

(define (get_rightmost t)
  (cond
    [(empty? (node-right t)) (node-key t)]
    [else (get_rightmost (node-right t))]))

(define (remove_rightmost t)
  (cond
    [(empty? (node-right t)) (node-left t)]
    [else (rebalance (subst-R t (remove_rightmost (node-right t))))]))

(define (deleteavl t d)
  (cond
    [(empty? t) t]
    [(< d (node-key t)) (rebalance (subst-L t (deleteavl (node-left t) d)))]
    [(> d (node-key t)) (rebalance (subst-R t (deleteavl (node-right t) d)))]
    [(empty? (node-right t)) (rebalance (node-left t))]
    [(empty? (node-left t)) (rebalance (node-right t))]
    [else
      (rebalance
        (build-node (get_rightmost (node-left t)) (remove_rightmost (node-left t)) (node-right t)))]))

(define (listavl t) (listavl-helper empty t))
(define (listavl-helper l t)
  (cond
    [(empty? t) l]
    [else (listavl-helper 
           (cons (node-key t) (listavl-helper l (node-right t)))
           (node-left t))]))