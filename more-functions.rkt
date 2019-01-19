;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname A6f) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "avl-implementation.rkt")

;; This function defines an empty set
(define emptyset empty)

;; This function checks if s is an emptyset or not using the build-in function empty?.
;; Since it is called once no matter the size of the input, the running time is O(1).
(define (emptyset? s)
  (if (empty? s) true false))

;; This function makes an avl tree with the single element n
;; Since it is called once with a single input, the running time is O(1)
(define (singleton n)
  (insertavl empty n))

;; This function returns the number of elements in s.
;; This is done by calling the provided function (siveavl t).
;; Since it is called once, the running time is O(1)
(define (size s) (sizeavl s))

;; This function returns a set containing all the elements in s1 and s2.
;; This is done by inserting all the elements of the smaller set into the larger set. using the provided function (insertavl t d) and (listavl t).
;; Therefore the running time of (union s1 s2) depends on the input size of sets s1 and s2 where
;;                                                             N = (min (size s1) (size s2)) and
;;                                                             M = (max (size s1) (size s2)),
;;                                            Total running time = O(N * log(M)).
(define (union s1 s2)
  (cond
    [(empty? s1) s2]
    [(empty? s2) s1]
    [(< (sizeavl s1)(sizeavl s2)) (union1 s2 s1)]
    [else (union1 s1 s2)]))

;; This function convert the smaller set into a list using the provided function,
;;                                           (listavl t), with O(N) running time.
(define (union1 s1 s2)
  (union2 s1 (listavl s2)))

;; This function inserts the list of elements of the smaller set into the larger set using the provided function,
;;                                                                   (insertavl t d), with O(log M) running time.
(define (union2 t s)
  (if (empty? s) t (union2 (insertavl t (first s))(rest s))))

;; This function returns a set of elements that are in both s1 and s2.
;; This is done by checking if any of the elements of the smaller set is in the larger set, if it is, then add the element to a new set.
(define (intersection s1 s2)
  (cond
    [(or (empty? s1)(empty? s2)) empty]
    [(< (sizeavl s1)(sizeavl s2))(intersection1 s1 s2 0 empty)]
    [else (intersection1 s2 s1 0 empty)]))

;; This function checks if any element of the smaller set is in the bigger set.
;; This is done by comparing every term of smaller set with the elements in the larger set.
;; The running time is O(N) as it called once for every term in the set.
(define (intersection1 s1 s2 n acc)
  (cond
    [(= n (sizeavl s1)) acc]
    [else (intersection1 s1 s2 (add1 n)
                         (if (empty? (intersection2 (nth s1 n) s2)) acc
                         (insertavl acc (intersection2 (nth s1 n) s2))))]))

;; This function checks if an single element of the s1 is in s2.
;; This is done by traversing s2 and comparing keys of s2 to the single element, if found, return the element, if not, return empty.
(define (intersection2 n s2)
  (cond
    [(= n (node-key s2)) n]
    [else
     (cond
      [(and (empty? (node-left s2)) (empty? (node-right s2))) empty]
      [(< n (node-key s2))
       (if (empty? (node-left s2)) empty
           (intersection2 n (node-left s2)))]
      [else (if (empty? (node-right s2)) empty
                        (intersection2 n (node-right s2)))])]))

;; This function returns the difference of set s1 and set s2.
;; This is done by removing the repeated elements of s2 from s1 using the provided function (insertavl t d).
;; Therefore the running time of (difference s1 s2) depends on the input size of sets s1 and s2 where
;;                                                               N = (min (size s1) (size s2)) and
;;                                                               M = (max (size s1) (size s2)),
;;                                              Total running time = O(N * log(M)).
(define (difference s1 s2)
  (cond
    [(empty? s1) empty]
    [(empty? s2) s1]
    [else (difference1 s1 s2)]))

;; This function convert s1 into a list using the provided function,
;;                                           (listavl t), with O(N) running time.
(define (difference1 s1 s2)
  (difference2 s2 (listavl s1) empty))

;; This function inserts elements in s1 but not in s2 into a new set.
;; This is done by checking if the list of elements in s1 is in s2,
;;     if it is, leave it, if not, add it to the new set.
;;  Using the provided function (insertavl t d), with O(log M) running time.
(define (difference2 s2 lst1 acc)
(cond
  [(empty? lst1) acc]
  [(t-member s2 (first lst1)) (difference2 s2 (rest lst1) acc)]
  [else (difference2 s2 (rest lst1) (insertavl acc (first lst1)))]))

;;This function tests whether the element n is in s.
(define (t-member s n)
  (cond
    [(empty? s) false]
    [(< n (node-key s)) (t-member (node-left s) n)]
    [(> n (node-key s)) (t-member (node-right s) n)]
    [else true]))

;; This returns the ith element of set s.
;; This is done using the provided function (sizeavl t) with running time at max O(N) for it is called once for every term before reaching the desired one.
(define (nth s i)
  (cond
    [(empty? s) empty]
    [(= (sizeavl (node-left s)) i) (node-key s)]
    [(< (sizeavl (node-left s)) i) (nth (node-right s) (- i (add1 (sizeavl (node-left s)))))]
    [else (nth (node-left s) i )]))
