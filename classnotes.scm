#lang mzscheme

;;; This file will contain many of the code examples from class.  


;;;;;;;;;;;;;;;;;;;;;;;;
;;; THE BAICS ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;


;; define a function which squares a number: example of the basic syntax of scheme
(define (square x)
  (* x x))

;; simple predicate function (notice the ? in the name, scheme has a pleasantly large namespace)
(define (even? n)
  (= (remainder n 2) 0))

;; example of an if statement
(define (next-collatz n)
  (if (even? n)
      (/ n 2)
      (+ 1 (* n 3))))


;;;;;;;;;;;;;;;;;;
;;; LIST STUFF;;;;
;;;;;;;;;;;;;;;;;;


;; cons: takes two objects and makes a pair
;; the case you usually care about is when the first is some object and the second is a list
;; (cons 1 (cons 2 (cons 3 (cons 4 '()))) makes a list (1 2 3 4)

;; car: takes a pair, grabs the first thing from it
;; in the case of lists, takes the head of the list

;; cdr: takes a pair, grabs the second thing from it
;; in the case of lists, grabs the tail

;; '() represents the end of a list


;; computes the length of a list
(define (my-length l)
  (if (null? l)
      0
      (+ 1 (my-length (cdr l)))))

;; this version is "tail recursive"
;; it will use less memory, at the expense of (for now) having to write an extra function
;; when using explicit recursion, tail recursion is more efficient.  
(define (my-length-tail-rec l)
  (my-length-tail-rec* l 0))
(define (my-length-tail-rec* l res)
  (if (null? l)
      res
      (my-length-tail-rec* (cdr l) (+ res 1))))

;; accesses the nth element of a list
;; no error checking
(define (my-list-ref l n)
  (if (= n 0)
      (car l)
      (my-list-ref (- l 1) (cdr l))))

      
;; this reveres a list, but in a yucky way
(define (reverse-bad l)
  (reverse-bad* l '()))
(define (reverse-bad* l res)
  (if (null? l) ;; null? determines if an object is equal to the empty list
      res
      (reverse-bad* (cdr l)
                    (cons (car l) res))))
;; if this doesnt make sence, do it by hand, and see it does in fact make sence



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HIGHER ORDER FUNCTIONS ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; in scheme, functions are a data-type
;;; it should be obvious what the following function does, but maybe not obvious that you can do it
(define (apply-to-5 f)
  (f 5))
(define (twenty-five) (apply-to-5 square))

;;; you have four basic high order functions: map, fold-left, fold-right and filter (some people might include otherse, we'll see em)
;;; lets start with map
;;; map does this (map f (a_0 a_1 ... a_n)) = ((f a_0) (f a_1) ... (f a_n))
;;; it is the simplest approximation of iteration in functional programming
;;; we will define it a few different ways
(define (my-map f l)
  (if (null? l)
      '()
      (cons (f (car l)) (my-map f (cdr l)))))

(define (square-each l) (my-map square l))



;;; the next functions are fold-left and fold right, which we will first define mathematically
;;; let l = (a_0 a_1 ... a_{n-1}) be a list of length n
;;; let t_0 and s_0 be some values and f and g some function
;;; define t_k = (f t_{k-1} a_{k-1})
;;; define s_k = (g a_{n-k} s_{k-1}) 
;;; define (fold-left f t_0 l) = t_n
;;; define (fold-right g s_0 l) = s_n

(define (fold-right g s l)
  (if (null? l)
      s
      (g (car l) (fold-right g s (cdr l)))))

(define (fold-left f t l)
  (if (null? l)
      t
      (fold-left f (f t (car l)) (cdr l))))

;;; lets see an example, with fold-left and f = flip-cons
(define (flip-cons x y)
  (cons y x))
;;; let t_0 = '() and l = (1 2 3 4)
;;; t_1 = (flip-cons '() 1) = (cons 1 '()) = (1)
;;; t_2 = (flip-cons (1) 2) = (cons 2 (1)) = (2 1)
;;; t_3 = (flip-cons (2 1) 3) = (cons 3 (2 1)) = (3 2 1)
;;; t_4 = (flip-cons (3 2 1) 4) = (cons 4 (3 2 1)) = (4 3 2 1)

;;; we can now define reverse in a way that doesnt suck
(define (my-reverse l) (fold-left flip-cons '() l))

;;; how about my-append?  with generic lists we dont have tail pointers, so we have to walk through the first list
;;; this makes append expensive, but still reasonable to define
(define (my-append lst1 lst2) (fold-right cons lst2 lst1))
;;; it takes the last thing off the first list, shoves it on the front of the second list, and repeats
;;; quite pretty

;;; how about summing a list?
(define (sum-list l) (fold-right + 0 l))




;;; yaaaay lambda
(define (count pred lst)
  (fold-left (lambda (n x)
               (if (pred x)
                   (+ n 1)
                   n))
             0 lst))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LAMBDA + FUNCTION ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;


;;; builds a function which adds n to its argument
(define (make-adder n)
  (lambda (x) (+ n x)))
(define (identity x) x)

;;;; what opperations are defined on functions?
(define (compose f . gs)
  (if (null? gs)
      f
      (lambda xs 
        (f (apply (apply compose gs) xs)))))

;; a not-gate for functions
(define (f-not f)
  (compose not f))


;;; flips the arguments of a function, no more flip-cons for reverse
(define (flip f)
  (lambda (x y) (f y x)))
(define (my-reverse2 l) (fold-left (flip cons) '() l))

;;; i know some args now, will know the rest later!
;;; this is one of the most important functions!!!!
(define (curry f . xs)
  (lambda ys
    (apply f (append xs ys))))

(define ++ (curry + 1))
(define is? (curry curry eqv?))
(define zero? (is? 0))


;;; filter is a very hip higher order function
;;; kicks out anything not satisfying pred
(define (filter pred lst)
  (fold-right (lambda (x m)
                (if (pred x)
                    (cons x m)
                    m))
              '() lst))

;;; hey, enough messing around, lets write a real program!
(define (q-sort compar lst)
  (if (null? lst)
      '()
      (append (q-sort compar (filter (curry compar (car lst)) (cdr lst)))
              (list (car lst))
              (q-sort compar (filter (f-not (curry compar (car lst))) (cdr lst))))))


        



