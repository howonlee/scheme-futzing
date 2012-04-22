#lang mzscheme

;;; Code examples from class from Joseph 

;; definition of a function. this squares a number.
(define (square x)
  (* x x))

;; simple predicate function. this sees if number is even.
(define (even? n)
  (= (remainder n 2) 0))

;; example of an if statement
;; format: (if pred ifpredistrue ifpredisfalse)
(define (next-collatz n)
  (if (even? n)
      (/ n 2)
      (+ 1 (* n 3))))
	  
;;; LIST MANIPULATION;;;

;; cons: takes two objects and makes a pair
;; you can do this recursively, so second object usually ends up being a list itself
;; (cons 1 (cons 2 (cons 3 (cons 4 '()))) makes a list (1 2 3 4)

;; car: takes a pair, grabs the first thing from it
;; in the case of lists, takes the head of the list. so remember, pairs are lists themselves

;; cdr: takes a pair, grabs the second thing from it
;; in the case of lists, grabs the tail of the list.

;; '() represents the end of a list, or the empty list

;; computes the length of a list
(define (my-length l)
  (if (null? l)
      0
      (+ 1 (my-length (cdr l)))))

;; that was ok. however, it could be better.
;; the following version is tail-recursing
;; it will use less memory, at the expense of (for now) having to write an extra function
;; when using explicit recursion, tail recursion is more efficient.  
(define (my-length-tail-rec l)
  (my-length-tail-rec* l 0))
(define (my-length-tail-rec* l res)
  (if (null? l)
      res
      (my-length-tail-rec* (cdr l) (+ res 1))))

;; this accesses the nth element of a list
;; no error checking
(define (my-list-ref l n)
  (if (= n 0)
      (car l)
      (my-list-ref (- l 1) (cdr l))))

      
;; this reverses a list, but in a yucky way
(define (reverse-bad l)
  (reverse-bad* l '()))
(define (reverse-bad* l res)
  (if (null? l) ;; null? determines if an object is equal to the empty list
      res
      (reverse-bad* (cdr l)
                    (cons (car l) res))))
;; if this doesnt make sense, do it by hand, and see it does in fact make sense


;;; HIGHER ORDER FUNCTIONS ;;;

;;; in scheme, functions are a data-type
;;; it should be obvious what the following function does, but it is maybe not obvious that you can do it
(define (apply-to-5 f)
  (f 5))
(define (twenty-five) (apply-to-5 square))

;;; you have four basic high order functions: map, fold-left, fold-right and filter (some people might include others, we'll see em)
;;; lets start with map
;;; map does this (map f (a_0 a_1 ... a_n)) = ((f a_0) (f a_1) ... (f a_n))

;;; in other words, it takes a list and a function, and applies the function
;;; to each member of the list

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

;;; how about summing a list?
(define (sum-list l) (fold-right + 0 l))

;;; lambda can be used, and it's anonymous
(define (count pred lst)
  (fold-left (lambda (n x)
               (if (pred x)
                   (+ n 1)
                   n))
             0 lst))

;;; USING LAMBDA ;;;

;;; builds a function which adds n to its argument
(define (make-adder n)
  (lambda (x) (+ n x)))
(define (identity x) x)

;;;; what operations are defined on functions?
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

;;; i know some args now, will know the rest later
(define (curry f . xs)
  (lambda ys
    (apply f (append xs ys))))

(define ++ (curry + 1))
(define is? (curry curry eqv?))
(define zero? (is? 0))

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

			  ;;; list breaking functions:

;;; take builds a new list from the first n elements of the given list, like so:
;;; (take 3 '(1 2 3 4 5 6 7 8)) = (1 2 3)
(define (take n lst)
  (if (= n 0)
      `()
      (cons (car lst)  (take (- n 1) (cdr lst)))
  ))

;;; drop is the "dual" of take
;;; (drop 3 '(1 2 3 4 5 6 7 8)) = (4 5 6 7 8)
;;; you have the law that (append (take n l) (drop n l)) = l
(define (drop n lst)
  (if (= n 0)
      lst
      (cdr (drop (- n 1) lst))
  )
)

;;; take while is like take, but takes until a predicate is false
;;; (take-while even? '(0 2 4 1 3 4 5 1)) = (0 2 4)
(define (take-while pred lst)
  (if (pred (car lst))
      (cons (car lst)  (take-while pred (cdr lst)))
      `()
  ))

;;drop while is the opposite of take-while
(define (drop-while pred lst)
  (if (pred (car lst))
      (drop-while pred (cdr lst))
      lst
  )
)

;;; break is extremely powerful.  Basically, it finds the first time a predicate is true and breaks the list into two parts at that predicate
;;; (break even? '(1 3 2 4 5 2 4 6)) = ((1 3) . (2 4 5 2 4 6))
(define (break pred lst) (list
                          (take-while pred lst)
                          (drop-while pred lst)
                          )
  )

;;; other things to try: take-last, drop-last, cycle, partition, group-by, etc

;;; function stuff

;;; remember flip-cons?  We don't need to define that separately any more
;;; try writing flip, given ((flip f) x y) = (f y x)
(define (flip f)
  (lambda (x y) (f y x)))

;;; go ahead and define f-and, f-or
;;; ((f-and f g) x) = (and (f x) (g x))
(define (f-and f g)
  (lambda (x)
    (and (f x) (g x))))
(define (f-or f g)
  (lambda (x)
    (or (f x) (g x))))

;;; write a version of fold-left and fold-right that have a new feature: the ability to stop
;;; what it should do is the second argument, stop-when, should be applied to each argument of the list before continuing.
;;; you stop when you get to the end of the list or when stop-when returns true, at which point you return the accumulator
;;; it allows for easy bail-out of folds
;;; you probably want to copy and paste fold-left and fold-right and modify them
(define (fold-left-stop f stop-when accum lst)
  (if (or (null? lst) (stop-when (car lst)))
      accum
      (fold-left-stop f stop-when (f accum (car lst)) (cdr lst))))
(define (fold-right-stop f stop-when accum lst)
  (if (or (null? lst) (stop-when (car lst)))
      accum
      (f (car lst) (fold-right-stop f stop-when accum (cdr lst)))))

;;; use those to define searching functions :)
;;; find returns the first thing in the list which satisfies pred, and #f otherwise
(define (not-drop-while pred lst)
  (if (not (pred (car lst)))
      (not-drop-while pred (cdr lst))
      lst
  ))

(define (find pred lst)
  (car (not-drop-while pred lst)))
;;; other sorts of find functions are member?, which returns true if the first argument is in the list
;;; also consider writing some functions on associated lists, aka alists.  These are lists of cons cells where the car of each cell is a key and the cdr is a value
;;; alists can be rough maps that work great on small domains
