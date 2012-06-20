#lang mzscheme

(require "classnotes.scm")
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


;;; (break even? '(1 3 2 4 5 2 4 6)) = ((1 3) . (2 4 5 2 4 6))
(define (break pred lst) (list
                          (take-while pred lst)
                          (drop-while pred lst)
                          )
  )

;;; other things to try: take-last, drop-last, cycle, partition, group-by, etc


;;; function stuff

;;; try writing flip, given ((flip f) x y) = (f y x)
(define (flip f)
  (lambda (x y) (f y x)))

;;; in class we defined f-not
;;; go ahead and define f-and, f-or
;;; ((f-and f g) x) = (and (f x) (g x))
(define (f-and f g)
  (lambda (x)
    (and (f x) (g x))))
(define (f-or f g)
  (lambda (x)
    (or (f x) (g x))))
(define (fold-left-stop f stop-when accum lst)
  (if (or (null? lst) (stop-when (car lst)))
      accum
      (fold-left-stop f stop-when (f accum (car lst)) (cdr lst))))
(define (fold-right-stop f stop-when accum lst)
  (if (or (null? lst) (stop-when (car lst)))
      accum
      (f (car lst) (fold-right-stop f stop-when accum (cdr lst)))))

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

