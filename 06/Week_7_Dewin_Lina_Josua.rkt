#lang racket

(require 2htdp/image)

;Solution of exercise sheet 6,

;author:

;Dewin Bagci, 6815336
;Josua Spisak, 6944594
;Lina Kaine, 6499396


;;;1

;;1.1
#|
The functions take, drop and merge are linear because we have basic termination condition. we start with one
element and continue with its successor.
The basic termination for the functions take and drop is if xs is null and then we have the empty
list and in merge we have two of them because merge is also a tree recursion so  we
check for both xs and ys if they are null. if they arent null we continue with
the recursion and call take drop or merge until the termination condition is fullfilled.
in merge sort we also terminate if our input n is lesser than 2 but after that we dont use the
successor of our element but instead use half of it.

both merge and merge-sort are tree recursions as we use either merge or merge-sort two times during
one recursion.
take and drop arent tree-recursions because we have just one recursiv call

merge-sort is also a nested recursion because we call not only merge-sort but take drop and merge
as well
none of the other functions is nested as they only call themselves

all of the functions are direct because they dont mutually call each other
therefore none of them are indirect
we also dont have end-recursion.

;;1.2

merge and merge-sort are both higher order functions.
In merge we have a boolean condition as signature ( merge rel<?...).
In merge-sort we call the functions part1 and part2 with other functions (take / drop) as
agruments. Additionaly in merge-sort the return value is also a function.
|#

;;1.3

;naive-recursive
(define (take n xs)
   (cond
      ((null? xs) '())
      ((= 0 n) '())
      (else (cons (car xs)
                  (take (- n 1) (cdr xs))))))

;sample output
(take 3 '(a b c d)) ; -> '(a b c)

;tail-recursive
(define (take-acc n xs acc)
  (cond [(or (null? xs) (= 0 n)) acc]
        (else (take-acc (- n 1)
                        (cdr xs)
                        (cons acc (car xs))))))

(define (takeE n xs)
  (flatten (take-acc n xs '())))

;sample output
(takeE 3 '(a b c d)) ; -> '(a b c)

;;1.4
(define baum1 (above/align
                 "center"
                 ;; der Stern an der Spitze
                 (star-polygon 40 5 2 "solid" "gold")
                 ;; die Zweige
                 (ellipse 20 40 "solid" "darkgreen")
                 (ellipse 80 50 "solid" "darkolivegreen")
                 (ellipse 130 60 "solid" "olivedrab")
                 (ellipse 180 80 "solid" "darkgreen")
                 ;; der Stamm
                 (rectangle 40 60 "solid" "brown") ))

baum1