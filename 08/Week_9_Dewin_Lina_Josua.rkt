#lang racket

(require racket/trace)

;Solution of exercise sheet 8,

;author:

;Dewin Bagci, 6815336
;Josua Spisak, 6944594
;Lina Kaine, 6499396

;;;1

(display "EXERCISE 1 SAMPLE OUTPUT \n")

;;1.1
#|A racket function is a higher order function when it uses one ore more functions as arguments or the return
value of a function is still a function/operation.|#

;;1.2
;(a)
#|The semantic of this function is incorrect. It first checks the input argument for an Integer or not.
If it is true it checks the Integer value for odd and print the opposite. This is not a higer order function. The function call.
|#
(define (gerade-oder-ungerade x)
(if (integer? x)
    (if (odd? x )
        'gerade
        'ungerade)
    'keinInteger))

(gerade-oder-ungerade 1)

;(b)
;The return value of map is procedure that means map is a higher order function.
map

;(c)
#|Returns a list of pairs where each element of xs are assignet to an element of ys. The assignet is index based. 0 -> 0, 1 -> 1...
Function is is not a higher order function since we use concrete lists as arguments and the function returns a list. 
|#
(define (erstelle-assoziationsliste xs ys)
  (map cons
       xs
       ys))

(erstelle-assoziationsliste '(1 2 3) '(a b c))

;(d)
#|This functions returns an operation/procedure depends on the input arguments and is therefore a higher order function.
If x is less than y the procedure will be <. If x is greater than y the procedure will be > otherwise the procedure is =.|#
( define (ermittle-vergleichsoperation x y)
   (cond [(< x y) <]
         [(> x y) >]
         [(= x y) =]))

(ermittle-vergleichsoperation 1 2)

;(e)
#|This is a higher order function since we can use the paramater f for different operations/procedures.|#
(define (schweinchen-in-der-mitte f arg1)
  (lambda (arg2 arg3) (f arg2 arg1 arg3 )))

;If we use list as argument procedure and racket evaluates '(1 4 3).
((schweinchen-in-der-mitte list 4) 1 3)

;If we use + as argument procedure and racket evaluates the return value as the number 8.
((schweinchen-in-der-mitte + 4) 1 3)

;;1.3
#|The example from (e) creates the list '(1 4 3). From the function definition we can see that f and arg1 are bound var. regarding to the
definition schweinchen-in-der-mitte. The scope of f and arg1 is the complete function. In the body of the functions definition we see the var. arg2 and arg3 which
are bound to the lambda expression. Since we have only bound var. they are all replaceable with each function call.
|#

;;1.4
(foldl (curry + 2) 3 '(3 4 5))

(map gerade-oder-ungerade  '(4 587 74 69 969 97 459 4))

(filter number?  '((a b) () 1 (()) 4 -7 "a"))

((compose(curry foldl + 0) (curry filter (curryr < 0 ))) '(5682 48 24915 -45 -6 48))

;;;2

(display "EXERCISE 2 SAMPLE OUTPUT \n")

;Basic higher order function for all list modifications in this exercise.
(define (modify-all f xs)
  (filter number? (map (curry f)  xs)))

;Function for 2.2. Returns the number if the number modulo 11 or 9 is 0, #void otherwise.
(define (mod-11-9 number)
  (cond [(or (= 0 (modulo number 11))
             (= 0 (modulo number 9))) number]))


;TODO

;2.1
(modify-all sqr '(1 2 3 4 5))

;2.2
(modify-all mod-11-9 '(11 9 1 12 22 1 18 23 27 88))

