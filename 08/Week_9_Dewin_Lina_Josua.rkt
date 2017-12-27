#lang racket

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
;This evaluates to 21 the 3 is basicly our starting point and then we use our procedure wich is
;curry + 2 on each element from the list starting with the left one so 3 + (5 + 2) = 10 + (4 + 2) = 16 + (3 + 2) = 21
(foldl (curry + 2) 3 '(3 4 5))

;evaluates to (ungerade gerade ungerade...) we use our function on each element.
;The value 4 is even so for some reason we call it ungerade wich means odd wich is odd
(map gerade-oder-ungerade  '(4 587 74 69 969 97 459 4))

;evaluates to all numbers in the list so 1 4 and -7
(filter number?  '((a b) () 1 (()) 4 -7 "a"))

#|This evaluates to -51.
we start with filtering each element of the list wich is less then 0 so we get -45 and -6 then we fold this wich gets us -51.
The compose is taking the procedure wich filters the elements and adds them and uses it on the given list|#
((compose(curry foldl + 0) (curry filter (curryr < 0 ))) '(5682 48 24915 -45 -6 48))

;;;2

(display "EXERCISE 2 SAMPLE OUTPUT \n")

;2.1 .Basic higher order function for all list modifications in this exercise.
(define (modify-all f xs)
  (filter number? (map (curry f)  xs)))

;Helper function for 2.2. Returns the number if the number modulo 11 or 9 is 0, #void otherwise.
(define (mod-11-9 number)
  (cond [(or (= 0 (modulo number 11))
             (= 0 (modulo number 9))) number]))

;Helper function for 2.3 Returns the number if the number is even and greater than 6, #void otherwise
(define (odd-bigger-6 number)
  (cond [(and (odd? number)
              (< 6 number) number)]))

;Function for 2.4. Split a list in two sublists for given predicate. The first sublist contains all elements that
;fits the predicate and the second sublist all other elements.
(define (split-by-predicate predicate list)
  (let* ([filtered (filter predicate list)]
         [unfiltered (remove* filtered list)])
    (cons filtered
          (cons unfiltered '()))))

;2.1
(modify-all sqr '(1 2 3 4 5))

;2.2
(modify-all mod-11-9 '(11 9 1 12 22 1 18 23 27 88))

;2.3
(modify-all odd-bigger-6 '(1 2 4 5 6 7 8 9 10 11 12 13 43 44 55 71))

;2.4
(split-by-predicate even? '(1 2 3 4 5))
(split-by-predicate number? '(a 2 b c 5))
(split-by-predicate boolean? '(#t #t #f 4 5))
(split-by-predicate string? '(a "b" 3 "c" f #f))

;;;3

(require se3-bib/setkarten-module)
(display "EXERCISE 3 SAMPLE OUTPUT \n")

;;3.1
(define (gamemap karte1 karte2 karte3 karte4 karte5 karte6 karte7 karte8 karte9 karte10 karte11 karte12)
  (list karte1 karte2 karte3 karte4 karte5 karte6 karte7 karte8 karte9 karte10 karte11 karte12))

;;3.2
(define (karthProduct list1 list2 list3)
  (if (empty? list1)
      list3
      (karthProduct (cdr list1) list2 (append  (map (curry list (car list1)) list2) list3 ))))

(define generategamecards
  (let ([colors (list 'red 'green 'blue)]
        [modes (list 'outline 'solid 'hatched)]
        [patterns (list 'waves 'oval 'rectangle)]
        [numbers (list 1 2 3)])
    (let ([set (karthProduct numbers (karthProduct patterns (karthProduct modes colors '()) '() ) '() )])
      set)))

(define (flattenlistoflists list)
  (if (empty? list)
      '()
      (cons (flatten (car list)) (flattenlistoflists (cdr list)))))

(define gamecards
  (flattenlistoflists generategamecards))

(define (showOneGamecard list)
  (show-set-card (first list) (second list) (third list) (fourth list)))
    
(showOneGamecard (car gamecards))

(define showAllGamecards
  (map showOneGamecard gamecards))

showAllGamecards

;;3.3
(define (is-a-set? card1 card2 card3)
  (and
       (compareAttribute first card1 card2 card3)
       (compareAttribute second card1 card2 card3)
       (compareAttribute third card1 card2 card3)
       (compareAttribute fourth card1 card2 card3)))

(define (compareAttribute f card1 card2 card3)
       (or (and (eq? (f card1) (f card2))
                (eq? (f card1) (f card3)))
           (and (not (eq? (f card1) (f card2)))
                (not (eq? (f card2) (f card3)))
                (not (eq? (f card1) (f card3))))))

(is-a-set? '(2 red oval hatched) '(2 red rectangle hatched) '(2 red wave hatched))
(is-a-set? '(2 red rectangle outline) '(2 green rectangle outline) '(1 green rectangle solid))

;;4
#|
(define (elementsfromlist n inlist outlist)
  (if (= n 0)
      (reverse outlist)
      (elementsfromlist (- n 1) (cdr inlist) (cons (car inlist) outlist))))

(define (findsetsfromfield list)
  (findsetsfromfield (cons (is-a-set? (first list)(second list) (car(cdr(car(cdr(car list)))))) (cdr list))))

(define findsets
  (let
      ([playfield  (elementsfromlist 12 (shuffle gamecards) '())])
        (findsetsfromfield (karthProduct playfield (karthProduct playfield playfield '()) '()))))
|#
;;instead of first second and so on in findsetsfrom field i need  to car the list take the first 4 elements for first the next 4 for second and so on



