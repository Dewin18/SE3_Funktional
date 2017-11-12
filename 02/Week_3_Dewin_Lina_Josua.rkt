#lang racket

;Solution of exercise sheet 2,

;author:

;Dewin Bagci, 6815336
;Josua Spisak, 6944594
;Lina Kaine, 6499396

;[1 Symbole und Werte, Umgebungen]

;for testing
(define wuff 'Flocki)
(define Hund wuff)
(define Wolf 'wuff)

(define (welcherNameGiltWo PersonA PersonB)
  (let ((PersonA 'Zaphod)
        (PersonC PersonA))
    PersonC ))

(define xs1 '(0 2 3 wuff Hund))
(define xs2 (list wuff Hund))
(define xs3 (cons Hund wuff))

;1.1. wuff -> 'Flocki
;Function wuff is defined with the return value 'Flocki

;1.2. Hund -> 'Flocki
;Function Hund is defined as wuff, which leads to the function wuff with the return value​ ​'Flocki

;1.3. Wolf -> 'wuff
;Function Wolf is defined with the return value 'wuff

;1.4. (quote Hund) -> 'Hund 
;quote returns variables/functions as a symbol

;1.5. (eval Wolf) -> 'Flocki
;eval takes Wolfs value “wuff” and calculates it corresponding function Wuff, which has the returning value 'Flocki

;1.6. (eval Hund) -> undefined
;tries to evaluate Hund, but there is no Hund function, missing symbol (sign)

;1.7. (eval 'Wolf) -> 'wuff
;'Wolf  is evaluated directly, since a symbol is delivered

;1.8. (welcherNameGiltWo ’lily ’potter) -> 'lily 
;personA is saved in personC, personC is the return value. 'Zaphod doesn’t overwrite personA before 'lily is defined as personC since there is no let*.

;1.9. (cdddr xs1) -> '(wuff Hund) 
;starts at the third tail-element of the list xs1 and returns the rest of the list

;1.10. (cdr xs2) -> '(Flocki) 
;evaluates the tail '(wuff Hund). Both evaluates to 'Flocki and is returned as one element.

;1.11. (cdr xs3) -> 'Flocki 
;wuff evaluates to 'Flocki.

;1.12. (sqrt 1/4) -> 1⁄2
;calculates the square root

;1.13. (eval '(welcherNameGiltWo ’Wolf ’Hund)) -> 'Wolf 
;evaluates the list, which leads to the performance of the function welcherNameGiltWo.

;1.14. (eval (welcherNameGiltWo ’Hund ’Wolf )) -> 'Flocki 
;evaluates the results of the function welcherNameGiltWo and 'Hund turns to 'Flocki. 

;[2 Rechnen mit exakten Zahlen:]

;[2.1 Die Fakultät einer Zahl]

;FACULTY
(define (fak n)
 (if (= n 0)
     1
     (* n (fak (* (- n 1))))))

(fak 12)

;[2.2 Potenzen von Rationalzahlen]

;POWER - R^N
(define (power r n)
  (cond [(= n 0) 1]
        [(odd? n) (* r (power r (- n 1)))]  ; n = odd
        [else (sqr (power r (/ n 2)))]))    ; n = even 

(power 6 33)

;[2.3 Die Eulerzahl e:]

;recrsive subroutine - euler calculation
(define (eulerSum n)
  (if (<= n 1)
      n
      (+ (/ n (fak (- n 1)))
              (eulerSum (- n 1)))))

;EULER
(define e
  (let [(fractions 1000)]
  (/ (eulerSum fractions) 2)))


;prints the first 1000 digits of eulers e as integers
(* (power 10 1001) e)


;[2.4 π]

;PI
(define (my-pi n)
  (* 4 (piSum n)))

;recrsive subroutine - pi calculation
(define (piSum n)
  (if (= n 1)
      n
   (if (even? n)
      (piSum (+ n 1))
      (if (odd? (- (/ n 2) 0.5))
          (- (piSum (- n 2)) (/ 1 n))
          (+ (piSum (- n 2)) (/ 1 n))))))

;prints the first 1000 digits of pi as integers
(* (power 10 1001) (my-pi 1000))

;[3 Typprädikate]

(define (type-of input)
  (cond
    [(boolean? input)
     'boolean]
    [(pair? input)
     'pair]
    [(list? input)
     'list]
    [(symbol? input)
     'symbol]
    [(number? input)
     'number]
    [(char? input)
     'char]
    [(string? input)
     'String]
    [(vector? input)
     'vector]
    [(procedure? input)
     'procedure]
    [else 'error]))

;test queries for type-of
(type-of (* 2 3 4))
(type-of(not 42))
(type-of '( eins zwei drei))
(type-of '())
(define ( id z ) z)
(type-of( id sin))
(type-of(string-ref "SE3" 2))
(type-of(lambda (x) x))
(type-of type-of)
(type-of(type-of type-of))
       
      
