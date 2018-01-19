#lang racket

;;;1
;;1
;1
(define (xy->index x y)
  (+ (* y 9) x))

(xy->index 0 0)
(xy->index 3 1)
(xy->index 8 8)
;2
(define zeilListe '(0 1 2 3 4 5 6 7 8))
(define (zeile->indizes y)
  (map   (curry + (* y  9)) zeilListe))

(zeile->indizes 6)
(zeile->indizes 7)
(zeile->indizes 8)

(define spaltListe '(0 9 18 27 36 45 54 63 72))
(define (spalte->indizes x)
  (map   (curry + x) spaltListe))

(spalte->indizes 6)

(define (quadrant->indizes q)
  (cond [(< q 3) (quadrantIndizes 0 q)]
        [(< q 6) (quadrantIndizes 27 (- q 3))]
        [else (quadrantIndizes 54 (- q 6))]))

(define (quadrantIndizes s q)
  (let ([x (* q 3)])
  (list (+ s x) (+ s x 1) (+ s x 2) (+ s x 9) (+ s x 10) (+ s x 11) (+ s x 18) (+ s x 19) (+ s x 20))))

(quadrant->indizes 8)

;3
;input: list and position
;returns element at the position from the list
(define (xthElementFromList list x)
  (cond [(= x 0) (car list)]
        [(= x 1) (second list)]
        [else (car (drop list  x))]))

(define (spiel->eintraege spiel indexs)
  (let ([d (car indexs)]
        [s (if (vector? spiel)
               (vector->list spiel)
               spiel)])
  (list->vector (map (curry xthElementFromList s) indexs))))

(define spiel (vector
                0 0 0 0 0 9 0 7 0
                0 0 0 0 8 2 0 5 0
                3 2 7 0 0 0 0 4 0
                0 1 6 0 4 0 0 0 0
                0 5 0 0 0 0 3 0 0
                0 0 0 0 9 0 7 0 0
                0 0 0 6 0 0 0 0 5
                8 0 2 0 0 0 0 0 0
                0 0 4 2 0 0 0 0 8))
'#(6 8 5 4 3 9 2 7 1
   4 9 1 7 8 2 6 5 3
   3 2 7 5 6 1 8 4 9
   9 1 6 3 4 7 5 8 2
   7 5 8 1 2 6 3 9 4
   2 4 3 8 9 5 7 1 6
   1 3 9 6 7 8 4 2 5
   8 6 2 9 5 4 1 3 7
   5 7 4 2 1 3 9 6 8)
(spiel->eintraege spiel (quadrant->indizes 8))
(spiel->eintraege spiel (spalte->indizes 8))
(spiel->eintraege spiel (zeile->indizes 8))

;4
;checks if all the parts of a sudoku are konsitent and returns true if they are
(define (spiel-konsistent? spiel)
  (and (konsistent? spiel 8 spalte->indizes) (konsistent? spiel 8 zeile->indizes)(konsistent? spiel 8 quadrant->indizes)))
;helps spiel-konsitent
;input spiel a number and a function
;returns boolean
(define (konsistent? spiel n f)
  (if (= n 0)
      (not (check-duplicates (filter (curry < 0) (vector->list(spiel->eintraege spiel (f n))))))
      (and (not (check-duplicates (filter (curry < 0)(vector->list (spiel->eintraege spiel (f n)))))) (konsistent? spiel (- n 1) f))))

(spiel-konsistent? spiel)

(define (spiel-geloest? spiel)
  (and (spiel-konsistent? spiel) (equal? false (vector-member 0 spiel))))

(spiel-geloest? spiel)
;;2
;1
;input a sudoku and a number
;returns a copied version of the sudoku in wich every place that was undefinded before and cant host the number is replaced with aX
(define (markiere-ausschluss spiel number)
  (let [(Xspiel (vector-copy spiel))]
    (schliesse-aus Xspiel number)))

(require racket/trace)
;input a sudoku a list of indizes a vector of numbers and something irrelevant
;returns the sudoku with every 0 in the vector replaced with a X
(define (ZerotoX Xspiel indizes numbers shit)
  (let [(Mspiel (vector->mutable Xspiel))]
  (if (empty? indizes)
      Mspiel
      (if (equal? '#(0) (vector-take numbers 1))
            (ZerotoX Mspiel (cdr indizes) (vector-drop numbers 1)(vector-set! Mspiel (car indizes) 'X))
          (ZerotoX Mspiel (cdr indizes) (vector-drop numbers 1) 'b)))))

;makes a mutable vector out of a vector
(define (vector->mutable vector)
  (list->vector (vector->list vector)))
;all indizes for a sudoku
(define vectorIndizes (flatten (append (zeile->indizes 0) (zeile->indizes 1)(zeile->indizes 2)(zeile->indizes 3)(zeile->indizes 4)(zeile->indizes 5)(zeile->indizes 6)(zeile->indizes 7)(zeile->indizes 8))))

;input a sudoku and a number
;returns a sudoku wich marked every position the number cant be
(define (schliesse-aus spiel number)
  (sindMoeglich? spiel number 8))
;input a sudoku a number and the number of itterations
;checks the diffrent rules of a sudoku for each quadrant/line/column from the itteration to 0
(define (sindMoeglich? spiel number itterations)
    (if (< 0 itterations)
        (sindMoeglich? (Moeglich (Moeglich (Moeglich spiel number itterations quadrant->indizes) number itterations spalte->indizes) number itterations zeile->indizes) number (- itterations 1))
        (Moeglich (Moeglich (Moeglich spiel number 0 quadrant->indizes) number 0 spalte->indizes) number 0 zeile->indizes)))
;helps sindMoeglich and checks one of the things to check
(define (Moeglich spiel number itterations func)
  (let [(indi (func  itterations))
        (Numbers (spiel->eintraege spiel (func  itterations)))]
    (if (vector-member number Numbers)
        (ZerotoX spiel indi Numbers 'b)
        spiel)))
(markiere-ausschluss spiel 5)

;2
;input a sudoku and a number
;returns every position wwhere the number has to be
(define (eindeutige-positionen spiel number)
  (let [(Xspiel (markiere-ausschluss spiel number))]
    (remove-duplicates (sort (eindeutigePos Xspiel number 8) < ))))
;helps eindeutige-positon by checking columns lines and quadrants
(define (eindeutigePos spiel number itterations)
  (if (< 0 itterations)
      (append (eindeutig spiel number itterations zeile->indizes) (eindeutig spiel number itterations spalte->indizes) (eindeutig spiel number itterations quadrant->indizes) (eindeutigePos spiel number (- itterations 1)))
      (append (eindeutig spiel number itterations zeile->indizes) (eindeutig spiel number itterations spalte->indizes) (eindeutig spiel number itterations quadrant->indizes))))

;helps eindeutigePos by checking a specific part
(define (eindeutig spiel number itterations func)
  (let [(indi (func  itterations))
        (Numbers (spiel->eintraege spiel (func  itterations)))]
    (if (equal? (vector 0) (vector-filter (curry eq? 0) Numbers))
        (list (indizefromRef Numbers indi))
        '())))
;input a vector and a indize
;returns the position of the vector where the first 0 is
(define (indizefromRef vectore  indize)
      (if (equal? (vector 0) (vector-take vectore 1))
      (car indize)
     (indizefromRef (vector-drop vectore 1) (cdr indize))))

(eindeutige-positionen spiel 5)

;3
;if the sudoku is solved we return it ese we call try-loesen wich tries to solve it
(define (loese-spiel spiel)
  (if (spiel-geloest? spiel)
      spiel
      (try-loesen spiel)))
;input a sudoku
;uses a recrusiv function to fill every obvious position with the relative number
(define (try-loesen spiel)
  (let [(einer (eindeutige-positionen spiel 1))
        (zweier (eindeutige-positionen spiel 2))
        (dreier (eindeutige-positionen spiel 3))
        (vierer (eindeutige-positionen spiel 4))
        (fünfer (eindeutige-positionen spiel 5))
        (sechser (eindeutige-positionen spiel 6))
        (sibener (eindeutige-positionen spiel 7))
        (achter (eindeutige-positionen spiel 8))
        (neuner (eindeutige-positionen spiel 9))]
    (if (empty? (append einer zweier dreier vierer fünfer sechser sibener achter neuner)) ;checks wheter a obvious number still exists
        "Geht leider nicht ohne Backtracking"
        (letrec
            ([fügeEindeutigeEin
              (lambda (number nothing) ;again the problem with vector-set! wich returns void but we have to use it somewhere
                (if (< number 10) ;we start from 0 to 9
                    (if (empty? (eindeutige-positionen spiel number)) ;if ther is no obvious positon for the number we try the next higher one
                        (fügeEindeutigeEin (+ 1 number) spiel)
                        (fügeEindeutigeEin number (vector-set! spiel (car (eindeutige-positionen spiel number)) number))) ;if ther is a obvious position we fill it with the number
                    (loese-spiel spiel)))]);once weve gone throug all of the number we call loese-spiel to see about our next step
        (fügeEindeutigeEin 0 spiel)))));basic call for our function starts with 0
;;3
(require 2htdp/image)
;drawings
(define undefined
  (rectangle 10 10 "outline" "black"))
(define annoted
  (rectangle 10 10 "solid" "red"))
(define (numberPlace number)
  (overlay
   (text (number->string number) 7 "black")
   undefined))
;if we have a number we display it in the box else we dont
(define (zeichne-box spiel pos)
  (cond [(= 0 (vector-ref spiel pos)) undefined]
        [(equal? 'X (vector-ref spiel pos)) annoted]
        [else (numberPlace (vector-ref spiel pos))]))
;draws the gamestate
(define (zeichne-spiel spiel)
  (let ([empty (empty-scene 0 0)])
    (above
     (zeichneReihen spiel 0)
     empty)))
;helps the function zeichne-spiel
(define (zeichneReihen spiel counter)
  (if (< counter 9)
         (above
       (zeichne-Platz spiel (zeile->indizes counter) (empty-scene 0 0))
       (zeichneReihen spiel (+ counter 1)))
         (empty-scene 0 0)))
          
;helps zeichneReihen
(define (zeichne-Platz spiel indizes drawing)
  (if (empty? indizes)
        drawing
        (zeichne-Platz spiel (cdr indizes) (beside drawing (zeichne-box spiel (car indizes))))))

(zeichne-spiel spiel)