#lang racket

;Solution of exercise sheet 5,

;author:

;Dewin Bagci, 6815336
;Josua Spisak, 6944594
;Lina Kaine, 6499396

;;;1

;; we chose a list with every attribute of the butterfly

(define (butterfly farbe1 farbe2 fuehlerform1 fuehlerform2 muster1 muster2 fluegelform1 fluegelform2)
  (list  farbe1 farbe2
         fuehlerform1 fuehlerform2
         muster1 muster2
         fluegelform1 fluegelform2))




;this function returns the dominant attributes of the given butterfly
(define (getVisible butterfly)
  (string-append (getVisibleFarbe butterfly) (getVisibleFuehler butterfly) (getVisibleMuster butterfly) (getVisibleFluegel butterfly)))

;this function returns the dominant color of the the given butterfly
(define (getVisibleFarbe butterfly)
  (cond [(equal? "blue" (car(member "blue" butterfly))) "blue "]
        [(equal? "green" (car (member "green" butterfly))) "green "]
        [(equal? "yellow" (car (member "yellow" butterfly))) "yellow "]
        [else "red "]))

;this function returns the dominant Fuehler of the the given butterfly
(define (getVisibleFuehler butterfly)
  (cond [(equal? "gekruemmt" (car (member "gekruemmt" butterfly))) "gekruemmt "]
        [(equal? "geschweift" (car (member "geschweift" butterfly))) "geschweift "]
        [else "gerade "]))

;this function returns the dominant Muster of the the given butterfly
(define (getVisibleMuster butterfly)
  (cond [(equal? "sterne" (car (member "sterne" butterfly))) "sterne "]
        [(equal? "punkte" (car (member "punkte" butterfly))) "punkte "]
        [else "streifen "]))

;this function returns the dominant Fluegel of the the given butterfly
(define (getVisibleFluegel butterfly)
  (cond [(equal? "elliptisch" (car (member "elliptisch" butterfly))) "elliptisch"]
        [(equal? "rhombisch" (car (member "rhombisch" butterfly))) "rhombisch"]
        [else "hexagonal"]))

;exmaple use of getVisible with a butterfly
(getVisible (butterfly "blue" "red" "gerade" "gekruemmt" "sterne" "punkte" "elliptisch" "hexagonal"))

