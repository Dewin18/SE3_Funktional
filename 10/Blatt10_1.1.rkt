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
(define (xthElementFromList list x)
  (cond [(= x 0) (car list)]
        [(= x 1) (second list)]
        [else (car (drop list  x ))]))

(define (spiel->eintraege spiel indexs)
  (let ([d (car indexs)]
        [s (vector->list spiel)])
  (list->vector (map (curry xthElementFromList s) indexs))))

(define spiel #(0 0 0 0 0 9 0 7 0
                0 0 0 0 8 2 0 5 0
                3 2 7 0 0 0 0 4 0
                0 1 6 0 4 0 0 0 0
                0 5 0 0 0 0 3 0 0
                0 0 0 0 9 0 7 0 0
                0 0 0 6 0 0 0 0 5
                8 0 2 0 0 0 0 0 0
                0 0 4 2 0 0 0 0 8))
(spiel->eintraege spiel (quadrant->indizes 8))
(spiel->eintraege spiel (spalte->indizes 8))
(spiel->eintraege spiel (zeile->indizes 8))

;4

(define (spiel-konsistent? spiel)
  (and (konsistent? spiel 8 spalte->indizes) (konsistent? spiel 8 zeile->indizes)(konsistent? spiel 8 quadrant->indizes)))

(define (konsistent? spiel n f)
  (if (= n 0)
      (not (check-duplicates (filter (curry < 0) (vector->list(spiel->eintraege spiel (f n))))))
      (and (not (check-duplicates (filter (curry < 0)(vector->list (spiel->eintraege spiel (f n)))))) (konsistent? spiel (- n 1) f))))

(spiel-konsistent? spiel)

