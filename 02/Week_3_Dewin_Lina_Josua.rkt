#lang racket

;Solution of exercise sheet 2,

;author:

;Dewin Bagci, 6815336
;Josua Spisak, 6944594
;Lina Kaine, 6499396

;[1 Symbole und Werte, Umgebungen]

;TODO

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

;TODO
       
      
