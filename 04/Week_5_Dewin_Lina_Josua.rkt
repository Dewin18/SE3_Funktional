#lang racket

;Solution of exercise sheet 4,

;author:

;Dewin Bagci, 6815336
;Josua Spisak, 6944594
;Lina Kaine, 6499396

;;;1.

;Die Klammern werden von innen nach außen ausgewertet, deshalb ist das Minimum von 5 und 1 = 1. Das Maximum von 1 und 6 ergibt dann 6.
(max(min 5(- 8 7)) 6)

;Quote stops the function from evaluating. 
'(+ ,(- 13 11) 17)

;prints the first element from the tail of the list.
(cadr '(Good King Wenceslas))

;the nested list (On the feast of Steven) is the third element in the list looked out.
;We kick out the first three elements so get an empty list back.
(cdddr '(looked out (On the feast of Steven)))

;a pair is formed between the symbol 'When and the list (the snow lay round about).
(cons 'When '(the snow lay round about))

;since 'crisp is the second part of the pair, we get a pair instead of a combined list.
(cons '(Deep and) 'crisp)

;the second part '(and even) is equivalent to (list 'and 'even). 
(equal? (list 'and 'even) '(and even))

;in '(the 'red-nosed 'reindeer) we have doubled the quotes for red-nosed and reindeer. this is why the lists arent equal.
(eq? (list 'Rudolph 'the 'red-nosed 'reindeer) (cons 'Rudolph '(the 'red-nosed 'reindeer)))
;for better visibility
(list 'Rudolph 'the 'red-nosed 'reindeer)
(cons 'Rudolph '(the 'red-nosed 'reindeer))


;;;2.
;; the message with all of it parts like the uberschrift(title) and so on
;; this function shows the grammar we use together with the function we call here
;; this function can be used to generate the Message according to our grammar with the right input (below are a few test cases)
(define (funkspruch shipname callsign position caseOfEmergency notes)
  (string-append
   (ueberschrift shipname callsign) "\n"           ;with the backslash n we make sure that the text is formated
   (positionInformation position) "\n"             ;we use functions for most of the parts for clarity if the text of one part is alredy obvious we dont use a function
   caseOfEmergency "\n"
   notes "\n"
   "ICH SENDE DEN TRÄGER --" "\n"
   (getKeyString callsign) "\n"                     
   "OVER \n"))

;the function the genereate the title with 2 inputs shipname and callsign. we decided to use HIER IST instead of DE or DELTA ECHO
(define (ueberschrift shipname callsign) 
  (string-append
   "MAYDAY MAYDAY MAYDAY \n"
   "HIER IST " "\n"
   shipname " " shipname " " shipname " " (getKeyString callsign) "\n"    ;getKeyString is a function from the last exercise wich we reuse here
   "MAYDAY " shipname " ICH BUCHSTABIERE " (getKeyString shipname) "\n"
   "RUFZEICHEN " (getKeyString callsign)))

;the function generates the text wich gives information about our position
(define (positionInformation position)
  (string-append
   "NOTFALLPOSITION UNGEFÄHR " position))
;the list of pairs we used last exercie wich we need again for some parts of the message
(define buchstabiertafel (list (cons "A" "ALPHA")
                               (cons "B" "BRAVO")
                               (cons "C" "CHARLIE")
                               (cons "D" "DELTA")
                               (cons "E" "ECHO")
                               (cons "F" "FOXTROT")
                               (cons "G" "GOLF")
                               (cons "H" "HOTEL")
                               (cons "I" "INDIA")
                               (cons "J" "JULIET")
                               (cons "K" "KILO")
                               (cons "L" "LIMA")
                               (cons "M" "MIKE")
                               (cons "N" "NOVEMBER")
                               (cons "O" "OSCAR")
                               (cons "P" "PAPA")
                               (cons "Q" "QUEBEC")
                               (cons "R" "ROMEO")
                               (cons "S" "SIERRA")
                               (cons "T" "TANGO")
                               (cons "U" "UNIFORM")
                               (cons "V" "VIKTOR")
                               (cons "W" "WHISKEY")
                               (cons "X" "X-RAY")
                               (cons "Y" "YANKEE")
                               (cons "Z" "ZULU")
                               (cons "0" "NADAZERO")
                               (cons "1" "UNAONE")
                               (cons "2" "DUOTWO")
                               (cons "3" "TERRATHREE")
                               (cons "4" "CARREFOUR")
                               (cons "5" "PENTAFIVE")
                               (cons "6" "SOXISIX")
                               (cons "7" "SETTESEVEN")
                               (cons "8" "OKTOEIGHT")
                               (cons "9" "NOVONINE")
                               (cons "," "DECIMAL")
                               (cons "." "STOP")))
;function from last exercise
(define (getKey input)
  (cdr (assoc input buchstabiertafel)))
;function from last exercise
(define (getKeyString input)
  (if (< 0 (string-length input))
      (let ([currentChar (substring input 0 1)]   ;we declare a currentchar as the first character of a string
            [input (substring input 1)])          ;we declare a new input wich is the input from last time without the first element
        (string-append (string-append (getKey currentChar) " ") (getKeyString input)))   ;we combine our current char with "" to form a string and combine this string with the string 
                                                                                         ;that will be genrated from this function wtih the new input
        " "))

(display (funkspruch "UNICORN" "UCRN" "5 SM NORDWESTLICH LEUCHTTURM ROTER SAND" "SCHWERE SCHLAGSEITE WIR SINKEN" "KEINE VERLETZTEN"))
;(display (funkspruch "NAUTILUS" "DEYJ" "10 SM OESTLICH POINT NEMO 48° 52’ 31,75” S, 123° 23’ 33,07“ W" "EINE RIESENKRAKE HAT DAS SCHIFF UMSCHLUNGEN, EIN GROßES LECK IM RUMPF" "20 PERSONEN AN BOARD, TREIBEN ANTRIEBSLOS AN DER WASSEROBERFLAECHE"))
;(display (funkspruch "MALTESEFALCON" "HUQ9" "N 54° 34’ 5,87”, E 8° 27’ 33,41" "AUF SANDBANK AUFGELAUFEN" "10 MANN AN BORD, DAS SCHIFF IST 88M LANG, SCHWARZER RUMPF, UNFALLZEIT 0730 UTC."))


;;;3.
;;1
;the difference beetween inner and outer reduction is the sequenze in wich we evaluate a complex expression.
;in the example (hoch3 (+ 3 (hoch3 3))) the first thing we do in inner reduction is look at (hoch3 3) wich is 27. Then we look at (+ 3 27) wich is 30 and then we finally look at (hoch3 30) wich is 27000.
;However if we use outer reduction we would first look at (hoch3 (+ 3 (hoch3 3))) wich results in (* (+ 3 (hoch3 3)) (+ 3 (hoch3 3)) (+ 3 (hoch3 3))) after the next few steps we have
;(* (+ 3 (* 3 3 3))(+ 3 (* 3 3 3))(+ 3 (* 3 3 3)) and then we get (* (+ 3 27)(+ 3 27)(+ 3 27)) leading to (* 30 30 30) and we finaly get 27000

(define (hoch3 x)
  (* x x x))

(hoch3 (+ 3 (hoch3 3)))
;;2
;for special expressions rackets evalautes strictly from left to right

;;3
(define (new-if condition? then-clause else-clause)
 (cond (condition? then-clause)
       (else else-clause)))
(define (faculty product counter max-count)

  (new-if (> counter max-count)
         product
         (faculty (* counter product)
                  (+ counter 1)
                  max-count)))
;(faculty 1 1 5)
;wenn man versucht dieses programm zu benutzen übergeben wir zuerst > 1 5 an unsere new if funktion mit
;then-clause als 1 und else-clause als rekursiver ausdruck von faculty mit product als * counter product
;und counter als + 1 counterund dem vorherigen max-count. in der new-if funktion prüfen wir dann unsere
; condition also > 1 5 also führen wir die else clause aus. allerdings werten wir direckt schon alle
;inputs aus also prüfen wir nicht erst ob die neue condition stimmt sondern machen direckt weiter und
;versuchen die then und die else clause auzuwerten was leider unendlich lange dauert um dies zu verhindern
;brachen wir hier spezialformen ein anderes beispiel wäre ' womit wir ja eine auswertung verhindern wollen
;würde dies erst nach der auswertung bemerkt werden gäbe es ein problem

;;4
;TODO
 
